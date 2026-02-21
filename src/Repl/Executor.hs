module Repl.Executor (
  replDefine,
  replRun,
  replExecute,
  replInspect,
  replIsComplete
) where

import qualified Prelude ()
import MHSPrelude

import MicroHs.Ident (mkIdent)
import MicroHs.SymTab (stLookup, Entry(..))
import MicroHs.Expr (showExpr)
import MicroHs.TypeCheck (Symbols)
import System.IO (putStrLn)

import Repl.Context
import Repl.Error
import Repl.Utils
import Repl.Analysis
import Repl.Compiler

data MetaCommand
  = TypeOfExpr String
  | KindOfType String

replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet = do
  let snippet' = ensureTrailingNewline snippet
  case appendDefinition ctx snippet' of
    Left err -> pure (Left err)
    Right defsWithNew -> do
      cm <- compileModule ctx (moduleFromDefs defsWithNew)
      case cm of
        Left err'          -> pure (Left err')
        Right (_, cache', syms') -> pure (Right ctx{ rcDefs = defsWithNew, rcCache = cache', rcSyms = syms' })

replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt = do
  let block = runBlock stmt
      src = moduleSourceWith ctx block
  cm <- compileModule ctx src
  case cm of
    Left err -> pure (Left err)
    Right (cmdl, cache', syms') -> do
      r <- runAction cache' cmdl runResultIdent
      case r of
        Left e  -> pure (Left e)
        Right _ -> pure (Right ctx{ rcCache = cache', rcSyms = syms' })

replIsComplete :: ReplCtx -> String -> IO String
replIsComplete ctx snippet = do
  if all isws snippet then pure "complete" else do
    let ls = lines (ensureTrailingNewline snippet)
        go n
          | n < 0 = pure "invalid"
          | otherwise = do
              let (defLines, runLines) = splitAt n ls
                  defPart = unlines defLines
                  runPart = unlines (dropWhileEnd allwsLine runLines)
                  candidateDefs = currentDefsSource ctx ++ defPart
              if canParseDefinition candidateDefs
                then if all allwsLine runLines
                     then pure "complete"
                     else if canParseExpression runPart || canParseExpression ("do\n" ++ indent runPart)
                          then pure "complete"
                          else go (n - 1)
                else go (n - 1)
    res <- go (length ls)
    if res == "invalid" && isIncomplete snippet
      then pure "incomplete"
      else pure res

replInspect :: ReplCtx -> String -> IO (Either ReplError String)
replInspect ctx name = do
  let ident = mkIdent name
      (typeTable, valueTable) = rcSyms ctx
  case stLookup "value" ident valueTable of
    Right (Entry _ sigma) -> return (Right $ name ++ " :: " ++ showExpr sigma)
    Left _ -> case stLookup "type" ident typeTable of
      Right (Entry _ kind) -> return (Right $ name ++ " :: " ++ showExpr kind)
      Left _ -> return (Left $ ReplRuntimeError $ "Identifier not found: " ++ name)

replExecute :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replExecute ctx snippet
  | Just cmd <- parseMetaCommand snippet = runMetaCommand ctx cmd
replExecute ctx snippet = do
  let ls = lines (ensureTrailingNewline snippet)
      go n
        | n < 0 = pure $ Left (ReplParseError "unable to parse snippet")
        | otherwise = do
            let (defLines, runLines) = splitAt n ls
                defPart = unlines defLines
                runPart = unlines (dropWhileEnd allwsLine runLines)
                candidateDefs = currentDefsSource ctx ++ defPart
            if canParseDefinition candidateDefs
              then if all allwsLine runLines
                   then replDefine ctx defPart
                   else if canParseExpression runPart
                        then do
                          eCtx' <- replDefine ctx defPart
                          case eCtx' of
                            Left err -> pure (Left err)
                            Right ctx' -> replRun ctx' runPart
                        else
                          let runBlock = "do\n" ++ indent runPart
                          in if canParseExpression runBlock
                             then do
                               eCtx' <- replDefine ctx defPart
                               case eCtx' of
                                 Left err -> pure (Left err)
                                 Right ctx' -> replRun ctx' runBlock
                             else go (n - 1)
              else go (n - 1)
  go (length ls)

runMetaCommand :: ReplCtx -> MetaCommand -> IO (Either ReplError ReplCtx)
runMetaCommand ctx cmd =
  case cmd of
    TypeOfExpr expr -> replTypeOf ctx expr
    KindOfType ty -> replKindOf ctx ty

replTypeOf :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replTypeOf ctx expr =
  let expr' = trimWs expr
      probeName = "xhReplTypeProbe"
      probeIdent = mkIdent probeName
      probeDef = unlines
        [ probeName ++ " = ("
        , indent expr'
        , "  )"
        ]
      src = moduleSourceWith ctx probeDef
  in if null expr'
       then pure (Left (ReplRuntimeError ":type expects an expression"))
       else do
         cm <- compileModule ctx src
         case cm of
           Left err -> pure (Left err)
           Right (_, _, (_, valueTable)) ->
             case stLookup "value" probeIdent valueTable of
               Right (Entry _ sigma) -> do
                 putStrLn ("> " ++ expr' ++ " :: " ++ showExpr sigma)
                 pure (Right ctx)
               Left _ ->
                 pure (Left (ReplRuntimeError "failed to infer expression type"))

replKindOf :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replKindOf ctx ty =
  let ty' = trimWs ty
      probeName = "XhReplKindProbe"
      probeIdent = mkIdent probeName
      probeDef = "type " ++ probeName ++ " = " ++ ty' ++ "\n"
      src = moduleSourceWith ctx probeDef
  in if null ty'
       then pure (Left (ReplRuntimeError ":kind expects a type"))
       else do
         cm <- compileModule ctx src
         case cm of
           Left err -> pure (Left err)
           Right (_, _, (typeTable, _)) ->
             case stLookup "type" probeIdent typeTable of
               Right (Entry _ kind) -> do
                 putStrLn ("> " ++ ty' ++ " :: " ++ showExpr kind)
                 pure (Right ctx)
               Left _ ->
                 pure (Left (ReplRuntimeError "failed to infer type kind"))

parseMetaCommand :: String -> Maybe MetaCommand
parseMetaCommand raw =
  let snippet = trimWs raw
  in case parseWith ":type" TypeOfExpr snippet of
       Just cmd -> Just cmd
       Nothing -> parseWith ":kind" KindOfType snippet
  where
    parseWith keyword ctor snippet =
      if startsWith keyword snippet && hasBoundary keyword snippet
        then Just (ctor (drop (length keyword) snippet))
        else Nothing

    hasBoundary keyword snippet =
      let rest = drop (length keyword) snippet
      in null rest || isws (head rest)

startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix

trimWs :: String -> String
trimWs = dropWhile isws . dropWhileEnd isws
