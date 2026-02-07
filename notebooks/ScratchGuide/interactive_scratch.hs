{-
  Interactive Scratch-like Program in Haskell
  
  This program demonstrates Scratch blocks with proper interactive input
  that works in a terminal but not in IHaskell/Jupyter.
  
  To run this program:
  1. Save as interactive_scratch.hs
  2. Compile: ghc interactive_scratch.hs
  3. Run: ./interactive_scratch
  
  Or run directly: runhaskell interactive_scratch.hs
-}

import Control.Monad (forever, when, unless)
import Control.Concurrent (threadDelay)
import Data.IORef
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Random (randomRIO)
import Data.Char (toLower)

-- ============================================
-- Scratch-like Variables (using IORef)
-- ============================================

data GameState = GameState
  { xPos :: IORef Int
  , yPos :: IORef Int
  , score :: IORef Int
  , gameRunning :: IORef Bool
  }

-- ============================================
-- Scratch Block Equivalents
-- ============================================

-- goto x:_ y:_
gotoXY :: GameState -> Int -> Int -> IO ()
gotoXY state x y = do
  writeIORef (xPos state) x
  writeIORef (yPos state) y
  putStrLn $ "Moved to (" ++ show x ++ ", " ++ show y ++ ")"

-- change score by _
changeScoreBy :: GameState -> Int -> IO ()
changeScoreBy state delta = do
  modifyIORef (score state) (+ delta)
  newScore <- readIORef (score state)
  putStrLn $ "Score: " ++ show newScore

-- set score to _
setScore :: GameState -> Int -> IO ()
setScore state value = do
  writeIORef (score state) value
  putStrLn $ "Score set to " ++ show value

-- pick random _ to _
pickRandom :: Int -> Int -> IO Int
pickRandom low high = randomRIO (low, high)

-- repeat until
repeatUntil :: IO Bool -> IO () -> IO ()
repeatUntil condition action = do
  action
  done <- condition
  unless done $ repeatUntil condition action

-- ============================================
-- Interactive Game Loop
-- ============================================

-- Main game loop with keyboard input
gameLoop :: GameState -> IO ()
gameLoop state = do
  running <- readIORef (gameRunning state)
  when running $ do
    -- Display current state
    x <- readIORef (xPos state)
    y <- readIORef (yPos state)
    s <- readIORef (score state)
    
    putStrLn "\n=========================================="
    putStrLn $ "Position: (" ++ show x ++ ", " ++ show y ++ ")"
    putStrLn $ "Score: " ++ show s
    putStrLn "=========================================="
    putStrLn "Commands:"
    putStrLn "  w/a/s/d - Move up/left/down/right"
    putStrLn "  r - Pick random position"
    putStrLn "  + - Increase score"
    putStrLn "  - - Decrease score"
    putStrLn "  0 - Reset to origin (0,0)"
    putStrLn "  q - Quit"
    putStrLn "=========================================="
    putStr "Enter command: "
    
    -- Get single character input
    c <- getChar
    putStrLn ""  -- newline after input
    
    -- Process command
    case toLower c of
      'w' -> do  -- Move up
        modifyIORef (yPos state) (+ 10)
        newY <- readIORef (yPos state)
        putStrLn $ "Moved up! Y = " ++ show newY
        
      's' -> do  -- Move down
        modifyIORef (yPos state) (subtract 10)
        newY <- readIORef (yPos state)
        putStrLn $ "Moved down! Y = " ++ show newY
        
      'a' -> do  -- Move left
        modifyIORef (xPos state) (subtract 10)
        newX <- readIORef (xPos state)
        putStrLn $ "Moved left! X = " ++ show newX
        
      'd' -> do  -- Move right
        modifyIORef (xPos state) (+ 10)
        newX <- readIORef (xPos state)
        putStrLn $ "Moved right! X = " ++ show newX
        
      'r' -> do  -- Random position
        randX <- pickRandom (-100) 100
        randY <- pickRandom (-100) 100
        gotoXY state randX randY
        
      '+' -> changeScoreBy state 1
      
      '-' -> changeScoreBy state (-1)
      
      '0' -> do  -- Reset to origin
        gotoXY state 0 0
        putStrLn "Reset to origin!"
        
      'q' -> do  -- Quit
        writeIORef (gameRunning state) False
        putStrLn "Goodbye!"
        
      _ -> putStrLn "Invalid command!"
    
    threadDelay 500000  -- 0.5 second delay
    gameLoop state

-- ============================================
-- Demo Programs (Scratch-like Scripts)
-- ============================================

-- Demo 1: Repeat until score reaches 5
demo1 :: GameState -> IO ()
demo1 state = do
  putStrLn "\n=== Demo 1: Repeat Until Score = 5 ==="
  setScore state 0
  
  repeatUntil checkDone $ do
    currentScore <- readIORef (score state)
    putStrLn $ "Current score: " ++ show currentScore
    
    -- Pick random 1 to 3
    random <- pickRandom 1 3
    putStrLn $ "Rolled: " ++ show random
    
    -- If random > 1 then change score by 1
    when (random > 1) $ do
      changeScoreBy state 1
    
    threadDelay 1000000  -- 1 second
  
  putStrLn "Demo complete! Score reached 5."
  where
    checkDone = do
      s <- readIORef (score state)
      return (s >= 5)

-- Demo 2: Move in a square pattern
demo2 :: GameState -> IO ()
demo2 state = do
  putStrLn "\n=== Demo 2: Move in Square Pattern ==="
  gotoXY state 0 0
  threadDelay 1000000
  
  -- Move right
  gotoXY state 50 0
  threadDelay 1000000
  
  -- Move up
  gotoXY state 50 50
  threadDelay 1000000
  
  -- Move left
  gotoXY state 0 50
  threadDelay 1000000
  
  -- Move down (back to start)
  gotoXY state 0 0
  threadDelay 1000000
  
  putStrLn "Square complete!"

-- Demo 3: Forever loop (limited to 5 iterations for demo)
demo3 :: GameState -> IO ()
demo3 state = do
  putStrLn "\n=== Demo 3: Forever Loop (5 iterations) ==="
  setScore state 0
  
  sequence_ $ replicate 5 $ do
    s <- readIORef (score state)
    putStrLn $ "Hello " ++ show s
    changeScoreBy state 1
    threadDelay 1000000

-- ============================================
-- Main Program
-- ============================================

main :: IO ()
main = do
  -- Set unbuffered input for real-time character reading
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  putStrLn "=========================================="
  putStrLn "  Scratch-like Interactive Program"
  putStrLn "=========================================="
  
  -- Initialize game state (when green flag clicked)
  gameState <- GameState
    <$> newIORef 0      -- xPos
    <*> newIORef 0      -- yPos
    <*> newIORef 0      -- score
    <*> newIORef True   -- gameRunning
  
  putStrLn "\nChoose a mode:"
  putStrLn "1. Run automated demos"
  putStrLn "2. Interactive game loop"
  putStr "Enter choice (1 or 2): "
  
  choice <- getChar
  putStrLn "\n"
  
  case choice of
    '1' -> do
      -- Run demos
      demo1 gameState
      threadDelay 2000000
      
      demo2 gameState
      threadDelay 2000000
      
      demo3 gameState
      
      putStrLn "\nAll demos complete!"
      
    '2' -> do
      -- Run interactive game loop
      putStrLn "Starting interactive mode..."
      threadDelay 1000000
      gameLoop gameState
      
    _ -> putStrLn "Invalid choice. Exiting."
  
  putStrLn "\nProgram finished!"
