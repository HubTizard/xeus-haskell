{-
  Scratch-like Programming for xeus-haskell MicroHS Console
  
  This version is optimized for the xeus-haskell MicroHS console.
  Pure functional programming without mutable state.
  
  HOW TO USE THIS FILE IN XEUS-HASKELL MICROHS CONSOLE:
  =====================================================
  
  The MicroHS console does NOT support :load commands!
  
  Instead, COPY-PASTE code sections directly into the console:
  
  1. Open this file in a text editor
  2. Copy a section (e.g., "Define GameState")
  3. Paste into the MicroHS console
  4. Press Enter
  5. Test the functions
  6. Continue with next section
  
  IMPORTANT: Work incrementally - one section at a time!
  
  TYPE CHECKING WITH :t
  =====================
  
  The :type command IS AVAILABLE in xeus-haskell MicroHS console!
  
  Use :type to check types:
  
  1. Check value types:
     :type 5 -- 5 is type class Num (Int, Integer etc)
     :type 3.14 -- 3.14 is type class Fractional (Float, Double, Rational etc)
     :type "hello" -- "hello" is of type class IsString (String, Text etc)
  
  2. Check function type signatures:
     :type (2+) -- a function that maps from a Num to a Num
     :type map -- a function that takes a function and list as arguments, and applies the function to each element
     :type moveRight -- after definition below
  
  3. Check complex expressions after definition below:
     :type moveRight initialState
     :typerepeatN 5 moveRight
  
  ALTERNATIVE: Explicit type annotations (for clarity):
  
   1.   x = 5 :: Int
         :type x
     
   2.   f = (2+) :: Int -> Int
	:type f
  
  LEARNING FROM TYPE ERRORS:
  
     -- Try invalid operations to understand the type system:
     -- "hello" + 5  -- Error will show type mismatch
     -- moveRight 5  -- Error will show GameState expected
  
  Reference: Based on Scratch visual programming concepts
-}

-- ============================================
-- SECTION 1: TYPE DEFINITIONS
-- ============================================
-- Copy this section first

-- Game state (immutable)
data GameState = GameState
  { xPos :: Int
  , yPos :: Int
  , score :: Int
  } deriving (Show)

-- Custom type synonyms (not in Scratch)
type Apple = Int
type Pen = Int
type Pear = Int
type Chair = Int

-- Test with explicit types
testApple = 5 :: Apple
:type testApple

-- ============================================
-- SECTION 2: INITIAL STATE
-- ============================================
-- Copy after Section 1

-- "When green flag clicked" - initial state
initialState = GameState 0 0 0

-- Test it:
initialState


-- ============================================
-- SECTION 3: BASIC MOVEMENT (Scratch motion blocks)
-- ============================================
-- Copy after Section 2

-- Move right (like "move 10 steps" in Scratch)
moveRight state = state { xPos = xPos state + 10 }

-- Move left
moveLeft state = state { xPos = xPos state - 10 }

-- Move up
moveUp state = state { yPos = yPos state + 10 }

-- Move down
moveDown state = state { yPos = yPos state - 10 }

-- Move specific number of steps
moveSteps n state = state { xPos = xPos state + n }

-- Test with explicit types:
testMove = moveRight :: GameState -> GameState
moveRight initialState


-- ============================================
-- SECTION 4: POSITION OPERATIONS
-- ============================================
-- Copy after Section 3

-- Goto x:y (like Scratch "goto x:_ y:_")
let gotoXY x y state = state { xPos = x, yPos = y }

-- Get current position
let getX state = xPos state
let getY state = yPos state

-- Test:
-- gotoXY 50 50 initialState


-- ============================================
-- SECTION 5: SCORE OPERATIONS (Scratch variables)
-- ============================================
-- Copy after Section 4

-- Change score by (like Scratch "change score by 1")
let changeScoreBy delta state = state { score = score state + delta }

-- Set score to (like Scratch "set score to 0")
let setScore value state = state { score = value }

-- Get score
let getScore state = score state

-- Test:
-- changeScoreBy 10 initialState
-- setScore 100 initialState


-- ============================================
-- SECTION 6: CONTROL FLOW - REPEAT
-- ============================================
-- Copy after Section 5

-- Repeat N times (like Scratch "repeat 10")
let repeatN 0 f x = x
let repeatN n f x = repeatN (n-1) f (f x)

-- Test with type annotation:
-- let testRepeat = repeatN :: Int -> (a -> a) -> a -> a
-- repeatN 5 moveRight initialState


-- ============================================
-- SECTION 7: CONTROL FLOW - REPEAT UNTIL
-- ============================================
-- Copy after Section 6

-- Repeat until condition (like Scratch "repeat until")
let repeatUntil condition action maxIters x = 
    if maxIters <= 0 then x
    else if condition x then x
    else repeatUntil condition action (maxIters - 1) (action x)

-- Test:
-- let atX100 state = xPos state >= 100
-- repeatUntil atX100 moveRight 100 initialState


-- ============================================
-- SECTION 8: CONTROL FLOW - IF THEN
-- ============================================
-- Copy after Section 7

-- If-then (like Scratch "if then")
let ifThen condition action state = 
    if condition state then action state else state

-- Test:
-- let scoreHigh state = score state > 50
-- ifThen scoreHigh (changeScoreBy 10) (setScore 60 initialState)


-- ============================================
-- SECTION 9: UTILITY FUNCTIONS
-- ============================================
-- Copy after Section 8

-- Display state nicely
let displayState state = 
    "Position: (" ++ show (xPos state) ++ ", " ++ show (yPos state) ++ 
    "), Score: " ++ show (score state)

-- Execute a list of commands
let executeCommands [] state = state
let executeCommands (cmd:cmds) state = 
    executeCommands cmds (cmd state)

-- Test:
-- displayState initialState
-- displayState (moveRight initialState)


-- ============================================
-- SECTION 10: DEMO PROGRAMS
-- ============================================
-- Copy after Section 9

-- Demo 1: Simple movement
let demo1 = moveRight initialState

-- Demo 2: Multiple movements
let demo2 = repeatN 5 moveRight initialState

-- Demo 3: Move in square
let demo3 = 
    let s0 = initialState
        s1 = gotoXY 0 0 s0
        s2 = gotoXY 50 0 s1
        s3 = gotoXY 50 50 s2
        s4 = gotoXY 0 50 s3
        s5 = gotoXY 0 0 s4
    in s5

-- Demo 4: Repeat until score reaches 10
let demo4 = 
    let atScore10 state = score state >= 10
    in repeatUntil atScore10 (changeScoreBy 1) 100 initialState

-- Demo 5: Complex pattern
let demo5 = 
    let s0 = initialState
        s1 = setScore 0 s0
        s2 = repeatN 3 moveRight s1
        s3 = repeatN 2 moveUp s2
        s4 = changeScoreBy 10 s3
        s5 = gotoXY 0 0 s4
    in s5

-- Test demos:
-- demo1
-- demo2
-- displayState demo3
-- displayState demo4
-- displayState demo5


-- ============================================
-- SECTION 11: COMMAND SEQUENCES
-- ============================================
-- Copy after Section 10

-- Create reusable command sequences
let sequence1 = [moveRight, moveRight, moveUp, changeScoreBy 5]

let sequence2 = [moveRight, moveUp, changeScoreBy 10, moveLeft, moveDown]

-- Test:
-- executeCommands sequence1 initialState
-- displayState (executeCommands sequence2 initialState)


-- ============================================
-- SECTION 12: CURRIED FUNCTIONS (Gattegno)
-- ============================================
-- Copy after Section 11

-- "2 apples plus 3 apples equals 5 apples"
let twoApples = 2 :: Apple
let threeApples = 3 :: Apple
let fiveApples = twoApples + threeApples

-- Same with pens
let twoPens = 2 :: Pen
let threePens = 3 :: Pen
let fivePens = twoPens + threePens

-- Curried addition
let add2 = (2+)
let add3 = (3+)

-- Test:
-- fiveApples
-- fivePens
-- add2 5
-- add3 7

-- Type annotation workaround:
-- let add2Typed = (2+) :: Int -> Int


-- ============================================
-- SECTION 13: FUNCTION COMPOSITION
-- ============================================
-- Copy after Section 12

-- Compose additions (like Scratch nested blocks)
let add5 = (3+) . (2+)

-- Compose movements
let moveRightUp = moveUp . moveRight
let moveLeftDown = moveDown . moveLeft

-- Test:
-- add5 0
-- moveRightUp initialState
-- displayState (moveRightUp initialState)


-- ============================================
-- SECTION 14: HIGHER-ORDER FUNCTIONS
-- ============================================
-- Copy after Section 13

-- Map: apply function to each element
let applyToAll f list = map f list

-- Filter: keep only matching elements
let keepMatching pred list = filter pred list

-- Examples with explicit types:
-- let doubled = map (2*) [1,2,3,4] :: [Int]
-- let evens = filter even [1,2,3,4,5,6] :: [Int]

-- Test:
-- map (2+) [1,2,3,4]
-- filter even [1,2,3,4,5,6]
-- foldr (+) 0 [1,2,3,4,5]


-- ============================================
-- SECTION 15: ROD ARITHMETIC (Gattegno)
-- ============================================
-- Copy after Section 14

-- Cuisenaire rods
let white = 1
let red = 2
let green = 3
let pink = 4
let yellow = 5
let darkGreen = 6

-- Train equations (rod + rod = rod)
let trainEq1 = red + green == yellow
let trainEq2 = white + green == pink
let trainEq3 = red + pink == darkGreen

-- Test:
-- trainEq1
-- trainEq2
-- red + green


-- ============================================
-- SECTION 16: PATTERN MATCHING
-- ============================================
-- Copy after Section 15

-- Factorial (recursive pattern)
let factorial 0 = 1; factorial n = n * factorial (n-1)

-- List sum
let listSum [] = 0; listSum (x:xs) = x + listSum xs

-- Count down
let countdown 0 = [0]; countdown n = n : countdown (n-1)

-- Test:
-- factorial 5
-- listSum [1,2,3,4,5]
-- countdown 5


-- ============================================
-- SECTION 17: TYPE CHECKING WITH :t
-- ============================================
-- Copy this section to explore types

-- Use :t to check types directly:
-- :t 5
-- :t 3.14
-- :t "hello"
-- :t (2+)
-- :t map
-- :t moveRight

-- You can also use explicit annotations for clarity:

-- Basic types
let intExample = 42 :: Int
let doubleExample = 3.14 :: Double
let stringExample = "hello" :: String
let boolExample = True :: Bool

-- Function types (annotate before definition)
let addInts = \x y -> x + y :: Int -> Int -> Int
let isEven = \x -> x `mod` 2 == 0 :: Int -> Bool

-- State transformation types
let moveRightTyped = moveRight :: GameState -> GameState
let gotoXYTyped = gotoXY :: Int -> Int -> GameState -> GameState

-- Test to understand types with :t:
-- :t moveRightTyped
-- :t gotoXYTyped

-- Learning from type errors:
-- Try invalid operations to see type mismatches:
-- "hello" + 5  -- Error shows type mismatch
-- moveRight 5  -- Error shows GameState expected


-- ============================================
-- USAGE EXAMPLES
-- ============================================

{-
COMPLETE USAGE EXAMPLE:

Copy sections 1-9 first to set up basic functions.

Then try these in the console:

-- Basic movement
demo1
demo2
displayState demo3

-- Score manipulation
let s1 = changeScoreBy 5 initialState
let s2 = changeScoreBy 3 s1
displayState s2

-- Repeat patterns
repeatN 3 moveRight initialState
displayState (repeatN 5 moveRight initialState)

-- Repeat until
let atX50 state = xPos state >= 50
repeatUntil atX50 moveRight 100 initialState

-- Command sequences
let myCommands = [moveRight, moveUp, changeScoreBy 10]
executeCommands myCommands initialState

-- Function composition
let moveDiagonal = moveUp . moveRight
displayState (repeatN 3 moveDiagonal initialState)

-- Curried functions
add2 10
add5 20
(2+) 3

-- Rod arithmetic
red + green
trainEq1

-- Check types with :t (it works!)
:t 5
:t (2+)
:t moveRight
:t map

-- Explore complex type signatures
:t repeatN
:t executeCommands

-- Use explicit annotations for clarity if desired:
let testFunc = moveRight :: GameState -> GameState
-}


-- ============================================
-- TROUBLESHOOTING
-- ============================================

{-
COMMON ISSUES:

1. Parse error with :load
   → Solution: Don't use :load, copy-paste instead

2. Multi-line definition fails
   → Solution: Use semicolons:
     let f 0 = 1; f n = n * f (n-1)

3. Want to check types
   → Solution: Use :t command:
     :t 5
     :t moveRight
     :t map

4. Undefined value
   → Solution: Make sure previous sections are loaded

5. Type mismatch
   → Solution: Use :t to debug:
     :t yourExpression
     Check each part of complex expressions

DEBUGGING WITH :t:

Break complex expressions into parts and check each:

let result = moveRight (addScore 10 initialState)

-- Check each component:
:t moveRight
:t addScore
:t addScore 10
:t initialState
:t addScore 10 initialState
:t moveRight (addScore 10 initialState)

LEARNING FROM TYPE ERRORS:

Type errors are valuable learning tools:

-- These should fail with informative type errors:
"hello" + 5        -- Shows String vs Num
moveRight 5        -- Shows Int vs GameState
initialState ++ [] -- Shows GameState vs List

The error messages tell you exactly what types are expected!
-}


-- ============================================
-- SUMMARY OF SCRATCH MAPPINGS
-- ============================================

{-
SCRATCH BLOCK          → HASKELL EQUIVALENT

Variables:
set var to 0          → let x = 0
change var by 1       → x + 1 (returns new value)
my variable           → x

Motion:
move 10 steps         → moveSteps 10 state
goto x:0 y:0          → gotoXY 0 0 state
turn right            → moveRight state

Control:
repeat 10             → repeatN 10 action state
repeat until          → repeatUntil condition action 100 state
if then               → if condition then action else state

Operators:
x + y                 → x + y
x * y                 → x * y
x > y                 → x > y

Sensing:
x position            → xPos state
y position            → yPos state

Data:
add to list           → item : list
item of list          → list !! index
length of list        → length list
-}
