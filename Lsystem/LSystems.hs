module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (a, _, _)
  = a

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (_, a, _)
  = a

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_, _, r)
  = r

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar a [] = error "No binding!"
lookupChar a xs
  = [m' | (x, m) <- xs, m' <- m, x == a]

-- Expand command string s once using rule table r
expandOne' :: String -> Rules -> String
expandOne' "" _ = "" 
expandOne' ms xs
  = [s |m <- ms, (x ,rule) <- xs, s <- rule, m == x]

expandOne :: String -> Rules -> String
expandOne "" _ = "" 
expandOne ms xs
  = concatMap (lookupChar' xs) ms
  where 
    lookupChar' x y = lookupChar y x


expand :: String -> Int -> Rules -> String
expand ms n xs = foldl expandOne ms (replicate n xs)
  

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move '[' _ turtleState = turtleState
move 'L' a (b, z)      = (b, z + a)
move 'R' a (b, z)      = (b, z - a)
move 'F' _ ((x, y), z) = ((x + cos z', y + sin z'), z)
  where
    z' = z  * pi / 180





    
--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--
                        
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 commands a colour = x 
  where
    (x, y) = trace1' commands ((0.0, 0.0), 90)
    trace1' :: Commands -> TurtleState -> ([ColouredLine], Commands)
    trace1' "" _ = ([], "")
    trace1' (m : ms) turtleState
      | m == 'F'  = ((iState, fState, colour): branch, commandsLeft)
      | m == '['  = (branch ++ mainBranch, commands)
      | m == ']'  = ([], ms)
      | otherwise = (branch, commandsLeft)
      where
        (mainBranch, commands)   = trace1' commandsLeft turtleState
        (branch, commandsLeft)   = trace1' ms turtleState'
        turtleState'@(fState, _) = move m a turtleState
        (iState, _)              = turtleState 


trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands a colour = trace2' commands [((0.0, 0.0), 90)] ((0.0, 0.0), 90)
  where
    trace2' :: Commands -> [TurtleState] -> TurtleState ->[ColouredLine]
    trace2' "" _ _ = []
    trace2' (x : xs) stacks turtleState
      | x == '['  = trace2' xs (stack : stacks) turtleState
      | x == ']'  = trace2' xs (tail stacks) turtleState''
      | x == 'F'  = (iState, fState, colour) : trace2' xs stacks turtleState'
      | otherwise = trace2' xs stacks turtleState' 
      where
        turtleState''            = head stacks
        stack@(iState, _)        = turtleState
        turtleState'@(fState, _) = move x a turtleState 

-- extension Colour Variation
colourVary :: [ColouredLine] -> [ColouredLine]
colourVary [] = []
colourVary a = head(map loopColour a) : colourVary (tail (map loopColour a))
  where
    loopColour (a, b, c)
      = (a, b, c')
      where
        c'
          | c == blue    = green 
          | c == green   = cyan
          | c == cyan    = red
          | c == red     = magenta
          | c == magenta = yellow
          | c == yellow  = white
          | c == white   = black
          | c == black   = blue
    

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

-- This is the fuction to call
-- drawLines (colourVary ((trace2 (expandLSystem star 8) (angle star) blue)))
  

----------------------------------------------------------
-- Some test systems.

star, cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

star
  = (144,
     "N",
     [('S', "[N]MMM-[N]MMM-[N]MMM-[N]MMM-[N]MMM"),
     ('M', "M"),
     ('N', "-XS"),
     ('X', "MX"),
     ('[', "["),
     (']', "]"),
     ('+', "+"),
     ('-', "-")
     ]
    )

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('S', ""),
     ('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]

