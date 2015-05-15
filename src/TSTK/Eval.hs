{-|
Module: TSTK.Eval
Description: Functions for evaluating TSTK code.
License: GPL-3

This module contains facilities to evaluate parse TSTK code.
-}

module TSTK.Eval(addAST, eval, newState, State(ast, place, stk, labels)) where

import TSTK.Parse

-- | Holds the current state of evaluation in a TSTK program.
data State = State { ast :: [Node] -- ^ The abstract syntax tree of the TSTK code.
                   , place :: Int -- ^ The place in the AST being evaluated.
                   , stk :: [Integer] -- ^ The stack for this TSTK program.
                   , labels :: [(String, Int)] -- ^ A map of label names to
                                               -- their place in the AST.
                   }

-- | Executes the next statement in the AST.
execNext :: State -> IO State
execNext state = exec $ state {place = (place state) + 1}

-- | Constructs a new state from an AST.
newState :: [Node] -> State
newState ast = State ast 0 [] []

-- | Appends an AST to the given state.
addAST state newAST = state {ast = (ast state) ++ newAST}

-- | Evaluates the AST and returns an IO monad.
eval :: State -- ^ The initial state of the TSTK program.
        -> IO State -- ^ An IO monad containing the state after evaluation.
eval state = exec $ state {labels = (getLabels (ast state))}

-- | Returns a map of labels to their positions in the AST.
getLabels ::
  [Node] -- ^ The given AST.
  -> [(String, Int)]
getLabels ast = getLabels' ast 0
  where
    getLabels' (node:nodes) place = case node of
      Label name -> (name, place):(getLabels' nodes (place + 1))
      _ -> getLabels' nodes (place + 1)

-- | Executes a given statement in the AST and returns an IO monad.
exec ::
  State -- ^ The state of the TSTK program.
  -> IO State -- ^ The resulting IO monad.
exec state = if (place state) < (length $ ast state)
             then run state
             else return state

run :: State -> IO State
run state@(State ast place stk labels) = case (ast !! place) of
    Label name -> execNext state
    Refer name -> refer name state
    Command name -> command name state
    Number n ->  execNext state {stk = n:stk}

refer :: String -> State -> IO State
refer name state@(State ast place stk labels) = case (lookup name labels) of
  Nothing -> fail ("Couldn't find label " ++ name)
  Just pos -> execNext state {stk = ((toInteger pos):stk)}

-- TODO: This should be broken up into multiple statements
-- | Executes a command and returns an IO monad.
command ::
  String -- ^ The name of the command.
  -> State -- ^ The state of the current execution environment.
  -> IO State -- ^ The resulting IO monad.
command name state@(State ast place stk labels) = case (name, stk) of
  ("add", (n:m:rest)) -> execNext state {stk = ((m + n):rest)}
  ("sub", (n:m:rest)) -> execNext state {stk = ((m - n):rest)}
  ("mul", (n:m:rest)) -> execNext state {stk = ((m * n):rest)}
  ("div", (n:m:rest)) -> execNext state {stk = ((m `div` n):rest)}
  ("dup", (n:rest)) -> execNext state {stk = (n:n:rest)}
  ("jmp", (n:rest)) -> exec state {place = (fromInteger n), stk = rest}
  ("jeq", (n:m:pos:rest)) -> if m == n then exec $ state {place = (fromInteger pos),
                                                        stk = rest}
                             else execNext state {stk = rest}
  ("jnq", (n:m:pos:rest)) -> if m /= n then exec $ state {place = (fromInteger pos),
                                                        stk = rest}
                             else execNext state {stk = rest}
  ("jgt", (n:m:pos:rest)) -> if m > n then exec $ state {place = (fromInteger pos),
                                                         stk = rest}
                             else execNext state {stk = rest}
  ("jlt", (n:m:pos:rest)) -> if m < n then exec $ state {place = (fromInteger pos),
                                                         stk = rest}
                             else execNext state {stk = rest}
  ("nth", (n:rest)) -> execNext state {stk = ((rest !! (fromInteger n)):rest)}
  ("pop", (n:rest)) -> execNext state {stk = rest}
  ("ppos", rest) -> execNext state {stk = ((toInteger place):rest)}
  ("print", (n:rest)) -> do { print n
                            ; execNext state {stk = rest}
                            }
  ("cprint", (n:rest)) -> do { putChar (toEnum (fromInteger n))
                             ; execNext state {stk = rest}
                             }
  ("read", rest) -> do { num <- getLine
                       ; execNext
                         state {stk = ((read num :: Integer):rest)}
                       }
  ("cread", rest) -> do { ch <- getChar
                        ; execNext
                          state {stk = ((toInteger (fromEnum ch)):rest)}
                        }
  ("size", rest) -> execNext state {stk = ((toInteger (length rest)):rest)}
  ("swap", (n:m:rest)) -> execNext state {stk = (m:n:rest)}
  (_, ns) -> fail ("Command \"" ++ name ++ "\" does not exist or does not work "
                   ++ "when only " ++ show (length ns) ++ " elements are on "
                   ++ "the stack")
