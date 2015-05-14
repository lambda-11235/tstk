
module Eval(addAST, eval, newState, State(ast, place, stk, labels)) where

import Parse

-- | Holds the current state of evaluation.
data State = State { ast :: [Node]
                   , place :: Int
                   , stk :: [Integer]
                   , labels :: [(String, Int)]
                   }

-- | Gets the next place in the AST at the current state.
nextPl :: State -> State
nextPl state = state {place = (place state) + 1}

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
exec state@(State ast place stk labels) = if place < (length ast) then
  case (ast !! place) of
    Label name -> exec $ nextPl state
    Refer name -> case (lookup name labels) of
      Nothing -> fail ("Couldn't find label " ++ name)
      Just pos -> exec $ nextPl $ state {stk = ((toInteger pos):stk)}
    Command name -> command name state
    Number n ->  exec $ nextPl $ state {stk = n:stk}
  else
    return state

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
  where
    -- | Executes the next statement in the AST.
    execNext :: State -> IO State
    execNext = exec . nextPl
