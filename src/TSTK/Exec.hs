{-|
Module: Exec
Description: Functions for evaluating TSTK code.
License: GPL-3

This module contains facilities to evaluate parse TSTK code.
-}

module TSTK.Exec (addAST, exec, newState, State(coms, place, stk, labels)) where

import TSTK.AST

import qualified Data.Map as M


-- | Holds the current state of evaluation in a TSTK program.
data State = State { coms :: [Command]        -- ^ The commands of the TSTK code.
                   , place :: Int             -- ^ The place in the code being evaluated.
                   , stk :: [Integer]         -- ^ The stack for this TSTK program.
                   , labels :: M.Map String Int -- ^ A map of label names to
                                                -- their place in the code.
                   }

-- | Executes the next statement in the AST.
execNext :: State -> IO State
execNext state = exec $ state {place = (place state) + 1}

-- | Constructs a new state from an AST.
newState :: State
newState = State [] 0 [] M.empty


-- | Converts an AST with labels to a series of commands, with references being
-- replaced by their addresses.
convertLabels :: AST -> Maybe [Command]
convertLabels ast = conLbls (getLabels ast) ast
  where
    conLbls _ [] = return []
    conLbls lbls ((Label _):ast) = conLbls lbls ast
    conLbls lbls ((Refer name):ast) = do addr <- M.lookup name lbls
                                         coms <- conLbls lbls ast
                                         return ((Int $ toInteger addr):coms)
    conLbls lbls ((Command com):ast) = do coms <- conLbls lbls ast
                                          return (com:coms)

-- | Returns a map of label names to the addresses they refer to.
getLabels :: AST -> M.Map String Int
getLabels ast = getLabels' ast 0
  where
    getLabels' [] _ = M.empty
    getLabels' ((Label name) : ast) n = let lbls = getLabels' ast n
                                        in M.insert name n lbls
    getLabels' (_:ast) n = getLabels' ast (n + 1)

-- | Appends an AST to the given state.
addAST :: State -> AST -> Maybe State
addAST state newAST = do newComs <- convertLabels newAST
                         return $ state {coms = (coms state) ++ newComs}

-- | Executes a given statement in the AST and returns an IO monad.
exec ::
  State -- ^ The state of the TSTK program.
  -> IO State -- ^ The resulting IO monad.
exec state = if (place state) < (length $ coms state)
             then run state
             else return state

run :: State -> IO State
run state@(State coms place stk labels) = case (coms !! place) of
    Com name -> command name state
    Int n ->  execNext state {stk = n:stk}

set :: Integer -> Integer -> [Integer] -> Maybe [Integer]
set _ _ [] = Nothing
set n val (x:xs) | n < 0 = Nothing
                 | n == 0 = Just (val:xs)
                 | otherwise = do xs' <- set (n - 1) val xs
                                  return $ x:xs'

-- TODO: This should be broken up into multiple statements
-- | Executes a command and returns an IO monad.
command ::
  String -- ^ The name of the command.
  -> State -- ^ The state of the current execution environment.
  -> IO State -- ^ The resulting IO monad.
command name state@(State coms place stk labels) = case (name, stk) of
  ("add", (n:m:rest)) -> execNext state {stk = ((m + n):rest)}
  ("sub", (n:m:rest)) -> execNext state {stk = ((m - n):rest)}
  ("mul", (n:m:rest)) -> execNext state {stk = ((m * n):rest)}
  ("div", (n:m:rest)) -> execNext state {stk = ((m `div` n):rest)}

  ("dup", (n:rest)) -> execNext state {stk = (n:n:rest)}

  ("jmp", (n:rest)) -> exec state {place = (fromInteger n), stk = rest}

  ("jeq", (pos:n:m:rest)) -> if m == n then exec $ state {place = (fromInteger pos),
                                                          stk = rest}
                             else execNext state {stk = rest}

  ("jnq", (pos:n:m:rest)) -> if m /= n then exec $ state {place = (fromInteger pos),
                                                          stk = rest}
                             else execNext state {stk = rest}

  ("jgt", (pos:n:m:rest)) -> if m > n then exec $ state {place = (fromInteger pos),
                                                         stk = rest}
                             else execNext state {stk = rest}

  ("jlt", (pos:n:m:rest)) -> if m < n then exec $ state {place = (fromInteger pos),
                                                         stk = rest}
                             else execNext state {stk = rest}

  ("get", (n:rest)) -> execNext state {stk = ((rest !! (fromInteger n)):rest)}

  ("set", (n:val:rest)) -> case set n val rest of
                            Just stk -> execNext state {stk = stk}
                            Nothing -> fail $ (show n) ++ " is not within the bounds of the stack"

  ("pop", (n:rest)) -> execNext state {stk = rest}

  ("ppos", rest) -> execNext state {stk = ((toInteger place):rest)}

  ("print", (n:rest)) -> do print n
                            execNext state {stk = rest}

  ("cprint", (n:rest)) -> do putChar (toEnum (fromInteger n))
                             execNext state {stk = rest}

  ("read", rest) -> do  num <- getLine
                        execNext state {stk = ((read num :: Integer):rest)}

  ("cread", rest) -> do ch <- getChar
                        execNext state {stk = ((toInteger (fromEnum ch)):rest)}

  ("size", rest) -> execNext state {stk = ((toInteger (length rest)):rest)}

  ("dbg", stk) -> do print $ reverse stk
                     execNext state

  ("swap", (n:m:rest)) -> execNext state {stk = (m:n:rest)}
  (_, ns) -> fail ("Command \"" ++ name ++ "\" does not exist or does not work "
                   ++ "when only " ++ show (length ns) ++ " elements are on "
                   ++ "the stack")
