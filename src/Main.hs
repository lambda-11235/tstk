
module Main(main) where

import Parse(program)
import Eval(addAST, eval, newState, State(ast, place, stk, labels))

import Control.Monad(unless)
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec(parse, parseFromFile)

main = do args <- getArgs
          if not (null args) then runFiles args else repl

-- | Runs multiple file one after the other in the same runtime.
runFiles :: [String] -> IO ()
runFiles (file:files) = do result <- parseFromFile program file
                           case result of
                             Left err -> print err
                             Right ast -> do state <- eval $ newState ast
                                             runFiles' files state
  where
    runFiles' [] state = return ()
    runFiles' (file:files) state = do result <- parseFromFile program file
                                      case result of
                                        Left err -> print err
                                        Right ast -> do state <- eval $ addAST state ast
                                                        runFiles' files state

-- | Starts the TSTK REPL.
repl ::  IO ()
repl = do code <- getLine
          case parse program "TSTK parser" code of
            Left err -> print err
            Right ast -> do state <- eval $ newState ast
                            repl' state

  where
    repl' :: State -> IO ()
    repl' state = do print $ stk state
                     iseof <- isEOF
                     unless iseof $
                       do code <- getLine
                          case parse program "TSTK parser" code of
                            Left err -> print err
                            Right ast -> do state <- eval $ addAST state ast
                                            repl' state
