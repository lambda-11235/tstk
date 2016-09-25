
module Main(main) where

import TSTK.Lexer
import TSTK.Exec

import Control.Monad(unless)
import System.Environment
import System.IO

main = do args <- getArgs
          if not (null args) then runFiles args else repl

-- | Runs multiple file one after the other in the same runtime.
runFiles :: [String] -> IO ()
runFiles files = runFiles' files newState
  where
    runFiles' [] state = return ()
    runFiles' (file:files) state = do conts <- readFile file
                                      let ast = scan conts
                                      case addAST state ast of
                                       Nothing -> putStrLn "Failed to load AST"
                                       Just state' -> do state'' <- exec state'
                                                         runFiles' files state''

repl ::  IO ()
repl = repl' newState
  where
    repl' state = do line <- getLine
                     let ast = scan line
                     case addAST state ast of
                      Nothing -> putStrLn "Failed to load AST"
                      Just state' -> do state'' <- exec state'
                                        print $ reverse $ stk state''
                                        repl' state''
