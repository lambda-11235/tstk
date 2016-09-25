{
module TSTK.Lexer (scan) where

import TSTK.AST
}

%wrapper "posn"

@alpha = [a-zA-Z]
@digit = [0-9]

tokens :-

  $white+                               ;
  "#".*                                 ;
  ":"@alpha+":"                         { \p s -> Label (init $ tail s) }
  "@"@alpha+                            { \p s -> Refer (tail s) }
  "-"? @digit+                          { \p s -> Command (Int (read s)) }
  @alpha+                               { \p s -> Command (Com s) }

{
scan :: String -> AST
scan = alexScanTokens
}
