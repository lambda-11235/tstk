{- A parser for tstk.
-}

module Parse(Node(Label, Refer, Command, Number), program) where

import Text.ParserCombinators.Parsec hiding (label, token)

-- | This is the abstract syntax trees' node data type.
data Node = Label String
          | Refer String -- ^ Represents a reference in TSTK.
          | Command String
          | Number Integer

instance Show Node where
  show node = case node of
    Label str -> ":" ++ str ++ ":"
    Refer str -> "@" ++ str
    Command str -> str
    Number n -> show n

-- | Simple parser that returns a list of nodes in the AST, which is always one
-- level deep.
program :: Parser [Node]
program = do{ skipMany ((many1 space) <|> comment)
            ; many programPart
            }

-- | This takes care of any spaces or comments.
programPart :: Parser Node
programPart = do{ tok <- token
                ; skipMany ((many1 space) <|> comment)
                ; return tok
                }

comment :: Parser [Char]
comment = do { char '#'
             ; manyTill anyChar newline
             }

token :: Parser Node
token = label <|> refer <|> command <|> integer

label :: Parser Node
label = do{ char ':'
          ; name <- (many1 alpha)
          ; char ':'
          ; return $ Label name
          }

refer :: Parser Node
refer = do{ char '@'
          ; name <- (many1 alpha)
          ; return $ Refer name
          }

command :: Parser Node
command = do{ name <- many1 alpha
            ; return $ Command name
            }

integer :: Parser Node
integer = do{ sign <- option '0' (char '-')
            ; ds <- many1 digit
            ; return $ Number (read (sign:ds) :: Integer)
            }

alpha :: Parser Char
alpha = letter

{- digit = ...; does not need to be declared -}
