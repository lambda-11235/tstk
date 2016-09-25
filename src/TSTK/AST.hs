
module TSTK.AST where


type AST = [Node]

data Node = Label String
          | Refer String
          | Command Command
          deriving (Eq, Show)

data Command = Com String
             | Int Integer
             deriving (Eq, Show)
