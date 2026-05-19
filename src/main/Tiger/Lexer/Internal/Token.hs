module Tiger.Lexer.Internal.Token(
    SourceLoc(column, lineNumber, sourceFile, SourceLoc)
  , Token(loc, Token, typ)
  , TokenType(..)
  ) where

data Token
  = Token { typ :: TokenType
          , loc :: SourceLoc
          }
  deriving (Eq, Show)

data SourceLoc
  = SourceLoc { sourceFile :: FilePath
              , lineNumber :: Word
              , column     :: Word
              }
  deriving (Eq, Show)

data TokenType
  = And
  | Array
  | Assign
  | Break
  | Colon
  | Comma
  | Divide
  | Do
  | Dot
  | Else
  | End
  | EOF
  | Equals
  | For
  | Function
  | GreaterEquals
  | GreaterThan
  | Identifier Text
  | If
  | In
  | Int Int64
  | LeftBrace
  | LeftBracket
  | LeftParen
  | LessEquals
  | LessThan
  | Let
  | Minus
  | Multiply
  | Nil
  | NotEquals
  | Of
  | Or
  | Plus
  | RightBrace
  | RightBracket
  | RightParen
  | Semicolon
  | Stringy Text
  | Then
  | To
  | Type
  | Var
  | While
  deriving (Eq, Show)
