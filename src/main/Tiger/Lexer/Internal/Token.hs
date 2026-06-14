module Tiger.Lexer.Internal.Token(
    delex
  , SourceLoc(column, lineNumber, sourceFile, SourceLoc)
  , Token(loc, Token, typ)
  , TokenType(..)
  ) where

data Token
  = Token { typ :: TokenType
          , loc :: SourceLoc
          }
  deriving (Eq, Ord, Show)

data SourceLoc
  = SourceLoc { sourceFile :: FilePath
              , lineNumber :: Word
              , column     :: Word
              }
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

delex :: TokenType -> Text
delex And             = "&"
delex Array           = "array"
delex Assign          = ":="
delex Break           = "break"
delex Colon           = ":"
delex Comma           = ","
delex Divide          = "/"
delex Do              = "do"
delex Dot             = "."
delex Else            = "else"
delex End             = "end"
delex EOF             = "<end of file>"
delex Equals          = "="
delex For             = "for"
delex Function        = "function"
delex GreaterEquals   = ">="
delex GreaterThan     = ">"
delex (Identifier s)  = s
delex If              = "if"
delex In              = "in"
delex (Int n)         = showText n
delex LeftBrace       = "{"
delex LeftBracket     = "["
delex LeftParen       = "("
delex LessEquals      = "<="
delex LessThan        = "<"
delex Let             = "let"
delex Minus           = "-"
delex Multiply        = "*"
delex Nil             = "nil"
delex NotEquals       = "<>"
delex Of              = "of"
delex Or              = "|"
delex Plus            = "+"
delex RightBrace      = "}"
delex RightBracket    = "]"
delex RightParen      = ")"
delex Semicolon       = ";"
delex (Stringy s)     = showText s
delex Then            = "then"
delex To              = "to"
delex Type            = "type"
delex Var             = "var"
delex While           = "while"
