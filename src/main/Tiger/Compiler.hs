module Tiger.Compiler(compile) where

import Tiger.Lexer.Lexer(lex)
import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Expr)
import Tiger.Parser.Parser(parse)
import Tiger.Parser.ParserError(ParserError)


data CompilationError
  = BadLex   { blError :: Text }
  deriving Show

type Program = [Token]

compile :: Text -> Validation (NonEmpty CompilationError) Program
compile src = tokensV
  where
    tokensV = src |> lex &> (first $ map BadLex)
