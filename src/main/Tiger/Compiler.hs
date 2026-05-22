module Tiger.Compiler(compile) where

import Tiger.Lexer.Lexer(lex)

import Tiger.Parser.AST(Expr)
import Tiger.Parser.Parser(parse)
import Tiger.Parser.ParserError(ParserError)


data CompilationError
  = BadLex   { blError :: Text }
  | BadParse { bpError :: ParserError }
  deriving Show

type Program = Expr

compile :: Text -> Validation (NonEmpty CompilationError) Program
compile src = astV
  where
    tokensV = src |>                      lex &> (first $ map BadLex)
    astV    = tokensV `bindValidation` (parse &> (first $ map BadParse))
