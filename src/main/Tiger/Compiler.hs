module Tiger.Compiler(compile, compileForTest) where

import Tiger.Lexer.Lexer(lex)

import Tiger.Parser.Parser(parse)

import Tiger.ErrorParser(formatErrorOutput, CompilationError(BadLex, BadParse))



type Program = Expr
compile :: (FilePath, Text) -> Validation Text Program
compile input = first (formatErrorOutput $ snd input) $ compileForTest input

compileForTest :: (FilePath, Text) -> Validation (NonEmpty CompilationError) Program
compileForTest input = astV
  where
    tokensV = input |>                      lex &> (first $ map BadLex)
    astV    = tokensV `bindValidation` (  parse &> (first $ map BadParse))
