module Tiger.Compiler(compile, compileForTest, Program) where

import Tiger.Lexer.Lexer(lex)

import Tiger.Parser.Parser(parse)

import Tiger.Analyzer.Analyzer(analyze, IRValue)

import Tiger.ErrorParser(formatErrorOutput, CompilationError(BadAnalysis, BadLex, BadParse))


type Program = IRValue

compile :: (FilePath, Text) -> Validation Text Program
compile input = first (formatErrorOutput $ snd input) $ compileForTest input

compileForTest :: (FilePath, Text) -> Validation (NonEmpty CompilationError) Program
compileForTest input = irExprV
  where
    tokensV = input |>                      lex &> (first $ map BadLex)
    astV    = tokensV `bindValidation` (  parse &> (first $ map BadParse))
    irExprV =    astV `bindValidation` (analyze &> (first $ map BadAnalysis))
