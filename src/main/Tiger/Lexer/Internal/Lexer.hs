module Tiger.Lexer.Internal.Lexer(lex) where

import Tiger.Lexer.Internal.Grammar(runLexer)
import Tiger.Lexer.Internal.Token(Token)

import qualified Data.List.NonEmpty as NE


lex :: (FilePath, Text) -> Validation (NonEmpty Text) [Token]
lex (path, src) =
  case runLexer path src of
    Left  err    -> Failure $ NE.singleton $ asText err
    Right tokens -> Success tokens
