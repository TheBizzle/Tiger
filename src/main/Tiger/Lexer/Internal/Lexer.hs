module Tiger.Lexer.Internal.Lexer(lex) where

import Tiger.Lexer.Internal.Grammar(alexScanTokens)
import Tiger.Lexer.Internal.Token(Token)

import qualified Data.List.NonEmpty as NE


lex :: Text -> Validation (NonEmpty Text) [Token]
lex src =
  case alexScanTokens $ asString src of
    Left  err    -> Failure $ NE.singleton $ asText err
    Right tokens -> Success tokens
