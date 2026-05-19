module Tiger.Lexer.Internal.Lexer(lex) where

import Tiger.Lexer.Internal.Grammar(alexScanTokens)


lex :: Text -> Either Text ()
lex src =
  case alexScanTokens $ asString src of
    Left  err -> Left $ asText err
    Right _   -> Right ()
