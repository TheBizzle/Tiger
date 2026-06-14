module Tiger.Lexer.Internal.LexerError(mungeError) where

import Tiger.Common(formatError)

import qualified Data.Text as Text



mungeError :: FilePath -> Text -> Word -> Word -> Word -> Text
mungeError filepath source offset line column =
    formatError filepath source line column "L0" errorMessage howToFix badBoy
  where
    errorMessage = "Unexpected character: " <> badBoy
    howToFix     = "Use a valid character instead" :: Text
    badBoy       = (map Text.singleton $ charAtIndex offset source) `orElse` "<EOF>"

charAtIndex :: Word -> Text -> Maybe Char
charAtIndex n haystack =
    if Text.null remainder then
      Nothing
    else
      Just $ Text.head remainder
  where
    remainder = Text.drop (fromIntegral n) haystack
