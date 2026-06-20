module Tiger.Common(formatError, lineAtIndex) where

import Data.List qualified as List
import Data.Text qualified as Text


formatError :: FilePath -> Text -> Word -> Word -> Text -> Text -> Maybe Text -> Text -> Text
formatError filepath source lineNum columnNum errCode errorMessage howToFixM offender =
    path <> ":" <> lineNumStr <> ":" <> columnNumStr <> ": error: [Tiger-" <> errCode <> "]\n" <>
      "     " <> errorMessage <> "\n" <>
      optionalFixLine <>
      "    |\n" <>
      paddedLineNum <> " | " <> (sanitize line) <> "\n" <>
      "    | " <> spacing <> carets
  where
    path             = asText filepath
    lineNumStr       = showText lineNum
    columnNumStr     = showText columnNum
    paddedLineNum    = Text.justifyLeft 3 ' ' $ showText lineNum
    line             = (lineAtIndex (lineNum - 1) source) `orElse` internalErrorMsg
    internalErrorMsg = "<INTERNAL ERROR: No such line - " <> (showText lineNum) <> ">"
    spacing          = Text.replicate (fromIntegral $ columnNum - 1) " "
    carets           = Text.replicate         (Text.length offender) "^"

    optionalFixLine = maybe "" (\howToFix -> "     Suggested fix: " <> howToFix <> "\n") howToFixM

    sanitize = Text.map $ \c -> if c == '\t' then ' ' else c

lineAtIndex :: Word -> Text.Text -> Maybe Text
lineAtIndex n haystack =
    case List.drop (fromIntegral n) $ Text.lines haystack of
      []    -> Nothing
      (h:_) -> Just h
