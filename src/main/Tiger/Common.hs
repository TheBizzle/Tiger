module Tiger.Common(formatError, lineAtIndex) where

import qualified Data.List as List
import qualified Data.Text as Text


formatError :: FilePath -> Text -> Word -> Word -> Text -> Text -> Text -> Text -> Text
formatError filepath source lineNum columnNum errCode errorMessage howToFix offender =
    path <> ":" <> lineNumStr <> ":" <> columnNumStr <> ": error: [Tiger-" <> errCode <> "]\n" <>
      "     " <> errorMessage <> "\n" <>
      "     Suggested fix: " <> howToFix <> "\n" <>
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

    sanitize = Text.map $ \c -> if c == '\t' then ' ' else c

lineAtIndex :: Word -> Text.Text -> Maybe Text
lineAtIndex n haystack =
    case List.drop (fromIntegral n) $ Text.lines haystack of
      []    -> Nothing
      (h:_) -> Just h
