module Tiger.ErrorParser(formatErrorOutput, CompilationError(BadLex, BadParse)) where

import Tiger.Common(formatError)

import Tiger.Lexer.Token(delex, SourceLoc(SourceLoc), Token(Token), TokenType)

import Tiger.Parser.AST(Symbol(Symbol))
import Tiger.Parser.ParserError(ParserError(ParserError), ParserErrorType(Aborted, BadSyntax))

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as Text


data CompilationError
  = BadLex      { blError ::          Text }
  | BadParse    { bpError ::   ParserError }
  deriving Show

formatErrorOutput :: Text -> NonEmpty CompilationError -> Text
formatErrorOutput source = (map $ formatErrorMessage source) &> NE.toList &> (Text.intercalate "\n\n")

-- Making Alex return something other than a `String` is impractical, so we do the munging for the lexer
-- errors in `Grammar.x`/`LexerError.hs`. --Jason B. (6/6/26)
formatErrorMessage :: Text -> CompilationError -> Text
formatErrorMessage      _ (BadLex            errText) = errText
formatErrorMessage source (BadParse      parserError) = formatParserError   source parserError

formatParserError :: Text -> ParserError -> Text
formatParserError source (ParserError typ (SourceLoc path line column)) =
    formatError path source line column errCode message howTo "?"
  where
    (message, howTo, errCodeNum) = formatParserErrorType typ
    errCode                      = "P" <> (showText errCodeNum)

formatParserErrorType :: ParserErrorType -> (Text, Text, Word)
formatParserErrorType Aborted   = ("Unexpected abort", "Fix your parser"       , 0)
formatParserErrorType BadSyntax = ("Unexpected token", "Write valid Tiger code", 1)
