module Tiger.ErrorParser(formatErrorOutput, CompilationError(BadAnalysis, BadLex, BadParse)) where

import Tiger.Common(formatError)

import Tiger.Lexer.Token(delex, SourceLoc(SourceLoc), Token(Token), TokenType)

import Tiger.Parser.AST(Symbol(Symbol))
import Tiger.Parser.ParserError(ParserError(ParserError), ParserErrorType(Aborted, BadSyntax))

import Tiger.Analyzer.AnalyzerError(
    AnalyzerError(AnalyzerError)
  , AnalyzerErrorType( ArityMismatch, BadInternalState, CannotSetForVar, CanOnlyIndexArray
                     , CanOnlyLookupInRecord, DuplicateFieldInInst, DuplicateFieldInType, DuplicateFunc
                     , DuplicateType, IllegalBreak, MissingProperty, NonUnitProcedure, NoSuchFn
                     , NoSuchProperty, NoSuchRecordType, NoSuchType, NoSuchVariable
                     , TypelessVarCannotInitToNil, TypeMismatch, VarCannotInitInTermsOfSelf
                     )
  )

import Tiger.Analyzer.Type(Type(Array, Int, Named, Nil, Record, String, Unit), UniqueID(UniqueID))

import Data.List.NonEmpty qualified as NE
import Data.Text          qualified as Text


data CompilationError
  = BadLex      { blError ::          Text }
  | BadParse    { bpError ::   ParserError }
  | BadAnalysis { baError :: AnalyzerError }
  deriving Show

formatErrorOutput :: Text -> NonEmpty CompilationError -> Text
formatErrorOutput source = (map $ formatErrorMessage source) &> NE.toList &> (Text.intercalate "\n\n")

-- Making Alex return something other than a `String` is impractical, so we do the munging for the lexer
-- errors in `Grammar.x`/`LexerError.hs`. --Jason B. (6/6/26)
formatErrorMessage :: Text -> CompilationError -> Text
formatErrorMessage      _ (BadLex            errText) = errText
formatErrorMessage source (BadParse      parserError) = formatParserError   source parserError
formatErrorMessage source (BadAnalysis analyzerError) = formatAnalyzerError source analyzerError

formatParserError :: Text -> ParserError -> Text
formatParserError source (ParserError typ (SourceLoc path line column)) =
    formatError path source line column errCode message (Just howTo) "?"
  where
    (message, howTo, errCodeNum) = formatParserErrorType typ
    errCode                      = "P" <> (showText errCodeNum)

formatParserErrorType :: ParserErrorType -> (Text, Text, Word)
formatParserErrorType Aborted   = ("Unexpected abort", "Fix your parser"       , 0)
formatParserErrorType BadSyntax = ("Unexpected token", "Write valid Tiger code", 1)

formatAnalyzerError :: Text -> AnalyzerError -> Text
formatAnalyzerError source (AnalyzerError typ token) =
    formatError path source line column errCode message (Just howTo) $ delex tokenType
  where
    Token tokenType (SourceLoc path line column) = token
    (message, howTo, errCodeNum)                 = formatAnalyzerErrorType tokenType typ
    errCode                                      = "A" <> (showText errCodeNum)

formatAnalyzerErrorType :: TokenType -> AnalyzerErrorType -> (Text, Text, Word)
formatAnalyzerErrorType _ (ArityMismatch (Symbol fnName) expectedN gotN) =
    (msg, "Supply the correct number of arguments", 0)
  where
    msg = "Function `" <> fnName <> "` takes " <> (showText expectedN) <> " arguments, but got " <> (showText gotN) <> "."

formatAnalyzerErrorType tokType BadInternalState =
    (msg, "Report this bug to the language developer with your source code", 1)
  where
    msg = "Fatal internal error on `" <> (delex tokType) <> "`"

formatAnalyzerErrorType tokType CannotSetForVar =
    (msg, "Just don't touch that variable", 2)
  where
    msg = "Attempted to set `" <> (delex tokType) <> "`, but a `for` loop's counter variable cannot be assigned to."

formatAnalyzerErrorType tokType (CanOnlyIndexArray typ) =
    (msg, "Ensure that this value is an array", 3)
  where
    msg = "Attempted to index `" <> (delex tokType) <> "`, but it is " <> (str typ) <> ", not an array."

formatAnalyzerErrorType tokType (CanOnlyLookupInRecord typ) =
    (msg, "Ensure that this value is a record", 4)
  where
    msg = "Property " <> (delex tokType) <> " does not exist on " <> (str typ) <> ", because it is not a record."

formatAnalyzerErrorType tokType DuplicateFieldInInst =
    (msg, "Only set the field once", 5)
  where
    msg = "Duplicate field in instance: " <> (delex tokType)

formatAnalyzerErrorType tokType DuplicateFieldInType =
    (msg, "Rename one of the fields, or only declare the field once", 6)
  where
    msg = "Duplicate field in type: " <> (delex tokType)

formatAnalyzerErrorType tokType DuplicateFunc =
    (msg, "Rename one of the functions", 7)
  where
    msg = "Duplicate function: " <> (delex tokType)

formatAnalyzerErrorType tokType DuplicateType =
    (msg, "Rename one of the types", 8)
  where
    msg = "Duplicate type: " <> (delex tokType)

formatAnalyzerErrorType _ IllegalBreak =
    (msg, "Ensure that `break` is only being used within `while` or `for`", 9)
  where
    msg :: Text
    msg = "Illegal use of `break`"

formatAnalyzerErrorType _ (MissingProperty (Symbol name)) =
    (msg, "Supply a property with the given name", 10)
  where
    msg = "This record type requires a property named `" <> name <> "`, but none was provided"

formatAnalyzerErrorType tokType NonUnitProcedure =
    (msg, "Discard the return value, or declare an output type for the procedure that matches the return value", 11)
  where
    msg = "Procedure has a return value: " <> (delex tokType)

formatAnalyzerErrorType tokType NoSuchFn =
    (msg, "Ensure that you are calling the correct function, and that it is defined in this scope", 12)
  where
    msg = "No such function: " <> (delex tokType)

formatAnalyzerErrorType tokType (NoSuchProperty (UniqueID uid)) =
    (msg, "Check the value and property name", 13)
  where
    msg = "No such property on this record (#" <> (showText uid) <> "): " <> (delex tokType)

formatAnalyzerErrorType tokType NoSuchRecordType =
    (msg, "Check the type definition", 14)
  where
    msg = "No such record type: " <> (delex tokType)

formatAnalyzerErrorType tokType NoSuchType =
    (msg, "Ensure that you are referencing the correct type, and that it is defined in this scope", 15)
  where
    msg = "No such type: " <> (delex tokType)

formatAnalyzerErrorType tokType NoSuchVariable =
    (msg, "Ensure that you are referencing the correct variable, and that it is defined in this scope", 16)
  where
    msg = "No such variable: " <> (delex tokType)

formatAnalyzerErrorType tokType TypelessVarCannotInitToNil =
    (msg, "Declare a record type on the variable or initialize to a different value", 17)
  where
    msg = "Cannot initialize `" <> (delex tokType) <> "` to `nil` without a declaring it with a record type."

formatAnalyzerErrorType tokType (TypeMismatch expected got) =
    (msg, "Ensure that type system rules are respected", 18)
  where
    msg = "Could not match expected type `" <> (str2 expected) <> "` with actual type `" <> (str2 got) <>
            "`, regarding value `" <> (delex tokType) <> "`"

formatAnalyzerErrorType tokType VarCannotInitInTermsOfSelf =
    (msg, "Check for name shadowing and do not define variables in terms of themselves", 19)
  where
    msg = "`" <> (delex tokType) <> "` cannot be defined in terms of itself"

str :: Type -> Text
str (Array _ (UniqueID uid))         = "an `array_" <> (showText uid)
str Int                              = "an int"
str (Named (Symbol name) namedTypeM) = "a `" <> name <> "@" <> (maybe "<unfilled>" (const "<filled>") namedTypeM) <> "`"
str Nil                              = "nil"
str (Record fields (UniqueID uid))   = "a `record_" <> (showText uid) <> "{ " <> (strFields $ map fst fields) <> " }`"
str String                           = "a string"
str Unit                             = "undefined"

str2 :: Type -> Text
str2 (Array _ (UniqueID uid))         = "array_" <> (showText uid)
str2 Int                              = "int"
str2 (Named (Symbol name) namedTypeM) = "a `" <> name <> "@" <> (maybe "<unfilled>" (const "<filled>") namedTypeM) <> "`"
str2 Nil                              = "nil"
str2 (Record fields (UniqueID uid))   = "record_" <> (showText uid) <> "{ " <> (strFields $ map fst fields) <> " }"
str2 String                           = "string"
str2 Unit                             = "undefined"

strFields :: [Symbol] -> Text
strFields = map (\(Symbol n) -> n) &> Text.intercalate ", "
