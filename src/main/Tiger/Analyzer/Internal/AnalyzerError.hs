module Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerErrorType( ArityMismatch, BadInternalState, CannotSetForVar, CanOnlyIndexArray
                     , CanOnlyLookupInRecord, DuplicateFieldInInst, DuplicateFieldInType, DuplicateFunc
                     , DuplicateType, IllegalBreak, MissingProperty, mtExpected, mtGot, NonUnitProcedure
                     , NoSuchFn, NoSuchProperty, NoSuchRecordType, NoSuchType, NoSuchVariable
                     , TypelessVarCannotInitToNil, TypeMismatch, VarCannotInitInTermsOfSelf
                     )
  , AnalyzerError(offender, AnalyzerError, typ)
  ) where

import Tiger.Lexer.Token(Token)

import Tiger.Parser.Internal.AST(Symbol)

import Tiger.Analyzer.Internal.Type(Type, UniqueID)


data AnalyzerErrorType
  = ArityMismatch              { amFnName :: Symbol, amExpected :: Word, amGot :: Word }
  | BadInternalState
  | CannotSetForVar
  | CanOnlyIndexArray          { badType :: Type }
  | CanOnlyLookupInRecord      { badType :: Type }
  | DuplicateFieldInInst
  | DuplicateFieldInType
  | DuplicateFunc
  | DuplicateType
  | IllegalBreak
  | MissingProperty            { propName :: Symbol }
  | NonUnitProcedure
  | NoSuchFn
  | NoSuchProperty             { recordTypeID :: UniqueID }
  | NoSuchRecordType
  | NoSuchType
  | NoSuchVariable
  | TypelessVarCannotInitToNil
  | TypeMismatch               { mtExpected :: Type, mtGot :: Type }
  | VarCannotInitInTermsOfSelf
  deriving (Eq, Show)

data AnalyzerError =
  AnalyzerError { typ :: AnalyzerErrorType, offender :: Token }
  deriving Show
