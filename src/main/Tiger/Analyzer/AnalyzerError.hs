{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Analyzer.AnalyzerError(module Tiger.Analyzer.Internal.AnalyzerError) where

import Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerError(AnalyzerError)
  , AnalyzerErrorType( ArityMismatch, BadInternalState, CannotSetForVar, CanOnlyIndexArray
                     , CanOnlyLookupInRecord, DuplicateFieldInInst, DuplicateFieldInType, DuplicateFunc
                     , DuplicateType, IllegalBreak, MissingProperty, NonUnitProcedure, NoSuchFn
                     , NoSuchProperty, NoSuchRecordType, NoSuchType, NoSuchVariable
                     , TypelessVarCannotInitToNil, TypeMismatch, VarCannotInitInTermsOfSelf
                     )
  )
