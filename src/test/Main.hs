module Main(main) where

import System.Directory(doesFileExist, makeAbsolute)
import System.FilePath((</>))

import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit(assertFailure, HasCallStack, testCase)

import Tiger.Compiler(compileForTest)
import Tiger.ErrorParser(CompilationError(BadAnalysis, BadEval, BadParse), formatErrorOutput)

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
import Tiger.Analyzer.Internal.Address(TypeAddress(IntAddress, StringAddress))
import Tiger.Analyzer.Internal.IRValue(IRValue(irvExpr))
import Tiger.Analyzer.Internal.Type(Type(Array, Named, Record), UniqueID(UniqueID))

import Tiger.Evaluator.Evaluator(eval)
import Tiger.Evaluator.Value(Value(TArray, TInt, TNil, TRecord, TString, TUnit))

import Data.List.NonEmpty           qualified as NE
import Data.Map                     qualified as Map
import Data.Text.IO                 qualified as TIO
import Tiger.Analyzer.Internal.Type qualified as Type


data TigerTestResult
  = Pass (Maybe Value) -- `Nothing` => Don't run it
  | Fail (NonEmpty TigerError)
  | Skip
  deriving Show

data TigerTest
  = TigerTest { testName :: Text         -- e.g. "test1"
              , testFile :: FilePath     -- Relative path inside fixtureDir
              , desc     :: Text
              , expected :: TigerTestResult
              }
  deriving Show

data TigerError
  = LError { lError ::              Text }
  | PError { pError ::   ParserErrorType }
  | AError { aError :: AnalyzerErrorType }
  deriving Show

main :: IO ()
main = defaultMain $ testGroup "Tiger" [testGroup "enabled" $ map makeTest allTests]

allTests :: [TigerTest]
allTests =
  [ pass  "test1"  "Array type and array variable"                                                   $ Just $ TArray $ map TInt [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , pass  "test2"  "`arr1` is valid since expression 0 is int = myint"                               $ Just $ TArray $ map TInt [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , pass  "test3"  "A record type and a record variable"                                             $ Just $ TRecord (Map.fromList [(Symbol "name", TString "Somebody"), (Symbol "age", TInt 1000)]) $ Symbol "rectype"
  , pass  "test4"  "Recursive function"                                                              $ Just $ TInt 3628800
  , pass  "test5"  "Recursive types"                                                                 $ Just $ TRecord (Map.fromList [(Symbol "hd", TInt 0), (Symbol "tl", TNil)]) $ Symbol "intlist"
  , pass  "test6"  "Mutually recursive procedures"                                                   $ Nothing -- Infinite loop
  , pass  "test7"  "Mutually recursive functions"                                                    $ Nothing -- Infinite loop
  , pass  "test8"  "`if`"                                                                            $ Just $ TInt 40
  , fail  "test9"  "Error: Types of `if` branches differ"                                            $ (AError $ TypeMismatch Type.Int  Type.String) :| []
  , fail  "test10" "Error: `while` with non-`Unit` body"                                             $ (AError $ TypeMismatch Type.Unit Type.Int   ) :| []
  , fail  "test11" "Error: `for` with non-`Int` upper bound; index variable erroneously assigned to" $ (AError $ CannotSetForVar) :| []
  , pass  "test12" "`for` and `let`"                                                                 $ Just $ TUnit
  , fail  "test13" "Error: Comparing incompatible types"                                             $ (AError $ TypeMismatch Type.Int Type.String) :| []
  , fail  "test14" "Error: Comparing a record with an array"                                         $ (AError $ TypeMismatch (Record [(Symbol "name", StringAddress), (Symbol "id", IntAddress)] uid1) (Array IntAddress uid1)) :| []
  , fail  "test15" "Error: `if` without `else` return non-`Unit`"                                    $ (AError $ TypeMismatch Type.Unit Type.Int) :| []
  , fail  "test16" "Error: Mutually recursive types that do not pass through record or array"        $ (AError $ NoSuchType) :| [AError NoSuchType, AError NoSuchType, AError NoSuchType]
  , fail  "test17" "Error: Definition of recursive types is non-contiguous"                          $ (AError $ NoSuchType) :| []
  , fail  "test18" "Error: Definition of recursive functions is non-contiguous"                      $ (AError $ NoSuchFn) :| []
  , fail  "test19" "Error: Second function uses variables scoped to the first one"                   $ (AError $ NoSuchVariable) :| []
  , fail  "test20" "Error: Undeclared variable `i`"                                                  $ (AError $ NoSuchVariable) :| []
  , fail  "test21" "Error: Procedure returns value and procedure is used in arexpr"                  $ (AError $ TypeMismatch Type.Int Type.Unit) :| []
  , fail  "test22" "Error: Field `nam` not in record type"                                           $ (AError $ NoSuchProperty uid1) :| []
  , fail  "test23" "Error: Record field assignment type mismatches"                                  $ (AError $ TypeMismatch Type.String Type.Int) :| [AError $ TypeMismatch Type.Int Type.String]
  , fail  "test24" "Error: Indexing a non-array"                                                     $ (AError $ (CanOnlyIndexArray Type.Int)) :| []
  , fail  "test25" "Error: Accessing property on a non-record"                                       $ (AError $ (CanOnlyLookupInRecord Type.Int)) :| []
  , fail  "test26" "Error: Adding `Int` and `String`"                                                $ (AError $ TypeMismatch Type.Int Type.String) :| []
  , pass  "test27" "Varname shadowing"                                                               $ Just $ TInt 2
  , fail  "test28" "Error: No structural typing of records"                                          $ (AError $ TypeMismatch (Record [(Symbol "name", StringAddress), (Symbol "id", IntAddress)] uid1) (Record [(Symbol "name", StringAddress), (Symbol "id", IntAddress)] uid2)) :| []
  , fail  "test29" "Error: No structural typing of arrays"                                           $ (AError $ TypeMismatch (Array IntAddress uid1) (Array IntAddress uid2)) :| []
  , pass  "test30" "Type synonyms"                                                                   $ Just $ TInt 0
  , fail  "test31" "Error: Initializing variable to mismatched type"                                 $ (AError $ TypeMismatch Type.Int Type.String) :| []
  , fail  "test32" "Error: Initializing array with value of wrong type"                              $ (AError $ TypeMismatch Type.Int Type.String) :| []
  , fail  "test33" "Error: Unknown type"                                                             $ (AError $ NoSuchType) :| []
  , fail  "test34" "Error: Function parameter type mismatch"                                         $ (AError $ TypeMismatch Type.Int Type.String) :| []
  , fail  "test35" "Error: Function called with too few arguments"                                   $ (AError $ ArityMismatch (Symbol "g") 2 1) :| []
  , fail  "test36" "Error: Function called with too many arguments"                                  $ (AError $ ArityMismatch (Symbol "g") 2 3) :| []
  , pass  "test37" "Redeclaration of variable"                                                       $ Just $ TInt 0
  , fail  "test38" "Error: Redeclaring consecutive types with different values"                      $ (AError $ DuplicateType) :| []
  , fail  "test39" "Error: Redeclaring consecutive functions"                                        $ (AError $ DuplicateFunc) :| []
  , fail  "test40" "Error: Non-function procedure returns a value"                                   $ (AError $ NonUnitProcedure) :| []
  , pass  "test41" "Type name shadowing"                                                             $ Just $ TInt 0
  , pass  "test42" "Correct declarations"                                                            $ Just $ TUnit
  , fail  "test43" "Error: Initializing to `()` and causing type mismatch in addition"               $ (AError $ TypeMismatch Type.Int Type.Unit) :| []
  , pass  "test44" "Valid `nil` initialization and assignment"                                       $ Just $ TUnit
  , fail  "test45" "Error: Initializing to `nil` without explicit record type"                       $ (AError $ TypelessVarCannotInitToNil) :| []
  , pass  "test46" "Valid record comparisons"                                                        $ Just $ TInt 0
  , pass  "test47" "Valid redeclaration of type in different sequence"                               $ Just $ TInt 0
  , pass  "test48" "Valid redeclaration of function in different sequence"                           $ Just $ TInt 0
  , fail  "test49" "Error: Syntax error on `rectype nil`"                                            $ (PError BadSyntax) :| [PError Aborted]
  , pass  "test50" "Referencing record type param"                                                   $ Just $ TInt 0
  , pass  "test51" "Reference `for` variable"                                                        $ Just $ TUnit
  , pass  "test52" "`nil` return from `if`+`else` branch"                                            $ Just $ TNil
  , pass  "test53" "`if`+`else` in function"                                                         $ Just $ TRecord (Map.fromList [(Symbol "first", TInt 2), (Symbol "rest", TNil)]) $ Symbol "list"
  , fail  "test54" "Error: Variable defined in terms of self"                                        $ (AError $ VarCannotInitInTermsOfSelf) :| []
  , fail  "test55" "Error: Redundant field in record declaration"                                    $ (AError $ DuplicateFieldInType) :| []
  , fail  "test56" "Error: Redundant field in record instantiation"                                  $ (AError $ DuplicateFieldInInst) :| []
  , fail  "test57" "Error: Setting non-existent field in record and extra fields"                    $ (AError $ MissingProperty $ Symbol "first") :| [AError $ NoSuchProperty uid1, AError $ NoSuchProperty uid2, AError $ MissingProperty $ Symbol "first"]
  , fail  "test58" "Error: Trying to construct record from non-record type"                          $ (AError $ NoSuchRecordType) :| []
  , fail  "test59" "Error: `break` outside of `while`/`for`"                                         $ (AError $ IllegalBreak) :| []
  , pass  "test60" "Legal `break`s in `while`/`for`"                                                 $ Just $ TUnit
  , fail  "test61" "Error: Setting `for` variable"                                                   $ (AError $ CannotSetForVar) :| []
  , fail  "test62" "Error: `for` with non-`Int` upper bound"                                         $ (AError $ TypeMismatch Type.Int Type.String) :| []
  , fail  "test63" "Error: `while` with non-`Unit` body"                                             $ (AError $ TypeMismatch Type.Unit Type.Int) :| []
  , fail  "test64" "Error: `for` with non-`Unit` body"                                               $ (AError $ TypeMismatch Type.Unit Type.Int) :| []
  , pass  "test65" "Forward type reference in record type"                                           $ Just $ TString ""
  , pass  "test66" "Forward nominal type reference"                                                  $ Just $ TString ""
  , fail  "test67" "Error: Test bed for checking escape analysis behavior"                           $ (AError $ NoSuchVariable) :| []
  , pass  "queens" "8-queens benchmark program"                                                      $ Just $ TUnit
  , pass  "merge"  "merge-sort benchmark program"                                                    $ Nothing -- Reads user input
  ]

pass :: Text -> Text -> Maybe Value -> TigerTest
pass name desc valueM = named name desc $ Pass valueM

fail :: Text -> Text -> NonEmpty TigerError -> TigerTest
fail name desc errs = named name desc $ Fail errs

_skip :: Text -> Text -> TigerTest
_skip name desc = named name desc Skip

named :: Text -> Text -> TigerTestResult -> TigerTest
named name desc expected =
  TigerTest { testName = name
            , testFile = asPath $ name <> ".tig"
            , desc
            , expected
            }

uid1 :: UniqueID
uid1 = UniqueID 9001

uid2 :: UniqueID
uid2 = UniqueID 9002

makeTest :: TigerTest -> TestTree
makeTest test = testCase (asString $ testName test <> ": " <> desc test) runIt
  where
    runIt =
      do
        let path  = fixtureDir </> testFile test
        exists   <- doesFileExist path
        if exists then do
          src      <- TIO.readFile path
          fullPath <- makeAbsolute path
          case (compileForTest (fullPath, src), test.expected) of
            (             _, Skip         ) -> return ()
            (Success      _, Fail  reasons) -> assertFail $ shouldHaveFailedMsg     test.desc reasons
            (Failure errors, Pass        _) -> assertFail $ shouldHavePassedMsg src test.desc  errors
            (Failure errors, Fail  reasons) -> checkFailureMatch src (NE.toList errors) $ NE.toList reasons
            (Success      _, Pass  Nothing) -> return ()
            (Success actual, Pass expected) -> do
              valueV <- eval actual.irvExpr
              checkSuccessMatch src expected valueV
        else
          assertFail $ "Fixture file not found: " <> (asText path)

    checkSuccessMatch _ Nothing (Success y) =
      assertFail $ "At runtime, expected the code to fail, but succeeded with `" <> (showText y) <> "`."
    checkSuccessMatch _ (Just x) (Success y) | x /= y =
      assertFail $ "At runtime, expected: `" <> (showText x) <> "`\n" <>
                   "But got:              `" <> (showText y) <> "`"
    checkSuccessMatch src (Just x) (Failure errors) =
      assertFail $ "At runtime, expected `" <> (showText x) <> "`, but the code failed:\n" <>
        (formatErrorOutput src $ map BadEval errors)
    checkSuccessMatch _ _ _ =
      return ()

    checkFailureMatch   _         []          [] = return ()
    checkFailureMatch src     errors          [] = assertFail $ "Unexpected errors:\n" <> (formatErrorOutput src $ NE.fromList errors)
    checkFailureMatch   _         []     reasons = assertFail $ "Missing errors: " <> (showText reasons)
    checkFailureMatch src (e:errors) (r:reasons) = maybe (checkFailureMatch src errors reasons) assertFail $ helper e r
      where
        helper (BadParse    (  ParserError Aborted                    _)) (PError Aborted                   ) = Nothing
        helper (BadParse    (  ParserError BadSyntax                  _)) (PError BadSyntax                 ) = Nothing
        helper (BadAnalysis (AnalyzerError (ArityMismatch x y z)      _)) (AError (ArityMismatch a b c)     ) | x == a && y == b && z == c = Nothing
        helper (BadAnalysis (AnalyzerError BadInternalState           _)) (AError BadInternalState          ) = Nothing
        helper (BadAnalysis (AnalyzerError CannotSetForVar            _)) (AError CannotSetForVar           ) = Nothing
        helper (BadAnalysis (AnalyzerError (CanOnlyIndexArray t)      _)) (AError (CanOnlyIndexArray u)     ) | typesMatch t u = Nothing
        helper (BadAnalysis (AnalyzerError (CanOnlyLookupInRecord t)  _)) (AError (CanOnlyLookupInRecord u) ) | typesMatch t u = Nothing
        helper (BadAnalysis (AnalyzerError DuplicateFieldInInst       _)) (AError DuplicateFieldInInst      ) = Nothing
        helper (BadAnalysis (AnalyzerError DuplicateFieldInType       _)) (AError DuplicateFieldInType      ) = Nothing
        helper (BadAnalysis (AnalyzerError DuplicateFunc              _)) (AError DuplicateFunc             ) = Nothing
        helper (BadAnalysis (AnalyzerError DuplicateType              _)) (AError DuplicateType             ) = Nothing
        helper (BadAnalysis (AnalyzerError IllegalBreak               _)) (AError IllegalBreak              ) = Nothing
        helper (BadAnalysis (AnalyzerError (MissingProperty name1)    _)) (AError (MissingProperty name2)   ) | name1 == name2 = Nothing
        helper (BadAnalysis (AnalyzerError NonUnitProcedure           _)) (AError NonUnitProcedure          ) = Nothing
        helper (BadAnalysis (AnalyzerError NoSuchFn                   _)) (AError NoSuchFn                  ) = Nothing
        helper (BadAnalysis (AnalyzerError (NoSuchProperty _)         _)) (AError (NoSuchProperty _)        ) = Nothing
        helper (BadAnalysis (AnalyzerError NoSuchRecordType           _)) (AError NoSuchRecordType          ) = Nothing
        helper (BadAnalysis (AnalyzerError NoSuchType                 _)) (AError NoSuchType                ) = Nothing
        helper (BadAnalysis (AnalyzerError NoSuchVariable             _)) (AError NoSuchVariable            ) = Nothing
        helper (BadAnalysis (AnalyzerError TypelessVarCannotInitToNil _)) (AError TypelessVarCannotInitToNil) = Nothing
        helper (BadAnalysis (AnalyzerError (TypeMismatch t1 t2)       _)) (AError (TypeMismatch u1 u2)      ) | typesMatch t1 u1 && typesMatch t2 u2 = Nothing
        helper (BadAnalysis (AnalyzerError VarCannotInitInTermsOfSelf _)) (AError VarCannotInitInTermsOfSelf) = Nothing
        helper                                                          a                                   b =
          Just $ "Expected a `" <> (showText b) <> "`, but got a `" <> (showText a) <> "`."

        typesMatch (Array eta _) (Array etb _) = eta == etb
        typesMatch (Record xs _) (Record ys _) = xs == ys
        typesMatch a@(Named _ _)             _ = error $ "How did you get a named `" <> (showText a) <> "` here?!"
        typesMatch             _ b@(Named _ _) = error $ "How did you get a named `" <> (showText b) <> "` here?!"
        typesMatch             a             b = a == b

    shouldHaveFailedMsg desc reasons =
      "Expected phase to reject this program (" <> desc <> "), but it accepted it.\n" <>
        "The missing errors are: " <> (showText reasons)

    shouldHavePassedMsg src desc errors =
      "Expected phase to succeed (" <> desc <> "), but it failed:\n" <>
        (formatErrorOutput src errors)

    assertFail :: HasCallStack => Text -> IO a
    assertFail = asString &> assertFailure

fixtureDir :: FilePath
fixtureDir = "src/test/fixtures/tiger"
