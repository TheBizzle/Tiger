module Tiger.Analyzer.Internal.LValueAnalyzer(crawlLValue) where

import Control.Monad.State(gets)

import Tiger.Parser.AST(Expr(LValueExpr, token), LValue(ArrayIndex, RecordField, Variable))

import Tiger.Analyzer.Internal.Address(LValueAddress(ArrayElemAddress, RecordPropAddress, VarAddress))

import Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerErrorType( BadInternalState, CannotSetForVar, CanOnlyIndexArray, CanOnlyLookupInRecord
                     , NoSuchProperty, NoSuchRecordType, NoSuchVariable, VarCannotInitInTermsOfSelf
                     )
  )

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(vars)
  , andIfValidMV, err, fail, findFieldAddr, findInEnv, lookupTypeOf, protecteds, resolveTypeAddr, typeErrorOr
  , Verification, win
  )

import Tiger.Analyzer.Internal.IRValue(IRValue(IRValue))

import Tiger.Analyzer.Internal.Scope(
    Environment(envVars)
  , VarInfo(PreVar, VIAddress)
  )

import Tiger.Analyzer.Internal.Type(Type(Array, Record, Unit))

import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Tiger.Analyzer.Internal.Type as Type


crawlLValue :: (Expr -> Verification IRValue) -> Bool -> LValue -> Verification IRValue
crawlLValue crawlExpr isSetting var@(Variable symbol token) =
  (lookupInfoOf crawlExpr var) `andIfValidMV` (
    \(addrM, typ) -> do
      let goodEnd = win $ IRValue (LValueExpr (Variable symbol token) token) typ
      case (isSetting, addrM) of
        (True, Just (VarAddress addr)) -> do
          prots <- gets protecteds
          if addr `Set.member` prots then
            fail CannotSetForVar token
          else
            goodEnd
        (_, _) -> goodEnd
    )

crawlLValue crawlExpr _ (RecordField lvalue property token) =
  (lookupInfoOf crawlExpr lvalue) `andIfValidMV` (
    \case
      (Nothing, _) ->
        fail NoSuchRecordType token
      (Just addr, Record fields uid) ->
        if not $ any (fst &> (== property)) fields then
          fail (NoSuchProperty uid) token
        else
          (lookupTypeOf token (RecordPropAddress addr property)) `andIfValidMV` (
            \typ -> win $ IRValue (LValueExpr (RecordField lvalue property token) token) typ
            )
      (_, badType) ->
        fail (CanOnlyLookupInRecord badType) token
    )

crawlLValue crawlExpr _ (ArrayIndex lvalue index token) =
  (lookupInfoOf crawlExpr lvalue) `andIfValidMV` (
    \case
      (_, Array elemTypeAddr _) -> do
        let expr = LValueExpr (ArrayIndex lvalue index token) token
        (resolveTypeAddr token elemTypeAddr) `andIfValidMV` (IRValue expr &> win)
      (_, badType) ->
        fail (CanOnlyIndexArray badType) token
    )

lookupInfoOf :: (Expr -> Verification IRValue) -> LValue -> Verification (Maybe LValueAddress, Type)
lookupInfoOf crawlExpr (ArrayIndex lvalue indexExpr _) =
  (crawlExpr indexExpr) `andIfValidMV` (
    \case
      (IRValue _ indexType) ->
        (indexType, Type.Int, indexExpr.token) `typeErrorOr` do
          infoV          <- lookupInfoOf crawlExpr lvalue
          let pairV       = infoV `bindValidation` (makePair indexExpr)
          pairV `failOrM` (
            \(addrM, addr) ->
              (resolveTypeAddr indexExpr.token addr) `andIfValidMV` (\typ -> win (addrM, typ))
            )
    )
  where
    makePair     _ (addrM, Array eType _) = Success $ (map ArrayElemAddress addrM, eType)
    makePair iExpr (    _,             _) = err BadInternalState iExpr.token

lookupInfoOf crawlExpr (RecordField lvalue propName token) =
    (lookupInfoOf crawlExpr lvalue) `andIfValidMV` (
      \(addrM, typ) ->
        (findAddr typ) `failOrM` (resolveTypeAddr token) `andIfValidMV` (
          \fieldType -> win (map (flip RecordPropAddress propName) addrM, fieldType)
          )
      )
  where
    findAddr (Record fields uid) = findFieldAddr token uid propName fields
    findAddr                   _ = err NoSuchRecordType token

lookupInfoOf _ (Variable varName token) =
  findInEnv varName token envVars NoSuchVariable $
    \case
      PreVar Unit    -> fail VarCannotInitInTermsOfSelf token
      PreVar typ     -> win (Nothing, typ)
      VIAddress addr -> do
        vars     <- gets vars
        let varV  = maybe (err BadInternalState token) Success $ Map.lookup addr vars
        varV `failOrM` (\var -> win (Just $ VarAddress addr, var))
