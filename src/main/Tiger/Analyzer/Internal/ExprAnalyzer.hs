module Tiger.Analyzer.Internal.ExprAnalyzer(crawlAST, crawlExpr) where

import Control.Monad.State(gets, modify)

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(
    Decl(VariableDecl)
  , Expr( ArrayExpr, AssignExpr, BreakExpr, CallExpr, ForExpr, IfExpr, IntExpr, LetExpr, LValueExpr, NilExpr
        , OpExpr, RecordExpr, SeqExpr, StringExpr, token, WhileExpr
        )
  , LValue
  , Operator( DivideOp, EqualsOp, GreaterOrEqualsOp, GreaterThanOp, LessOrEqualsOp, LessThanOp, MinusOp
            , NotEqualsOp, PlusOp, TimesOp
            )
  , Symbol
  , VarDecl(VarDecl)
  )

import Tiger.Analyzer.Internal.Address(NamedVarAddress(NamedVarAddress))

import Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerErrorType( ArityMismatch, BadInternalState, DuplicateFieldInInst, IllegalBreak, MissingProperty
                     , NoSuchFn, NoSuchProperty, NoSuchRecordType, TypeMismatch
                     )
  )

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(functions, isInFor, isInWhile, protecteds)
  , andIfValidMV, err, fail, findInEnv, isSubtypeOf, lookupTypeOfSym, mapMSequA, resolveTypeAddr, scopes
  , stackFrame, typeErrorOr, Verification, win
  )

import Tiger.Analyzer.Internal.DeclAnalyzer(crawlDecls)
import Tiger.Analyzer.Internal.Function(arity, Function(Function))
import Tiger.Analyzer.Internal.LValueAnalyzer(crawlLValue)
import Tiger.Analyzer.Internal.IRValue(IRValue(IRValue, irvType))
import Tiger.Analyzer.Internal.Scope(Environment(envFuncs), FuncInfo(FIAddress, PreFunc), Scope(Scope))
import Tiger.Analyzer.Internal.Type(Type(Array, Record, Unit), UniqueID(UniqueID))

import Data.List                    qualified as List
import Data.List.NonEmpty           qualified as NE
import Data.Map                     qualified as Map
import Data.Set                     qualified as Set
import Tiger.Analyzer.Internal.Type qualified as Type


crawlAST :: Expr -> Verification IRValue
crawlAST expr = crawlExpr expr

crawlExpr :: Expr -> Verification IRValue
crawlExpr      (ArrayExpr typeName size init           token) = crawlNewArray typeName size init token
crawlExpr      (AssignExpr lValue rValue               token) = crawlAssign lValue rValue token
crawlExpr      (BreakExpr                              token) = crawlBreak token
crawlExpr      (CallExpr name args                     token) = crawlCall name args token
crawlExpr      (ForExpr varName isE lowerB upperB body token) = crawlFor varName isE lowerB upperB body token
crawlExpr      (IfExpr ante conseq alt                 token) = crawlIf ante conseq alt token
crawlExpr this@(IntExpr _                                  _) = win $ IRValue this Type.Int
crawlExpr      (LetExpr decls body                     token) = crawlLet decls body token
crawlExpr      (LValueExpr lValue                          _) = crawlLValue crawlExpr False lValue
crawlExpr this@(NilExpr                                    _) = win $ IRValue this Type.Nil
crawlExpr this@(OpExpr left op right                       _) = crawlOperator left op right this
crawlExpr      (RecordExpr fields typeName             token) = crawlRecord fields typeName token
crawlExpr this@(SeqExpr statements                         _) = crawlStatements statements this
crawlExpr this@(StringExpr _                               _) = win $ IRValue this Type.String
crawlExpr      (WhileExpr cond body                    token) = crawlWhile cond body token

crawlNewArray :: Symbol -> Expr -> Expr -> Token -> Verification IRValue
crawlNewArray typeName size init token =
  do
    sizeV <- crawlExpr size
    initV <- crawlExpr init
    typeV <- lookupTypeOfSym token typeName
    ((,) <$> initV <*> typeV <* sizeV) `failOrM` (
      \case
        (IRValue _ initType, literalType@(Array elemTypeAddr _)) ->
          (resolveTypeAddr token elemTypeAddr) `andIfValidMV` (
            \elemType ->
              (initType, elemType, init.token) `typeErrorOr` do
                win $ IRValue (ArrayExpr typeName size init token) literalType
            )
        (_, literalType) ->
          fail (TypeMismatch (Array undefined $ UniqueID 100001) literalType) init.token
      )

crawlAssign :: LValue -> Expr -> Token -> Verification IRValue
crawlAssign lValue rValue token =
  do
    leftV  <- crawlLValue crawlExpr True lValue
    rightV <- crawlExpr rValue
    ((,) <$> leftV <*> rightV) `failOrM` (uncurry buildAssignment)
  where
    buildAssignment (IRValue _ lType) (IRValue rExpr rType) =
      (rType, lType, rExpr.token) `typeErrorOr` do
        win $ IRValue (AssignExpr lValue rValue token) Unit

crawlBreak :: Token -> Verification IRValue
crawlBreak token =
  do
    isInFor   <- gets isInFor
    isInWhile <- gets isInWhile
    if (not isInFor) && (not isInWhile) then
      fail IllegalBreak token
    else
      win $ IRValue (BreakExpr token) Type.Unit

crawlCall :: Symbol -> [Expr] -> Token -> Verification IRValue
crawlCall name argExprs token =
  do
    fnM <- lookupFunction name token
    fnM `failOrM` (
      \fn@(Function params returnType _) ->
        if (arity fn) /= (length argExprs) then
          fail (ArityMismatch name (arity fn) $ length argExprs) token
        else
          (mapMSequA crawlExpr argExprs) `andIfValidMV` (
            \args -> do
              let pairs     = List.zip (map snd params) args
              let mismatchM = find (\(paramT, IRValue _ argT) -> not $ paramT `isSubtypeOf` argT) pairs
              case mismatchM of
                Just (paramT, IRValue expr typ) -> fail (TypeMismatch paramT typ) expr.token
                Nothing                         -> win $ IRValue (CallExpr name argExprs token) returnType
            )
      )
  where
    lookupFunction :: Symbol -> Token -> Verification Function
    lookupFunction name' token' =
      findInEnv name' token' envFuncs NoSuchFn $
        \case
          PreFunc params typ -> win $ Function params typ undefined
          FIAddress addr     -> do
            funcs     <- gets functions
            let funcM  = Map.lookup addr funcs
            maybe (fail BadInternalState token) win funcM

crawlFor :: Symbol -> Bool -> Expr -> Expr -> Expr -> Token -> Verification IRValue
crawlFor varName isEscape lowerB upperB body token =
  stackFrame $ do
    wasInFor <- gets isInFor
    modify $ \s -> s { isInFor = True }

    lowerV <- crawlExpr lowerB
    varV   <- crawlDecls crawlExpr $ NE.singleton $ VariableDecl $ VarDecl varName True Nothing lowerB token

    (Scope _ addr) :| _ <- gets scopes
    let varAddr          = NamedVarAddress varName addr
    modify $ \s -> s { protecteds = varAddr `Set.insert` s.protecteds }

    upperV <- crawlExpr upperB
    bodyV  <- crawlExpr body
    res    <- ((,,) <$> (varV *> lowerV) <*> upperV <*> bodyV) `failOrM` (uncurry3 buildFor)

    modify $ \s -> s { isInFor = wasInFor }
    return res
  where
    buildFor (IRValue lowerExpr lowerType) (IRValue upperExpr upperType) (IRValue bodyExpr bodyType) =
      ( lowerType, Type.Int , lowerExpr.token) `typeErrorOr` do
      ( upperType, Type.Int , upperExpr.token) `typeErrorOr` do
        (bodyType, Type.Unit,  bodyExpr.token) `typeErrorOr` do
          win $ IRValue (ForExpr varName isEscape lowerB upperB body token) bodyType

crawlIf :: Expr -> Expr -> Maybe Expr -> Token -> Verification IRValue
crawlIf antecedent consequent alternativeM token =
    do
      anteV    <- crawlExpr antecedent
      conseqV  <- crawlExpr consequent
      altMV    <- traverse crawlExpr alternativeM
      let altV  = sequenceA altMV
      ((,,) <$> anteV <*> conseqV <*> altV) `failOrM` checkIf
  where
    checkIf ((IRValue anteExpr anteType), (IRValue conseqExpr conseqType), altM) =
      (anteType, Type.Int, anteExpr.token) `typeErrorOr`
        case altM of
          Just (IRValue altExpr altType) ->
            (altType, conseqType, altExpr.token) `typeErrorOr`
              buildIf anteExpr conseqExpr (Just altExpr) conseqType
          Nothing ->
            (conseqType, Unit, conseqExpr.token) `typeErrorOr`
              buildIf anteExpr conseqExpr Nothing conseqType

    buildIf ante conseq altM typ = win $ IRValue (IfExpr ante conseq altM token) typ

crawlLet :: (NonEmpty Decl) -> Expr -> Token -> Verification IRValue
crawlLet decls body token =
  stackFrame $ do
    declsV <- crawlDecls crawlExpr decls
    bodyV  <- declsV `failOrM` (const $ crawlExpr body)
    bodyV `failOrM` (irvType &> (IRValue $ LetExpr decls body token) &> win)

crawlOperator :: Expr -> Operator -> Expr -> Expr -> Verification IRValue
crawlOperator left op right thisExpr =
  do
    lefty     <- crawlExpr left
    righty    <- crawlExpr right
    let pairV  = (\l r -> (irvType l, irvType r)) <$> lefty <*> righty
    pairV `failOrM` (uncurry $ f op)
  where
    f PlusOp            = ints
    f MinusOp           = ints
    f TimesOp           = ints
    f DivideOp          = ints
    f EqualsOp          = anys
    f NotEqualsOp       = anys
    f LessThanOp        = intstrs
    f LessOrEqualsOp    = intstrs
    f GreaterThanOp     = intstrs
    f GreaterOrEqualsOp = intstrs

    ints Type.Int  Type.Int = win $ IRValue thisExpr Type.Int
    ints Type.Int rightType = fail (TypeMismatch Type.Int rightType) right.token
    ints leftType         _ = fail (TypeMismatch Type.Int  leftType)  left.token

    anys leftType rightType
      | leftType `isSubtypeOf` rightType = win $ IRValue thisExpr Type.Int
      | otherwise                        = fail (TypeMismatch leftType rightType) right.token

    intstrs Type.String Type.String = win $ IRValue thisExpr Type.String
    intstrs    Type.Int    Type.Int = win $ IRValue thisExpr Type.Int
    intstrs    Type.Int   rightType = fail (TypeMismatch Type.Int    rightType) right.token
    intstrs Type.String   rightType = fail (TypeMismatch Type.String rightType) right.token
    intstrs    leftType    Type.Int = fail (TypeMismatch Type.Int     leftType)  left.token
    intstrs    leftType Type.String = fail (TypeMismatch Type.String  leftType)  left.token
    intstrs    leftType           _ = fail (TypeMismatch Type.Int     leftType)  left.token

crawlRecord :: [(Symbol, Expr, Token)] -> Symbol -> Token -> Verification IRValue
crawlRecord fields typeName lToken =
  do
    let dupeTokens  = fst $ foldl' checkForDupes ([], Set.empty) fields
    let checkedVs   = traverse (err DuplicateFieldInInst) $ List.reverse dupeTokens
    typeV          <- lookupRecordType lToken typeName
    (checkedVs *> typeV) `failOrM` (
      \case
        typ@(Record typeFields uid) -> do
          let typeFNames    = Set.fromList $ map fst typeFields
          let extraTokens   = fst $ foldl' checkForExtras ([], typeFNames) fields
          let extraVs       = traverse (err (NoSuchProperty uid)) $ List.reverse extraTokens
          let missingTokens = fst $ foldl' checkForMissing ([], Set.fromList $ map fst3 fields) typeFNames
          let missingVs     = traverse (\s -> err (MissingProperty s) lToken) $ List.reverse missingTokens
          (extraVs *> missingVs) `failOrM` (const $ win $ IRValue (RecordExpr fields typeName lToken) typ)
        typ -> fail (TypeMismatch (Record [] $ UniqueID 100001) typ) lToken
      )
  where
    checkForDupes :: ([Token], Set Symbol) -> (Symbol, a, Token) -> ([Token], Set Symbol)
    checkForDupes (baddies, names) (name, _, token) =
      if name `Set.member` names then
        (token : baddies, name `Set.insert` names)
      else
        (        baddies, name `Set.insert` names)

    checkForExtras :: ([Token], Set Symbol) -> (Symbol, a, Token) -> ([Token], Set Symbol)
    checkForExtras (baddies, expecteds) (name, _, token) =
      if not $ name `Set.member` expecteds then
        (token : baddies, name `Set.insert` expecteds)
      else
        (        baddies, name `Set.insert` expecteds)

    checkForMissing :: ([Symbol], Set Symbol) -> Symbol -> ([Symbol], Set Symbol)
    checkForMissing (baddies, actuals) name =
      if not $ name `Set.member` actuals then
        (name : baddies, name `Set.insert` actuals)
      else
        (       baddies, name `Set.insert` actuals)

    lookupRecordType :: Token -> Symbol -> Verification Type
    lookupRecordType token name =
      (lookupTypeOfSym token name) `andIfValidMV` (
        \case typ@(Record _ _) -> win typ
              _                -> fail NoSuchRecordType token
        )

crawlStatements :: [(Expr, Token)] -> Expr -> Verification IRValue
crawlStatements pairs this =
  (mapMSequA (fst &> crawlExpr) pairs) `andIfValidMV` (
    \exprs -> win $ IRValue this $ (map irvType $ lastMaybe exprs) `orElse` Unit
    )
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ List.last xs

crawlWhile :: Expr -> Expr -> Token -> Verification IRValue
crawlWhile cond body token =
  do
    wasInWhile <- gets isInWhile
    modify $ \s -> s { isInWhile = True }

    condV <- crawlExpr cond
    bodyV <- crawlExpr body
    res   <- ((,) <$> condV <*> bodyV) `failOrM` buildWhile

    modify $ \s -> s { isInWhile = wasInWhile }
    return res
  where
    buildWhile ((IRValue condExpr condType), (IRValue bodyExpr bodyType)) =
      (condType, Type.Int, condExpr.token) `typeErrorOr` do
      (bodyType,     Unit, bodyExpr.token) `typeErrorOr` do
        win $ IRValue (WhileExpr cond body token) bodyType
