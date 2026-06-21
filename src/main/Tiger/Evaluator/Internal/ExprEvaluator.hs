module Tiger.Evaluator.Internal.ExprEvaluator(evalExpr, evalInt, evalString) where

import Control.Monad.State(gets, modify)

import Data.List((!!))
import Data.Maybe(fromJust)

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(
    Decl(VariableDecl)
  , Expr( ArrayExpr, AssignExpr, BreakExpr, CallExpr, ForExpr, IfExpr, IntExpr, LetExpr, LValueExpr, NilExpr
        , OpExpr, RecordExpr, SeqExpr, StringExpr, token, WhileExpr
        )
  , LValue(ArrayIndex, lValToken, RecordField, Variable)
  , Operator( DivideOp, EqualsOp, GreaterOrEqualsOp, GreaterThanOp, LessOrEqualsOp, LessThanOp, MinusOp
            , NotEqualsOp, PlusOp, TimesOp
            )
  , Symbol
  , VarDecl(VarDecl)
  )

import Tiger.Evaluator.Internal.Common(
    andIfValidMV, andIfValidMVNoB, err, Evaluation
  , EvaluatorState(functions, vars)
  , fail, findInEnv, flattenVM
  , FnBody(PrimitiveBody, UserDefinedBody)
  , Function(Function)
  , mapMSequA, mapMSequANoB, stackFrame, win
  )

import Tiger.Evaluator.Internal.DeclEvaluator(evalDecls)

import Tiger.Evaluator.Internal.EvaluatorError(
    EvaluatorErrorType(DivisionByZero, NoSuchFn, NoSuchVar, TypeMismatch)
  )

import Tiger.Evaluator.Internal.Scope(
    Environment(envFuncs, envVars)
  , LValueAddress(ArrayElemAddress, RecordPropAddress, VarAddress)
  )

import Tiger.Evaluator.Internal.Value(
    ControlFlow(Break, Pure)
  , typeOf
  , Value(TArray, TInt, TNil, TRecord, TString, TUnit)
  )

import Data.List                     qualified as List
import Data.List.NonEmpty            qualified as NE
import Data.Map                      qualified as Map
import Tiger.Evaluator.Internal.Type qualified as Type


evalExpr :: Expr -> Evaluation ControlFlow
evalExpr      (ArrayExpr _ size init                  _) = evalNewArray size init
evalExpr      (AssignExpr lValue rValue               _) = evalAssign lValue rValue
evalExpr      (BreakExpr                          token) = evalBreak token
evalExpr      (CallExpr name args                 token) = evalCall name args token
evalExpr      (ForExpr varName lowerB upperB body token) = evalFor varName lowerB upperB body token
evalExpr      (IfExpr ante conseq alt                 _) = evalIf ante conseq alt
evalExpr      (IntExpr n                              _) = winCF $ TInt n
evalExpr      (LetExpr decls body                     _) = evalLet decls body
evalExpr      (LValueExpr lValue                  token) = evalLValue evalExpr lValue token
evalExpr      (NilExpr                                _) = winCF TNil
evalExpr this@(OpExpr left op right                   _) = evalOperator left op right this
evalExpr      (RecordExpr fields typeName             _) = evalRecord fields typeName
evalExpr      (SeqExpr statements                     _) = evalStatements statements
evalExpr      (StringExpr s                           _) = winCF $ TString s
evalExpr      (WhileExpr cond body                    _) = evalWhile cond body

evalExprNoB :: Expr -> Evaluation Value
evalExprNoB expr = (evalExpr expr) `andIfValidMVNoB` win

evalNewArray :: Expr -> Expr -> Evaluation ControlFlow
evalNewArray sizeExpr initExpr =
  do
    sizeV <- evalInt     sizeExpr
    initV <- evalExprNoB initExpr
    ((,) <$> sizeV <*> initV) `failOrM` (
      \(size, init) -> winCF $ TArray $ map (const init) [1..size]
      )

evalAssign :: LValue -> Expr -> Evaluation ControlFlow
evalAssign lValue rValue =
  do
    laddrV <- lookupLValue evalExpr lValue
    valueV <- evalExprNoB           rValue
    flattenVM $ (updateLV []) <$> laddrV <*> valueV
  where
    updateLV trail (ArrayElemAddress  arrayAddr index) value = updateLV ((Left index):trail)  arrayAddr value
    updateLV trail (RecordPropAddress recordAddr prop) value = updateLV ((Right prop):trail) recordAddr value
    updateLV trail (VarAddress                varAddr) value =
      (gets $ vars &> Map.lookup varAddr &> maybe (err NoSuchVar lValue.lValToken) Success) `andIfValidMV` (
        \var -> do
          let newRoot = helper trail var value
          modify $ \s -> s { vars = Map.insert varAddr newRoot s.vars }
          winCF TUnit
        )

    helper [] _ val =
      val
    helper ((Left n):t) (TArray xs) val =
      TArray $ List.take n xs <> [helper t (xs !! n) val] <> List.drop (n + 1) xs
    helper ((Right prop):t) (TRecord obj name) val =
      TRecord (Map.adjust (\v -> helper t v val) prop obj) name
    helper _ _ _ =
      error "Bad program state when assigning"

evalBreak :: Token -> Evaluation ControlFlow
evalBreak token = win $ Break token

evalCall :: Symbol -> [Expr] -> Token -> Evaluation ControlFlow
evalCall name argExprs token =
  (lookupFunction name token) `andIfValidMV` (
    \(Function paramNames body) ->
      case (nonEmpty argExprs, body) of
        (    Nothing,   PrimitiveBody pbody) -> wrapCF pbody
        (    Nothing, UserDefinedBody  expr) -> evalExpr expr
        (Just argsNE,                     _) -> do
          let pairs = NE.zip (NE.fromList paramNames) argsNE
          let decls = map (\(aName, aVal) -> VariableDecl $ VarDecl aName Nothing aVal token) pairs
          case body of
            PrimitiveBody   pbody -> evalLetBase decls $ Left pbody
            UserDefinedBody  expr -> evalLet     decls expr
    )
  where
    lookupFunction :: Symbol -> Token -> Evaluation Function
    lookupFunction name' token' =
      findInEnv name' token' envFuncs NoSuchFn $
        \addr -> do
          funcs     <- gets functions
          let funcM  = Map.lookup addr funcs
          maybe (fail NoSuchFn token) win funcM

evalFor :: Symbol -> Expr -> Expr -> Expr -> Token -> Evaluation ControlFlow
evalFor varName lowerB upperB body token =
  do
    lowerV <- evalInt lowerB
    upperV <- evalInt upperB
    flattenVM $ runFor <$> lowerV <*> upperV
  where
    synthesizeLet name i = NE.singleton $ VariableDecl $ VarDecl name Nothing (IntExpr i token) token

    runFor :: Int -> Int -> Evaluation ControlFlow
    runFor i upper =
      if i <= upper then
        (stackFrame $ (evalDecls evalExpr $ synthesizeLet varName i) >> (evalExpr body)) `andIfValidMV` (
          \case
            Break _ -> winCF TUnit
            _       -> runFor (i + 1) upper
          )
      else
        winCF TUnit

evalIf :: Expr -> Expr -> Maybe Expr -> Evaluation ControlFlow
evalIf antecedent consequent alternativeM =
  (evalBool antecedent) `andIfValidMV` (
    \case
      True  -> evalExpr consequent
      False -> maybe (winCF TUnit) evalExpr alternativeM
    )

evalLet :: NonEmpty Decl -> Expr -> Evaluation ControlFlow
evalLet decls body = evalLetBase decls $ Right body

evalLetBase :: NonEmpty Decl -> Either (Evaluation Value) Expr -> Evaluation ControlFlow
evalLetBase decls bodyE =
  stackFrame $ (evalDecls evalExpr decls) `andIfValidMV` (const $ either wrapCF evalExpr bodyE)

evalLValue :: (Expr -> Evaluation ControlFlow) -> LValue -> Token -> Evaluation ControlFlow
evalLValue evaluExpr lvalue token =
    lookupLValue evaluExpr lvalue `andIfValidMV` (resolveLValue [])
  where
    resolveLValue :: [Either Int Symbol] -> LValueAddress -> Evaluation ControlFlow
    resolveLValue trail (ArrayElemAddress   arrayAddr index) = resolveLValue ((Left index):trail)  arrayAddr
    resolveLValue trail (RecordPropAddress recordAddr  prop) = resolveLValue ((Right prop):trail) recordAddr
    resolveLValue trail (VarAddress varAddr) =
      do
        varM <- gets $ vars &> Map.lookup varAddr
        maybe (fail NoSuchVar token) (helper trail) varM
      where
        helper               []             acc = winCF acc
        helper ((Left     n):t) (TArray     xs) = helper t $ xs !! n
        helper ((Right prop):t) (TRecord obj _) = helper t $ fromJust $ Map.lookup prop obj
        helper                _               _ = error "Bad program state when looking up value"

evalOperator :: Expr -> Operator -> Expr -> Expr -> Evaluation ControlFlow
evalOperator left op right expr =
  do
    leftV  <- evalExprNoB left
    rightV <- evalExprNoB right
    ((, op, ) <$> leftV <*> rightV) `failOrM` (
      \case
        (TInt    n1,            PlusOp, TInt    n2) -> winCF $ TInt $ n1 + n2
        (TInt    n1,           MinusOp, TInt    n2) -> winCF $ TInt $ n1 - n2
        (TInt    n1,           TimesOp, TInt    n2) -> winCF $ TInt $ n1 * n2
        (TInt     _,          DivideOp, TInt     0) -> fail DivisionByZero expr.token
        (TInt    n1,          DivideOp, TInt    n2) -> winCF $ TInt $ n1 `quot` n2
        (         x,          EqualsOp,          y) -> winCF $ TInt $ boolInt $       x == y
        (         x,       NotEqualsOp,          y) -> winCF $ TInt $ boolInt $ not $ x == y
        (TInt    n1,        LessThanOp, TInt    n2) -> winCF $ TInt $ boolInt $ n1 <  n2
        (TString s1,        LessThanOp, TString s2) -> winCF $ TInt $ boolInt $ s1 <  s2
        (TInt    n1,    LessOrEqualsOp, TInt    n2) -> winCF $ TInt $ boolInt $ n1 <= n2
        (TString s1,    LessOrEqualsOp, TString s2) -> winCF $ TInt $ boolInt $ s1 <= s2
        (TInt    n1,     GreaterThanOp, TInt    n2) -> winCF $ TInt $ boolInt $ n1 >  n2
        (TString s1,     GreaterThanOp, TString s2) -> winCF $ TInt $ boolInt $ s1 >  s2
        (TInt    n1, GreaterOrEqualsOp, TInt    n2) -> winCF $ TInt $ boolInt $ n1 >= n2
        (TString s1, GreaterOrEqualsOp, TString s2) -> winCF $ TInt $ boolInt $ s1 >= s2
        (         x,                 _,          _) -> fail (TypeMismatch Type.Int $ typeOf x) expr.token
      )
  where
    boolInt :: Bool -> Int
    boolInt True  = 1
    boolInt False = 0

evalRecord :: [(Symbol, Expr, Token)] -> Symbol -> Evaluation ControlFlow
evalRecord fields typeName =
  (mapMSequANoB (snd3 &> evalExpr) fields) `andIfValidMV` (
    \values -> do
      let pairs = List.zip (map fst3 fields) values
      winCF $ TRecord (Map.fromList pairs) typeName
    )

evalStatements :: [(Expr, Token)] -> Evaluation ControlFlow
evalStatements pairs =
  (mapMSequA (fst &> evalExpr) pairs) `andIfValidMV` (
    \values -> win $ foldl' scoopUpLastValue (Pure TUnit) values
    )
  where
    scoopUpLastValue acc@(Break _) _ = acc
    scoopUpLastValue             _ x = x

evalWhile :: Expr -> Expr -> Evaluation ControlFlow
evalWhile cond body =
  (evalBool cond) `andIfValidMV` (
    \case
      False -> winCF TUnit
      True  -> evalExpr body `andIfValidMV` (
        \case
          Break _ -> winCF TUnit
          _       -> evalWhile cond body
        )
    )

evalBool :: Expr -> Evaluation Bool
evalBool expr =
  (evalInt expr) `andIfValidMV` (
    \case
       0 -> win False
       _ -> win True
    )

evalInt :: Expr -> Evaluation Int
evalInt expr =
  (evalExpr expr) `andIfValidMVNoB` (
    \case
       (TInt n) -> win n
       x        -> fail (TypeMismatch Type.Int $ typeOf x) expr.token
    )

evalString :: Expr -> Evaluation Text
evalString expr =
  (evalExpr expr) `andIfValidMVNoB` (
    \case
       (TString s) -> win s
       x           -> fail (TypeMismatch Type.String $ typeOf x) expr.token
    )

lookupLValue :: (Expr -> Evaluation ControlFlow) -> LValue -> Evaluation LValueAddress
lookupLValue evaluExpr (ArrayIndex lvalue indexExpr _) =
  do
    addrV  <- lookupLValue evaluExpr lvalue
    indexV <- evalInt indexExpr
    return $ ArrayElemAddress <$> addrV <*> indexV

lookupLValue evaluExpr (RecordField lvalue property _) =
  (lookupLValue evaluExpr lvalue) `andIfValidMV` (
    \addr -> win $ RecordPropAddress addr property
    )

lookupLValue _ (Variable varName token) =
  findInEnv varName token envVars NoSuchVar $ VarAddress &> Success &> return

winCF :: Value -> Evaluation ControlFlow
winCF value = win $ Pure value

wrapCF :: Evaluation Value -> Evaluation ControlFlow
wrapCF res = res >>= (map Pure &> return)
