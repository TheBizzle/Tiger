module Tiger.Analyzer.Internal.EscapeAnalyzer(findEscapers) where

import Control.Monad.State(gets, modify)

import Data.List(groupBy)
import Data.Maybe(mapMaybe)

import Tiger.Parser.AST(
    Decl(FunctionDecl, VariableDecl, TypeDecl)
  , Expr( ArrayExpr, AssignExpr, BreakExpr, CallExpr, ForExpr, IfExpr, IntExpr, LetExpr, LValueExpr, NilExpr
        , OpExpr, RecordExpr, SeqExpr, StringExpr, token, WhileExpr
        )
  , Field(Field)
  , FuncDecl(FuncDecl, funcDeclName)
  , LValue(ArrayIndex, RecordField, Variable)
  , Symbol
  , VarDecl(VarDecl, varInit, varName)
  )

import Tiger.Analyzer.Internal.Address(
    FuncAddress(FuncAddress)
  , NamedVarAddress(NamedVarAddress)
  , ScopeAddress(ScopeAddress, n)
  )

import Tiger.Analyzer.Internal.Scope(
    Environment(Env, envFuncs, envVars)
  , FuncInfo(FIAddress)
  , Scope(address, environ, Scope)
  , VarInfo(VIAddress)
  )

import Data.List.NonEmpty qualified as NE
import Data.Map           qualified as Map
import Data.Set           qualified as Set


data EscapistState
  = EscapistState {
      escapistFuncs :: Set FuncAddress
    , escapistVars  :: Set NamedVarAddress
    , lastScopeAddr :: ScopeAddress
    , scopes        :: NonEmpty Scope
    }

type Stately a = State EscapistState a
type Output    = Stately ()

findEscapers :: Expr -> (Set FuncAddress, Set NamedVarAddress)
findEscapers expr = (result.escapistFuncs, result.escapistVars)
  where
    result = execState (findInExpr expr) initialState
    env    = Env Map.empty Map.empty Map.empty
    initialState =
      EscapistState {
        escapistFuncs = Set.empty
      , escapistVars  = Set.empty
      , lastScopeAddr = ScopeAddress 0
      , scopes        = NE.singleton $ Scope env $ ScopeAddress 0
      }

findInExpr :: Expr -> Output
findInExpr (ArrayExpr _ size init              _) = findInExprs [size, init]
findInExpr (AssignExpr lValue rValue           _) = (findInLValue lValue) >> (findInExpr rValue)
findInExpr (BreakExpr                          _) = return ()
findInExpr (CallExpr name args                 _) = (findCall name) >> (findInExprs args)
findInExpr (ForExpr varName lowerB upperB body _) = findInFor varName lowerB upperB body
findInExpr (IfExpr ante conseq altM            _) = findInExprs $ [ante, conseq] <> (maybeToList altM)
findInExpr (IntExpr _                          _) = return ()
findInExpr (LetExpr decls body                 _) = stackFrame $ (findInDecls decls) >> (findInExpr body)
findInExpr (LValueExpr lValue                  _) = findInLValue lValue
findInExpr (NilExpr                            _) = return ()
findInExpr (OpExpr left _ right                _) = findInExprs [left, right]
findInExpr (RecordExpr fields _                _) = findInExprs $ map snd3 fields
findInExpr (SeqExpr statements                 _) = findInExprs $ map fst  statements
findInExpr (StringExpr _                       _) = return ()
findInExpr (WhileExpr cond body                _) = findInExprs [cond, body]

findInExprs :: (Traversable t) => t Expr -> Output
findInExprs = mapM_ findInExpr

findCall :: Symbol -> Output
findCall name =
  do
    head :| upperScopes <- gets scopes
    when (not $ name `Map.member` head.environ.envFuncs) $
      case climbAndFind upperScopes name of
        Just (FIAddress addr) -> modify $ \s -> s { escapistFuncs = Set.insert addr s.escapistFuncs }
        _                     -> return ()
  where
    climbAndFind    []        _ = Nothing
    climbAndFind (h:t) funcName = (funcName `Map.lookup` h.environ.envFuncs) <|> climbAndFind t funcName

findInDecls :: [Decl] -> Output
findInDecls decls = mapM_ findInBatch $ buildBatches decls
  where
    buildBatches :: [Decl] -> [Either (NonEmpty FuncDecl) (NonEmpty VarDecl)]
    buildBatches = (groupBy sameConstructor) &> (mapMaybe groupToBatch)

    sameConstructor (FunctionDecl _) (FunctionDecl _) = True
    sameConstructor (TypeDecl     _) (TypeDecl     _) = True
    sameConstructor (VariableDecl _) (VariableDecl _) = True
    sameConstructor                _                _ = False

    groupToBatch js =
      case nonEmpty js of
        Nothing -> error "Not possible for `groupBy` to produce an empty group!"
        Just ne ->
          case ne of
            (VariableDecl _ :| _) -> Just $ Right $ map (\case (VariableDecl z) -> z; _ -> error "cannot") ne
            (FunctionDecl _ :| _) -> Just $  Left $ map (\case (FunctionDecl x) -> x; _ -> error "cannot") ne
            (TypeDecl     _ :| _) -> Nothing

findInBatch :: Either (NonEmpty FuncDecl) (NonEmpty VarDecl) -> Output
findInBatch (Right vars) = mapM_ (\v -> (storeVar v.varName) >> (findInExpr v.varInit)) vars
findInBatch (Left funcs) =
  do
    void $ mapM (funcDeclName &> storeFunc) funcs
    void $ findInExprs $ map (\(FuncDecl _ params _ body _) -> buildBody params body) funcs
  where
    buildBody params baseBody = LetExpr (map makeDecl params) baseBody baseBody.token
      where
        makeDecl (Field name typeName nameToken typeToken) =
          VariableDecl $ VarDecl name (Just (typeName, typeToken)) (SeqExpr [] nameToken) nameToken

    storeFunc name =
      updateScope $
        \scope ->
          let addr = FIAddress $ FuncAddress name scope.address in
          scope {
            environ = scope.environ { envFuncs = Map.insert name addr scope.environ.envFuncs }
          }

findInFor :: Symbol -> Expr -> Expr -> Expr -> Output
findInFor varName lowerB upperB body =
  stackFrame $ do
    findInExprs [lowerB, upperB]
    storeVar varName
    findInExpr body

findInLValue :: LValue -> Output
findInLValue (ArrayIndex  lvalue indexExpr _) = (findInLValue lvalue) >> (findInExpr indexExpr)
findInLValue (RecordField lvalue         _ _) =  findInLValue lvalue
findInLValue (Variable                name _) =
  do
    head :| upperScopes <- gets scopes
    when (not $ name `Map.member` head.environ.envVars) $
      case climbAndFind upperScopes name of
        Just (VIAddress addr) -> modify $ \s -> s { escapistVars = Set.insert addr s.escapistVars }
        _                     -> return ()
  where
    climbAndFind    []       _ = Nothing
    climbAndFind (h:t) varName = (varName `Map.lookup` h.environ.envVars) <|> climbAndFind t varName

storeVar :: Symbol -> Output
storeVar name =
  updateScope $
    \scope ->
      let addr = VIAddress $ NamedVarAddress name scope.address in
      scope {
        environ = scope.environ { envVars = Map.insert name addr scope.environ.envVars }
      }

updateScope :: (Scope -> Scope) -> Stately ()
updateScope f = modify $ \s -> s { scopes = case scopes s of h :| t -> (f h) :| t }

popScope :: Stately ()
popScope = modify helper
  where
    helper state = state { scopes = newScopes }
      where
        (_myScope, tailMNE) = NE.uncons state.scopes
        newScopes           = tailMNE `orElse` (error whinerMsg)
        whinerMsg           = "Critical error!  You should never be able to pop the global scope!" :: Text

pushScope :: Stately ()
pushScope = modify helper
  where
    helper state = state { scopes = newScope `NE.cons` state.scopes, lastScopeAddr = newAddr }
      where
        newAddr  = ScopeAddress $ state.lastScopeAddr.n + 1
        newScope = Scope (Env Map.empty Map.empty Map.empty) newAddr

stackFrame :: Stately a -> Stately a
stackFrame fv = pushScope *> fv <* popScope
