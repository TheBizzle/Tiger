module Tiger.Evaluator.Internal.Common(
    andIfValidMV, andIfValidMVNoB, err, Evaluated, Evaluation, fail, failOrMNoB, findInEnv, flattenVM
  , EvaluatorState(EvaluatorState, functions, lastScopeAddr, scopes, vars)
  , FnBody(pBody, PrimitiveBody, udBody, UserDefinedBody)
  , Function(fBody, fParamNames, Function)
  , mapMSequA, mapMSequANoB, stackFrame, updateEnv, win
  ) where

import Control.Monad.State(gets, modify, StateT)

import Data.List.NonEmpty((<|))

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Expr, Symbol)

import Tiger.Evaluator.Internal.EvaluatorError(
    EvaluatorError(EvaluatorError)
  , EvaluatorErrorType(IllegalBreak)
  )

import Tiger.Evaluator.Internal.Scope(
    Environment(Env)
  , FuncAddress, NamedVarAddress
  , Scope(environ, Scope)
  , ScopeAddress(ScopeAddress, n)
  )

import Tiger.Evaluator.Internal.Value(ControlFlow(Break, Pure), Value)

import Data.List.NonEmpty qualified as NE
import Data.Map           qualified as Map


data EvaluatorState =
  EvaluatorState { functions     :: Map FuncAddress Function
                 , lastScopeAddr :: ScopeAddress
                 , scopes        :: NonEmpty Scope
                 , vars          :: Map NamedVarAddress Value
                 }

data Function
  = Function { fParamNames :: [Symbol], fBody :: FnBody }

data FnBody
  = PrimitiveBody   {  pBody :: Evaluation Value }
  | UserDefinedBody { udBody :: Expr             }

type Evaluated  a = Validation (NonEmpty EvaluatorError) a
type Evaluation a = Stately (Evaluated a)
type Stately    a = StateT EvaluatorState IO a

andIfValidMV :: (Monad m) => m (Validation (NonEmpty e) a) -> (a -> m (Validation (NonEmpty e) b))
                                                           -> m (Validation (NonEmpty e) b)
andIfValidMV aVM f =
  do
    aV <- aVM
    aV `failOrM` f

mapMSequA :: (Monad m, Applicative f, Traversable t) => (a -> m (f b)) -> t a -> m (f (t b))
mapMSequA f xs =
  do
    resVs <- mapM f xs
    return $ sequenceA resVs

andIfValidMVNoB :: Evaluation ControlFlow -> (Value -> Evaluation a) -> Evaluation a
andIfValidMVNoB aVM f =
  do
    aV <- aVM
    aV `failOrMNoB` f

failOrMNoB :: Evaluated ControlFlow -> (Value -> Evaluation a) -> Evaluation a
failOrMNoB v vm = validation (Failure &> return) unwrapCF v
  where
    unwrapCF (Break token) = fail IllegalBreak token
    unwrapCF (Pure      x) = vm x

mapMSequANoB :: (a -> Evaluation ControlFlow) -> [a] -> Evaluation [Value]
mapMSequANoB f xs =
  do
    tVs     <- mapM f xs
    let tsV  = sequenceA tVs
    validation (Failure &> return) unwrap tsV
  where
    unwrap ts =
      do
        x <- mapM unwrapCF ts
        return $ sequenceA x

    unwrapCF (Break token) = fail IllegalBreak token
    unwrapCF (Pure      x) = win x

flattenVM :: Monad m => Validation fs (m (Validation fs a)) -> m (Validation fs a)
flattenVM = validation (Failure &> return) id

findInEnv :: Symbol -> Token -> (Environment -> Map Symbol a) -> EvaluatorErrorType
                    -> (a -> Evaluation b) -> Evaluation b
findInEnv name token getMap errorType builder =
  do
    head :| tail <- gets scopes
    climbScopes head tail
  where
    climbScopes (Scope env _) scopes =
      case Map.lookup name $ getMap env of
        Just  x -> builder x
        Nothing -> case scopes of
                     []    -> return $ err errorType token
                     (h:t) -> climbScopes h t

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
    helper state = state { scopes = newScope <| state.scopes, lastScopeAddr = newAddr }
      where
        newAddr  = ScopeAddress $ state.lastScopeAddr.n + 1
        newScope = Scope (Env Map.empty Map.empty) newAddr

stackFrame :: Evaluation a -> Evaluation a
stackFrame fv = pushScope *> fv <* popScope

updateEnv :: (Environment -> Environment) -> Stately ()
updateEnv f = updateScope $ \s -> s { environ = f s.environ }

updateScope :: (Scope -> Scope) -> Stately ()
updateScope f = modify $ \s -> s { scopes = case scopes s of h :| t -> (f h) :| t }

fail :: EvaluatorErrorType -> Token -> Evaluation a
fail errorType token = return $ err errorType token

err :: EvaluatorErrorType -> Token -> Evaluated a
err errorType token = Failure $ NE.singleton $ EvaluatorError errorType token

win :: a -> Evaluation a
win = Success &> return
