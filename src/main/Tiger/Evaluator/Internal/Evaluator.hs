module Tiger.Evaluator.Internal.Evaluator(eval, initialState, simpleEval) where

import Control.Monad.State(runStateT)

import Tiger.Parser.AST(Expr)

import Tiger.Evaluator.Internal.Common(
    Evaluated, EvaluatorState(EvaluatorState, functions, lastScopeAddr, scopes, vars)
  )

import Tiger.Evaluator.Internal.ExprEvaluator(evalExpr)
import Tiger.Evaluator.Internal.Primitives(primitives)

import Tiger.Evaluator.Internal.Scope(Environment(Env), Scope(Scope), ScopeAddress(ScopeAddress))
import Tiger.Evaluator.Internal.Value(ControlFlow(Break, Pure), Value)

import Data.List.NonEmpty qualified as NE
import Data.Map           qualified as Map


simpleEval :: Expr -> IO (Evaluated Value)
simpleEval = eval initialState &> (map fst)

eval :: EvaluatorState -> Expr -> IO (Evaluated Value, EvaluatorState)
eval state expr = resultMV <&> (mapFst unwrapCF)
  where
    resultMV = runStateT (evalExpr expr) state

    unwrapCF (Failure        es) = Failure es
    unwrapCF (Success (Break _)) = error "Illegal top-level break"
    unwrapCF (Success (Pure  x)) = Success x

initialState :: EvaluatorState
initialState =
  EvaluatorState { functions     = primsState
                 , lastScopeAddr = ScopeAddress 0
                 , scopes        = NE.singleton $ Scope env $ ScopeAddress 0
                 , vars          = Map.empty
                 }
  where
    (primsEnv, primsState) = primitives

    env = Env primsEnv Map.empty
