module Tiger.Evaluator.Internal.Evaluator(eval) where

import Control.Monad.State(evalStateT)

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


eval :: Expr -> IO (Evaluated Value)
eval expr = resultMV <&> unwrapCF
  where
    resultMV = evalStateT (evalExpr expr) initialState

    unwrapCF (Failure        es) = Failure es
    unwrapCF (Success (Break _)) = error "Illegal top-level break"
    unwrapCF (Success (Pure  x)) = Success x

    (primsEnv, primsState) = primitives

    env = Env primsEnv Map.empty

    initialState =
      EvaluatorState { functions     = primsState
                     , lastScopeAddr = ScopeAddress 0
                     , scopes        = NE.singleton $ Scope env $ ScopeAddress 0
                     , vars          = Map.empty
                     }
