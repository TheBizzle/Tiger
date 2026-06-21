module Tiger.Evaluator.Internal.DeclEvaluator(evalDecls) where

import Control.Monad.State(gets, modify)

import Tiger.Parser.AST(
    Decl(FunctionDecl, VariableDecl)
  , Expr
  , Field(fieldName)
  , FuncDecl(FuncDecl)
  , VarDecl(VarDecl)
  )

import Tiger.Evaluator.Internal.Common(
    andIfValidMVNoB, Evaluation
  , EvaluatorState(functions, lastScopeAddr, vars)
  , FnBody(UserDefinedBody)
  , Function(Function)
  , updateEnv, win
  )

import Tiger.Evaluator.Internal.Scope(
    Environment(envFuncs, envVars)
  , FuncAddress(FuncAddress)
  , NamedVarAddress(NamedVarAddress)
  , ScopeAddress(n)
  )

import Tiger.Evaluator.Internal.Value(ControlFlow)

import Data.Map qualified as Map


evalDecls :: (Expr -> Evaluation ControlFlow) -> [Decl] -> Evaluation ()
evalDecls evaluExpr decls =
  do
    results <- mapM (evalDecl evaluExpr) decls
    return $ sequenceA_ results

evalDecl :: (Expr -> Evaluation ControlFlow) -> Decl -> Evaluation ()
evalDecl _ (FunctionDecl (FuncDecl name params _ body _)) =
  do
    scaddr   <- gets lastScopeAddr
    let addr  = FuncAddress name $ scaddr { n = scaddr.n + 1 }
    let ps    = map fieldName params
    let func  = Function ps $ UserDefinedBody body
    updateEnv $ \env -> env { envFuncs = Map.insert name addr env.envFuncs }
    modify $ \s -> s { functions = Map.insert addr func s.functions }
    win ()

evalDecl evaluExpr (VariableDecl (VarDecl name _ initialExpr _)) =
  (evaluExpr initialExpr) `andIfValidMVNoB` (
    \initial -> do
      scaddr   <- gets lastScopeAddr
      let addr  = NamedVarAddress name $ scaddr { n = scaddr.n + 1 }
      updateEnv $ \env -> env { envVars = Map.insert name addr env.envVars }
      modify $ \s -> s { vars = Map.insert addr initial s.vars }
      win ()
    )

evalDecl _ _ = win ()
