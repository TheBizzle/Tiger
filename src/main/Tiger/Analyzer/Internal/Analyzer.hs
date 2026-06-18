module Tiger.Analyzer.Internal.Analyzer(analyze) where

import Tiger.Parser.AST(Expr)

import Tiger.Analyzer.Internal.Address(ScopeAddress(ScopeAddress))
import Tiger.Analyzer.Internal.ExprAnalyzer(crawlAST)

import Tiger.Analyzer.Internal.Common(
    AnalyzerState( AnalyzerState, functions, lastScopeAddr, isInFor, isInWhile, nextUniqueID, protecteds
                 , scopes, types, vars
                 )
  , Validated
  )

import Tiger.Analyzer.Internal.IRValue(IRValue)
import Tiger.Analyzer.Internal.Primitives(primitives)
import Tiger.Analyzer.Internal.Scope(Environment(Env), Scope(Scope))

import Data.List.NonEmpty qualified as NE
import Data.Map           qualified as Map
import Data.Set           qualified as Set


analyze :: Expr -> Validated IRValue
analyze = verify

verify :: Expr -> Validated IRValue
verify expr = evalState (crawlAST expr) initialState
  where
    (primsEnv, primsState) = primitives

    env = Env primsEnv Map.empty Map.empty

    initialState =
      AnalyzerState { functions     = primsState
                    , lastScopeAddr = ScopeAddress 0
                    , isInFor       = False
                    , isInWhile     = False
                    , nextUniqueID  = 0
                    , protecteds    = Set.empty
                    , scopes        = NE.singleton $ Scope env $ ScopeAddress 0
                    , types         = Map.empty
                    , vars          = Map.empty
                    }
