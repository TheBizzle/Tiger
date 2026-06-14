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
import Tiger.Analyzer.Internal.Scope(Environment(Env), Scope(Scope))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set


analyze :: Expr -> Validated IRValue
analyze = verify

verify :: Expr -> Validated IRValue
verify expr = evalState (crawlAST expr) initialState
  where
    env = Env Map.empty Map.empty Map.empty

    initialState =
      AnalyzerState { functions     = Map.empty
                    , lastScopeAddr = ScopeAddress 0
                    , isInFor       = False
                    , isInWhile     = False
                    , nextUniqueID  = 0
                    , protecteds    = Set.empty
                    , scopes        = NE.singleton $ Scope env $ ScopeAddress 0
                    , types         = Map.empty
                    , vars          = Map.empty
                    }
