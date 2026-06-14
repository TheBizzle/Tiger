module Tiger.Analyzer.Internal.Scope(
    Environment(Env, envFuncs, envTypes, envVars)
  , FuncInfo(fiAddr, FIAddress, pfParams, pfTypeM, PreFunc)
  , Scope(address, environ, Scope)
  , TypeInfo(tiAddr, TIAddress, PreType)
  , VarInfo( viAddr, VIAddress, PreVar)
  ) where

import Tiger.Parser.AST(Symbol)

import Tiger.Analyzer.Internal.Address(FuncAddress, NamedVarAddress, ScopeAddress, TypeAddress)
import Tiger.Analyzer.Internal.Type(Type)


data FuncInfo
  = FIAddress { fiAddr :: FuncAddress }
  | PreFunc   { pfParams :: [(Symbol, Type)], pfTypeM :: Type }
  deriving (Eq, Show)

data TypeInfo
  = TIAddress { tiAddr :: TypeAddress }
  | PreType   { tiAddr :: TypeAddress }
  deriving (Eq, Show)

data VarInfo
  = VIAddress { viAddr :: NamedVarAddress }
  | PreVar    { pvType ::            Type }
  deriving (Eq, Show)

data Environment =
  Env { envFuncs :: Map Symbol FuncInfo
      , envTypes :: Map Symbol TypeInfo
      , envVars  :: Map Symbol  VarInfo
      }
  deriving (Eq, Show)

data Scope
  = Scope { environ :: Environment, address :: ScopeAddress }
  deriving (Eq, Show)
