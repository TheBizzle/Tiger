module Tiger.Evaluator.Internal.Scope(
    Environment(Env, envFuncs, envVars)
  , FuncAddress(FuncAddress, funcName, funcScopeAddr)
  , LValueAddress( arrayAddr, arrayIndex, ArrayElemAddress, property, recordAddr, RecordPropAddress, varAddr
                 , VarAddress
                 )
  , NamedVarAddress(NamedVarAddress, varName, varScopeAddr)
  , Scope(address, environ, Scope)
  , ScopeAddress(n, ScopeAddress)
  , TermAddress(FAddr, faddrValue, LAddr, laddrValue)
  ) where

import Tiger.Parser.AST(Symbol)


data TermAddress
  = FAddr { faddrValue ::   FuncAddress }
  | LAddr { laddrValue :: LValueAddress }
  deriving (Eq, Ord, Show)

data FuncAddress
  = FuncAddress { funcName :: Symbol, funcScopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)

data LValueAddress
  = ArrayElemAddress  {  arrayAddr :: LValueAddress, arrayIndex :: Int }
  | RecordPropAddress { recordAddr :: LValueAddress, property :: Symbol }
  | VarAddress        {    varAddr :: NamedVarAddress }
  deriving (Eq, Ord, Show)

data NamedVarAddress
  = NamedVarAddress { varName :: Symbol, varScopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)

data Environment =
  Env { envFuncs :: Map Symbol FuncAddress
      , envVars  :: Map Symbol NamedVarAddress
      }
  deriving (Eq, Show)

data Scope
  = Scope { environ :: Environment, address :: ScopeAddress }
  deriving (Eq, Show)

newtype ScopeAddress
  = ScopeAddress { n :: Word }
  deriving (Eq, Ord, Show)
