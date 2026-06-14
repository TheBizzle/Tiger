module Tiger.Analyzer.Internal.Address(
    FuncAddress(FuncAddress, funcName, funcScopeAddr)
  , LValueAddress(arrayAddr, ArrayElemAddress, property, recordAddr, RecordPropAddress, varAddr, VarAddress)
  , NamedVarAddress(NamedVarAddress, varName, varScopeAddr)
  , ScopeAddress(n, ScopeAddress)
  , TermAddress(FAddr, faddrValue, LAddr, laddrValue, TAddr, taddrValue)
  , TypeAddress(IntAddress, StringAddress, TypeAddress, typeName, typeScopeAddr)
  ) where

import Tiger.Parser.AST(Symbol)


data TermAddress
  = FAddr { faddrValue ::   FuncAddress }
  | LAddr { laddrValue :: LValueAddress }
  | TAddr { taddrValue ::   TypeAddress }
  deriving (Eq, Ord, Show)

data FuncAddress
  = FuncAddress { funcName :: Symbol, funcScopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)

data LValueAddress
  = ArrayElemAddress  {  arrayAddr :: LValueAddress }
  | RecordPropAddress { recordAddr :: LValueAddress, property :: Symbol }
  | VarAddress        {    varAddr :: NamedVarAddress }
  deriving (Eq, Ord, Show)

data NamedVarAddress
  = NamedVarAddress { varName :: Symbol, varScopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)

data TypeAddress
  = IntAddress
  | StringAddress
  | TypeAddress   { typeName :: Symbol, typeScopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)

newtype ScopeAddress
  = ScopeAddress { n :: Word }
  deriving (Eq, Ord, Show)
