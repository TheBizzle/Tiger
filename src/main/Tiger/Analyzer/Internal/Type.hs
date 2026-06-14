module Tiger.Analyzer.Internal.Type(
    isSubtypeOf
  , Type(Array, elemTypeAddr, fields, Int, Named, nameSym, namedTypeM, Nil, Record, String, typeID, Unit)
  , UniqueID(idNum, UniqueID)
  ) where

import Tiger.Parser.AST(Symbol)

import Tiger.Analyzer.Internal.Address(TypeAddress)


newtype UniqueID
  = UniqueID { idNum :: Word }
  deriving (Eq, Ord, Show)

data Type
  = Array { elemTypeAddr :: TypeAddress, typeID :: UniqueID }
  | Int
  | Named { nameSym :: Symbol, namedTypeM :: Maybe TypeAddress }
  | Nil
  | Record { fields :: [(Symbol, TypeAddress)], typeID :: UniqueID }
  | String
  | Unit
  deriving (Eq, Ord, Show)

isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf (Record _    _)             Nil = True
isSubtypeOf             Nil (Record _    _) = True
isSubtypeOf (Record _ uid1) (Record _ uid2) = uid1 == uid2
isSubtypeOf               a               b = a == b
