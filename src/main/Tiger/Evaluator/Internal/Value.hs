module Tiger.Evaluator.Internal.Value(
    ControlFlow(Break, bToken, cfValue, Pure)
  , typeOf
  , Value(tarray, TArray, tint, TInt, TNil, trecord, TRecord, trName, tstring, TString, TUnit)
  ) where

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Symbol)

import Data.Map                      qualified as Map
import Tiger.Evaluator.Internal.Type qualified as Type


data ControlFlow
  = Break { bToken  :: Token }
  | Pure  { cfValue :: Value }
  deriving (Eq, Show)

data Value
  = TArray  { tarray :: [Value] }
  | TInt    { tint   :: Int }
  | TNil
  | TRecord { trecord :: Map Symbol Value, trName :: Symbol }
  | TString { tstring :: Text }
  | TUnit
  deriving (Eq, Show)

typeOf :: Value -> Type.Type
typeOf (TArray _)     = Type.Array
typeOf (TInt   _)     = Type.Int
typeOf TNil           = Type.Nil
typeOf (TRecord fs _) = Type.Record $ Map.keys fs
typeOf (TString _)    = Type.String
typeOf TUnit          = Type.Unit
