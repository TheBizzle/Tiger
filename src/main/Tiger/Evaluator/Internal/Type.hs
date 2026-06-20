module Tiger.Evaluator.Internal.Type(Type(Array, fields, Int, Nil, Record, String, Unit)) where

import Tiger.Parser.AST(Symbol)


data Type
  = Array
  | Int
  | Nil
  | Record { fields :: [Symbol] }
  | String
  | Unit
  deriving (Eq, Ord, Show)
