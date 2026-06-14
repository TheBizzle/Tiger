module Tiger.Analyzer.Internal.Function(arity, Function(Function, params, returnType)) where

import Tiger.Parser.AST(Symbol)

import Tiger.Analyzer.Internal.IRValue(IRValue)
import Tiger.Analyzer.Internal.Type(Type)


data Function
  = Function { params :: [(Symbol, Type)], returnType :: Type, body :: IRValue }
  deriving (Eq, Show)

arity :: Function -> Word
arity (Function params _ _) = length params
