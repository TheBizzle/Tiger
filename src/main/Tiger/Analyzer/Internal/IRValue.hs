module Tiger.Analyzer.Internal.IRValue(IRValue(IRValue, irvExpr, irvType)) where

import Tiger.Parser.AST(Expr)

import Tiger.Analyzer.Internal.Type(Type)


data IRValue
  = IRValue { irvExpr :: Expr, irvType :: Type }
  deriving (Eq, Show)
