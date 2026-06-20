module Tiger.Evaluator.Internal.EvaluatorError(
    EvaluatorErrorType(DivisionByZero, IllegalBreak, mtExpected, mtGot, NoSuchFn, NoSuchVar, TypeMismatch)
  , EvaluatorError(offender, EvaluatorError, typ)
  ) where

import Tiger.Lexer.Token(Token)

import Tiger.Evaluator.Internal.Type(Type)


data EvaluatorErrorType
  = DivisionByZero
  | IllegalBreak
  | NoSuchFn
  | NoSuchVar
  | TypeMismatch   { mtExpected :: Type, mtGot :: Type }
  deriving Show

data EvaluatorError =
  EvaluatorError { typ :: EvaluatorErrorType, offender :: Token }
  deriving Show
