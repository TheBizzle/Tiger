{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Evaluator.EvaluatorError(module Tiger.Evaluator.Internal.EvaluatorError) where

import Tiger.Evaluator.Internal.EvaluatorError(
    EvaluatorErrorType(DivisionByZero, IllegalBreak, NoSuchFn, NoSuchVar, TypeMismatch)
  , EvaluatorError(EvaluatorError)
  )
