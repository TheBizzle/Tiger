{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Evaluator.Value(module Tiger.Evaluator.Internal.Value) where

import Tiger.Evaluator.Internal.Value(
    Value(tarray, TArray, tint, TInt, TNil, trecord, TRecord, trName, tstring, TString, TUnit)
  )
