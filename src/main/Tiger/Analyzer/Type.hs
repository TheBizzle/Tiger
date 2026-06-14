{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Analyzer.Type(module Tiger.Analyzer.Internal.Type) where

import Tiger.Analyzer.Internal.Type(
    Type(Array, Int, Named, Nil, Record, String, Unit)
  , UniqueID(UniqueID)
  )
