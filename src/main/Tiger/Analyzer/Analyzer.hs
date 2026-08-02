{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Analyzer.Analyzer(module Tiger.Analyzer.Internal.Analyzer, module Tiger.Analyzer.Internal.IRValue) where

import Tiger.Analyzer.Internal.Analyzer(analyze)
import Tiger.Analyzer.Internal.IRValue(IRValue(irvExpr))
