module Tiger.Analyzer.StackFrame.Common(asLabel, Label(Label, lSymbol), wordSize32Bit, wordSize64Bit) where

import Tiger.Parser.AST(Symbol(Symbol))


-- Denotes abstract names for memory addresses
newtype Label = Label { lSymbol :: Symbol }

asLabel :: Text -> Label
asLabel = Symbol &> Label

wordSize32Bit :: Word
wordSize32Bit = 4

wordSize64Bit :: Word
wordSize64Bit = 8
