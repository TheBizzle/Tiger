{-# LANGUAGE TypeFamilies #-}
module Tiger.Analyzer.StackFrame.WasmFrame() where

import Control.Monad.State(gets, modify)

import Tiger.Analyzer.StackFrame.Common(Label, wordSize32Bit)

import Tiger.Analyzer.StackFrame.Translator(
    Ephemeral
  , Frame(fBuildNewFrame, fFormals, fName, fRegisterLocal, fWordSize, VarLoc)
  , genEphemeral
  , TranslatorState(numInRegs)
  )

import Data.List qualified as List


instance Frame WasmFrame where
  type VarLoc WasmFrame = WasmVarLoc

  fFormals       = wasmFormals
  fName          = wasmName
  fBuildNewFrame = buildNewFrame
  fRegisterLocal = registerLocal
  fWordSize      = const wordSize

data WasmVarLoc
  = InMemory { byteOffset ::      Word }
  | InLocal  {  ephemeral :: Ephemeral }
  deriving (Eq, Show)

data WasmFrame
  = WasmFrame { wasmFormals     :: [WasmVarLoc]
              , wasmLocalsCount :: Word
              , wasmName        :: Label
              }

buildNewFrame :: Label -> [Bool] -> State TranslatorState WasmFrame
buildNewFrame label isEscapes =
  do
    formals <- mapM calcLoc $ List.zip [0..] isEscapes
    return $ WasmFrame formals 0 label

registerLocal :: WasmFrame -> Bool -> State TranslatorState (WasmFrame, WasmVarLoc)
registerLocal frame False = genEphemeral <&> (\eph -> (frame, InLocal eph))
registerLocal frame  True = return (newFrame, varLoc)
  where
    newFrame = frame { wasmLocalsCount = frame.wasmLocalsCount + 1 }
    varLoc   = InMemory $ -wordSize * newFrame.wasmLocalsCount

calcLoc :: (Word, Bool) -> State TranslatorState WasmVarLoc
calcLoc (i, isEscape) =
  do
    currentInRegs <- gets numInRegs
    if currentInRegs < numFormalsInRegs && not isEscape then do
      modify $ \s -> s { numInRegs = s.numInRegs + 1 }
      genEphemeral <&> InLocal
    else
      return $ InMemory $ (prologueLength + i) * wordSize

numFormalsInRegs :: Word
numFormalsInRegs = 0

prologueLength :: Word
prologueLength = 0

wordSize :: Word
wordSize = wordSize32Bit
