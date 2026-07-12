{-# LANGUAGE TypeFamilies #-}
module Tiger.Analyzer.StackFrame.AArch64Frame() where

import Control.Monad.State(gets, modify)

import Tiger.Analyzer.StackFrame.Common(Label, wordSize64Bit)

import Tiger.Analyzer.StackFrame.Translator(
    Ephemeral
  , Frame(fBuildNewFrame, fFormals, fName, fRegisterLocal, fWordSize, VarLoc)
  , genEphemeral
  , TranslatorState(numInRegs)
  )

import Data.List qualified as List


instance Frame AArch64Frame where
  type VarLoc AArch64Frame = AArch64VarLoc

  fFormals       = aarch64Formals
  fName          = aarch64Name
  fBuildNewFrame = buildNewFrame
  fRegisterLocal = registerLocal
  fWordSize      = const wordSize

data AArch64VarLoc
  = InFrame    { byteOffset ::      Word }
  | InRegister { ephemeral  :: Ephemeral }
  deriving (Eq, Show)

data AArch64Frame
  = AArch64Frame { aarch64Formals     :: [AArch64VarLoc]
                 , aarch64LocalsCount :: Word
                 , aarch64Name        :: Label
                 }

buildNewFrame :: Label -> [Bool] -> State TranslatorState AArch64Frame
buildNewFrame label isEscapes =
  do
    formals <- mapM calcLoc $ List.zip [0..] isEscapes
    return $ AArch64Frame formals 0 label

registerLocal :: AArch64Frame -> Bool -> State TranslatorState (AArch64Frame, AArch64VarLoc)
registerLocal frame False = genEphemeral <&> (\eph -> (frame, InRegister eph))
registerLocal frame  True = return (newFrame, varLoc)
  where
    newFrame = frame { aarch64LocalsCount = frame.aarch64LocalsCount + 1 }
    varLoc   = InFrame $ -wordSize * newFrame.aarch64LocalsCount

calcLoc :: (Word, Bool) -> State TranslatorState AArch64VarLoc
calcLoc (i, isEscape) =
  do
    currentInRegs <- gets numInRegs
    if currentInRegs < numFormalsInRegs && not isEscape then do
      modify $ \s -> s { numInRegs = s.numInRegs + 1 }
      genEphemeral <&> InRegister
    else
      return $ InFrame $ (prologueLength + i) * wordSize

numFormalsInRegs :: Word
numFormalsInRegs = 8

prologueLength :: Word
prologueLength = 0

wordSize :: Word
wordSize = wordSize64Bit
