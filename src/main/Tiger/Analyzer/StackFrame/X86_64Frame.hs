{-# LANGUAGE TypeFamilies #-}
module Tiger.Analyzer.StackFrame.X86_64Frame() where

import Control.Monad.State(gets, modify)

import Tiger.Analyzer.StackFrame.Common(Label, wordSize64Bit)

import Tiger.Analyzer.StackFrame.Translator(
    Ephemeral
  , Frame(fBuildNewFrame, fFormals, fName, fRegisterLocal, fWordSize, VarLoc)
  , genEphemeral
  , TranslatorState(numInRegs)
  )

import Data.List qualified as List


instance Frame X86_64Frame where
  type VarLoc X86_64Frame = X86_64VarLoc

  fFormals       = x86_64Formals
  fName          = x86_64Name
  fBuildNewFrame = buildNewFrame
  fRegisterLocal = registerLocal
  fWordSize      = const wordSize

data X86_64VarLoc
  = InFrame    { byteOffset ::      Word }
  | InRegister { ephemeral  :: Ephemeral }
  deriving (Eq, Show)

data X86_64Frame
  = X86_64Frame { x86_64Formals     :: [X86_64VarLoc]
                , x86_64LocalsCount :: Word
                , x86_64Name        :: Label
                }

buildNewFrame :: Label -> [Bool] -> State TranslatorState X86_64Frame
buildNewFrame label isEscapes =
  do
    formals <- mapM calcLoc $ List.zip [0..] isEscapes
    return $ X86_64Frame formals 0 label

registerLocal :: X86_64Frame -> Bool -> State TranslatorState (X86_64Frame, X86_64VarLoc)
registerLocal frame False = genEphemeral <&> (\eph -> (frame, InRegister eph))
registerLocal frame  True = return (newFrame, varLoc)
  where
    newFrame = frame { x86_64LocalsCount = frame.x86_64LocalsCount + 1 }
    varLoc   = InFrame $ -wordSize * newFrame.x86_64LocalsCount

calcLoc :: (Word, Bool) -> State TranslatorState X86_64VarLoc
calcLoc (i, isEscape) =
  do
    currentInRegs <- gets numInRegs
    if currentInRegs < numFormalsInRegs && not isEscape then do
      modify $ \s -> s { numInRegs = s.numInRegs + 1 }
      genEphemeral <&> InRegister
    else
      return $ InFrame $ (prologueLength + i) * wordSize

numFormalsInRegs :: Word
numFormalsInRegs = 6

-- 0: Caller's stored `ebp`.  1: The `return` pointer to jump back to. --Jason B. (7/4/26)
prologueLength :: Word
prologueLength = 2

wordSize :: Word
wordSize = wordSize64Bit
