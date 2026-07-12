{-# LANGUAGE TypeFamilies #-}
module Tiger.Analyzer.StackFrame.X86Frame() where

import Control.Monad.State(gets, modify)

import Tiger.Analyzer.StackFrame.Common(Label, wordSize32Bit)

import Tiger.Analyzer.StackFrame.Translator(
    Ephemeral
  , Frame(fBuildNewFrame, fFormals, fName, fRegisterLocal, fWordSize, VarLoc)
  , genEphemeral
  , TranslatorState(numInRegs)
  )

import Data.List qualified as List


instance Frame X86Frame where
  type VarLoc X86Frame = X86VarLoc

  fFormals       = x86Formals
  fName          = x86Name
  fBuildNewFrame = buildNewFrame
  fRegisterLocal = registerLocal
  fWordSize      = const wordSize

data X86VarLoc
  = InFrame    { byteOffset ::      Word }
  | InRegister { ephemeral  :: Ephemeral }
  deriving (Eq, Show)

data X86Frame
  = X86Frame { x86Formals     :: [X86VarLoc]
             , x86LocalsCount :: Word
             , x86Name        :: Label
             }

buildNewFrame :: Label -> [Bool] -> State TranslatorState X86Frame
buildNewFrame label isEscapes =
  do
    formals <- mapM calcLoc $ List.zip [0..] isEscapes
    return $ X86Frame formals 0 label

-- Unlike function parameters, local variables can live in registers. --Jason B. (7/4/26)
registerLocal :: X86Frame -> Bool -> State TranslatorState (X86Frame, X86VarLoc)
registerLocal frame False = genEphemeral <&> (\eph -> (frame, InRegister eph))
registerLocal frame  True = return (newFrame, varLoc)
  where
    newFrame = frame { x86LocalsCount = frame.x86LocalsCount + 1 }
    varLoc   = InFrame $ -wordSize * newFrame.x86LocalsCount
    -- Offsets are negative (relative to `ebp`), since x86's stack grows downwards. --Jason B. (7/4/26)

calcLoc :: (Word, Bool) -> State TranslatorState X86VarLoc
calcLoc (i, isEscape) =
  do
    currentInRegs <- gets numInRegs
    if currentInRegs < numFormalsInRegs && not isEscape then do
      modify $ \s -> s { numInRegs = s.numInRegs + 1 }
      genEphemeral <&> InRegister
    else
      return $ InFrame $ (prologueLength + i) * wordSize

-- x86 puts all of the call's parameters on the stack, regardless of escape. --Jason B. (7/4/26)
numFormalsInRegs :: Word
numFormalsInRegs = 0

-- 0: Caller's stored `ebp`.  1: The `return` pointer to jump back to. --Jason B. (7/4/26)
prologueLength :: Word
prologueLength = 2

wordSize :: Word
wordSize = wordSize32Bit
