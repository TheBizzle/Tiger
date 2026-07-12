{-# LANGUAGE TypeFamilies #-}
module Tiger.Analyzer.StackFrame.LLVMFrame() where

import Control.Monad.State(gets, modify)

import Tiger.Analyzer.StackFrame.Common(Label, wordSize64Bit)

import Tiger.Analyzer.StackFrame.Translator(
    Ephemeral
  , Frame(fBuildNewFrame, fFormals, fName, fRegisterLocal, fWordSize, VarLoc)
  , genEphemeral
  , TranslatorState(numInRegs)
  )

import Data.List qualified as List


instance Frame LLVMFrame where
  type VarLoc LLVMFrame = LLVMVarLoc

  fFormals       = llvmFormals
  fName          = llvmName
  fBuildNewFrame = buildNewFrame
  fRegisterLocal = registerLocal
  fWordSize      = const wordSize

data LLVMValue
  deriving (Eq, Show)

data LLVMVarLoc
  = InAlloca {     value :: LLVMValue }
  | InSSA    { ephemeral :: Ephemeral }
  deriving (Eq, Show)

data LLVMFrame
  = LLVMFrame { llvmFormals :: [LLVMVarLoc]
              , llvmName    :: Label
              }

buildNewFrame :: Label -> [Bool] -> State TranslatorState LLVMFrame
buildNewFrame label isEscapes =
  do
    formals <- mapM calcLoc $ List.zip [0..] isEscapes
    return $ LLVMFrame formals label

registerLocal :: LLVMFrame -> Bool -> State TranslatorState (LLVMFrame, LLVMVarLoc)
registerLocal frame False = genEphemeral <&> (\eph -> (frame, InSSA eph))
registerLocal frame  True =
  do
    value <- extractCurrentAlloca
    return (frame, InAlloca value)

calcLoc :: (Word, Bool) -> State TranslatorState LLVMVarLoc
calcLoc (i, isEscape) =
  do
    currentInRegs <- gets numInRegs
    if currentInRegs < numFormalsInRegs && not isEscape then do
      modify $ \s -> s { numInRegs = s.numInRegs + 1 }
      genEphemeral <&> InSSA
    else
      (extractAlloca i) <&> InAlloca

extractAlloca :: Word -> State TranslatorState LLVMValue
extractAlloca _i = undefined

extractCurrentAlloca :: State TranslatorState LLVMValue
extractCurrentAlloca = undefined

numFormalsInRegs :: Word
numFormalsInRegs = 0

wordSize :: Word
wordSize = wordSize64Bit -- Questionable.  LLVM is bitness-agnostic. --Jason B. (7/11/26)
