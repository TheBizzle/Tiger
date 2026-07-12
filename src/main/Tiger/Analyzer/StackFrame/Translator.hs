{-# LANGUAGE TypeFamilies #-}
module Tiger.Analyzer.StackFrame.Translator(
    Env(feFormals, feLabel, feLevel, feReturnType, FunEntry, VarEntry, veVarLoc, veType)
  , Ephemeral
  , Frame(fBuildNewFrame, fFormals, fName, fRegisterLocal, fWordSize, VarLoc)
  , genEphemeral
  , genLabel
  , Level(Level, lFrame, lParent, Outermost)
  , Translator(tlAllocLocal, tlFormals, tlNewLevel, tlOutermost, Translator)
  , TranslatorState(nextEphemeralBasis, nextLabelBasis, numInRegs, TranslatorState)
  , TranslVarLoc(taLevel, taVarLoc, TranslVarLoc)
  ) where

import Control.Monad.State(gets, modify)

import Tiger.Analyzer.StackFrame.Common(asLabel, Label)
import Tiger.Analyzer.Type(Type)


-- Denotes names for local variables that get stored into registers --Jason B. (7/4/26)
newtype Ephemeral
  = Ephemeral { ephValue :: Word }
  deriving (Eq, Ord, Show)

-- One of these per function declaration --Jason B. (7/4/26)
type role Level representational
data Level f
  = Outermost -- TODO: Contains all of the standard library functions, but explicitly has no `Frame`
  | Level { lParent :: Level f, lFrame :: f }

type role Translator nominal
data Translator f =
  Translator { tlOutermost  :: Level f
             , tlNewLevel   :: Level f -> Label -> [Bool] -> Level f -- parent -> name -> formals -> result
             , tlFormals    :: Level f -> [TranslVarLoc f]
             , tlAllocLocal :: Level f -> Bool -> TranslVarLoc f
             }

type role Env nominal
data Env f
  = VarEntry { veVarLoc :: TranslVarLoc f, veType :: Type }
  | FunEntry { feLevel :: Level f, feLabel :: Label, feFormals :: [Type], feReturnType :: Type }

type role TranslVarLoc nominal
data TranslVarLoc f =
  TranslVarLoc { taLevel :: Level f, taVarLoc :: VarLoc f }

data TranslatorState =
  TranslatorState { nextEphemeralBasis :: Word
                  , nextLabelBasis     :: Word
                  , numInRegs          :: Word
                  }

class Frame f where
  type VarLoc f
  fBuildNewFrame :: Label -> [Bool] -> State TranslatorState f
  fFormals       :: f -> [VarLoc f]
  fName          :: f -> Label
  fRegisterLocal :: f -> Bool -> State TranslatorState (f, VarLoc f)
  fWordSize      :: f -> Word

genEphemeral :: State TranslatorState Ephemeral
genEphemeral =
  do
    x <- gets nextEphemeralBasis
    modify $ \s -> s { nextEphemeralBasis = x + 1 }
    return $ Ephemeral x

genLabel :: State TranslatorState Label
genLabel =
  do
    x <- gets nextLabelBasis
    modify $ \s -> s { nextLabelBasis = x + 1 }
    return $ asLabel $ showText x
