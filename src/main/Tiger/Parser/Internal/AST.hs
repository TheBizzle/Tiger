{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Parser.Internal.AST where

import Tiger.Lexer.Token(SourceLoc)


type Symbol = Text

data LValue
  = Variable { lValSymbol :: Symbol
             , lValSrcLoc :: SourceLoc
             }
  | RecordField { lValLVal   :: LValue
                , lValSymbol :: Symbol
                , lValSrcLoc :: SourceLoc
                }
  | ArrayIndex { lValLVal   :: LValue
               , lValIndex  :: Expr
               , lValSrcLoc :: SourceLoc
               }
  deriving (Eq, Show)

data Expr
  = LValueExpr { lValValue :: LValue
               , srcLoc    :: SourceLoc
               }
  | NilExpr { srcLoc :: SourceLoc }
  | IntExpr    { intValue :: Int
               , srcLoc   :: SourceLoc
               }
  | StringExpr { strValue :: Text
               , srcLoc :: SourceLoc
               }
  | CallExpr { callFunc :: Symbol
             , callArgs :: [Expr]
             , srcLoc   :: SourceLoc
             }
  | OpExpr { opLeft     :: Expr
           , opOperator :: Operator
           , opRight    :: Expr
           , srcLoc     :: SourceLoc
           }
  | RecordExpr { recFields :: [(Symbol, Expr, SourceLoc)]
               , recType   :: Symbol
               , srcLoc    :: SourceLoc
               }
  | SeqExpr { statements :: [(Expr, SourceLoc)]
            , srcLoc     :: SourceLoc
            }
  | AssignExpr { assignLVal :: LValue
               , assignExpr :: Expr
               , srcLoc     :: SourceLoc
               }
  | IfExpr { antecedent  :: Expr
           , consequent  :: Expr
           , alternative :: Maybe Expr
           , srcLoc      :: SourceLoc
           }
  | WhileExpr { whileTest :: Expr
              , whileBody :: Expr
              , srcLoc    :: SourceLoc
              }
  | ForExpr { forVar    :: Symbol
            , forEscape :: Bool
            , forLow    :: Expr
            , forHigh   :: Expr
            , forBody   :: Expr
            , srcLoc    :: SourceLoc
            }
  | BreakExpr { srcLoc  :: SourceLoc }
  | LetExpr { letDecls :: [Decl]
            , letBody  :: Expr
            , srcLoc   :: SourceLoc
            }
  | ArrayExpr { arrayType :: Symbol
              , arraySize :: Expr
              , arrayInit :: Expr
              , srcLoc    :: SourceLoc
              }
  deriving (Eq, Show)

data Decl
  = FunctionDecl { funcDecls :: [FuncDecl] }
  | VarDecl { varName   :: Symbol
            , varEscape :: Bool
            , varTypeM  :: Maybe (Symbol, SourceLoc)
            , varInit   :: Expr
            , varSrcLoc :: SourceLoc
            }
  | TypeDecl { typeDecls :: [TypeDeclEntry] }
  deriving (Eq, Show)

data TypeDeclEntry
  = TypeDeclEntry { typeDeclName   :: Symbol
                  , typeDeclType   :: TigerType
                  , typeDeclSrcLoc :: SourceLoc
                  }
  deriving (Eq, Show)

data TigerType
  = NamedType  { typeSymbol :: Symbol, typeSrcLoc :: SourceLoc }
  | RecordType { fields     :: [Field] }
  | ArrayType  { typeSymbol :: Symbol, typeSrcLoc :: SourceLoc }
  deriving (Eq, Show)

data Operator
  = PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqualsOp
  | NotEqualsOp
  | LessThanOp
  | LessOrEqualsOp
  | GreaterThanOp
  | GreaterOrEqualsOp
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Used in record types and param lists
data Field
  = Field { fieldName   :: Symbol
          , fieldEscape :: Bool
          , fieldType   :: Symbol
          , fieldSrcLoc :: SourceLoc
          }
  deriving (Eq, Show)

data FuncDecl
  = FuncDecl { funcDeclName   :: Symbol
             , funcDeclParams :: [Field]
             , funcDeclTypeM  :: Maybe (Symbol, SourceLoc)
             , funcDeclBody   :: Expr
             , funcDeclSrcLoc :: SourceLoc
             }
  deriving (Eq, Show)
