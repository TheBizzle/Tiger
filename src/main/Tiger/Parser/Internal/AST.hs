{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Parser.Internal.AST where

import Tiger.Lexer.Token(Token)


newtype Symbol
  = Symbol { symbolText :: Text }
  deriving (Eq, Ord, Show)

data LValue
  = ArrayIndex { lValLVal  :: LValue
               , lValIndex :: Expr
               , lValToken :: Token
               }
  | RecordField { lValLVal   :: LValue
                , lValSymbol :: Symbol
                , lValToken  :: Token
                }
  | Variable { lValSymbol :: Symbol
             , lValToken  :: Token
             }
  deriving (Eq, Ord, Show)

data Expr
  = AssignExpr { assignLVal :: LValue
               , assignExpr :: Expr
               , token      :: Token
               }
  | ArrayExpr { arrayType :: Symbol
              , arraySize :: Expr
              , arrayInit :: Expr
              , token     :: Token
              }
  | BreakExpr { token   :: Token }
  | CallExpr { callFunc :: Symbol
             , callArgs :: [Expr]
             , token    :: Token
             }
  | ForExpr { forVar    :: Symbol
            , forEscape :: Bool
            , forLow    :: Expr
            , forHigh   :: Expr
            , forBody   :: Expr
            , token     :: Token
            }
  | IfExpr { antecedent  :: Expr
           , consequent  :: Expr
           , alternative :: Maybe Expr
           , token       :: Token
           }
  | IntExpr { intValue :: Int
            , token    :: Token
            }
  | LetExpr { letDecls :: NonEmpty Decl
            , letBody  :: Expr
            , token    :: Token
            }
  | LValueExpr { lValValue :: LValue
               , token     :: Token
               }
  | NilExpr { token  :: Token }
  | OpExpr { opLeft     :: Expr
           , opOperator :: Operator
           , opRight    :: Expr
           , token      :: Token
           }
  | RecordExpr { recFields :: [(Symbol, Expr, Token)]
               , recType   :: Symbol
               , token     :: Token
               }
  | SeqExpr { statements :: [(Expr, Token)]
            , token      :: Token
            }
  | StringExpr { strValue :: Text
               , token    :: Token
               }
  | WhileExpr { whileTest :: Expr
              , whileBody :: Expr
              , token     :: Token
              }
  deriving (Eq, Ord, Show)

-- By "operator", he apparently means "binary infix operator"
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
  deriving (Eq, Ord, Show)

data Decl
  = FunctionDecl { funcDecl' :: FuncDecl }
  | TypeDecl     { typeDecl' :: TypeDeclEntry }
  | VariableDecl { varDecl'  :: VarDecl }
  deriving (Eq, Ord, Show)

data VarDecl
  = VarDecl { varName   :: Symbol
            , varEscape :: Bool
            , varTypeM  :: Maybe (Symbol, Token)
            , varInit   :: Expr
            , varToken  :: Token
            }
  deriving (Eq, Ord, Show)

data TypeDeclEntry
  = TypeDeclEntry { typeDeclName  :: Symbol
                  , typeDeclType  :: Type
                  , typeDeclToken :: Token
                  }
  deriving (Eq, Ord, Show)

data Type
  = ArrayType  { typeSymbol :: Symbol, typeToken :: Token }
  | NamedType  { typeSymbol :: Symbol, typeToken :: Token }
  | RecordType { fields     :: [Field] }
  deriving (Eq, Ord, Show)

-- Used in record types and param lists
data Field
  = Field { fieldName      :: Symbol
          , fieldEscape    :: Bool
          , fieldType      :: Symbol
          , fieldNameToken :: Token
          , fieldTypeToken :: Token
          }
  deriving (Eq, Ord, Show)

data FuncDecl
  = FuncDecl { funcDeclName   :: Symbol
             , funcDeclParams :: [Field]
             , funcDeclTypeM  :: Maybe (Symbol, Token)
             , funcDeclBody   :: Expr
             , funcDeclToken  :: Token
             }
  deriving (Eq, Ord, Show)
