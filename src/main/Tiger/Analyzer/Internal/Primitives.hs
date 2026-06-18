module Tiger.Analyzer.Internal.Primitives(primitives) where

import Tiger.Lexer.Token(SourceLoc(SourceLoc), Token(Token), TokenType(Semicolon))

import Tiger.Parser.AST(Expr(IntExpr, SeqExpr, StringExpr), Symbol(Symbol))

import Tiger.Analyzer.Internal.Address(FuncAddress(FuncAddress), ScopeAddress(ScopeAddress))
import Tiger.Analyzer.Internal.Function(Function(Function))
import Tiger.Analyzer.Internal.IRValue(IRValue(IRValue))
import Tiger.Analyzer.Internal.Scope(FuncInfo(FIAddress))
import Tiger.Analyzer.Internal.Type(Type)

import Data.Map                     qualified as Map
import Tiger.Analyzer.Internal.Type qualified as Type


primitives :: (Map Symbol FuncInfo, Map FuncAddress Function)
primitives = (env, state)

env :: Map Symbol FuncInfo
env = Map.fromList $ map (fst3 &> (Symbol &&& (toAddress &> FIAddress))) triples

state :: Map FuncAddress Function
state = Map.fromList $ map toEntry triples

triples :: [(Text, [(Text, Type)], Type)]
triples =
  [ (      "chr",                                      [("i",    Type.Int)], Type.String)
  , (   "concat",                  [("x", Type.String), ("y", Type.String)], Type.String)
  , (     "exit",                                      [("i",    Type.Int)],   Type.Unit)
  , (    "flush",                                                        [],   Type.Unit)
  , (  "getchar",                                                        [], Type.String)
  , (      "not",                                      [("i",    Type.Int)],    Type.Int)
  , (      "ord",                                      [("s", Type.String)],    Type.Int)
  , (   "printi",                                      [("i",    Type.Int)],   Type.Unit)
  , (    "print",                                      [("s", Type.String)],   Type.Unit)
  , (     "size",                                      [("s", Type.String)],    Type.Int)
  , ("substring", [("s", Type.String), ("f", Type.Int), ("n",    Type.Int)], Type.String)
  ]

toEntry :: (Text, [(Text, Type)], Type) -> (FuncAddress, Function)
toEntry (name, args, returnType) = (toAddress name, func)
    where
      func = Function (map (mapFst Symbol) args) returnType $ toIRValue returnType

toAddress :: Text -> FuncAddress
toAddress name = FuncAddress (Symbol name) $ ScopeAddress 0

toIRValue :: Type -> IRValue
toIRValue typ = IRValue (toBody typ) typ

toBody :: Type -> Expr
toBody Type.Int    = IntExpr     0 fakeToken
toBody Type.String = StringExpr "" fakeToken
toBody Type.Unit   = SeqExpr    [] fakeToken
toBody x           = error $ "No primitive of this type should exist: " <> (showText x)

fakeToken :: Token
fakeToken = Token Semicolon $ SourceLoc "<built-in>" 0 0
