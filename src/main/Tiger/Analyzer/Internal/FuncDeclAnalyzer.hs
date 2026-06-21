module Tiger.Analyzer.Internal.FuncDeclAnalyzer(crawlFuncDecl) where

import Control.Monad.State.Lazy(gets, modify)

import Data.Functor.Compose(Compose(Compose, getCompose))

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(
    Decl(VariableDecl)
  , Field(Field)
  , FuncDecl(FuncDecl)
  , Expr(ArrayExpr, IntExpr, LetExpr, NilExpr, SeqExpr, StringExpr, token)
  , Symbol(Symbol)
  , VarDecl(VarDecl)
  )

import Tiger.Analyzer.Internal.Address(FuncAddress(FuncAddress), TypeAddress)
import Tiger.Analyzer.Internal.AnalyzerError(AnalyzerErrorType(DuplicateFunc, NonUnitProcedure))

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(functions, scopes)
  , andIfValidMV, fail, flattenVM, lookupTypeOfSym, mapMSequA, resolveTypeAddr, succeed, typeErrorOr
  , Verification, win
  )

import Tiger.Analyzer.Internal.Function(Function(Function))
import Tiger.Analyzer.Internal.IRValue(IRValue(IRValue))
import Tiger.Analyzer.Internal.Scope(Environment(Env), FuncInfo(FIAddress), Scope(Scope))
import Tiger.Analyzer.Internal.Type(Type(Array, Named, Nil, Record, Unit))

import Data.Map                     qualified as Map
import Tiger.Analyzer.Internal.Type qualified as Type


crawlFuncDecl :: (Expr -> Verification IRValue) -> FuncDecl -> Verification (Symbol, Function)
crawlFuncDecl crawlExpr (FuncDecl funcName fields typePairM body token) =
  do
    paramsV     <- getCompose $ traverse compose $ map toTriple fields
    returnTypeV <- maybe (win Unit) (uncurry $ flip lookupTypeOfSym) typePairM
    bodyLetV    <- synthesizeLet body fields
    flattenVM $ (registerFunction crawlExpr token funcName) <$> paramsV <*> returnTypeV <*> bodyLetV
  where
    compose (z, b, tok) = Compose $ (map (z, ) <$> lookupTypeOfSym tok b)

    toTriple (Field name typ _ typeToken) = (name, typ, typeToken)

synthesizeLet :: Expr -> [Field] -> Verification Expr
synthesizeLet body fields =
  case nonEmpty fields of
    Nothing -> win body
    Just fs -> (mapMSequA makeDecl fs) `andIfValidMV` makeLet
  where
    makeLet decls = win $ LetExpr decls body body.token

    makeDecl (Field name typeName nameToken typeToken) =
      (lookupTypeOfSym typeToken typeName) `andIfValidMV` (initialValue nameToken) `andIfValidMV` (
        \initExpr -> win $ VariableDecl $ VarDecl name (Just (typeName, typeToken)) initExpr nameToken
        )

initialValue :: Token -> Type -> Verification Expr
initialValue token              Type.Int = win $ IntExpr     0 token
initialValue token                   Nil = win $ NilExpr       token
initialValue token          (Record _ _) = win $ NilExpr       token
initialValue token           Type.String = win $ StringExpr "" token
initialValue token                  Unit = win $ SeqExpr    [] token
initialValue token (Named _     Nothing) = win $ NilExpr       token
initialValue token (Named _ (Just addr)) =  detour token addr
initialValue token        (Array addr _) = (detour token addr) `andIfValidMV` (
  \initExpr -> win $ ArrayExpr (Symbol "ruination") (IntExpr 0 token) initExpr token
  )

detour :: Token -> TypeAddress -> Verification Expr
detour token addr = (resolveTypeAddr token addr) `andIfValidMV` (initialValue token)

registerFunction :: (Expr -> Verification IRValue) -> Token -> Symbol -> [(Symbol, Type)] -> Type
                                                   -> Expr -> Verification (Symbol, Function)
registerFunction crawlExpr token funcName params returnType bodyLet =
  (crawlExpr bodyLet) `andIfValidMV` (
    \bodyIR@(IRValue bodyExpr bodyType) ->
      if returnType == Unit && bodyType /= Unit then
        fail NonUnitProcedure token
      else
        (returnType, bodyType, bodyExpr.token) `typeErrorOr`
          (declareFunction funcName params returnType bodyIR token) <&> (
              $> (funcName, Function params returnType bodyIR)
            )
    )

declareFunction :: Symbol -> [(Symbol, Type)] -> Type -> IRValue -> Token -> Verification ()
declareFunction name params returnType bodyIR token =
  do
    (Scope (Env funcMap typeMap varMap) addr) :| tail <- gets scopes
    case Map.lookup name funcMap of
      Just (FIAddress _) -> fail DuplicateFunc token
      _                  -> do
        funcs          <- gets functions
        let newAddr     = FuncAddress name addr
        let newFuncs    = Map.insert newAddr (Function params returnType bodyIR) funcs
        let newFuncMap  = Map.insert name    (FIAddress newAddr)                 funcMap
        let newScope    = Scope (Env newFuncMap typeMap varMap) addr
        let newScopes   = newScope :| tail
        modify $ \s -> s { functions = newFuncs, scopes = newScopes }
        succeed
