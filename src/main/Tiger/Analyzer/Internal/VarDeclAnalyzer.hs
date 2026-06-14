module Tiger.Analyzer.Internal.VarDeclAnalyzer(crawlVarDecl) where

import Control.Monad.State.Lazy(gets, modify)

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Expr, Symbol, VarDecl(VarDecl))

import Tiger.Analyzer.Internal.Address(NamedVarAddress(NamedVarAddress))

import Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerErrorType(TypelessVarCannotInitToNil, TypeMismatch)
  )

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(scopes, vars)
  , fail, isSubtypeOf, lookupTypeOfSym, succeed, typeErrorOr, Verification
  )

import Tiger.Analyzer.Internal.IRValue(IRValue(irvType))
import Tiger.Analyzer.Internal.Scope(Environment(Env), Scope(Scope), VarInfo(VIAddress))
import Tiger.Analyzer.Internal.Type(Type(Nil))

import qualified Data.Map as Map


crawlVarDecl :: (Expr -> Verification IRValue) -> VarDecl -> Verification (Symbol, Type)
crawlVarDecl crawlExpr (VarDecl varName _ typeTokenM initial token) =
  do
    initV         <- crawlExpr initial
    let initTypeV  = map irvType initV
    typeVM        <- traverse (fst &> (lookupTypeOfSym token)) typeTokenM
    ((,) <$> (sequenceA typeVM) <*> initTypeV) `failOrM` (
      \(typeM, iType) -> do
        let goodResult = (declareVar varName typeM iType token) <&> ($> (varName, typeM `orElse` iType))
        case (typeM, iType) of
          (Nothing, Nil) -> fail TypelessVarCannotInitToNil token
          (Nothing,   _) -> goodResult
          (Just t1,  t2) -> if t1 `isSubtypeOf` t2 then goodResult else fail (TypeMismatch t1 t2) token
      )

-- Oddly, this language does not care about redeclaring variables. --Jason B. (6/14/26)
declareVar :: Symbol -> Maybe Type -> Type -> Token -> Verification ()
declareVar name typeM initType token =
  do
    (Scope (Env funcMap typeMap varMap) addr) :| tail <- gets scopes
    vars    <- gets vars
    let typ  = typeM `orElse` initType
    (initType, typ, token) `typeErrorOr` do
      let newAddr   = NamedVarAddress name addr
      let newVars   = Map.insert newAddr typ                 vars
      let newVarMap = Map.insert name    (VIAddress newAddr) varMap
      let newScope  = Scope (Env funcMap typeMap newVarMap) addr
      let newScopes = newScope :| tail
      modify $ \s -> s { scopes = newScopes, vars = newVars }
      succeed
