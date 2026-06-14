module Tiger.Analyzer.Internal.DeclAnalyzer(crawlDecls) where

import Control.Monad.State.Lazy(gets, modify)

import Data.List(groupBy)

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(
    Decl(FunctionDecl, TypeDecl, VariableDecl)
  , Expr
  , Field(Field)
  , FuncDecl(FuncDecl)
  , Symbol
  , TypeDeclEntry
  , VarDecl(VarDecl)
  )

import Tiger.Analyzer.Internal.Address(
    FuncAddress(FuncAddress)
  , NamedVarAddress(NamedVarAddress)
  )

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(functions, scopes, vars)
  , andIfValidMV, lookupTypeOfSym, mapMSequA, succeed, updateEnv, Verification, win
  )

import Tiger.Analyzer.Internal.FuncDeclAnalyzer(crawlFuncDecl)
import Tiger.Analyzer.Internal.IRValue(IRValue)

import Tiger.Analyzer.Internal.Scope(
    Environment(envFuncs, envVars)
  , FuncInfo(FIAddress, PreFunc)
  , Scope(address, environ)
  , VarInfo(PreVar, VIAddress)
  )

import Tiger.Analyzer.Internal.Type(Type(Unit))
import Tiger.Analyzer.Internal.TypeBatchAnalyzer(crawlTypeBatch)
import Tiger.Analyzer.Internal.VarDeclAnalyzer(crawlVarDecl)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map


data DeclBatch
  = FuncBatch { _fbDecls :: NonEmpty      FuncDecl }
  | TypeBatch { _tbDecls :: NonEmpty TypeDeclEntry }
  |  VarBatch { _vbDecls :: NonEmpty       VarDecl }

crawlDecls :: (Expr -> Verification IRValue) -> NonEmpty Decl -> Verification ()
crawlDecls crawlExpr decls = (mapMSequA (crawlBatch crawlExpr) batches) <&> ($> ())
  where
    batches = NE.fromList $ buildBatches $ NE.toList decls

buildBatches :: [Decl] -> [DeclBatch]
buildBatches = (groupBy sameConstructor) &> (map groupToBatch)
  where
    sameConstructor (FunctionDecl _) (FunctionDecl _) = True
    sameConstructor (TypeDecl     _) (TypeDecl     _) = True
    sameConstructor (VariableDecl _) (VariableDecl _) = True
    sameConstructor                _                _ = False

    groupToBatch js =
      case nonEmpty js of
        Nothing -> error "Not possible for `groupBy` to produce an empty group!"
        Just ne ->
          case ne of
            (FunctionDecl _ :| _) -> FuncBatch $ map (\case (FunctionDecl x) -> x; _ -> error "unpossible") ne
            (TypeDecl     _ :| _) -> TypeBatch $ map (\case (TypeDecl     y) -> y; _ -> error "unpossible") ne
            (VariableDecl _ :| _) ->  VarBatch $ map (\case (VariableDecl z) -> z; _ -> error "unpossible") ne

-- I absolutely hate that the author has us do this.  This functionality seems to strictly make the semantics
-- of the language *worse*, while being totally unnecessary. --Jason B. (6/6/26)
crawlBatch :: (Expr -> Verification IRValue) -> DeclBatch -> Verification ()
crawlBatch crawlExpr (FuncBatch funcDecls) =
  (mapMSequA storePreFunc funcDecls) `andIfValidMV` (
    const $ (mapMSequA (crawlFuncDecl crawlExpr) funcDecls) `andIfValidMV` (
      \pairs -> do
        scope2 :| tail2  <- gets scopes
        let triples       = map (\(name, func) -> (name, FuncAddress name scope2.address, func)) pairs
        let someEnvFuncs  = Map.fromList $ NE.toList $ map (fst3 &&& (snd3 &> FIAddress)) triples
        let newEnvFuncs2  = someEnvFuncs `Map.union` scope2.environ.envFuncs
        let newScopes2    = (scope2 { environ = scope2.environ { envFuncs = newEnvFuncs2 } }) :| tail2
        let someNewFuncs  = Map.fromList $ NE.toList $ map (snd3 &&& thd3) triples
        functions        <- gets functions
        let newFuncs2     = someNewFuncs `Map.union` functions
        modify $ \state -> state { functions = newFuncs2, scopes = newScopes2 }
        succeed
      )
    )
  where
    storePreFunc (FuncDecl name params typeM _ _) =
      do
        fieldsV  <- mapMSequA lookupField params
        retTypeV <- retTypeFromM typeM
        ((,) <$> fieldsV <*> retTypeV) `failOrM` (
          \(fields, typ) -> do
            let prefunc = PreFunc fields typ
            updateEnv $ \env -> env { envFuncs = Map.insert name prefunc env.envFuncs }
            succeed
          )

    lookupField (Field name _ tName _ tToken) =
      (lookupTypeOfSym tToken tName) `andIfValidMV` ((name, ) &> win)

crawlBatch _ (TypeBatch typeDecls) =
  crawlTypeBatch typeDecls

crawlBatch crawlExpr (VarBatch varDecls) =
  (mapMSequA storePreVar varDecls) `andIfValidMV` (
    const $ (mapMSequA (crawlVarDecl crawlExpr) varDecls) `andIfValidMV` (
      \pairs -> do
        scope2 :| tail2 <- gets scopes
        let triples      = map (\(name, var) -> (name, NamedVarAddress name scope2.address, var)) pairs
        let someEnvVars  = Map.fromList $ NE.toList $ map (fst3 &&& (snd3 &> VIAddress)) triples
        let newEnvVars2  = someEnvVars `Map.union` scope2.environ.envVars
        let newScopes2   = (scope2 { environ = scope2.environ { envVars = newEnvVars2 } }) :| tail2
        let someNewVars  = Map.fromList $ NE.toList $ map (snd3 &&& thd3) triples
        vars            <- gets vars
        let newVars2     = someNewVars `Map.union` vars
        modify $ \state -> state { scopes = newScopes2, vars = newVars2 }
        succeed
      )
    )
  where
    storePreVar (VarDecl name _ typeM _ _) =
        (retTypeFromM typeM) `andIfValidMV` (
          \typ -> do
            let prevar = PreVar typ
            updateEnv $ \env -> env { envVars = Map.insert name prevar env.envVars }
            succeed
          )
      where

retTypeFromM :: Maybe (Symbol, Token) -> Verification Type
retTypeFromM typM = maybe (win Unit) (\(n, token) -> lookupTypeOfSym token n) typM
