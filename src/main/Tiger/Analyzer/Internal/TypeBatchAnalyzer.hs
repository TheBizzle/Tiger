module Tiger.Analyzer.Internal.TypeBatchAnalyzer(crawlTypeBatch) where

import Control.Monad.State.Lazy(gets, modify)

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Symbol, TypeDeclEntry(TypeDeclEntry, typeDeclName, typeDeclToken))

import Tiger.Analyzer.Internal.Address(TypeAddress(TypeAddress))

import Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerError(AnalyzerError)
  , AnalyzerErrorType(DuplicateType)
  )

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(scopes, types)
  , andIfValidMV, mapMSequA, succeed, updateEnv, Verification
  )

import Tiger.Analyzer.Internal.Scope(
    Environment(envTypes)
  , Scope(address, environ)
  , TypeInfo(PreType, TIAddress)
  )

import Tiger.Analyzer.Internal.Type(Type)
import Tiger.Analyzer.Internal.TypeDeclAnalyzer(crawlTypeDecl)

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set


crawlTypeBatch :: NonEmpty TypeDeclEntry -> Verification ()
crawlTypeBatch typeDecls =
  case findDupeNames typeDecls of
    Just dupeTokens -> return $ Failure $ dupeTokens <&> (AnalyzerError DuplicateType)
    Nothing         ->
      (mapMSequA storePreType typeDecls) `andIfValidMV` (
        const $ (spamDeclsUntilResolved $ NE.toList typeDecls) `andIfValidMV` (
          \pairs -> do
            scope2 :| tail2  <- gets scopes
            let triples       = map (\(name, typ) -> (name, TypeAddress name scope2.address, typ)) pairs
            let someEnvTypes  = Map.fromList $ map (fst3 &&& (snd3 &> TIAddress)) triples
            let newEnvTypes2  = someEnvTypes `Map.union` scope2.environ.envTypes
            let newScopes2    = (scope2 { environ = scope2.environ { envTypes = newEnvTypes2 } }) :| tail2
            let someNewTypes  = Map.fromList $ map (snd3 &&& thd3) triples
            types            <- gets types
            let newTypes2     = someNewTypes `Map.union` types
            modify $ \state -> state { scopes = newScopes2, types = newTypes2 }
            succeed
          )
        )

findDupeNames :: NonEmpty TypeDeclEntry -> Maybe (NonEmpty Token)
findDupeNames decls = helper (NE.toList decls) (Set.empty, Nothing)
  where
    helper    [] (     _, dupesM) = map NE.reverse dupesM
    helper (h:t) (knowns, dupesM) =
      if name `Set.member` knowns then
        helper t (newKnowns, (Just $ NE.singleton $ typeDeclToken h) <> dupesM)
      else
        helper t (newKnowns, dupesM)
      where
        name      = typeDeclName h
        newKnowns = name `Set.insert` knowns

storePreType :: TypeDeclEntry -> Verification ()
storePreType (TypeDeclEntry name _ _) =
  do
    addr        <- gets $ scopes &> NE.head &> address
    let pretype  = PreType $ TypeAddress name addr
    updateEnv $ \env -> env { envTypes = Map.insert name pretype env.envTypes }
    succeed

-- This enables forward references of types within batches.  See tests #16 and #66.  --Jason B. (6/14/26)
spamDeclsUntilResolved :: [TypeDeclEntry] -> Verification [(Symbol, Type)]
spamDeclsUntilResolved decls =
  do
    pairVs       <- mapM crawlTypeDecl decls
    let nestedVs  = List.zip decls pairVs
    case List.partition (snd &> null) nestedVs of
      (   [], succs) -> return $ sequenceA $ map snd succs
      (fails,    []) -> return $ sequenceA $ map snd fails
      (fails, succs) -> do
        retrieds <- spamDeclsUntilResolved $ map fst fails
        return $ sequenceA $ (map snd succs) <> (sequenceA retrieds)
