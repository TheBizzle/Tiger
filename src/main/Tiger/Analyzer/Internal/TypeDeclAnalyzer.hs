module Tiger.Analyzer.Internal.TypeDeclAnalyzer(crawlTypeDecl) where

import Control.Monad.State.Lazy(gets, modify)

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Field(Field), Symbol, TypeDeclEntry(TypeDeclEntry))

import Tiger.Analyzer.Internal.Address(TypeAddress(TypeAddress))
import Tiger.Analyzer.Internal.AnalyzerError(AnalyzerErrorType(DuplicateFieldInType, DuplicateType))

import Tiger.Analyzer.Internal.Common(
    AnalyzerState(nextUniqueID, scopes, types)
  , andIfValidMV, err, fail, lookupTypeAddrOfSym, lookupTypeOfSym, mapMSequA, Stately, succeed, Verification
  )

import Tiger.Analyzer.Internal.Scope(Environment(Env), Scope(Scope), TypeInfo(TIAddress))
import Tiger.Analyzer.Internal.Type(Type(Array, Record), UniqueID(UniqueID))

import Data.List        qualified as List
import Data.Map         qualified as Map
import Data.Set         qualified as Set
import Tiger.Parser.AST qualified as AST


crawlTypeDecl :: TypeDeclEntry -> Verification (Symbol, Type)
crawlTypeDecl (TypeDeclEntry name typ token) =
  (convertType typ) `andIfValidMV` (\t -> (declareType name t token) <&> ($> (name, t)))

convertType :: AST.Type -> Verification Type
convertType (AST.ArrayType name token) =
  (lookupTypeAddrOfSym token name) `andIfValidMV` (Array &> withUniqueID &> map Success)

convertType (AST.NamedType mySymbol token) =
  lookupTypeOfSym token mySymbol

convertType (AST.RecordType fields) =
  do
    let dupeTokens  = fst $ foldl' checkForDupes ([], Set.empty) fields
    let checkedsV   = traverse (err DuplicateFieldInType) $ List.reverse dupeTokens
    pairsV         <- mapMSequA withType fields
    (checkedsV *> pairsV) `failOrM` (Record &> withUniqueID &> (map Success))
  where
    checkForDupes :: ([Token], Set Symbol) -> Field -> ([Token], Set Symbol)
    checkForDupes (baddies, names) (Field name _ token _) =
      if name `Set.member` names then
        (token : baddies, name `Set.insert` names)
      else
        (        baddies, name `Set.insert` names)

    withType (Field name typeName _ typeToken) =
      (lookupTypeAddrOfSym typeToken typeName) <&> (map (name, ))

declareType :: Symbol -> Type -> Token -> Verification ()
declareType name typ token =
  do
    (Scope (Env funcMap typeMap varMap) addr) :| tail <- gets scopes
    case Map.lookup name typeMap of
      Just (TIAddress _) -> fail DuplicateType token
      _                  -> do
        types          <- gets types
        let newAddr     = TypeAddress name addr
        let newTypes    = Map.insert newAddr typ types
        let newTypeMap  = Map.insert name (TIAddress newAddr) typeMap
        let newScope    = Scope (Env funcMap newTypeMap varMap) addr
        let newScopes   = newScope :| tail
        modify $ \s -> s { scopes = newScopes, types = newTypes }
        succeed

withUniqueID :: (UniqueID -> Type) -> Stately Type
withUniqueID f =
  do
    uniqueID <- gets nextUniqueID
    modify $ \s -> s { nextUniqueID = uniqueID + 1 }
    return $ f $ UniqueID uniqueID
