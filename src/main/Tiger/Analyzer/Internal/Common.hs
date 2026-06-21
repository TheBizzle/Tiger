module Tiger.Analyzer.Internal.Common(
    AnalyzerState( AnalyzerState, functions, isInFor, isInWhile, lastScopeAddr, nextUniqueID, protecteds
                 , scopes, types, vars
                 )
  , andIfValidMV, err, fail, findFieldAddr, findInEnv, flattenVM, isSubtypeOf, lookupTypeAddrOfSym
  , lookupTypeOf, lookupTypeOfSym, mapMSequA, resolveTypeAddr, Stately, stackFrame, succeed, typeErrorOr
  , updateEnv, Validated, Verification, win
  ) where

import Control.Monad.State.Lazy(gets, modify)

import Data.List.NonEmpty((<|))

import Tiger.Lexer.Token(Token)

import Tiger.Parser.AST(Symbol(Symbol))

import Tiger.Analyzer.Internal.Address(
    FuncAddress
  , LValueAddress(ArrayElemAddress, RecordPropAddress, VarAddress)
  , NamedVarAddress
  , ScopeAddress(n, ScopeAddress)
  , TypeAddress(IntAddress, StringAddress)
  )

import Tiger.Analyzer.Internal.AnalyzerError(
    AnalyzerError(AnalyzerError)
  , AnalyzerErrorType(BadInternalState, NoSuchProperty, NoSuchRecordType, NoSuchType, TypeMismatch)
  )

import Tiger.Analyzer.Internal.Function(Function)
import Tiger.Analyzer.Internal.Scope(Environment(Env, envTypes), Scope(environ, Scope), TypeInfo(tiAddr))
import Tiger.Analyzer.Internal.Type(isSubtypeOf, Type(Array, Named, Record), UniqueID)

import Data.List.NonEmpty           qualified as NE
import Data.Map                     qualified as Map
import Tiger.Analyzer.Internal.Type qualified as Type


data AnalyzerState =
  AnalyzerState { functions     :: Map FuncAddress Function
                , isInFor       :: Bool
                , isInWhile     :: Bool
                , lastScopeAddr :: ScopeAddress
                , nextUniqueID  :: Word
                , protecteds    :: Set NamedVarAddress
                , scopes        :: NonEmpty Scope
                , types         :: Map TypeAddress Type
                , vars          :: Map NamedVarAddress Type
                }
  deriving (Eq, Show)

type Validated    a = Validation (NonEmpty AnalyzerError) a
type Verification a = Stately (Validated a)
type Stately      a = State AnalyzerState a

lookupTypeOf :: Token -> LValueAddress -> Verification Type
lookupTypeOf token (VarAddress addr) =
  do
    vars      <- gets vars
    let typeM  = Map.lookup addr vars
    maybe (fail BadInternalState token) win typeM

lookupTypeOf token (ArrayElemAddress addr) =
  (lookupTypeOf token addr) `andIfValidMV` (
    \case
      (Array elemTypeAddr _) -> resolveTypeAddr token elemTypeAddr
      _                      -> fail BadInternalState token
    )

lookupTypeOf token (RecordPropAddress addr p) =
  (lookupTypeOf token addr) `andIfValidMV` (
    \case
      (Record fields uid) -> (findFieldAddr token uid p fields) `failOrM` (resolveTypeAddr token)
      _                   -> fail NoSuchRecordType token
    )

lookupTypeOfSym :: Token -> Symbol -> Verification Type
lookupTypeOfSym token typeName = (lookupTypeAddrOfSym token typeName) `andIfValidMV` (resolveTypeAddr token)

lookupTypeAddrOfSym :: Token -> Symbol -> Verification TypeAddress
lookupTypeAddrOfSym     _ (Symbol "string") = win StringAddress
lookupTypeAddrOfSym     _ (Symbol    "int") = win IntAddress
lookupTypeAddrOfSym token              name = findInEnv name token envTypes NoSuchType $ tiAddr &> win

resolveTypeAddr :: Token -> TypeAddress -> Verification Type
resolveTypeAddr     _    IntAddress = win Type.Int
resolveTypeAddr     _ StringAddress = win Type.String
resolveTypeAddr token          addr = (gets $ types &> Map.lookup addr) >>= findTrueTypeM
  where
    findTrueTypeM Nothing                = badEnd
    findTrueTypeM (Just (Named _ addrM)) = maybe badEnd (resolveTypeAddr token) addrM
    findTrueTypeM (Just             typ) = win typ

    badEnd = fail NoSuchType token

findFieldAddr :: Token -> UniqueID -> Symbol -> [(Symbol, TypeAddress)] -> Validated TypeAddress
findFieldAddr token uid _             []          = err (NoSuchProperty uid) token
findFieldAddr     _   _ p ((h, tAddr):_) | p == h = Success tAddr
findFieldAddr token uid p (         _:t)          = findFieldAddr token uid p t

findInEnv :: Symbol -> Token -> (Environment -> Map Symbol a) -> AnalyzerErrorType
                    -> (a -> Verification b) -> Verification b
findInEnv name token getMap errorType builder =
  do
    head :| tail <- gets scopes
    climbScopes head tail
  where
    climbScopes (Scope env _) scopes =
      case Map.lookup name $ getMap env of
        Just  x -> builder x
        Nothing -> case scopes of
                     []    -> return $ err errorType token
                     (h:t) -> climbScopes h t

andIfValidMV :: (Monad m) => m (Validation (NonEmpty e) a) -> (a -> m (Validation (NonEmpty e) b))
                                                           -> m (Validation (NonEmpty e) b)
andIfValidMV aVM f =
  do
    aV <- aVM
    aV `failOrM` f

mapMSequA :: (Monad m, Applicative f, Traversable t) => (a -> m (f b)) -> t a -> m (f (t b))
mapMSequA f xs =
  do
    resVs <- mapM f xs
    return $ sequenceA resVs

flattenVM :: Monad m => Validation fs (m (Validation fs a)) -> m (Validation fs a)
flattenVM = validation (Failure &> return) id

popScope :: Stately ()
popScope = modify helper
  where
    helper state = state { scopes = newScopes }
      where
        (_myScope, tailMNE) = NE.uncons state.scopes
        newScopes           = tailMNE `orElse` (error whinerMsg)
        whinerMsg           = "Critical error!  You should never be able to pop the global scope!" :: Text

pushScope :: Stately ()
pushScope = modify helper
  where
    helper state = state { scopes = newScope <| state.scopes, lastScopeAddr = newAddr }
      where
        newAddr  = ScopeAddress $ state.lastScopeAddr.n + 1
        newScope = Scope (Env Map.empty Map.empty Map.empty) newAddr

typeErrorOr :: (Type, Type, Token) -> Verification a -> Verification a
(typeA, typeB, token) `typeErrorOr` alt =
  if not $ typeA `isSubtypeOf` typeB then
    fail (TypeMismatch typeB typeA) token
  else
    alt

stackFrame :: Stately a -> Stately a
stackFrame fv = pushScope *> fv <* popScope

updateEnv :: (Environment -> Environment) -> Stately ()
updateEnv f = updateScope $ \s -> s { environ = f s.environ }

updateScope :: (Scope -> Scope) -> Stately ()
updateScope f = modify $ \s -> s { scopes = case scopes s of h :| t -> (f h) :| t }

fail :: AnalyzerErrorType -> Token -> Verification a
fail errorType token = return $ err errorType token

err :: AnalyzerErrorType -> Token -> Validated a
err errorType token = Failure $ NE.singleton $ AnalyzerError errorType token

succeed :: Verification ()
succeed = win ()

win :: a -> Verification a
win = Success &> return
