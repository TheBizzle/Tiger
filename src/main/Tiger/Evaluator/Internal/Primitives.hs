module Tiger.Evaluator.Internal.Primitives(primitives) where

import Data.Maybe(fromJust)

import System.Exit(ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO(hFlush, hGetChar, stdin, stdout)

import Tiger.Lexer.Token(SourceLoc(SourceLoc), Token(Token), TokenType(Semicolon))

import Tiger.Parser.AST(Expr(LValueExpr), LValue(Variable), Symbol(Symbol))

import Tiger.Analyzer.Primitives(primArgs)

import Tiger.Evaluator.Internal.Common(
    andIfValidMV, Evaluation, FnBody(PrimitiveBody), Function(Function), win
  )

import Tiger.Evaluator.Internal.ExprEvaluator(evalInt, evalString)
import Tiger.Evaluator.Internal.Scope(FuncAddress(FuncAddress), ScopeAddress(ScopeAddress))
import Tiger.Evaluator.Internal.Value(Value(TInt, TString, TUnit))

import Data.Char    qualified as Char
import Data.Map     qualified as Map
import Data.Text    qualified as Text
import Data.Text.IO qualified as TIO


primitives :: (Map Symbol FuncAddress, Map FuncAddress Function)
primitives = (env, state)

env :: Map Symbol FuncAddress
env = Map.fromList $ map (fst &> (id &&& toAddress)) pairs

state :: Map FuncAddress Function
state = Map.fromList $ map (\(name, body) -> (toAddress name, toFunc name body)) pairs

toAddress :: Symbol -> FuncAddress
toAddress name = FuncAddress name $ ScopeAddress 0

toFunc :: Symbol -> Evaluation Value -> Function
toFunc name body = Function args $ PrimitiveBody body
  where
    args = fromJust $ Map.lookup name primArgs

pairs :: [(Symbol, Evaluation Value)]
pairs =
  map (mapFst Symbol) $
    [ (      "chr", runChr      )
    , (   "concat", runConcat   )
    , (     "exit", runExit     )
    , (    "flush", runFlush    )
    , (  "getchar", runGetChar  )
    , (      "not", runNot      )
    , (      "ord", runOrd      )
    , (   "printi", runPrintI   )
    , (    "print", runPrint    )
    , (     "size", runSize     )
    , ("substring", runSubstring)
    ]

-- chr: (i: Int) => String
runChr :: Evaluation Value
runChr =
  (evalArgInt "i") `andIfValidMV` (
    \i ->
      if i >= 0 && i <= 127 then
        win $ TString $ Text.singleton $ Char.chr i
      else
        error $ "The argument to `chr` must be in [0, 127], but got: `" <> (showText i) <> "`."
    )

-- concat: (x: String, y: String) => String
runConcat :: Evaluation Value
runConcat =
  do
    xV <- evalArgString "x"
    yV <- evalArgString "y"
    return $ (\x y -> TString $ x <> y) <$> xV <*> yV

-- exit: (i: Int) => Nothing
runExit :: Evaluation Value
runExit =
  (evalArgInt "i") `andIfValidMV` (
    \case 0 -> (liftIO exitSuccess) >> (win TUnit)
          i -> (liftIO $ exitWith $ ExitFailure i) >> (win TUnit)
  )

-- flush: () => Unit
runFlush :: Evaluation Value
runFlush =
  do
    liftIO $ hFlush stdout
    win TUnit

-- getchar: () => String
runGetChar :: Evaluation Value
runGetChar =
  do
    char <- liftIO $ hGetChar stdin
    win $ TString $ Text.singleton char

-- not: (i: Int) => Int
runNot :: Evaluation Value
runNot =
  (evalArgInt "i") `andIfValidMV` (
    \case 0 -> win $ TInt 1
          _ -> win $ TInt 0
    )

-- ord: (s: String) => Int
runOrd :: Evaluation Value
runOrd =
  (evalArgString "s") `andIfValidMV` (
    \str ->
      case Text.uncons str of
        Nothing       -> win $ TInt $ -1
        (Just (h, _)) -> win $ TInt $ Char.ord h
    )

-- printi: (i: Int) => Unit
runPrintI :: Evaluation Value
runPrintI =
  (evalArgInt "i") `andIfValidMV` (
    \i -> do
      liftIO $ TIO.hPutStr stdout $ showText i
      win TUnit
    )

-- print: (s: String) => Unit
runPrint :: Evaluation Value
runPrint =
  (evalArgString "s") `andIfValidMV` (
    \s -> do
      liftIO $ TIO.hPutStr stdout s
      win TUnit
    )

-- size: (s: String) => Int
runSize :: Evaluation Value
runSize =
  (evalArgString "s") `andIfValidMV` (
    \str -> win $ TInt $ Text.length str
    )

-- substring: (s: String, f: Int, n: Int) => String
runSubstring :: Evaluation Value
runSubstring =
  do
    sV <- evalArgString "s"
    fV <- evalArgInt    "f"
    nV <- evalArgInt    "n"
    return $ (\str f n -> TString $ Text.take n $ Text.drop f str) <$> sV <*> fV <*> nV

evalArgInt :: Text -> Evaluation Int
evalArgInt name = evalInt $ LValueExpr (Variable (Symbol name) fakeToken) fakeToken

evalArgString :: Text -> Evaluation Text
evalArgString name = evalString $ LValueExpr (Variable (Symbol name) fakeToken) fakeToken

fakeToken :: Token
fakeToken = Token Semicolon $ SourceLoc "<built-in>" 0 0
