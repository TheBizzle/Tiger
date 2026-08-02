module Main(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO(stderr)

import Tiger.Analyzer.Analyzer(IRValue(irvExpr))

import Tiger.Parser.AST(Symbol(symbolText))

import Tiger.Compiler(compile, Program)

import Tiger.Evaluator.Evaluator(eval, EvaluatorState, initialState)
import Tiger.Evaluator.EvaluatorError(EvaluatorError)
import Tiger.Evaluator.Value(Value(TArray, TInt, TNil, TRecord, TString, TUnit))

import Tiger.ErrorParser(CompilationError(BadEval), formatErrorOutput)

import Control.Exception qualified as Exception
import Data.Map          qualified as Map
import Data.Text.IO      qualified as TIO


main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs            [] = Exception.handle handler $ runPrompt initialState
    processArgs (filePath:[]) = (TIO.readFile filePath) >>= runFile
    processArgs             _ = (TIO.putStrLn "Usage: tiger [file]") >> (exitWith $ ExitFailure 64)

    handler :: Exception.IOException -> IO ()
    handler _ = TIO.putStrLn "EOF reached.  Exiting...." >> return ()

runPrompt :: EvaluatorState -> IO ()
runPrompt program =
  do
    putStrFlush "> "
    line <- TIO.getLine
    when (line /= "exit") $ do
      if (line /= "") then do
        (state, _) <- run True program line
        runPrompt state
      else
        runPrompt program

runFile :: Text -> IO ()
runFile code =
  do
    (_, errorCode) <- run False initialState code
    when (errorCode /= 0) $
      exitWith $ ExitFailure errorCode

run :: Bool -> EvaluatorState -> Text -> IO (EvaluatorState, Int)
run isREPL state inputText = compiled |> (validation reportErrors $ runProgram isREPL inputText state)
  where
    compiled = compile ("<REPL>", inputText)

    reportErrors errorText = (TIO.hPutStrLn stderr errorText) $> (state, 65)

runProgram :: Bool -> Text -> EvaluatorState -> Program -> IO (EvaluatorState, Int)
runProgram isREPL inputText state program =
  do
    (resultV, newState) <- eval state program.irvExpr
    output              <- validation handleBad handleGood resultV
    return (newState, output)
  where
    handleBad :: (NonEmpty EvaluatorError) -> IO Int
    handleBad errors =
      do
        TIO.hPutStrLn stderr $ formatErrorOutput inputText $ map BadEval errors
        return 70

    handleGood result =
      if isREPL then
        result |> showValue &> TIO.putStrLn &> ($> 0)
      else
        return 0

    showValue :: Value -> Text
    showValue (TArray values)      = "[" <> (showValues values) <> "]"
    showValue (TInt n)             = showText n
    showValue  TNil                = "nil"
    showValue (TRecord fields sym) = (showText sym.symbolText) <> "{ " <> (showFields $ Map.assocs fields) <> " }"
    showValue (TString str)        = showText str
    showValue  TUnit               = "()"

    showValues :: [Value] -> Text
    showValues   [x] = showValue x
    showValues (h:t) = (showValue h) <> ", " <> (showValues t)
    showValues    [] = ""

    showFields :: [(Symbol, Value)] -> Text
    showFields   [(n, v)] = (showText n) <> ": " <> (showValue v)
    showFields ((n, v):t) = (showText n) <> ": " <> (showValue v) <> ", " <> (showFields t)
    showFields         [] = ""
