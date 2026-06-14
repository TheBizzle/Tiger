module Main(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import Tiger.Compiler(compile)

import Data.Text.IO qualified as TIO


main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs [] = TIO.putStr $ showText $ compile ("", "")
    processArgs _  = (TIO.putStrLn "Usage: tiger") >> (exitWith $ ExitFailure 64)
