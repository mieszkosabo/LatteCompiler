module Main where

import Control.Monad
import Data.Either (isLeft)
import Parser.AbsLatte
import Parser.ErrM
import Parser.ParLatte (myLexer, pProgram)
import Src.Frontend.StaticCheck
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getContents, hGetContents, hPrint, hPutStr, hPutStrLn, stderr, stdin)

main :: IO ()
main = do
  files <- getArgs
  when
    (null files)
    (exitWithFailureMessage "No source file specified")
  let file = head files
  program <- parseFile file
  staticCheckResult <- runStaticCheck program
  when
    (isLeft staticCheckResult)
    ( do
        let (Left msg) = staticCheckResult
        exitWithFailureMessage (show msg)
    )
  hPutStrLn stderr "OK"

exitWithFailureMessage :: String -> IO a
exitWithFailureMessage msg = hPutStrLn stderr "ERROR" >> hPutStrLn stderr msg >> exitFailure

parse :: String -> IO Program
parse input =
  case pProgram (myLexer input) of
    (Right parsedProg) -> return parsedProg
    (Left msg) -> exitWithFailureMessage msg

parseFile :: String -> IO Program
parseFile filename = readFile filename >>= parse