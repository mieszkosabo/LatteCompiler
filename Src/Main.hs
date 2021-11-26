module Main where

import Control.Monad
import Data.Either (isLeft)
import qualified Data.Text as T
import Parser.AbsLatte
import Parser.ErrM
import Parser.ParLatte (myLexer, pProgram)
import Src.CodeGen.CodeGen
import Src.CodeGen.State
import Src.Frontend.StaticCheck
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IOMode (ReadMode, WriteMode), getContents, hGetContents, hPrint, hPutStr, hPutStrLn, openFile, stderr, stdin, stdout)
import System.Process (callCommand)

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
  let (Program _ topDefs) = program
  (_, state) <- runGen (genCode topDefs)
  print $ store state
  let code = unlines . reverse $ revCode state

  let newFilename = replaceFileExtension ".lat" ".ll" file
  writeFile newFilename code
  let bytecodeFilename = replaceFileExtension ".ll" ".bc" newFilename
  callCommand $ "llvm-as -o " ++ bytecodeFilename ++ " " ++ newFilename
  callCommand $ "llvm-link -o " ++ bytecodeFilename ++ " " ++ bytecodeFilename ++ " lib/runtime.bc"
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

replaceFileExtension :: String -> String -> String -> String
replaceFileExtension oldExt newExt filename =
  T.unpack (T.replace (T.pack oldExt) (T.pack newExt) (T.pack filename))