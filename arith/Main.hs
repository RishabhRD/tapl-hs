module Main (main) where

import Arith.Eval
import Arith.Lexer (lexString)
import Arith.Parser
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

runProgram :: String -> IO ()
runProgram input = do
  let tokens = lexString input
  case parseTerm tokens of
    Just (term, []) -> print (eval term)
    Just (_, leftover) -> putStrLn $ "Unconsumed input input: " ++ show leftover
    Nothing -> putStrLn "Error: Failed to parse input."

repl :: IO ()
repl = do
  putStr ">>> "
  hFlush stdout
  line <- getLine
  if line == ":quit" || line == ":q"
    then putStrLn "Bye!"
    else runProgram line >> repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--prompt" : _) -> repl
    (filePath : _) -> readFile filePath >>= runProgram
    _ -> putStrLn "Usage: <program> <source-file>"
