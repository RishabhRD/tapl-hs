module Main (main) where

import Arith.Eval
import Arith.Lexer (lexString)
import Arith.Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath : _) -> do
      content <- readFile filePath
      let tokens = lexString content
      case parseTerm tokens of
        Just (term, []) -> do
          let result = eval term
          print result
        Just (_, leftOver) -> do
          putStrLn "Error: Unconsumed input after parsing."
          print leftOver
        Nothing -> putStrLn "Error: Failed to parse input."
    [] -> putStrLn "Usage: <program> <source-file>"
