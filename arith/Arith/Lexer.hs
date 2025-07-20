module Arith.Lexer (lexString, lexFile) where

import Arith.Token
import Data.Char

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace '\t' = True
isWhitespace _ = False

isOperator :: Char -> Bool
isOperator x = case x of
  '(' -> True
  ')' -> True
  '{' -> True
  '}' -> True
  _ -> False

toTokenKeyword :: String -> Token
toTokenKeyword keyword = case keyword of
  "isZero" -> IsZeroT
  "succ" -> SuccT
  "pred" -> PredT
  "if" -> IfT
  "then" -> ThenT
  "else" -> ElseT
  _ -> InvalidT

toTokenNumber :: String -> Token
toTokenNumber "0" = ZeroT
toTokenNumber _ = InvalidT

toTokenOperator :: Char -> Token
toTokenOperator op = case op of
  '(' -> LParenT
  ')' -> RParenT
  '{' -> LBraceT
  '}' -> RBraceT
  _ -> InvalidT

lexStartWithKeyword :: String -> [Token]
lexStartWithKeyword str =
  let (keyword, rest) = span isAlpha str
   in (toTokenKeyword keyword) : (lexString rest)

lexStartWithNumber :: String -> [Token]
lexStartWithNumber str =
  let (num, rest) = span isDigit str
   in (toTokenNumber num) : (lexString rest)

lexStartWithOperator :: String -> [Token]
lexStartWithOperator [] = []
lexStartWithOperator (x : xs) = (toTokenOperator x) : (lexString xs)

lexString :: String -> [Token]
lexString [] = []
lexString (x : xs)
  | isWhitespace x = lexString xs
  | isAlpha x = lexStartWithKeyword (x : xs)
  | isOperator x = lexStartWithOperator (x : xs)
  | isDigit x = lexStartWithNumber (x : xs)
  | otherwise = InvalidT : (lexString xs)

lexFile :: FilePath -> IO [Token]
lexFile path = do
  contents <- readFile path
  return (lexString contents)
