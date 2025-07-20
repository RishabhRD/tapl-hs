module Arith.Parser (parseTerm) where

import Arith.Syntax
import Arith.Token
import Control.Applicative ((<|>))

consume :: Token -> [Token] -> Maybe [Token]
consume expected (t : ts) | t == expected = Just ts
consume _ _ = Nothing

parseZero :: [Token] -> Maybe (Term, [Token])
parseZero [] = Nothing
parseZero (x : xs) = case x of
  ZeroT -> Just (Zero, xs)
  _ -> Nothing

parseSucc :: [Token] -> Maybe (Term, [Token])
parseSucc tokens = do
  tokens1 <- consume SuccT tokens
  tokens2 <- consume LParenT tokens1
  (term, tokens3) <- parseTerm tokens2
  tokens4 <- consume RParenT tokens3
  return (Succ term, tokens4)

parsePred :: [Token] -> Maybe (Term, [Token])
parsePred tokens = do
  tokens1 <- consume PredT tokens
  tokens2 <- consume LParenT tokens1
  (term, tokens3) <- parseTerm tokens2
  tokens4 <- consume RParenT tokens3
  return (Pred term, tokens4)

parseIsZero :: [Token] -> Maybe (Term, [Token])
parseIsZero tokens = do
  tokens1 <- consume IsZeroT tokens
  (term, tokens2) <- parseTerm tokens1
  return (IsZero term, tokens2)

parseIf :: [Token] -> Maybe (Term, [Token])
parseIf ts = do
  ts1 <- consume IfT ts
  ts2 <- consume LParenT ts1
  (cond, ts3) <- parseTerm ts2
  ts4 <- consume RParenT ts3
  ts5 <- consume ThenT ts4
  ts6 <- consume LBraceT ts5
  (thenPart, ts7) <- parseTerm ts6
  ts8 <- consume RBraceT ts7
  ts9 <- consume ElseT ts8
  ts10 <- consume LBraceT ts9
  (elsePart, ts11) <- parseTerm ts10
  ts12 <- consume RBraceT ts11
  return (If cond thenPart elsePart, ts12)

parseTerm :: [Token] -> Maybe (Term, [Token])
parseTerm tokens =
  parseZero tokens
    <|> parseSucc tokens
    <|> parsePred tokens
    <|> parseIsZero tokens
    <|> parseIf tokens
