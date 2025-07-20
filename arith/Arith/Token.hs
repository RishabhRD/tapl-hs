module Arith.Token (Token (..)) where

data Token
  = TrueT
  | FalseT
  | ZeroT
  | SuccT
  | PredT
  | IsZeroT
  | IfT
  | ThenT
  | ElseT
  | LParenT
  | RParenT
  | LBraceT
  | RBraceT
  | InvalidT
  deriving (Show, Eq)
