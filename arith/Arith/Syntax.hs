module Arith.Syntax (Term (..), isNumeric, isValue) where

-- A term in the arith language.
data Term
  = True_
  | False_
  | Zero
  | If Term Term Term
  | Succ Term
  | Pred Term
  | IsZero Term
  deriving (Show, Eq)

-- Returns true if given term is a numeric value.
isNumeric :: Term -> Bool
isNumeric (Zero) = True
isNumeric (Succ t) = isNumeric t
isNumeric (_) = False

-- Returns true if given term is a value.
isValue :: Term -> Bool
isValue (True_) = True
isValue (False_) = True
isValue t = isNumeric t
