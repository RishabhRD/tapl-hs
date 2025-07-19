module Arith.Eval (eval) where

import Arith.Syntax

-- Evaluates given term one step. If term is in normal form, returns Nothing.
eval1 :: Term -> Maybe Term
eval1 (If True_ t _) = Just t
eval1 (If False_ _ t) = Just t
eval1 (If t1 t2 t3) = do
  t1' <- eval1 t1
  return (If t1' t2 t3)
eval1 (Succ t) = do
  t' <- eval1 t
  return (Succ t')
eval1 (Pred (Zero)) = Just Zero
eval1 (Pred (Succ t)) | isNumeric t = Just t
eval1 (Pred t) = do
  t' <- eval1 t
  return (Pred t')
eval1 (IsZero Zero) = Just True_
eval1 (IsZero (Succ t)) | isNumeric t = Just False_
eval1 (IsZero t) = do
  t' <- eval1 t
  return (IsZero t')
eval1 _ = Nothing

-- Evaluates the given term to its normal form.
eval :: Term -> Term
eval t =
  let t' = eval1 t
   in case t' of
        Nothing -> t
        (Just t'') -> eval t''
