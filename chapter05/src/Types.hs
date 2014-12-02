module Types (
  Unique, uniqueSupply,
  Ty(..), isArithmeticTy, isEqualityTy, isComparableTy, isCompatibleWith
) where

import Symbol ( Symbol )

newtype Unique = Unique Int
  deriving ( Eq, Show )

uniqueSupply :: [Unique]
uniqueSupply = map Unique [0..]

data Ty =
    Record [(Symbol, Ty)] Unique
  | Nil
  | Int
  | String
  | Array Ty Unique
  | Name Symbol (Maybe Ty)  -- TODO In SML this is a ref
  | Unit
  deriving ( Show )

-- TODO Look through Name everywhere?
isArithmeticTy :: Ty -> Bool
isArithmeticTy ty = case ty of
  Int -> True
  _ -> False

isEqualityTy :: Ty -> Bool
isEqualityTy ty = case ty of
  Record _ _ -> True
  Nil -> True
  Int -> True
  String -> True
  Array _ _ -> True
  _ -> False

isComparableTy :: Ty -> Bool
isComparableTy ty = case ty of
  Int -> True
  String -> True
  _ -> False

isCompatibleWith :: Ty -> Ty -> Bool
isCompatibleWith x y = case (x, y) of
  (Record _ u1, Record _ u2) -> u1 == u2
  (Nil, Record _ _) -> True
  (Record _ _, Nil) -> True
  (Int, Int) -> True
  (String, String) -> True
  (Array _ u1, Array _ u2) -> u1 == u2
  (Name _ (Just t1), Name _ (Just t2)) -> isCompatibleWith t1 t2
  (Unit, Unit) -> True
  _ -> False
