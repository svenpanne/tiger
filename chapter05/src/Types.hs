module Types where

import Symbol ( Symbol )

newtype Unique = Unique Int
  deriving ( Eq, Show )

data Ty =
    Record [(Symbol, Ty)] Unique
  | Nil
  | Int
  | String
  | Array Ty Unique
  | Name Symbol (Maybe Ty)  -- TODO In SML this is a ref
  | Unit
  deriving ( Show )
