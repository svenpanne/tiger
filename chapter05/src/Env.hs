module Env where

import Types ( Ty )
import Symbol ( Table, empty )

type TEnv = Table Ty

-- predefined types
baseTEnv :: TEnv
baseTEnv = empty

data EnvEntry =
    VarEntry Ty
  | FunEntry [Ty] Ty
  deriving ( Show )

type VEnv = Table EnvEntry

-- predefined functions
baseVEnv :: VEnv
baseVEnv = empty
