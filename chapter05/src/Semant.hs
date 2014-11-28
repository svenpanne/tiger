module Semant where

import AbSyn
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Env
import qualified Translate
import qualified Types

--------------------------------------------------------------------------------

-- The state of our monad is a pair of value/type environments.
data Envs = Envs {
  vEnv :: VEnv,
  tEnv :: TEnv
} deriving ( Show )

initialEnvs :: Envs
initialEnvs = Envs baseVEnv baseTEnv

--------------------------------------------------------------------------------

-- We need modifiable value/type environments and can fail.
type Trans = StateT Envs (Either String)

evalT :: Trans a -> Either String a
evalT = flip evalStateT initialEnvs

getVEnv :: Trans VEnv
getVEnv = gets vEnv

--------------------------------------------------------------------------------

data ExpTy = ExpTy Translate.Exp Types.Ty
  deriving ( Show )

transVar :: Var -> Trans ExpTy
transVar = undefined

transExp :: Exp -> Trans ExpTy
transExp e = case e of
  NilExp -> return $ ExpTy Translate.Exp Types.Nil
  IntExp _ -> return $ ExpTy Translate.Exp Types.Int
  StringExp _ _ -> return $ ExpTy Translate.Exp Types.String
  OpExp l o r p -> do ExpTy _ tl <- transExp l
                      ExpTy _ tr <- transExp r
                      return $ ExpTy Translate.Exp Types.Int

transDec :: Dec -> Trans ()
transDec = undefined

transTy :: Ty -> Trans Types.Ty
transTy = undefined
