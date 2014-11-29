module Semant ( ExpTy(..), semant ) where

import AbSyn
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Data.Functor.Identity
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
type Trans = StateT Envs (ErrorT String Identity)

evalT :: Trans a -> Either String a
evalT = runIdentity . runErrorT . flip evalStateT initialEnvs

throwT :: Pos -> String -> Trans a
throwT p m = lift (throwError (shows p . showString ": " $ m))

getVEnv :: Trans VEnv
getVEnv = gets vEnv

--------------------------------------------------------------------------------

data ExpTy = ExpTy {
  translatedExp :: Translate.Exp,
  typeOf :: Types.Ty
} deriving ( Show )

returnTy :: Types.Ty -> Trans ExpTy
returnTy = return . ExpTy Translate.Exp

--------------------------------------------------------------------------------

transVar :: Var -> Trans ExpTy
transVar = undefined

transExp :: Exp -> Trans ExpTy
transExp e = case e of
  NilExp -> returnTy Types.Nil
  IntExp _ -> returnTy Types.Int
  StringExp _ _ -> returnTy Types.String

  OpExp l PlusOp r p -> transArithExp l r p
  OpExp l MinusOp r p -> transArithExp l r p
  OpExp l TimesOp r p -> transArithExp l r p
  OpExp l DivideOp r p -> transArithExp l r p

  OpExp l EqOp r p -> transEqOp l r p
  OpExp l NeqOp r p -> transEqOp l r p

  OpExp l LtOp r p -> transCompExp l r p
  OpExp l LeOp r p -> transCompExp l r p
  OpExp l GtOp r p -> transCompExp l r p
  OpExp l GeOp r p -> transCompExp l r p

transArithExp :: Exp -> Exp -> Pos -> Trans ExpTy
transArithExp l r p = do
  etl <- transExp l
  etr <- transExp r
  case (typeOf etl, typeOf etr) of
    (Types.Int, Types.Int) -> return ()
    _ -> throwT p "two integers required"
  returnTy Types.Int

transEqOp :: Exp -> Exp -> Pos -> Trans ExpTy
transEqOp l r p = do
  etl <- transExp l
  etr <- transExp r
  case (typeOf etl, typeOf etr) of
    (Types.Int, Types.Int) -> return ()
    (Types.String, Types.String) -> return ()
    (Types.Record _ u1, Types.Record _ u2) ->
      unless (u1 == u2) $
        throwT p "records must be of the same type"
    (Types.Array _ u1, Types.Array _ u2) ->
      unless (u1 == u2) $
        throwT p "arrays must be of the same type"
    _ -> throwT p "two integers/strings/records/arrays required"
  returnTy Types.Int

transCompExp :: Exp -> Exp -> Pos -> Trans ExpTy
transCompExp l r p = do
  etl <- transExp l
  etr <- transExp r
  case (typeOf etl, typeOf etr) of
    (Types.Int, Types.Int) -> return ()
    (Types.String, Types.String) -> return ()
    _ -> throwT p "two integers/strings required"
  returnTy Types.Int

transDec :: Dec -> Trans ()
transDec = undefined

transTy :: Ty -> Trans Types.Ty
transTy = undefined

semant :: Exp -> Either String ExpTy
semant = evalT . transExp