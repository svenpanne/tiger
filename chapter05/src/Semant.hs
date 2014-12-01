module Semant ( ExpTy(..), semant ) where

import AbSyn
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Data.Functor.Identity
import Env
import Symbol
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

lookupValue :: Pos -> Symbol -> Trans EnvEntry
lookupValue p sym = do
  ve <- getVEnv
  case look sym ve of
    Nothing -> throwT p ("unknown identifier '" ++ show sym ++ "'")
    Just entry -> return entry

transVar :: Var -> Trans ExpTy
transVar v = case v of
  SimpleVar sym p -> do
    entry <- lookupValue p sym
    case entry of
      FunEntry _ _ -> throwT p ("'" ++ show sym ++ "' is a function, not a variable")
      VarEntry t -> returnTy $ actualTy t
  FieldVar v2 sym p -> do
    et <- transVar v2
    case typeOf et of
      Types.Record symTys _ ->
        case lookup sym symTys of
          Nothing -> throwT p ("'" ++ show sym ++ "' is not a field of the record")
          Just t2 -> returnTy t2
      _ -> throwT p ("expected record")
  SubscriptVar v2 e p -> do
    et <- transVar v2
    case typeOf et of
      Types.Array t _ -> do
        et2 <- transExp e
        case typeOf et2 of
          Types.Int -> returnTy t
          _ -> throwT p "two integers required"
      _ -> throwT p ("expected array")

actualTy :: Types.Ty -> Types.Ty
actualTy t = case t of
  Types.Name _ (Just t2) -> actualTy t2
  t2 -> t2

transExp :: Exp -> Trans ExpTy
transExp e = case e of
  VarExp v -> transVar v
  NilExp -> returnTy Types.Nil
  IntExp _ -> returnTy Types.Int
  StringExp _ _ -> returnTy Types.String
  OpExp l o r p -> transOp l o r p
  BreakExp _ -> returnTy Types.Unit  -- TODO Check for loop

transOp :: Exp -> Oper -> Exp -> Pos -> Trans ExpTy
transOp l o r p = do
  etl <- transExp l
  etr <- transExp r
  checkForOper o etl etr p
  returnTy Types.Int

checkForOper :: Oper -> ExpTy -> ExpTy -> Pos -> Trans ()
checkForOper o = case o of
  PlusOp -> checkArithExp
  MinusOp -> checkArithExp
  TimesOp -> checkArithExp
  DivideOp -> checkArithExp
  EqOp -> checkEqOp
  NeqOp -> checkEqOp
  LtOp -> checkCompExp
  LeOp -> checkCompExp
  GtOp -> checkCompExp
  GeOp -> checkCompExp

-- TODO Introduce sameType/Eq instance for Type?
checkArithExp :: ExpTy -> ExpTy -> Pos -> Trans ()
checkArithExp etl etr p =
  case (typeOf etl, typeOf etr) of
    (Types.Int, Types.Int) -> return ()
    _ -> throwT p "two integers required"

checkEqOp :: ExpTy -> ExpTy -> Pos -> Trans ()
checkEqOp etl etr p =
  case (typeOf etl, typeOf etr) of
    (Types.Int, Types.Int) -> return ()
    (Types.String, Types.String) -> return ()
    (Types.Record _ u1, Types.Record _ u2) ->
      unless (u1 == u2) $
        throwT p "records must be of the same type"  -- TODO nil?
    (Types.Array _ u1, Types.Array _ u2) ->
      unless (u1 == u2) $
        throwT p "arrays must be of the same type"
    _ -> throwT p "two integers/strings/records/arrays required"

checkCompExp :: ExpTy -> ExpTy -> Pos -> Trans ()
checkCompExp etl etr p =
  case (typeOf etl, typeOf etr) of
    (Types.Int, Types.Int) -> return ()
    (Types.String, Types.String) -> return ()
    _ -> throwT p "two integers/strings required"

transDec :: Dec -> Trans ()
transDec = undefined

transTy :: Ty -> Trans Types.Ty
transTy = undefined

semant :: Exp -> Either String ExpTy
semant = evalT . transExp
