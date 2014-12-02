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

-- The state of our monad is a pair of value/type environments plus a unique supply.
data TransState = TransState {
  vEnv :: VEnv,
  tEnv :: TEnv,
  uniqueSupply :: [Types.Unique]
} deriving ( Show )

initialTransState :: TransState
initialTransState = TransState baseVEnv baseTEnv Types.uniqueSupply

--------------------------------------------------------------------------------

-- We need modifiable value/type environments and can fail.
type Trans = StateT TransState (ErrorT String Identity)

evalT :: Trans a -> Either String a
evalT = runIdentity . runErrorT . flip evalStateT initialTransState

throwT :: Pos -> String -> Trans a
throwT pos = lift . throwError . shows pos . showString ": "

lookupT :: Pos -> (TransState -> Table a) -> Symbol -> Trans a
lookupT pos f sym = do
  env <- gets f
  case look sym env of
    Nothing -> throwT pos ("unknown identifier '" ++ show sym ++ "'")
    Just x -> return x

nextUnique :: Trans Types.Unique
nextUnique = do
  (unique:rest) <- gets uniqueSupply
  modify $ \s -> s{uniqueSupply = rest}
  return unique

--------------------------------------------------------------------------------

data ExpTy = ExpTy {
  translatedExp :: Translate.Exp,
  typeOf :: Types.Ty
} deriving ( Show )

returnTy :: Types.Ty -> Trans ExpTy
returnTy = return . ExpTy Translate.Exp

--------------------------------------------------------------------------------

-- TODO Use this more often.
actualTy :: Types.Ty -> Types.Ty
actualTy ty = case ty of
  Types.Name _ (Just realTy) -> actualTy realTy
  _ -> ty

getVarTy :: Pos -> EnvEntry -> Trans Types.Ty
getVarTy pos entry = case entry of
  FunEntry _ _ -> throwT pos "variable required"
  VarEntry ty -> return $ actualTy ty

getFunTy :: Pos -> EnvEntry -> Trans ([Types.Ty], Types.Ty)
getFunTy pos entry = case entry of
  FunEntry formals result -> return (formals, result)
  VarEntry _ -> throwT pos "function required"

getArrayBaseTy :: Pos -> Types.Ty -> Trans Types.Ty
getArrayBaseTy pos t = case t of
  Types.Array baseTy _ -> return baseTy
  _ -> throwT pos "array type required"

getRecordFields :: Pos -> Types.Ty -> Trans [(Symbol, Types.Ty)]
getRecordFields pos t = case t of
  Types.Record symTys _ -> return symTys
  _ -> throwT pos "record type required"

-- Used for lvalues and rvalues, at least for now.
transVar :: Var -> Trans ExpTy
transVar var = case var of
  SimpleVar varId pos -> do
    entry <- lookupT pos vEnv varId
    ty <- getVarTy pos entry
    returnTy ty
  FieldVar v field pos -> do
    etVar <- transVar v
    symTys <- getRecordFields pos (typeOf etVar)
    case lookup field symTys of
      Nothing -> throwT pos ("'" ++ show field ++ "' is not a field of the record type")
      Just t -> returnTy t
  SubscriptVar v idx pos -> do
    etV <- transVar v
    etIdx <- transExp idx
    baseTy <- getArrayBaseTy pos (typeOf etV)
    checkArithmeticTy pos (typeOf etIdx)
    returnTy baseTy

checkCompatibleTys :: Pos -> Types.Ty -> Types.Ty -> Trans ()
checkCompatibleTys pos ty1 ty2 =
  unless (ty1 `Types.isCompatibleWith` ty2) $
    throwT pos "incompatible types"

transCall :: Pos -> Symbol -> [Exp] -> Trans ExpTy
transCall pos func args = do
  entry <- lookupT pos vEnv func
  (formals, result) <- getFunTy pos entry
  unless (length args == length formals) $
    throwT pos ("expected " ++ show (length formals) ++ " number of arguments")
  etArgs <- mapM transExp args
  zipWithM_ (checkCompatibleTys pos) formals (map typeOf etArgs)
  returnTy result

checkField :: (Symbol, Types.Ty) -> (Symbol, Exp, Pos) -> Trans ()
checkField (s1, ty) (s2, expr, pos) = do
  unless (s1 == s2) $
    throwT pos ("expected field " ++ name s1)
  etExpr <- transExp expr
  checkCompatibleTys pos ty (typeOf etExpr)

transRecordExp :: Pos -> [(Symbol, Exp, Pos)] -> Symbol -> Trans ExpTy
transRecordExp pos fields typeId = do
  ty <- lookupT pos tEnv typeId
  symTys <- getRecordFields pos ty
  unless (length symTys == length fields) $
    throwT pos ("expected " ++ show (length symTys) ++ " number of fields")
  zipWithM_ checkField symTys fields
  returnTy ty

transSeq :: [(Exp,Pos)] -> Trans ExpTy
transSeq exprs
  | null exprs = returnTy Types.Unit
  | otherwise = fmap last $ mapM (transExp . fst) exprs

transAssign :: Pos -> Var -> Exp -> Trans ExpTy
transAssign pos var expr = do
  etVar <- transVar var
  etExpr <- transExp expr
  checkCompatibleTys pos (typeOf etVar) (typeOf etExpr)
  returnTy Types.Unit

transIf :: Pos -> Exp -> Exp -> Maybe Exp -> Trans ExpTy
transIf pos test then_ else_ = do
  etTest <- transExp test
  etThen <- transExp then_
  etElse <- maybe (returnTy Types.Unit) transExp else_
  checkArithmeticTy pos (typeOf etTest)
  checkCompatibleTys pos (typeOf etThen) (typeOf etElse)
  returnTy $ typeOf etThen

checkUnitTy :: Pos -> Types.Ty -> Trans ()
checkUnitTy pos ty = case ty of
  Types.Unit -> return ()
  _ -> throwT pos "no return value expected"

transWhile :: Pos -> Exp -> Exp -> Trans ExpTy
transWhile pos test body = do
  etTest <- transExp test
  etBody <- transExp body
  checkArithmeticTy pos (typeOf etTest)
  checkUnitTy pos (typeOf etBody)
  returnTy Types.Unit

transFor :: Pos -> Symbol -> Exp -> Exp -> Exp -> Trans ExpTy
transFor pos _var lo hi body = do
  etLo <- transExp lo
  etHi <- transExp hi
  etBody <- transExp body
  checkArithmeticTy pos (typeOf etLo)
  checkArithmeticTy pos (typeOf etHi)
  checkUnitTy pos (typeOf etBody)
  returnTy Types.Unit

-- TODO actually handle environments
transLet :: Pos -> [Dec] -> Exp -> Trans ExpTy
transLet _pos decs body = do
  mapM_ transDec decs
  transExp body

transArray :: Pos -> Symbol -> Exp -> Exp -> Trans ExpTy
transArray pos typeId size initVal = do
  ty <- lookupT pos tEnv typeId
  etSize <- transExp size
  etInitVal <- transExp initVal
  baseTy <- getArrayBaseTy pos ty
  checkArithmeticTy pos (typeOf etSize)
  checkCompatibleTys pos baseTy (typeOf etInitVal)
  returnTy ty

transExp :: Exp -> Trans ExpTy
transExp e = case e of
  VarExp var -> transVar var
  NilExp -> returnTy Types.Nil
  IntExp _ -> returnTy Types.Int
  StringExp _ _ -> returnTy Types.String
  CallExp func args pos -> transCall pos func args
  OpExp left oper right pos -> transOp pos left oper right
  RecordExp fields typeId pos -> transRecordExp pos fields typeId
  SeqExp exprs -> transSeq exprs
  AssignExp var expr pos -> transAssign pos var expr
  IfExp test then_ else_ pos -> transIf pos test then_ else_
  WhileExp test body pos -> transWhile pos test body
  ForExp var lo hi body pos -> transFor pos var lo hi body
  BreakExp _ -> returnTy Types.Unit  -- TODO Check for loop
  LetExp decs body pos -> transLet pos decs body
  ArrayExp typeId size initVal pos -> transArray pos typeId size initVal

transOp :: Pos -> Exp -> Oper -> Exp -> Trans ExpTy
transOp pos left oper right = do
  etLeft <- transExp left
  etRight <- transExp right
  checkForOper oper pos (typeOf etLeft)
  checkCompatibleTys pos (typeOf etLeft) (typeOf etRight)
  returnTy Types.Int

checkForOper :: Oper -> Pos -> Types.Ty -> Trans ()
checkForOper oper = case oper of
  PlusOp -> checkArithmeticTy
  MinusOp -> checkArithmeticTy
  TimesOp -> checkArithmeticTy
  DivideOp -> checkArithmeticTy
  EqOp -> checkEqualityTy
  NeqOp -> checkEqualityTy
  LtOp -> checkComparableTy
  LeOp -> checkComparableTy
  GtOp -> checkComparableTy
  GeOp -> checkComparableTy

checkArithmeticTy :: Pos -> Types.Ty -> Trans ()
checkArithmeticTy pos ty =
  unless (Types.isArithmeticTy ty) $
    throwT pos "arithmetic type (integer) required"

checkEqualityTy :: Pos -> Types.Ty -> Trans ()
checkEqualityTy pos ty =
  unless (Types.isEqualityTy ty) $
    throwT pos "equality type (record/integer/string/array) required"

checkComparableTy :: Pos -> Types.Ty -> Trans ()
checkComparableTy pos ty =
  unless (Types.isComparableTy ty) $
    throwT pos "comparable type (integer/string) required"

transFunDec :: FunDec -> Trans ()
transFunDec (FunDec funId params result body pos) = undefined

transVarDec :: Pos -> Symbol -> Maybe (Symbol, Pos) -> Exp -> Trans ()
transVarDec pos varId mbTy initVal = undefined

transTyDec :: TyDec -> Trans ()
transTyDec (TyDec typeId ty pos) = undefined

-- TODO recursive types
transDec :: Dec -> Trans ()
transDec dec = case dec of
  FunctionDec funDecs -> mapM_ transFunDec funDecs
  VarDec varId mbTy initVal pos -> transVarDec pos varId mbTy initVal
  TypeDec tyDecs -> mapM_ transTyDec tyDecs

transField :: Field -> Trans (Symbol, Types.Ty)
transField (Field fieldName typeId pos) = do
    fieldTy <- lookupT pos tEnv typeId
    return (fieldName, fieldTy)

transTy :: Ty -> Trans Types.Ty
transTy ty = case ty of
  NameTy typeId pos -> lookupT pos tEnv typeId
  RecordTy fields -> do
    fs <- mapM transField fields
    unique <- nextUnique
    return $ Types.Record fs unique
  ArrayTy typeId pos -> do
    baseTy <- lookupT pos tEnv typeId
    unique <- nextUnique
    return $ Types.Array baseTy unique

semant :: Exp -> Either String ExpTy
semant = evalT . transExp
