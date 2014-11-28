module AbSyn ( module AbSyn, module Lexer, module Symbol ) where

import Lexer ( Pos, Line, Column )
import Symbol ( Symbol)

data Var =
    SimpleVar Symbol Pos
  | FieldVar Var Symbol Pos
  | SubscriptVar Var Exp Pos
  deriving ( Show )

data Exp =
    VarExp Var
  | NilExp
  | IntExp Integer
  | StringExp String Pos
  | CallExp { func :: Symbol, args :: [Exp], pos :: Pos }
  | OpExp { left :: Exp, oper :: Oper, right :: Exp, pos :: Pos }
  | RecordExp { fields :: [(Symbol, Exp, Pos)], typ :: Symbol, pos :: Pos }
  | SeqExp [(Exp, Pos)]
  | AssignExp { var :: Var, exp :: Exp, pos :: Pos }
  | IfExp { test :: Exp, then' :: Exp, else' :: Maybe Exp, pos :: Pos }
  | WhileExp { test :: Exp, body :: Exp, pos :: Pos }
  | ForExp { var' :: Symbol, {- escape :: bool ref, -} lo :: Exp, hi :: Exp, body :: Exp, pos :: Pos }
  | BreakExp Pos
  | LetExp { decs :: [Dec], body :: Exp, pos :: Pos }
  | ArrayExp { typ :: Symbol, size :: Exp, init :: Exp, pos :: Pos }
  deriving ( Show )

data Dec =
    FunctionDec [FunDec]
  | VarDec { name' :: Symbol, {- escape: bool ref, -} typ' :: Maybe (Symbol, Pos), init' :: Exp, pos' :: Pos }
  | TypeDec [TyDec]
  deriving ( Show )

data Ty =
    NameTy Symbol Pos
  | RecordTy [Field]
  | ArrayTy Symbol Pos
  deriving ( Show )

data Oper =
    PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  deriving ( Show )

data Field = Field { name'' :: Symbol, {- escape: bool ref, -} typ'' :: Symbol, pos'' :: Pos }
  deriving ( Show )

data FunDec = FunDec { name''' :: Symbol, params :: [Field], result :: Maybe (Symbol, Pos), body' :: Exp, pos''' :: Pos }
  deriving ( Show )

data TyDec  = TyDec { name'''' :: Symbol, ty :: Ty, pos'''' :: Pos }
  deriving ( Show )
