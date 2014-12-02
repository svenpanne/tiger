module AbSyn ( module AbSyn, module Lexer, module Symbol ) where

import Lexer ( Pos, Line, Column )
import Symbol ( Symbol )

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
  | CallExp Symbol [Exp] Pos
  | OpExp Exp Oper Exp Pos
  | RecordExp [(Symbol, Exp, Pos)] Symbol Pos
  | SeqExp [(Exp, Pos)]
  | AssignExp Var Exp Pos
  | IfExp Exp Exp (Maybe Exp) Pos
  | WhileExp Exp Exp Pos
  | ForExp Symbol {- escape :: bool ref, -} Exp Exp Exp Pos
  | BreakExp Pos
  | LetExp [Dec] Exp Pos
  | ArrayExp Symbol Exp Exp Pos
  deriving ( Show )

data Dec =
    FunctionDec [FunDec]
  | VarDec Symbol {- escape: bool ref, -} (Maybe (Symbol, Pos)) Exp Pos
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

data Field = Field Symbol {- escape: bool ref, -} Symbol Pos
  deriving ( Show )

data FunDec = FunDec Symbol [Field] (Maybe (Symbol, Pos)) Exp Pos
  deriving ( Show )

data TyDec  = TyDec Symbol Ty Pos
  deriving ( Show )
