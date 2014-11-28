{
module Parser ( module AbSyn, parser ) where

import AbSyn
import Prelude hiding ( Ordering(..) )
import Lexer
}

%name parseProgram program
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ EOF }
%error { parseError }

%token
  while       { Token _ WHILE }
  for         { Token _ FOR }
  to          { Token _ TO }
  break       { Token _ BREAK }
  let         { Token _ LET }
  in          { Token _ IN }
  end         { Token _ END }
  function    { Token _ FUNCTION }
  var         { Token _ VAR }
  type        { Token _ TYPE }
  array       { Token _ ARRAY }
  if          { Token _ IF }
  then        { Token _ THEN }
  else        { Token _ ELSE }
  do          { Token _ DO }
  of          { Token _ OF }
  nil         { Token _ NIL }
  ','         { Token _ COMMA }
  ':'         { Token _ COLON }
  ';'         { Token _ SEMICOLON }
  '('         { Token _ LPAREN }
  ')'         { Token _ RPAREN }
  '['         { Token _ LBRACK }
  ']'         { Token _ RBRACK }
  '{'         { Token _ LBRACE }
  '}'         { Token _ RBRACE }
  '.'         { Token _ DOT }
  '+'         { Token _ PLUS }
  '-'         { Token _ MINUS }
  '*'         { Token _ TIMES }
  '/'         { Token _ DIVIDE }
  '='         { Token _ EQ }
  '<>'        { Token _ NEQ }
  '<'         { Token _ LT }
  '<='        { Token _ LE }
  '>'         { Token _ GT }
  '>='        { Token _ GE }
  '&'         { Token _ AND }
  '|'         { Token _ OR }
  ':='        { Token _ ASSIGN }
  INT         { Token _ (INT _) }
  ID          { Token _ (ID _) }
  STRING      { Token _ (STRING _) }

%%

program :: { Exp }
program : exp                                   { $1 }

decs :: { [Dec] }
decs : decs_rev                                 { groupDecs (reverse $1) }

decs_rev :: { [Dec] }
decs_rev :                                      { [] }
         | decs_rev dec                         { $2 : $1 }

dec :: { Dec }
dec : tydec                                     { $1 }
    | vardec                                    { $1 }
    | fundec                                    { $1 }

tydec :: { Dec }
tydec : type ID '=' ty                          { TypeDec [TyDec (tokenId $2) $4 (tokenPos $1)] }

ty :: { Ty }
ty : ID                                         { NameTy (tokenId $1) (tokenPos $1) }
   | '{' tyfields '}'                           { RecordTy $2 }
   | array of ID                                { ArrayTy (tokenId $3) (tokenPos $1) }

tyfields :: { [Field] }
tyfields :                                      { [] }
         | tyfields_rev                         { reverse $1 }

tyfields_rev :: { [Field] }
tyfields_rev : typefield                        { [$1] }
             | tyfields_rev ',' typefield       { $3 : $1 }

typefield :: { Field }
typefield : ID ':' type_id                      { Field (tokenId $1) (tokenId $3) (tokenPos $1) }

type_id :: { Token }
type_id : ID                                    { $1 }

vardec :: { Dec }
vardec : var ID opt_type ':=' exp               { VarDec (tokenId $2) $3 $5 (tokenPos $1) }

opt_type :: { Maybe (Symbol, Pos) }
opt_type :                                      { Nothing }
         | ':' type_id                          { Just (tokenId $2, tokenPos $1) }

fundec :: { Dec }
fundec : function ID '(' tyfields ')' opt_type '=' exp
                                                { FunctionDec [FunDec (tokenId $2) $4 $6 $8 (tokenPos $1)] }

exp :: { Exp }
exp : matched                                   { $1 }
    | unmatched                                 { $1 }

matched :: { Exp }
matched : disj                                  { $1 }
        | lvalue ':=' matched                   { AssignExp $1 $3 (tokenPos $2) }
        | ID '[' matched ']' of matched         { ArrayExp (tokenId $1) $3 $6 (tokenPos $1) }
        | if exp then matched else matched      { IfExp $2 $4 (Just $6) (tokenPos $1) }
        | while exp do matched                  { WhileExp $2 $4 (tokenPos $1) }
        | for ID ':=' exp to exp do matched     { ForExp (tokenId $2) $4 $6 $8 (tokenPos $1) }

unmatched :: { Exp }
unmatched : lvalue ':=' unmatched               { AssignExp $1 $3 (tokenPos $2) }
          | ID '[' matched ']' of unmatched     { ArrayExp (tokenId $1) $3 $6 (tokenPos $1) }
          | if exp then matched else unmatched  { IfExp $2 $4 (Just $6) (tokenPos $1) }
          | if exp then matched                 { IfExp $2 $4 Nothing (tokenPos $1) }
          | while exp do unmatched              { WhileExp $2 $4 (tokenPos $1) }
          | for ID ':=' exp to exp do unmatched { ForExp (tokenId $2) $4 $6 $8 (tokenPos $1) }

lvalue :: { Var }
lvalue : ID lvalue1                             { $2 (SimpleVar (tokenId $1) (tokenPos $1)) }

lvalue1 :: { Var -> Var }
lvalue1 :                                       { \v -> v }
        | '[' matched ']' lvalue1               { \v -> $4 (SubscriptVar v $2 (tokenPos $1)) }
        | '.' ID lvalue1                        { \v -> $3 (FieldVar v (tokenId $2) (tokenPos $1)) }

disj :: { Exp }
disj : disj '|' conj                            { IfExp $1 (IntExp 1) (Just $3) (tokenPos $2) }
     | conj                                     { $1 }

conj :: { Exp }
conj : conj '&' comp                            { IfExp $1 $3 (Just (IntExp 0)) (tokenPos $2) }
     | comp                                     { $1 }

comp :: { Exp }
comp : arith_exp rel arith_exp                  { OpExp $1 (snd $2) $3 (fst $2) }
     | arith_exp                                { $1 }

rel :: { (Pos, Oper) }
rel : '='                                       { (tokenPos $1, EqOp) }
    | '<>'                                      { (tokenPos $1, NeqOp) }
    | '<'                                       { (tokenPos $1, LtOp) }
    | '<='                                      { (tokenPos $1, LeOp) }
    | '>'                                       { (tokenPos $1, GtOp) }
    | '>='                                      { (tokenPos $1, GeOp) }

arith_exp :: { Exp }
arith_exp : arith_exp add_op term               { OpExp $1 (snd $2) $3 (fst $2) }
          | term                                { $1 }

add_op :: { (Pos, Oper) }
add_op : '+'                                    { (tokenPos $1, PlusOp) }
       | '-'                                    { (tokenPos $1, MinusOp) }

term :: { Exp }
term : term mul_op pref_exp                     { OpExp $1 (snd $2) $3 (fst $2) }
     | pref_exp                                 { $1 }

mul_op :: { (Pos, Oper) }
mul_op : '*'                                    { (tokenPos $1, TimesOp) }
       | '/'                                    { (tokenPos $1, DivideOp) }

pref_exp :: { Exp }
pref_exp : '-' pref_exp                         { OpExp (IntExp 0) MinusOp $2 (tokenPos $1) }
         | factor                               { $1 }

factor :: { Exp }
factor : lvalue                                 { VarExp $1 }
       | nil                                    { NilExp }
       | '(' expseq ')'                         { $2 }
       | INT                                    { IntExp (tokenInt $1) }
       | STRING                                 { StringExp (tokenString $1) (tokenPos $1) }
       | ID '(' call_args ')'                   { CallExp (tokenId $1) $3 (tokenPos $1) }
       | type_id '{' fields '}'                 { RecordExp $3 (tokenId $1) (tokenPos $1) }
       | break                                  { BreakExp (tokenPos $1) }
       | let decs in expseq end                 { LetExp $2 $4 (tokenPos $1) }

call_args :: { [Exp] }
call_args :                                     { [] }
          | call_args_rev                       { reverse $1 }

call_args_rev :: { [Exp] }
call_args_rev : exp                             { [$1] }
              | call_args_rev ',' exp           { $3 : $1 }

fields :: { [(Symbol, Exp, Pos)] }
fields :                                        { [] }
       | fields_rev                             { reverse $1 }

fields_rev :: { [(Symbol, Exp, Pos)] }
fields_rev : field                              { [$1] }
           | fields_rev ',' field               { $3 : $1 }

field :: { (Symbol, Exp, Pos) }
field : ID '=' exp                              { (tokenId $1, $3, tokenPos $2) }

expseq :: { Exp }
expseq :                                        { expseqToExp [] }
       | expseq_rev                             { expseqToExp (reverse $1) }

expseq_rev :: { [Exp] }
expseq_rev : exp                                { [$1] }
           | expseq_rev ';' exp                 { $3 : $1 }

{
-- tree helpers ----------------------------------------------------------------

-- Doing the grouping directly in the grammar would be possible, but much more
-- complicated.
groupDecs :: [Dec] -> [Dec]
groupDecs [] = []
groupDecs (x@(FunctionDec xs) : (y@(FunctionDec ys) : zs)) =
  groupDecs (FunctionDec (xs ++ ys) : zs)
groupDecs (x@(TypeDec xs) : (y@(TypeDec ys) : zs)) =
  groupDecs (TypeDec (xs ++ ys) : zs)
groupDecs (x:xs) = x : groupDecs xs

tokenInt :: Token -> Integer
tokenInt = tokenClassInt . tokenClass

tokenId :: Token -> Symbol
tokenId = tokenClassId . tokenClass

tokenString :: Token -> String
tokenString = tokenClassString . tokenClass

expseqToExp :: [Exp] -> Exp
expseqToExp [e] = e
expseqToExp es  = SeqExp [ (e, Pos "hurz" 0 0) | e <- es ]

-- alex/happy interface --------------------------------------------------------

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parser :: FilePath -> String -> Either String Exp
parser = runAlex' parseProgram
}
