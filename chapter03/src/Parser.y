{
module Parser ( parser ) where

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

program :: { () }
program : exp                                   { () }

decs :: { () }
decs :                                          { () }
     | decs dec                                 { () }

dec :: { () }
dec : tydec                                     { () }
    | vardec                                    { () }
    | fundec                                    { () }

tydec :: { () }
tydec : type ID '=' ty                          { () }

ty :: { () }
ty : ID                                         { () }
   | '{' tyfields '}'                           { () }
   | array of ID                                { () }

tyfields :: { () }
tyfields :                                      { () }
         | tyfields_rev                         { () }

tyfields_rev :: { () }
tyfields_rev : typefield                        { () }
             | tyfields_rev ',' typefield       { () }

typefield :: { () }
typefield : ID ':' type_id                      { () }

type_id :: { () }
type_id : ID                                    { () }

vardec :: { () }
vardec : var ID opt_type ':=' exp               { () }

opt_type :: { () }
opt_type :                                      { () }
         | ':' type_id                          { () }

fundec :: { () }
fundec : function ID '(' tyfields ')' opt_type '=' exp
                                                { () }

exp :: { () }
exp : matched                                   { () }
    | unmatched                                 { () }

matched :: { () }
matched : disj                                  { () }
        | lvalue ':=' matched                   { () }
        | ID '[' matched ']' of matched         { () }
        | if exp then matched else matched      { () }
        | while exp do matched                  { () }
        | for ID ':=' exp to exp do matched     { () }

unmatched :: { () }
unmatched : lvalue ':=' unmatched               { () }
          | ID '[' matched ']' of unmatched     { () }
          | if exp then matched else unmatched  { () }
          | if exp then matched                 { () }
          | while exp do unmatched              { () }
          | for ID ':=' exp to exp do unmatched { () }

lvalue :: { () }
lvalue : ID lvalue1                             { () }

lvalue1 :: { () }
lvalue1 :                                       { () }
        | '[' matched ']' lvalue1               { () }
        | '.' ID lvalue1                        { () }

disj :: { () }
disj : disj '|' conj                            { () }
     | conj                                     { () }

conj :: { () }
conj : conj '&' comp                            { () }
     | comp                                     { () }

comp :: { () }
comp : arith_exp rel arith_exp                  { () }
     | arith_exp                                { () }

rel :: { () }
rel : '='                                       { () }
    | '<>'                                      { () }
    | '<'                                       { () }
    | '<='                                      { () }
    | '>'                                       { () }
    | '>='                                      { () }

arith_exp :: { () }
arith_exp : arith_exp add_op term               { () }
          | term                                { () }

add_op :: { () }
add_op : '+'                                    { () }
       | '-'                                    { () }

term :: { () }
term : term mul_op pref_exp                     { () }
     | pref_exp                                 { () }

mul_op :: { () }
mul_op : '*'                                    { () }
       | '/'                                    { () }

pref_exp :: { () }
pref_exp : '-' pref_exp                         { () }
         | factor                               { () }

factor :: { () }
factor : lvalue                                 { () }
       | nil                                    { () }
       | '(' expseq ')'                         { () }
       | INT                                    { () }
       | STRING                                 { () }
       | ID '(' call_args ')'                   { () }
       | type_id '{' fields '}'                 { () }
       | break                                  { () }
       | let decs in expseq end                 { () }

call_args :: { () }
call_args :                                     { () }
          | call_args_rev                       { () }

call_args_rev :: { () }
call_args_rev : exp                             { () }
              | call_args_rev ',' exp           { () }

fields :: { () }
fields :                                        { () }
       | fields_rev                             { () }

fields_rev :: { () }
fields_rev : field                              { () }
           | fields_rev ',' field               { () }

field :: { () }
field : ID '=' exp                              { () }

expseq :: { () }
expseq :                                        { () }
       | expseq_rev                             { () }

expseq_rev :: { () }
expseq_rev : exp                                { () }
           | expseq_rev ';' exp                 { () }

{
-- alex/happy interface --------------------------------------------------------

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parser :: FilePath -> String -> Either String ()
parser = runAlex' parseProgram
}
