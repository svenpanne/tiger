{
module Parser ( Tree(..), parser, main ) where

import Prelude hiding ( Ordering(..) )
import DOT
import Lexer hiding ( main )
import System.Environment ( getArgs )
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
  INT         { Token _ (INT $$) }
  ID          { Token _ (ID $$) }
  STRING      { Token _ (STRING $$) }

%%

program : exp                                   { $1 }

decs : decs_rev                                 { reverse $1 }

decs_rev :                                      { [] }
         | decs_rev dec                         { $2 : $1 }

dec : tydec                                     { $1 }
    | vardec                                    { $1 }
    | fundec                                    { $1 }

tydec : type ID '=' ty                          { Tree "TypeDec" [Tree $2 [], $4] }

ty : ID                                         { Tree "NameTy" [Tree $1 []] }
   | '{' tyfields '}'                           { Tree "RecordTy" $2 }
   | array of ID                                { Tree "ArrayTy" [Tree $3 []] }

tyfields :                                      { [] }
         | tyfields_rev                         { reverse $1 }

tyfields_rev : typefield                        { [$1] }
             | tyfields_rev ',' typefield       { $3 : $1 }

typefield : ID ':' type_id                      { Tree "field" [Tree $1 [], Tree $3 []] }

type_id : ID                                    { $1 }

vardec : var ID opt_type ':=' exp               { Tree "VarDec" ([Tree $2 []] ++ $3 ++ [$5]) }

opt_type :                                      { [] }
         | ':' type_id                          { [Tree $2 []] }

fundec : function ID '(' tyfields ')' opt_type '=' exp { Tree "FunctionDec" ([Tree $2 []] ++ $4 ++ $6 ++ [$8]) }

exp : matched                                   { $1 }
    | unmatched                                 { $1 }

matched : disj                                  { $1 }
        | ID '[' matched ']' of matched         { Tree "ArrayExp" [Tree $1 [], $3, $6] }
        | lvalue ':=' matched                   { Tree "AssignExp" [$1, $3] }
        | if exp then matched else matched      { Tree "IfExp" [$2, $4, $6] }
        | while exp do matched                  { Tree "WhileExp" [$2, $4] }
        | for ID ':=' exp to exp do matched     { Tree "ForExp" [Tree $2 [], $4, $6, $8] }

unmatched : lvalue ':=' unmatched               { Tree "AssignExp" [$1, $3] }
          | ID '[' matched ']' of unmatched     { Tree "ArrayExp" [Tree $1 [], $3, $6] }
          | if exp then matched else unmatched  { Tree "IfExp" [$2, $4, $6] }
          | if exp then matched                 { Tree "IfExp" [$2, $4, Tree "SeqExp" []] }
          | while exp do unmatched              { Tree "WhileExp" [$2, $4] }
          | for ID ':=' exp to exp do unmatched { Tree "ForExp" [Tree $2 [], $4, $6, $8] }

lvalue : ID lvalue1                             { $2 (Tree "SimpleVar" [Tree $1 []]) }

lvalue1 :                                       { \v -> v }
        | '[' matched ']' lvalue1               { \v -> $4 (Tree "SubscriptVar" [v, $2]) }
        | '.' ID lvalue1                        { \v -> $3 (Tree "FieldVar" [v, Tree $2 []]) }

disj : disj '|' conj                            { Tree "IfExp" [$1, Tree "IntExp" [Tree "1" []], $3] }
     | conj                                     { $1 }

conj : conj '&' comp                            { Tree "IfExp" [$1, $3, Tree "IntExp" [Tree "0" []]] }
     | comp                                     { $1 }

comp : arith_exp rel arith_exp                  { Tree "OpExp" [$1, $2, $3] }
     | arith_exp                                { $1 }

rel : '='                                       { Tree "EqOp" [] }
    | '<>'                                      { Tree "NeqOp" [] }
    | '<'                                       { Tree "LtOp" [] }
    | '<='                                      { Tree "LeOp" [] }
    | '>'                                       { Tree "GtOp" [] }
    | '>='                                      { Tree "GeOp" [] }

arith_exp : arith_exp add_op term               { Tree "OpExp" [$1, $2, $3] }
          | term                                { $1 }

add_op : '+'                                    { Tree "PlusOp" [] }
       | '-'                                    { Tree "MinusOp" [] }

term : term mul_op pref_exp                     { Tree "OpExp" [$1, $2, $3] }
     | pref_exp                                 { $1 }

mul_op : '*'                                    { Tree "TimesOp" [] }
       | '/'                                    { Tree "DividedOp" [] }

pref_exp : '-' pref_exp                         { Tree "OpExp" [Tree "IntExp" [Tree "0" []], Tree "MinusOp" [],$2] }
         | factor                               { $1 }

factor : lvalue                                 { $1 }
       | nil                                    { Tree "NilExp" [] }
       | '(' expseq ')'                         { $2 }
       | INT                                    { Tree "IntExp" [Tree (show $1) []] }
       | STRING                                 { Tree "StringExp" [Tree (makeLabel $1) []] }
       | ID '(' call_args ')'                   { Tree "CallExp" (Tree $1 [] : $3) }
       | type_id '{' fields '}'                 { Tree "RecordExp" (Tree $1 [] : $3) }
       | break                                  { Tree "BreakExp" [] }
       | let decs in expseq end                 { Tree "LetExp" (reverse $2 ++ [$4]) }

call_args :                                     { [] }
          | call_args_rev                       { reverse $1 }

call_args_rev : exp                             { [$1] }
              | call_args_rev ',' exp           { $3 : $1 }

fields :                                        { [] }
       | fields_rev                             { reverse $1 }

fields_rev : field                              { [$1] }
           | fields_rev ',' field               { $3 : $1 }

field : ID '=' exp                              { Tree "Field" [Tree $1 [], $3] }

expseq :                                        { expseqToTree [] }
       | expseq_rev                             { expseqToTree (reverse $1) }

expseq_rev : exp                                { [$1] }
           | expseq_rev ';' exp                 { $3 : $1 }

{
-- tree helpers ----------------------------------------------------------------

expseqToTree :: [Tree String] -> Tree String
expseqToTree [e] = e
expseqToTree es  = Tree "SeqExp" es

makeLabel :: String -> String
makeLabel = concatMap (\c -> if c == '"' then "\\\"" else [c]) . show

-- alex/happy interface --------------------------------------------------------

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parser :: FilePath -> String -> Either String (Tree String)
parser = runAlex' parseProgram

-- for testing -----------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parser "<stdin>") getContents
              [f] -> fmap (parser f) (readFile f)
              _   -> error "expected max. 1 argument"
  either putStrLn (putStrLn . toDOT) result
}
