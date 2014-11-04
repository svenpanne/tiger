{
module Parser where

import Prelude hiding ( Ordering(..) )
import Control.Monad.Trans.State
import Lexer
}

%name parser exp
%tokentype { TokenClass }
%error { parseError }

%token
  while       { WHILE }
  for         { FOR }
  to          { TO }
  break       { BREAK }
  let         { LET }
  in          { IN }
  end         { END }
  function    { FUNCTION }
  var         { VAR }
  type        { TYPE }
  array       { ARRAY }
  if          { IF }
  then        { THEN }
  else        { ELSE }
  do          { DO }
  of          { OF }
  nil         { NIL }
  ','         { COMMA }
  ':'         { COLON }
  ';'         { SEMICOLON }
  '('         { LPAREN }
  ')'         { RPAREN }
  '['         { LBRACK }
  ']'         { RBRACK }
  '{'         { LBRACE }
  '}'         { RBRACE }
  '.'         { DOT }
  '+'         { PLUS }
  '-'         { MINUS }
  '*'         { TIMES }
  '/'         { DIVIDE }
  '='         { EQ }
  '<>'        { NEQ }
  '<'         { LT }
  '<='        { LE }
  '>'         { GT }
  '>='        { GE }
  '&'         { AND }
  '|'         { OR }
  ':='        { ASSIGN }
  INT         { INT $$ }
  ID          { ID $$ }
  STRING      { STRING $$ }

%%

decs : decs1                                    { reverse $1 }

decs1 :                                         { [] }
      | decs1 dec                               { $2 : $1 }

dec : tydec                                     { $1 }
    | vardec                                    { $1 }
    | fundec                                    { $1 }

tydec : type ID '=' ty                          { Tree "TypeDec" [Tree $2 [], $4] }

ty : ID                                         { Tree "NameTy" [Tree $1 []] }
   | '{' tyfields '}'                           { Tree "RecordTy" $2 }
   | array of ID                                { Tree "ArrayTy" [Tree $3 []] }

tyfields :                                      { [] }
         | tyfields1                            { reverse $1 }

tyfields1 : typefield                           { [$1] }
          | tyfields1 ',' typefield             { $3 : $1 }

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
          | call_args1                          { reverse $1 }

call_args1 : exp                                { [$1] }
           | call_args1 ',' exp                 { $3 : $1 }

fields :                                        { [] }
       | fields1                                { reverse $1 }

fields1 : field                                 { [$1] }
        | fields1 ',' field                     { $3 : $1 }

field : ID '=' exp                              { Tree "Field" [Tree $1 [], $3] }

expseq :                                        { expseqToTree [] }
       | expseq1                                { expseqToTree (reverse $1) }

expseq1 : exp                                   { [$1] }
        | expseq1 ';' exp                       { $3 : $1 }

{
parseError :: [TokenClass] -> a
parseError = error "parse error"

data Tree a = Tree a [Tree a] deriving Show

expseqToTree :: [Tree String] -> Tree String
expseqToTree [e] = e
expseqToTree es  = Tree "SeqExp" es

makeLabel :: String -> String
makeLabel = concatMap (\c -> if c == '"' then "\\\"" else [c]) . show

--------------------------------------------------------------------------------

data Node = Node Id Label
newtype Id = Id String
newtype Label = Label String
data Edge = Edge Id Id

renderAsDOT :: Tree Node -> String
renderAsDOT t = showString "graph g {" .
                showSepByLines showNode (nodes t) .
                showSepByLines showEdge (edges t) .
                showString "}" $ ""
  where nodes (Tree n ts) = n : concatMap nodes ts
        edges (Tree (Node i1 _) ts) =
          [ Edge i1 i2 | Tree (Node i2 _) _ <- ts ] ++ concatMap edges ts
        showNode (Node i l) =
          showId i . showString " [ label=\"" . showLabel l . showString "\" ]"
        showId (Id i) = showString i
        showLabel (Label l) = showString l
        showEdge (Edge f t) = showId f . showString " -- " . showId t
        showSepByLines f = foldr (\x s -> f x . showChar '\n' . s) id

addIds :: Tree String -> Tree Node
addIds = flip evalState 0 . go
  where go (Tree s ts) = do
          i <- get
          put (i + 1)
          ns <- mapM go ts
          return $ Tree (Node (Id ("n" ++ show i)) (Label s)) ns

toDOT :: Tree String -> String
toDOT = renderAsDOT . addIds

--------------------------------------------------------------------------------

main :: IO ()
main = getContents >>= putStrLn . toDOT . parser . map (\(Token _ tc) -> tc) . lexer
}
