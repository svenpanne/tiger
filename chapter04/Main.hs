module Main ( main ) where

import DOT ( Tree(..), toDOT )
import Parser hiding ( args, result )
import Symbol ( name )
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )

symbolToTree :: Symbol -> Tree String
symbolToTree s = Tree (name s) []

varToTree :: Var -> Tree String
varToTree v = case v of
  SimpleVar s _ -> symbolToTree s
  FieldVar w s _ -> Tree "." [varToTree w, symbolToTree s]
  SubscriptVar w e _ -> Tree "[]" [varToTree w, expToTree e]

expToTree :: Exp -> Tree String
expToTree e = case e of
  VarExp v -> varToTree v
  NilExp -> Tree "nil" []
  IntExp i -> Tree (show i) []
  StringExp s _ -> Tree (stringToLabel s) []
  CallExp f as _ -> Tree "$Call" ([symbolToTree f] ++ map expToTree as)
  OpExp l o r _ -> Tree (operToLabel o) [expToTree l, expToTree r]
  RecordExp fs t _ -> Tree "$Record" ([symbolToTree t] ++ map recordFieldToTree fs)
  SeqExp es -> Tree "$Seq" (map (expToTree . fst) es)
  AssignExp v x _ -> Tree ":=" [varToTree v, expToTree x]
  IfExp c t me _ ->
    Tree "$If" ([expToTree c, expToTree t] ++ maybeToTrees expToTree me)
  WhileExp c b _ -> Tree "$While" [expToTree c, expToTree b]
  ForExp v l h b _ ->
    Tree "$For" [symbolToTree v, expToTree l, expToTree h, expToTree b]
  BreakExp _ -> Tree "$Break" []
  LetExp ds b _ -> Tree "$Let" (map decToTree ds ++ [expToTree b])
  ArrayExp t s i _ -> Tree "$Array" [symbolToTree t, expToTree s, expToTree i]

stringToLabel :: String -> String
stringToLabel = concatMap (\c -> if c == '"' then "\\\"" else [c]) . show

decToTree :: Dec -> Tree String
decToTree d = case d of
  FunctionDec fs -> Tree "$Function" (map funDecToTree fs)
  VarDec n mt i _ -> Tree "$Var" ([symbolToTree n] ++
                                  maybeToTrees (symbolToTree . fst) mt ++
                                  [expToTree i])
  TypeDec ts -> Tree "$Type" (map tyDecTotree ts)

tyToTree :: Ty -> Tree String
tyToTree t = case t of
  NameTy s _ -> Tree "$NameTy" [symbolToTree s]
  RecordTy fs -> Tree "$RecordTy" (map fieldToTree fs)
  ArrayTy s _ -> Tree "$ArrayTy" [symbolToTree s]

operToLabel :: Oper -> String
operToLabel o = case o of
  PlusOp -> "+"
  MinusOp -> "-"
  TimesOp -> "*"
  DivideOp -> "/"
  EqOp -> "="
  NeqOp -> "<>"
  LtOp -> "<"
  LeOp -> "<="
  GtOp -> ">"
  GeOp -> ">="

recordFieldToTree :: (Symbol, Exp, Pos) -> Tree String
recordFieldToTree (s, e, _) = Tree "$AssignField" [symbolToTree s, expToTree e]

fieldToTree :: Field -> Tree String
fieldToTree (Field n t _) = Tree "$Field" [symbolToTree n, symbolToTree t]

funDecToTree :: FunDec -> Tree String
funDecToTree (FunDec n ps mr b _) =
  Tree "$FunDec" ([symbolToTree n] ++
                  map fieldToTree ps ++
                  maybeToTrees (symbolToTree . fst) mr ++
                  [expToTree b])

tyDecTotree :: TyDec -> Tree String
tyDecTotree (TyDec n t _) = Tree "$TyDec" [symbolToTree n, tyToTree t]

maybeToTrees :: (a -> Tree String) -> Maybe a -> [Tree String]
maybeToTrees f = maybe [] (\x -> [f x])

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parser "<stdin>") getContents
              [f] -> fmap (parser f) (readFile f)
              _   -> error "expected max. 1 argument"
  either (hPutStrLn stderr) (putStrLn . toDOT . expToTree) result
