module Chapter01 where

import Control.Monad ( liftM2 )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( StateT, evalStateT, modify, gets )
import Data.List ( intersperse )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

type Id = String

data BinOp = Plus | Minus | Times | Div deriving ( Show )

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving ( Show )

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp
         deriving ( Show )

-- a := 5 + 3 ; b := ( print ( a , a - 1 ) , 10 * a ) ; print ( b )
prog :: Stm
prog =
  CompoundStm
    (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
    (CompoundStm
      (AssignStm "b"
                 (EseqExp (PrintStm [IdExp "a",
                                     OpExp (IdExp "a") Minus (NumExp 1)])
                          (OpExp (NumExp 10) Times (IdExp "a"))))
      (PrintStm [IdExp "b"]))

--------------------------------------------------------------------------------

maxArgs :: Stm -> Int
maxArgs (CompoundStm s1 s2) = maxArgs s1 `max` maxArgs s2
maxArgs (AssignStm _ e) = maxArgsExp e
maxArgs (PrintStm es) = length es

maxArgsExp :: Exp -> Int
maxArgsExp (IdExp _) = 0
maxArgsExp (NumExp _) = 0
maxArgsExp (OpExp e1 _ e2) = maxArgsExp e1 `max` maxArgsExp e2
maxArgsExp (EseqExp s e) = maxArgs s `max` maxArgsExp e

--------------------------------------------------------------------------------
-- Note: Changed to a more convenient argument order for some functions below.

type Table = [(Id, Int)]

emptyTable :: Table
emptyTable = []

update :: Id -> Int -> Table -> Table
update i v = ((i, v) :)

lookUp :: Id -> Table -> Int
lookUp i = fromMaybe (error ("unbound identifier " ++ i)) . lookup i

--------------------------------------------------------------------------------

type Eval = StateT Table IO

evalE :: Eval a -> IO a
evalE = flip evalStateT emptyTable

lookUpE :: Id -> Eval Int
lookUpE = gets . lookUp

updateE :: Id -> Int -> Eval ()
updateE i = modify . update i

putStrLnE :: String -> Eval ()
putStrLnE = lift . putStrLn

--------------------------------------------------------------------------------

interp :: Stm -> IO ()
interp = evalE . interpStm

interpStm :: Stm -> Eval ()
interpStm (CompoundStm s1 s2) = interpStm s1 >> interpStm s2
interpStm (AssignStm i e) = interpExp e >>= updateE i
interpStm (PrintStm es) =  mapM interpExp es >>= putStrLnE . format
   where format = concat . intersperse " " . map show

interpExp :: Exp -> Eval Int
interpExp (IdExp i) = lookUpE i
interpExp (NumExp v) = return v
interpExp (OpExp e1 op e2) = liftM2 (funForOp op) (interpExp e1) (interpExp e2)
interpExp (EseqExp s e) = interpStm s >> interpExp e

funForOp :: BinOp -> Int -> Int -> Int
funForOp Plus = (+)
funForOp Minus = (-)
funForOp Times = (*)
funForOp Div = div

--------------------------------------------------------------------------------
-- Exercise 1.1a

type Key = String

data Tree = Leaf | Node Tree Key Tree deriving ( Show )

empty :: Tree
empty = Leaf

insert :: Key -> Tree -> Tree
insert key Leaf = Node Leaf key Leaf
insert key (Node l k r)
  | key < k = Node (insert key l) k r
  | key > k = Node l k (insert key r)
  | otherwise = Node l key r

member :: Key -> Tree -> Bool
member _ Leaf = False
member key (Node l k r)
  | key < k = member key l
  | key > k = member key r
  | otherwise = True

--------------------------------------------------------------------------------
-- Exercise 1.1b

data Tree' a = Leaf' | Node' (Tree' a) Key a (Tree' a) deriving ( Show )

empty' :: Tree' a
empty' = Leaf'

insert' :: Key -> a -> Tree' a -> Tree' a
insert' key val Leaf' = Node' Leaf' key val Leaf'
insert' key val (Node' l k v r)
  | key < k = Node' (insert' key val l) k v r
  | key > k = Node' l k v (insert' key val r)
  | otherwise = Node' l key v r

lookup' :: Key -> Tree' a -> a
lookup' key Leaf' = error (key ++ " not found")
lookup' key (Node' l k v r)
  | key < k = lookup' key l
  | key > k = lookup' key r
  | otherwise = v

--------------------------------------------------------------------------------
-- Exercise 1.1c

-- (a): Basically a list hanging to the left:
--
--             t
--            /
--           s
--          /
--         p
--        /
--       i
--      /
--     f
--    /
--   b
--
-- (b): Basically a list hanging to the right:
--
--   a
--    \
--     b
--      \
--       c
--        \
--         d
--          \
--           e
--            \
--             f
--              \
--               g
--                \
--                 h
--                  \
--                   i

--------------------------------------------------------------------------------
-- Exercise 1.1d
--
-- One could use AVL trees or red-black trees.
