module DOT ( Tree(..), toDOT ) where

import Control.Monad.Trans.State ( evalState, get, put )

data Tree a = Tree a [Tree a] deriving Show

data Node = Node Id Label
newtype Id = Id String
newtype Label = Label String
data Edge = Edge Id Id

renderAsDOT :: Tree Node -> String
renderAsDOT t = showString "graph g {\n" .
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
