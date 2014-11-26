module Symbol (
  Symbol, symbol, name,
  StringTable, emptyStringTable,
  Table, empty, look, enter
) where

import Data.Function ( on )
import Data.Hashable ( Hashable(..) )
import qualified Data.HashMap.Strict as M

--------------------------------------------------------------------------------

data Symbol = Symbol {
  name :: String,
  uniqueId :: Int
} deriving ( Show )

instance Eq Symbol where
  (==) = (==) `on` uniqueId

instance Ord Symbol where
  compare = compare `on` uniqueId

instance Hashable Symbol where
  hashWithSalt s = hashWithSalt s . uniqueId

--------------------------------------------------------------------------------

-- Computing the size of a HashMap takes O(n) time, so we cache it.
data StringTable = StringTable {
  hashMap :: M.HashMap String Symbol,
  size :: Int
} deriving ( Show )

emptyStringTable :: StringTable
emptyStringTable = StringTable M.empty 0

lookupStringTable :: String -> StringTable -> Maybe Symbol
lookupStringTable n = M.lookup n . hashMap

insertStringTable :: String -> StringTable -> (Symbol, StringTable)
insertStringTable n t = (s, StringTable (M.insert n s (hashMap t)) (size t + 1))
   where s = Symbol n (size t)

--------------------------------------------------------------------------------

-- Note: Different signature, because we want to be pure.
symbol :: String -> StringTable -> (Symbol, StringTable)
symbol n t =
  maybe (insertStringTable n t) (\s -> (s, t)) (lookupStringTable n t)

--------------------------------------------------------------------------------

newtype Table a = Table { unTable :: M.HashMap Symbol a }
  deriving ( Show )

empty :: Table a
empty = Table M.empty

look :: Symbol -> Table a -> Maybe a
look s =  M.lookup s . unTable

enter :: Symbol -> a -> Table a -> Table a
enter s v = Table . M.insert s v . unTable
