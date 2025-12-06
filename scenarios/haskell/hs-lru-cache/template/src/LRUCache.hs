
module LRUCache where

data LRUCache k v = LRUCache Int

newCache :: Int -> LRUCache k v
newCache c = LRUCache c

get :: Ord k => k -> LRUCache k v -> (Maybe v, LRUCache k v)
get _ c = (Nothing, c)

put :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
put _ _ c = c
