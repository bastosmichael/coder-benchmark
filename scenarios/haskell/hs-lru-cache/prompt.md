
You are a Haskell Expert.

Implement a pure functional `LRUCache` in `src/LRUCache.hs`.

Values should be stored in a way that allows efficient access and updates.
You may use `Data.Map` (from containers).

```haskell
module LRUCache (
    LRUCache,
    newCache,
    get,
    put
) where

import qualified Data.Map as M
import Data.Maybe (isJust)

data LRUCache k v = LRUCache {
    capacity :: Int,
    -- Add fields
} deriving (Show, Eq)

newCache :: Int -> LRUCache k v
newCache cap = undefined

-- Returns the value and the new state of the cache (LRU updated)
get :: Ord k => k -> LRUCache k v -> (Maybe v, LRUCache k v)
get key cache = undefined

-- Inserts key-value. 
-- * If key exists, updates value and moves to MRU.
-- * If key new and over capacity, evicts LRU.
put :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
put key val cache = undefined
```

Requirements:
1.  **Pure Functional**: No `IO` or `Ref`. State must be passed explicitly.
2.  **Efficiency**: `get` and `put` should be efficient. O(log N) is acceptable for Haskell `Map`.
3.  **Correctness**: Ensure strict LRU eviction policy.

Output full content of `src/LRUCache.hs`.
