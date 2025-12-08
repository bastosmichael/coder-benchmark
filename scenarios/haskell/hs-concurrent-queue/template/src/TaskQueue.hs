module TaskQueue (TaskQueue, newQueue, addTask) where

import Control.Concurrent
import Control.Concurrent.MVar

data TaskQueue = TaskQueue {}

newQueue :: Int -> Maybe (Int, Int) -> Int -> IO TaskQueue
newQueue _ _ _ = return TaskQueue {}

addTask :: TaskQueue -> IO a -> IO (IO a)
addTask _ _ = return (fail "Not implemented")
