
module Main where

import LRUCache
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = do
    putStrLn $ "Assertion failed: " ++ msg
    exitFailure

main :: IO ()
main = do
    let c0 = newCache 2
    
    -- Put 1, 2
    let c1 = put 1 1 c0
    let c2 = put 2 2 c1
    
    -- Get 1 -> (Just 1, c3)
    let (v1, c3) = get 1 c2
    assert (v1 == Just 1) "Get 1 should be Just 1"
    
    -- Put 3 -> Evicts 2 (since 1 was just accessed)
    let c4 = put 3 3 c3
    
    -- Get 2 -> Nothing
    let (v2, _) = get 2 c4
    assert (v2 == Nothing) "Get 2 should be Nothing (evicted)"
    
    -- Get 3 -> Just 3
    let (v3, _) = get 3 c4
    assert (v3 == Just 3) "Get 3 should be Just 3"
    
    -- Get 1 -> Just 1 (still there)
    let (v1b, _) = get 1 c4
    assert (v1b == Just 1) "Get 1 should be Just 1 (preserved)"
    
    putStrLn "All tests passed"
    exitSuccess
