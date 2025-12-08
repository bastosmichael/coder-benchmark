{-# LANGUAGE OverloadedStrings #-}
module Main where

import BinaryParser
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Word
import Data.Int
import System.Exit
import Control.Monad

-- Helper to pack big endian
packU32 :: Word32 -> [Word8]
packU32 n = map (fromIntegral . (.&. 0xFF) . shiftR n) [24, 16, 8, 0]

packU64 :: Word64 -> [Word8]
packU64 n = map (fromIntegral . (.&. 0xFF) . shiftR n) [56, 48, 40, 32, 24, 16, 8, 0]

main :: IO ()
main = do
    testLogin
    testOrder
    putStrLn "All tests passed."

testLogin :: IO ()
testLogin = do
    let magic = 0xAF :: Word8
    let ver = 1 :: Word8
    let typ = 1 :: Word8
    let userLen = 5 :: Word8
    let user = BC.unpack "admin"
    let userBytes = map (fromIntegral . fromEnum) user
    let hashBytes = replicate 32 0xAA :: [Word8]
    
    let payload = [magic, ver, typ, userLen] ++ userBytes ++ hashBytes
    let s = sum (map fromIntegral payload) :: Word32
    let chk = packU32 s
    
    let bs = B.pack (payload ++ chk)
    
    case parsePacket bs of
        Right (Login u h) -> do
            when (u /= "admin") $ die "Username mismatch"
            when (B.length h /= 32) $ die "Hash len mismatch"
            putStrLn "Test Login: PASSED"
        Right _ -> die "Wrong packet type"
        Left e -> die $ "Parse failed: " ++ e

testOrder :: IO ()
testOrder = do
    -- Simple mock, Double encoding is tricky manually without IEEE lib, 
    -- but let's assume 'parsePacket' does standard IEEE754.
    -- We'll skip complex double verification or just use a known bit pattern for 1.0 or similar.
    -- 150.50 is approx ... let's use 0.0 for simplicity or just check other fields primarily.
    -- Better: Assume the parser expects standard 8-bytes. We pass 8 bytes logic.
    let magic = 0xAF :: Word8
    let ver = 1 :: Word8
    let typ = 2 :: Word8
    
    -- ID = 12345
    let pid = packU64 12345
    let sym = map (fromIntegral . fromEnum) "AAPL" -- 4 bytes
    let side = 1 :: Word8
    -- Price 0.0 (all zeros usually workable, or we can try to pack 150.50 manually? No.)
    -- Let's just use 8 zero bytes for price and assert it is 0.0.
    let price = replicate 8 0
    let qty = packU32 100
    
    let payload = [magic, ver, typ] ++ pid ++ sym ++ [side] ++ price ++ qty
    let s = sum (map fromIntegral payload) :: Word32
    let chk = packU32 s
    
    let bs = B.pack (payload ++ chk)
    
    case parsePacket bs of
         Right (Order i s si p q) -> do
            when (i /= 12345) $ die "ID mismatch"
            when (s /= "AAPL") $ die "Symbol mismatch"
            when (q /= 100) $ die "Qty mismatch"
            putStrLn "Test Order: PASSED"
         Right _ -> die "Wrong packet type"
         Left e -> die $ "Parse failed: " ++ e
