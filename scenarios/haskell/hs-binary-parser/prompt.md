You are a Systems Engineer.

Implement a binary packet parser in Haskell module `BinaryParser`.

Protocol:
1. Header: Magic `0xAF` (1 byte), Version `1` (1 byte), Type (1 byte).
2. Payloads:
   - Login (Type 1): UserLen(1), Username(Ascii), Hash(32 bytes).
   - Order (Type 2): ID(8 bytes, I64 BigEndian), Symbol(4 bytes), Side(1), Price(8 bytes, Double), Qty(4 bytes, U32).
   - Heartbeat (Type 3): Timestamp(8 bytes, I64).
3. Checksum: Sum of all previous bytes modulo 2^32 (4 bytes at end).

Function signature:
```haskell
module BinaryParser (parsePacket, Packet(..)) where

import qualified Data.ByteString as B
import Data.Int
import Data.Word

data Packet = Login { username :: String, hash :: B.ByteString }
            | Order { id :: Int64, symbol :: String, side :: Word8, price :: Double, qty :: Word32 }
            | Heartbeat { timestamp :: Int64 }
            deriving (Show, Eq)

parsePacket :: B.ByteString -> Either String Packet
parsePacket bs = ...
```

Requirements:
- Strict validation.
- Return `Either String Packet`.
- Use standard `Data.ByteString` and `Data.Bits`.
