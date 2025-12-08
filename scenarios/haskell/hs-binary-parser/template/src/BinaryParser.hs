module BinaryParser (parsePacket, Packet(..)) where

import qualified Data.ByteString as B
import Data.Int
import Data.Word

data Packet = Login { username :: String, hash :: B.ByteString }
            | Order { packetId :: Int64, symbol :: String, side :: Word8, price :: Double, qty :: Word32 }
            | Heartbeat { timestamp :: Int64 }
            deriving (Show, Eq)

parsePacket :: B.ByteString -> Either String Packet
parsePacket _ = Left "Not implemented"
