
You are a Systems Engineer working on a high-frequency trading platform.

I have provided a stub in `src/parser.go`.
Your task is to implement a recursive binary packet parser that reads strict binary data from a byte slice.

Protocol Definition:
1. **Header**: 
   - Magic Byte (1 byte): Must be `0xAF`. If not, return error "invalid magic byte".
   - Version (1 byte): Must be `1`. If not, return error "unsupported version".
   - Packet Type (1 byte): `0x01` = Login, `0x02` = Order, `0x03` = Heartbeat.

2. **Payload** (Depends on Type):
   - **Login**: 
     - Username Length (1 byte, UInt8)
     - Username (ASCII string of length N)
     - Password Hash (32 bytes, Fixed buffer)
   - **Order**:
     - Order ID (8 bytes, Int64 Big Endian)
     - Symbol (4 bytes, ASCII string, e.g., "AAPL")
     - Side (1 byte): `0` = Buy, `1` = Sell.
     - Price (8 bytes, Float64 Big Endian)
     - Quantity (4 bytes, UInt32 Big Endian)
   - **Heartbeat**:
     - Timestamp (8 bytes, Int64 Big Endian)

3. **Checksum**:
   - The last 4 bytes of the total packet are a Checksum.
   - For this implementation, the "Checksum" shall be the **Simple SUM of all previous bytes modulo 2^32**.
   - You must verify this. If mismatch, return error "invalid checksum".

Structure Definitions:
```go
package src

import "errors"

type PacketType uint8

const (
	Login     PacketType = 0x01
	Order     PacketType = 0x02
	Heartbeat PacketType = 0x03
)

type Packet struct {
	Type      PacketType
	Username  string
	Hash      []byte      // 32 bytes
	OrderId   int64
	Symbol    string
	Side      uint8       // 0=Buy, 1=Sell
	Price     float64
	Quantity  uint32
	Timestamp int64
}

// ParsePacket takes a byte buffer and returns the parsed Packet or an error.
func ParsePacket(data []byte) (*Packet, error) {
	// your implementation
	return nil, nil
}
```

Requirements:
- Strict bounds checking. If buffer is too short, return "buffer underflow" error.
- Handle endianness correctly (Big Endian for numbers).
- No external libraries.
- Idiomatic Go error handling.
