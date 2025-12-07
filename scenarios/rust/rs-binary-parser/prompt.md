
You are a Systems Engineer working on a high-frequency trading platform.

I have provided a stub in `src/lib.rs`.
Your task is to implement a recursive binary packet parser that reads strict binary data from a byte slice.

Protocol Definition:
1. **Header**: 
   - Magic Byte (1 byte): Must be `0xAF`. If not, return Err("Invalid magic byte").
   - Version (1 byte): Must be `1`. If not, return Err("Unsupported version").
   - Packet Type (1 byte): `0x01` = Login, `0x02` = Order, `0x03` = Heartbeat.

2. **Payload** (Depends on Type):
   - **Login**: 
     - Username Length (1 byte, u8)
     - Username (ASCII string of length N)
     - Password Hash (32 bytes, Fixed buffer)
   - **Order**:
     - Order ID (8 bytes, i64 Big Endian)
     - Symbol (4 bytes, ASCII string, e.g., "AAPL")
     - Side (1 byte): `0` = Buy, `1` = Sell.
     - Price (8 bytes, f64 Big Endian)
     - Quantity (4 bytes, u32 Big Endian)
   - **Heartbeat**:
     - Timestamp (8 bytes, i64 Big Endian)

3. **Checksum**:
   - The last 4 bytes of the total packet are a Checksum.
   - For this implementation, the "Checksum" shall be the **Simple SUM of all previous bytes modulo 2^32**.
   - You must verify this. If mismatch, return Err("Invalid checksum").

Structure Definitions:
```rust
#[derive(Debug, PartialEq)]
pub enum Packet {
    Login {
        username: String,
        hash: [u8; 32],
    },
    Order {
        id: i64,
        symbol: String,
        side: u8, // 0=Buy, 1=Sell
        price: f64,
        quantity: u32,
    },
    Heartbeat {
        timestamp: i64,
    },
}

pub fn parse_packet(buffer: &[u8]) -> Result<Packet, String> {
    // your implementation
    Err("Not implemented".to_string())
}
```

Requirements:
- Strict bounds checking. If buffer is too short, return error.
- Handle endianness correctly (Big Endian for numbers).
- No external libraries (use `std` only).
