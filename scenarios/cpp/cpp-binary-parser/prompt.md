
You are a Systems Engineer working on a high-frequency trading platform.

I have provided a stub in `src/parser.cpp` (and `src/parser.hpp`).
Your task is to implement a recursive binary packet parser that reads strict binary data from a `std::vector<uint8_t>`.

Protocol Definition:
1. **Header**: 
   - Magic Byte (1 byte): Must be `0xAF`. If not, throw `std::runtime_error("Invalid magic byte")`.
   - Version (1 byte): Must be `1`. If not, throw `std::runtime_error("Unsupported version")`.
   - Packet Type (1 byte): `0x01` = Login, `0x02` = Order, `0x03` = Heartbeat.

2. **Payload** (Depends on Type):
   - **Login**: 
     - Username Length (1 byte, u8)
     - Username (ASCII string of length N)
     - Password Hash (32 bytes, Fixed buffer)
   - **Order**:
     - Order ID (8 bytes, int64_t Big Endian)
     - Symbol (4 bytes, UIString, e.g., "AAPL")
     - Side (1 byte): `0` = Buy, `1` = Sell.
     - Price (8 bytes, double Big Endian)
     - Quantity (4 bytes, uint32_t Big Endian)
   - **Heartbeat**:
     - Timestamp (8 bytes, int64_t Big Endian)

3. **Checksum**:
   - The last 4 bytes of the total packet are a Checksum.
   - For this implementation, the "Checksum" shall be the **Simple SUM of all previous bytes modulo 2^32**.
   - You must verify this. If mismatch, throw `std::runtime_error("Invalid checksum")`.

Structure Definitions:
```cpp
#pragma once
#include <vector>
#include <string>
#include <variant>
#include <array>
#include <cstdint>
#include <stdexcept>

struct Login {
    std::string username;
    std::array<uint8_t, 32> hash;
};

struct Order {
    int64_t id;
    std::string symbol;
    uint8_t side; // 0=Buy, 1=Sell
    double price;
    uint32_t quantity;
};

struct Heartbeat {
    int64_t timestamp;
};

using Packet = std::variant<Login, Order, Heartbeat>;

Packet parsePacket(const std::vector<uint8_t>& data);
```

Requirements:
- Strict bounds checking. If buffer is too short, throw "Buffer underflow".
- Handle endianness correctly (Big Endian for numbers). Use `ntohl`/`htonl` logic or manual shifting.
- No external libraries.
