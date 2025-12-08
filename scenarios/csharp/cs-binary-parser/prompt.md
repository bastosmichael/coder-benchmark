
You are a Systems Engineer working on a high-frequency trading platform.

I have provided a stub in `src/Parser.cs`.
Your task is to implement a recursive binary packet parser that reads strict binary data from a `byte[]`.

Protocol Definition:
1. **Header**: 
   - Magic Byte (1 byte): Must be `0xAF`. If not, throw `Exception` (or specific exception).
   - Version (1 byte): Must be `1`. If not, throw `Exception`.
   - Packet Type (1 byte): `0x01` = Login, `0x02` = Order, `0x03` = Heartbeat.

2. **Payload** (Depends on Type):
   - **Login**: 
     - Username Length (1 byte)
     - Username (ASCII string of length N)
     - Password Hash (32 bytes)
   - **Order**:
     - Order ID (8 bytes, long Big Endian)
     - Symbol (4 bytes, ASCII string)
     - Side (1 byte): `0` = Buy, `1` = Sell.
     - Price (8 bytes, double Big Endian)
     - Quantity (4 bytes, uint Big Endian)
   - **Heartbeat**:
     - Timestamp (8 bytes, long Big Endian)

3. **Checksum**:
   - The last 4 bytes of the total packet are a Checksum.
   - For this implementation, the "Checksum" shall be the **Simple SUM of all previous bytes (header + payload) modulo 2^32**.
   - You must verify this. If mismatch, throw `Exception`.

Structure Definitions:
```csharp
namespace BinaryParser;

public abstract record Packet;

public record Login(string Username, byte[] Hash) : Packet;

// Side: 0=Buy, 1=Sell
public record Order(long OrderId, string Symbol, byte Side, double Price, uint Quantity) : Packet;

public record Heartbeat(long Timestamp) : Packet;

public static class Parser
{
    public static Packet ParsePacket(byte[] data)
    {
        // implementation
        throw new NotImplementedException();
    }
}
```

Requirements:
- Strict bounds checking.
- Handle Endianness (Big Endian in protocol, System could be Little Endian).
- No external libraries. Use `System.Buffers.Binary.BinaryPrimitives` or manual shifting.
