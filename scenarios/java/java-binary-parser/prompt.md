
You are a Systems Engineer working on a high-frequency trading platform.

I have provided a stub in `src/main/java/Parser.java`.
Your task is to implement a recursive binary packet parser that reads strict binary data from a byte array.

Protocol Definition:
1. **Header**: 
   - Magic Byte (1 byte): Must be `0xAF`. If not, throw `IllegalArgumentException`.
   - Version (1 byte): Must be `1`. If not, throw `IllegalArgumentException`.
   - Packet Type (1 byte): `0x01` = Login, `0x02` = Order, `0x03` = Heartbeat.

2. **Payload** (Depends on Type):
   - **Login**: 
     - Username Length (1 byte, u8 - treat as unsigned)
     - Username (ASCII string of length N)
     - Password Hash (32 bytes)
   - **Order**:
     - Order ID (8 bytes, long Big Endian)
     - Symbol (4 bytes, ASCII string)
     - Side (1 byte): `0` = Buy, `1` = Sell.
     - Price (8 bytes, double Big Endian)
     - Quantity (4 bytes, int/unsigned int Big Endian)
   - **Heartbeat**:
     - Timestamp (8 bytes, long Big Endian)

3. **Checksum**:
   - The last 4 bytes of the total packet are a Checksum.
   - For this implementation, the "Checksum" shall be the **Simple SUM of all previous bytes modulo 2^32**.
   - You must verify this. If mismatch, throw `IllegalArgumentException`.

Structure Definitions:
```java
package main.java;

public class Parser {
    
    public static abstract class Packet {}
    
    public static class Login extends Packet {
        public String username;
        public byte[] hash;
    }
    
    public static class Order extends Packet {
        public long id;
        public String symbol;
        public byte side;
        public double price;
        public long quantity; // uint32 fits in long
    }
    
    public static class Heartbeat extends Packet {
        public long timestamp;
    }

    public static Packet parsePacket(byte[] data) {
        // implementation
        return null;
    }
}
```

Requirements:
- Strict bounds checking.
- Handle unsigned bytes where necessary (Java bytes are signed).
- No external libraries.
