You are a System Engineer.

Implement a binary packet parser in Ruby class `BinaryParser`.

Protocol:
1. Header: Magic `0xAF` (1 byte), Version `1` (1 byte), Type (1 byte).
2. Payloads:
   - Login (Type 1): UserLen(1), Username(Ascii), Hash(32 bytes).
   - Order (Type 2): ID(8 bytes, I64 BigEndian), Symbol(4 bytes), Side(1), Price(8 bytes, Double), Qty(4 bytes, U32).
   - Heartbeat (Type 3): Timestamp(8 bytes, I64).
3. Checksum: Sum of all previous bytes modulo 2^32 (4 bytes at end).

Function signature:
```ruby
class BinaryParser
  def parse_packet(data)
    # ...
  end
end
```

Requirements:
- Strict validation.
- Return Hash with `:type` ('Login', 'Order', etc) and fields.
- Raise error on invalid data.
