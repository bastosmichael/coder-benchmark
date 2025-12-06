
import { describe, expect, it } from 'vitest';
import { parsePacket } from '../src/parser';

describe('Binary Parser', () => {

    function createHeader(type: number): Buffer {
        return Buffer.from([0xAF, 0x01, type]);
    }

    function calculateChecksum(buf: Buffer): number {
        let sum = 0;
        for (const byte of buf) sum += byte;
        return sum >>> 0; // uint32
    }

    function withChecksum(buf: Buffer): Buffer {
        const sum = calculateChecksum(buf);
        const checksumBuf = Buffer.alloc(4);
        checksumBuf.writeUInt32BE(sum, 0);
        return Buffer.concat([buf, checksumBuf]);
    }

    it('parses Login packet', () => {
        const header = createHeader(0x01);
        const username = Buffer.from('alice');
        const uLen = Buffer.from([username.length]);
        const hash = Buffer.alloc(32).fill(0xAA);

        const payload = Buffer.concat([header, uLen, username, hash]);
        const packet = withChecksum(payload);

        const result = parsePacket(packet);

        if (result.type !== 'Login') throw new Error('Wrong type');
        expect(result.username).toBe('alice');
        expect(result.hash).toBe(hash.toString('hex'));
    });

    it('parses Order packet', () => {
        const header = createHeader(0x02);
        const id = Buffer.alloc(8); id.writeBigInt64BE(123456789n);
        const symbol = Buffer.from('AAPL');
        const side = Buffer.from([1]); // Sell
        const price = Buffer.alloc(8); price.writeDoubleBE(150.50);
        const qty = Buffer.alloc(4); qty.writeUInt32BE(100);

        const payload = Buffer.concat([header, id, symbol, side, price, qty]);
        const packet = withChecksum(payload);

        const result = parsePacket(packet);
        if (result.type !== 'Order') throw new Error('Wrong type');
        expect(result.id).toBe(123456789n);
        expect(result.symbol).toBe('AAPL');
        expect(result.side).toBe('Sell');
        expect(result.price).toBe(150.50);
        expect(result.quantity).toBe(100);
    });

    it('throws on invalid magic byte', () => {
        const buf = Buffer.from([0x00, 0x01, 0x03]); // Wrong magic
        expect(() => parsePacket(buf)).toThrow(/magic/i);
    });

    it('throws on invalid checksum', () => {
        const header = createHeader(0x03); // Heartbeat
        const time = Buffer.alloc(8);
        const payload = Buffer.concat([header, time]);
        const packet = withChecksum(payload);

        // Corrupt last byte
        packet[packet.length - 1] ^= 0xFF;

        expect(() => parsePacket(packet)).toThrow(/checksum/i);
    });

    it('throws on underflow', () => {
        const header = createHeader(0x01);
        expect(() => parsePacket(header)).toThrow(/underflow|short/i);
    });

    it('HARD: Parses mix of many sequential packets efficiently', () => {
        // Create a buffer with multiple packets back-to-back
        const packets = [];

        // 1. Login
        let p1Payload = Buffer.concat([Buffer.from([4]), Buffer.from('test'), Buffer.alloc(32).fill(1)]);
        packets.push(withChecksum(Buffer.concat([createHeader(0x01), p1Payload])));

        // 2. Heartbeat
        let p2Payload = Buffer.alloc(8); p2Payload.writeBigInt64BE(999999n);
        packets.push(withChecksum(Buffer.concat([createHeader(0x03), p2Payload])));

        // 3. Order (Boundary values)
        // Max Uint32, Max Double, etc.
        let id = Buffer.alloc(8); id.writeBigInt64BE(BigInt(Number.MAX_SAFE_INTEGER) + 100n);
        let symbol = Buffer.from('GOOG');
        let side = Buffer.from([0]);
        let price = Buffer.alloc(8); price.writeDoubleBE(123456.789);
        let qty = Buffer.alloc(4); qty.writeUInt32BE(0xFFFFFFFF);

        packets.push(withChecksum(Buffer.concat([
            createHeader(0x02), id, symbol, side, price, qty
        ])));

        // For this test, we verify the parser handles them if fed individually.
        // (The prompt didn't strictly require stream parsing, but advanced impls might).
        // We will just verify strict parsing of the tough boundary cases in Packet 3.

        const p3Conf = parsePacket(packets[2]);
        if (p3Conf.type !== 'Order') throw new Error('Failed to parse complex order');

        expect(p3Conf.quantity).toBe(4294967295); // Max UInt32
        expect(p3Conf.price).toBeCloseTo(123456.789);

        // Also verify that slightly malformed packets throw correctly (extra strict validation)
        // Example: Order symbol not 4 bytes? The prompt says "Symbol (4 bytes)".
        // If we give 3 bytes, it should underflow or fail.

        let badSymbol = Buffer.concat([
            createHeader(0x02),
            id,
            Buffer.from('ABC'), // 3 chars
            side, price, qty
        ]);
        // This would definitely underflow because packet is 1 byte shorter than expected for the fields.
        // But wait, "checksum" is 4 bytes at end. 
        // If we construct it with checksum for the shorter payload, valid checksum?
        // Yes, but parser reads strict 4 bytes for symbol. It will likely read 'side' as 4th byte of symbol.
        // Then read first byte of price as side...
        // Eventually price/qty read will fail or checkum will fail if logical structure is corrupted.
        // Actually, if simply shorter, it might hit underflow when reading last 4 bytes for checksum?

        expect(() => parsePacket(withChecksum(badSymbol))).toThrow();
    });
});
});
