package src

import (
	"bytes"
	"encoding/binary"
	"math"
	"testing"
)

func writeHeader(buf *bytes.Buffer, pType uint8) {
	buf.WriteByte(0xAF) // Magic
	buf.WriteByte(1)    // Version
	buf.WriteByte(pType)
}

func writeFooter(buf *bytes.Buffer) {
	// Checksum: sum mod 2^32
	var sum uint32
	for _, b := range buf.Bytes() {
		sum += uint32(b)
	}
	binary.Write(buf, binary.BigEndian, sum)
}

func TestHeartbeat(t *testing.T) {
	var buf bytes.Buffer
	writeHeader(&buf, 0x03)
	// Timestamp
	binary.Write(&buf, binary.BigEndian, int64(123456789))
	writeFooter(&buf)

	pkt, err := ParsePacket(buf.Bytes())
	if err != nil {
		t.Fatalf("Failed to parse heartbeat: %v", err)
	}
	if pkt.Type != Heartbeat {
		t.Errorf("Expected Heartbeat, got %v", pkt.Type)
	}
	if pkt.Timestamp != 123456789 {
		t.Errorf("Wrong timestamp: %d", pkt.Timestamp)
	}
}

func TestLogin(t *testing.T) {
	var buf bytes.Buffer
	writeHeader(&buf, 0x01)

	uname := "alice"
	buf.WriteByte(uint8(len(uname)))
	buf.WriteString(uname)

	// 32 byte hash
	hash := make([]byte, 32)
	for i := 0; i < 32; i++ {
		hash[i] = byte(i)
	}
	buf.Write(hash)

	writeFooter(&buf)

	pkt, err := ParsePacket(buf.Bytes())
	if err != nil {
		t.Fatalf("Failed to parse login: %v", err)
	}
	if pkt.Type != Login {
		t.Errorf("Expected Login")
	}
	if pkt.Username != "alice" {
		t.Errorf("Wrong username")
	}
	if !bytes.Equal(pkt.Hash, hash) {
		t.Errorf("Wrong hash")
	}
}

func TestOrder(t *testing.T) {
	var buf bytes.Buffer
	writeHeader(&buf, 0x02)

	// OrderId
	binary.Write(&buf, binary.BigEndian, int64(999))
	// Symbol "AAPL"
	buf.WriteString("AAPL")
	// Side 1 (Sell)
	buf.WriteByte(1)
	// Price 150.50
	binary.Write(&buf, binary.BigEndian, float64(150.50))
	// Quantity 10
	binary.Write(&buf, binary.BigEndian, uint32(10))

	writeFooter(&buf)

	pkt, err := ParsePacket(buf.Bytes())
	if err != nil {
		t.Fatalf("Failed to parse order: %v", err)
	}
	if pkt.Type != Order {
		t.Errorf("Expected Order")
	}
	if pkt.OrderId != 999 {
		t.Errorf("Wrong OrderId")
	}
	if pkt.Symbol != "AAPL" {
		t.Errorf("Wrong Symbol")
	}
	if pkt.Side != 1 {
		t.Errorf("Wrong Side")
	}
	if math.Abs(pkt.Price-150.50) > 0.001 {
		t.Errorf("Wrong Price")
	}
	if pkt.Quantity != 10 {
		t.Errorf("Wrong Quantity")
	}
}

func TestInvalidMagic(t *testing.T) {
	var buf bytes.Buffer
	buf.WriteByte(0x00) // Wrong magic
	buf.WriteByte(1)
	buf.WriteByte(0x03) // Heartbeat
	// Don't care about rest

	_, err := ParsePacket(buf.Bytes())
	if err == nil {
		t.Error("Expected error for invalid magic")
	}
}

func TestInvalidChecksum(t *testing.T) {
	var buf bytes.Buffer
	writeHeader(&buf, 0x03)
	binary.Write(&buf, binary.BigEndian, int64(1))

	// Write wrong checksum
	binary.Write(&buf, binary.BigEndian, uint32(0xDEADBEEF))

	_, err := ParsePacket(buf.Bytes())
	if err == nil {
		t.Error("Expected error for invalid checksum")
	}
}
