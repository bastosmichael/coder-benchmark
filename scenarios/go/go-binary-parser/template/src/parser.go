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
	Hash      []byte // 32 bytes
	OrderId   int64
	Symbol    string
	Side      uint8 // 0=Buy, 1=Sell
	Price     float64
	Quantity  uint32
	Timestamp int64
}

// ParsePacket takes a byte buffer and returns the parsed Packet or an error.
func ParsePacket(data []byte) (*Packet, error) {
	return nil, errors.New("Not implemented")
}
