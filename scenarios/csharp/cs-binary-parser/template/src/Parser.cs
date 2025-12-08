using System;

namespace BinaryParser;

public abstract record Packet;

public record Login(string Username, byte[] Hash) : Packet;

public record Order(long OrderId, string Symbol, byte Side, double Price, uint Quantity) : Packet;

public record Heartbeat(long Timestamp) : Packet;

public static class Parser
{
    public static Packet ParsePacket(byte[] data)
    {
        throw new NotImplementedException();
    }
}
