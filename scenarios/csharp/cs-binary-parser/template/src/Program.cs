using System;
using System.Buffers.Binary;
using System.Text;
using BinaryParser;

class Program
{
    static void WriteHeader(List<byte> buf, byte type)
    {
        buf.Add(0xAF);
        buf.Add(1);
        buf.Add(type);
    }

    static void WriteFooter(List<byte> buf)
    {
        uint sum = 0;
        foreach (var b in buf) sum += b;
        
        byte[] sumBytes = new byte[4];
        BinaryPrimitives.WriteUInt32BigEndian(sumBytes, sum);
        buf.AddRange(sumBytes);
    }

    static void TestHeartbeat()
    {
        var buf = new List<byte>();
        WriteHeader(buf, 3);
        long ts = 123456789;
        byte[] tsBytes = new byte[8];
        BinaryPrimitives.WriteInt64BigEndian(tsBytes, ts);
        buf.AddRange(tsBytes);
        WriteFooter(buf);

        var pkt = Parser.ParsePacket(buf.ToArray());
        if (pkt is not Heartbeat hb) throw new Exception("Expected Heartbeat");
        if (hb.Timestamp != 123456789) throw new Exception("Wrong timestamp");
    }

    static void TestLogin()
    {
        var buf = new List<byte>();
        WriteHeader(buf, 1);
        string u = "alice";
        buf.Add((byte)u.Length);
        buf.AddRange(Encoding.ASCII.GetBytes(u));
        
        byte[] hash = new byte[32];
        for(int i=0; i<32; i++) hash[i] = (byte)i;
        buf.AddRange(hash);
        
        WriteFooter(buf);

        var pkt = Parser.ParsePacket(buf.ToArray());
        if (pkt is not Login l) throw new Exception("Expected Login");
        if (l.Username != "alice") throw new Exception("Wrong username");
        // simple check
        if (l.Hash.Length != 32) throw new Exception("Wrong hash length");
    }

    static void Main(string[] args)
    {
        try
        {
            TestHeartbeat();
            TestLogin();
            Console.WriteLine("Tests passed");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Test failed: {ex}");
            Environment.Exit(1);
        }
    }
}
