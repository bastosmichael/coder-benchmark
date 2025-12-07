package main.java;
import java.util.*;

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
        public long quantity;
    }
    
    public static class Heartbeat extends Packet {
        public long timestamp;
    }

    public static Packet parsePacket(byte[] data) {
         throw new RuntimeException("Not implemented");
    }
}
