package main.java;
import java.nio.ByteBuffer;
import java.util.Arrays;

public class Main {
    private static void writeHeader(ByteBuffer buf, int type) {
        buf.put((byte)0xAF);
        buf.put((byte)1);
        buf.put((byte)type);
    }
    
    private static void writeFooter(ByteBuffer buf) {
        // sum bytes
        byte[] arr = buf.array();
        int pos = buf.position();
        long sum = 0;
        for(int i=0; i<pos; i++) {
            sum += (arr[i] & 0xFF);
        }
        buf.putInt((int)sum);
    }

    private static void testHeartbeat() {
        ByteBuffer buf = ByteBuffer.allocate(1024);
        writeHeader(buf, 3);
        long ts = 123456789L;
        buf.putLong(ts);
        writeFooter(buf);
        
        byte[] input = Arrays.copyOf(buf.array(), buf.position());
        Parser.Packet pkt = Parser.parsePacket(input);
        
        if (!(pkt instanceof Parser.Heartbeat)) throw new RuntimeException("Expected Heartbeat");
        if (((Parser.Heartbeat)pkt).timestamp != 123456789L) throw new RuntimeException("Wrong timestamp");
    }
    
    private static void testLogin() {
        ByteBuffer buf = ByteBuffer.allocate(1024);
        writeHeader(buf, 1);
        String u = "alice";
        buf.put((byte)u.length());
        buf.put(u.getBytes());
        byte[] hash = new byte[32];
        for(int i=0; i<32; i++) hash[i] = (byte)i;
        buf.put(hash);
        writeFooter(buf);
        
        byte[] input = Arrays.copyOf(buf.array(), buf.position());
        Parser.Packet pkt = Parser.parsePacket(input);
        
        if (!(pkt instanceof Parser.Login)) throw new RuntimeException("Expected Login");
        if (!((Parser.Login)pkt).username.equals("alice")) throw new RuntimeException("Wrong username");
        if (!Arrays.equals(((Parser.Login)pkt).hash, hash)) throw new RuntimeException("Wrong hash");
    }

    public static void main(String[] args) {
        try {
            testHeartbeat();
            testLogin();
            System.out.println("Tests passed");
        } catch(Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
