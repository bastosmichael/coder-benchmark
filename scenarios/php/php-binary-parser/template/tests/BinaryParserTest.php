<?php

require_once __DIR__ . '/../src/BinaryParser.php';

class BinaryParserTest {
    public function testLogin() {
        // Magic(AF) Ver(1) Type(1) | UserLen(5) 'admin' Hash(32 bytes) | Checksum
        $magic = "\xAF";
        $version = "\x01";
        $type = "\x01";
        $userLen = "\x05";
        $username = "admin";
        $hash = str_repeat("\xAA", 32);
        
        $payload = $magic . $version . $type . $userLen . $username . $hash; // 1+1+1+1+5+32 = 41 bytes
        
        // Checksum
        $sum = 0;
        foreach (str_split($payload) as $char) {
            $sum += ord($char);
        }
        $checksum = pack("N", $sum); // U32 Big Endian
        
        $data = $payload . $checksum;
        
        $result = BinaryParser::parse_packet($data);
        
        assert($result['type'] === 'Login', "Type should be Login");
        assert($result['username'] === 'admin', "Username match");
        assert(strlen($result['hash']) === 32, "Hash length");
        echo "Test Login: PASSED\n";
    }

    public function testOrder() {
        // Type 2
        // ID: 8 bytes, Symbol: 4 bytes, Side: 1, Price: 8 bytes, Qty: 4 bytes
        $magic = "\xAF";
        $ver = "\x01";
        $type = "\x02";
        
        $id = pack("J", 123456789012345); // J is 64-bit BigEndian
        $symbol = "AAPL";
        $side = "\x01"; // Buy
        $price = pack("E", 150.50); // E is double usually (machine dependent, but let's assume standard float64 IEEE 754)
        // PHP `pack` "E" is machine dependent double. "d" is double. The prompt said Double.
        // Let's rely on standard "d" or similar, but prompt implies standard network parsing.
        // Actually, for benchmark, standard `pack("d")` (machine dependent) is risky if cross platform, but fine for local test.
        // Better: Prompt asks for 8 bytes double.
        $price = strrev(pack("d", 150.50)); // Hack: "d" is often Little Endian on x86, network usually Big. 
        // Let's assume the user logic handles the endianness as requested. 
        // Actually, prompt says "ID(8 bytes, I64 BigEndian)... Price(8 bytes, Double)". 
        // Standard "n" is U16BE, "N" U32BE. "J" is 64BE PHP 5.6+.
        // Double doesn't have a strict BE pack code in old PHP, but "G" is Float Big Endian, "E" is Double Little Endian?
        // PHP 7.0+: "E" (double LE), "e" (double LE?), "G" (float BE), "g" (float LE). 
        // Actually "d" is just 'double', usually LE on x86.
        // Let's construct it manually or pick a simple value.
        // Let's use `pack("G", ...)` for Float BE if possible for 'Price' if we want BE, but prompt didn't specify Endian for Double, just "Double".
        // Let's stick to standard `pack("d", ...)` and expect the parser to use `unpack("d", ...)`
        
        $price = pack("E", 150.50); // Double Little Endian seems common for "Double" unless specified Big. 
        // Wait, prompt said: "ID(8 bytes, I64 BigEndian)... Price(8 bytes, Double)".
        // Usually mixed endian is rare. If ID is BigEndian, usually everything is.
        // Let's assume Big Endian for everything for consistency, so `pack("E")` might be wrong if it's LE.
        // PHP 7.2 added `G` (Float BE), `E` (Double Little Endian? No wait).
        // Let's use `strrev(pack("d", ...))` if we want to simulate Big Endian Double on LE machine.
        // Actually, let's keep it simple: Just assert the parser can read back what we wrote.
        // I will use `pack("d")` and if the model uses `unpack("d")` it matches.
        
        $price = pack("d", 150.50);
        $qty = pack("N", 100);
        
        $payload = $magic . $ver . $type . $id . $symbol . $side . $price . $qty;
        
        $sum = 0;
        foreach (str_split($payload) as $char) $sum += ord($char);
        $checksum = pack("N", $sum);
        
        $result = BinaryParser::parse_packet($payload . $checksum);
        assert($result['type'] === 'Order');
        assert($result['id'] === 123456789012345);
        assert($result['symbol'] === 'AAPL');
        assert($result['price'] == 150.50);
        echo "Test Order: PASSED\n";
    }
}

try {
    $test = new BinaryParserTest();
    $test->testLogin();
    $test->testOrder();
    echo "All tests passed.\n";
} catch (Throwable $e) {
    echo "FAILED: " . $e->getMessage() . "\n";
    exit(1);
}
