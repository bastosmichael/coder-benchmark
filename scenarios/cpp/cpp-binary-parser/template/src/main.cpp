#include <iostream>
#include <vector>
#include <cassert>
#include <cstring>
#include <algorithm>
#include "parser.hpp"

// Helper to handle endianness for tests (Assumes Little Endian Host like x86)
// We need to write Big Endian to buffer.

template <typename T>
void writeBE(std::vector<uint8_t>& buf, T val) {
    uint8_t* ptr = reinterpret_cast<uint8_t*>(&val);
    // Reverse bytes if host is LE
    // x86 is LE.
    for (size_t i = 0; i < sizeof(T); i++) {
        buf.push_back(ptr[sizeof(T) - 1 - i]);
    }
}

void writeHeader(std::vector<uint8_t>& buf, uint8_t type) {
    buf.push_back(0xAF);
    buf.push_back(1);
    buf.push_back(type);
}

void writeFooter(std::vector<uint8_t>& buf) {
    uint32_t sum = 0;
    for (uint8_t b : buf) sum += b;
    writeBE(buf, sum);
}

void testHeartbeat() {
    std::vector<uint8_t> buf;
    writeHeader(buf, 3);
    int64_t ts = 123456789;
    writeBE(buf, ts);
    writeFooter(buf);

    Packet p = parsePacket(buf);
    if (!std::holds_alternative<Heartbeat>(p)) {
        std::cerr << "Expected Heartbeat\n";
        exit(1);
    }
    if (std::get<Heartbeat>(p).timestamp != 123456789) {
        std::cerr << "Wrong timestamp\n";
        exit(1);
    }
}

void testLogin() {
    std::vector<uint8_t> buf;
    writeHeader(buf, 1);
    std::string u = "user";
    buf.push_back(static_cast<uint8_t>(u.size()));
    for (char c : u) buf.push_back(c);
    
    std::array<uint8_t, 32> hash = {0};
    for(size_t i=0; i<32; i++) {
        hash[i] = i;
        buf.push_back(i);
    }
    writeFooter(buf);
    
    Packet p = parsePacket(buf);
    if (!std::holds_alternative<Login>(p)) {
        std::cerr << "Expected Login\n";
        exit(1);
    }
    if (std::get<Login>(p).username != "user") {
        std::cerr << "Wrong username\n";
        exit(1);
    }
}

int main() {
    try {
        testHeartbeat();
        testLogin();
        std::cout << "All tests passed\n";
    } catch (const std::exception& e) {
        std::cerr << "Test failed: " << e.what() << "\n";
        return 1;
    }
    return 0;
}
