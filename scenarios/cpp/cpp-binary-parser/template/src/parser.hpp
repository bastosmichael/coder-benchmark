#pragma once
#include <vector>
#include <string>
#include <variant>
#include <array>
#include <cstdint>
#include <stdexcept>

struct Login {
    std::string username;
    std::array<uint8_t, 32> hash;
};

struct Order {
    int64_t id;
    std::string symbol;
    uint8_t side; // 0=Buy, 1=Sell
    double price;
    uint32_t quantity;
};

struct Heartbeat {
    int64_t timestamp;
};

using Packet = std::variant<Login, Order, Heartbeat>;

Packet parsePacket(const std::vector<uint8_t>& data);
