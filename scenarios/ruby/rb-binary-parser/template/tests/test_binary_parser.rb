require_relative '../src/binary_parser'
require 'test/unit'

class TestBinaryParser < Test::Unit::TestCase
  def test_login
    magic = "\xAF"
    version = "\x01"
    type = "\x01"
    user_len = "\x05"
    username = "admin"
    hash = "\xAA" * 32
    
    payload = magic + version + type + user_len + username + hash
    
    sum = payload.each_byte.reduce(0, :+)
    checksum = [sum].pack("N")
    
    data = payload + checksum
    
    parser = BinaryParser.new
    result = parser.parse_packet(data)
    
    assert_equal 'Login', result[:type]
    assert_equal 'admin', result[:username]
    assert_equal 32, result[:hash].bytesize
  end

  def test_order
    magic = "\xAF"
    version = "\x01"
    type = "\x02"
    
    id = [123456789012345].pack("Q>") # Q> is 64-bit BE
    symbol = "AAPL"
    side = "\x01"
    price = [150.50].pack("G") # G is Double BE
    qty = [100].pack("N")
    
    payload = magic + version + type + id + symbol + side + price + qty
    
    sum = payload.each_byte.reduce(0, :+)
    checksum = [sum].pack("N")
    
    parser = BinaryParser.new
    result = parser.parse_packet(payload + checksum)
    
    assert_equal 'Order', result[:type]
    assert_equal 123456789012345, result[:id]
    assert_equal 'AAPL', result[:symbol]
    assert_in_delta 150.50, result[:price], 0.001
  end
end
