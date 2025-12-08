import java.nio.ByteBuffer
import java.nio.ByteOrder

object TestBinaryParser {
  def main(args: Array[String]): Unit = {
    try {
      testLogin()
      testOrder()
      println("All tests passed.")
    } catch {
      case e: Throwable => 
        println(s"FAILED: ${e.getMessage}")
        e.printStackTrace()
        System.exit(1)
    }
  }

  def testLogin(): Unit = {
    val magic = 0xAF.toByte
    val version = 1.toByte
    val startType = 1.toByte
    val userLen = 5.toByte
    val userBytes = "admin".getBytes("UTF-8")
    val hash = Array.fill[Byte](32)(0xAA.toByte)
    
    val payload = Array(magic, version, startType, userLen) ++ userBytes ++ hash
    val sum = payload.map(_.toInt & 0xFF).sum
    val checksum = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(sum).array()
    
    val data = payload ++ checksum
    val result = BinaryParser.parsePacket(data)
    
    assert(result("type") == "Login")
    assert(result("username") == "admin")
    assert(result("hash").asInstanceOf[Array[Byte]].length == 32)
    println("Test Login: PASSED")
  }

  def testOrder(): Unit = {
    val magic = 0xAF.toByte
    val version = 1.toByte
    val tType = 2.toByte
    
    val id = 123456789012345L
    val symbol = "AAPL".getBytes("UTF-8")
    val side = 1.toByte
    val price = 150.50
    val qty = 100
    
    val bb = ByteBuffer.allocate(8 + 4 + 1 + 8 + 4).order(ByteOrder.BIG_ENDIAN)
    bb.putLong(id)
    bb.put(symbol)
    bb.put(side)
    bb.putDouble(price)
    bb.putInt(qty)
    val body = bb.array()
    
    val payload = Array(magic, version, tType) ++ body
    val sum = payload.map(_.toInt & 0xFF).sum
    val checksum = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(sum).array()
    
    val result = BinaryParser.parsePacket(payload ++ checksum)
    
    assert(result("type") == "Order")
    assert(result("id") == id)
    assert(new String(result("symbol").asInstanceOf[Array[Byte]]) == "AAPL" || result("symbol") == "AAPL")
    assert(Math.abs(result("price").asInstanceOf[Double] - 150.50) < 0.001)
    println("Test Order: PASSED")
  }
}
