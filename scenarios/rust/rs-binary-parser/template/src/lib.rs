#[derive(Debug, PartialEq)]
pub enum Packet {
    Login {
        username: String,
        hash: [u8; 32],
    },
    Order {
        id: i64,
        symbol: String,
        side: u8, // 0=Buy, 1=Sell
        price: f64,
        quantity: u32,
    },
    Heartbeat {
        timestamp: i64,
    },
}

pub fn parse_packet(buffer: &[u8]) -> Result<Packet, String> {
    Err("Not implemented".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_header(vec: &mut Vec<u8>, p_type: u8) {
        vec.push(0xAF);
        vec.push(1);
        vec.push(p_type);
    }

    fn write_footer(vec: &mut Vec<u8>) {
        let mut sum: u32 = 0;
        for b in vec.iter() {
            sum = sum.wrapping_add(*b as u32);
        }
        vec.extend_from_slice(&sum.to_be_bytes());
    }

    #[test]
    fn test_heartbeat() {
        let mut vec = Vec::new();
        write_header(&mut vec, 0x03);
        let ts: i64 = 123456789;
        vec.extend_from_slice(&ts.to_be_bytes());
        write_footer(&mut vec);

        match parse_packet(&vec) {
            Ok(Packet::Heartbeat { timestamp }) => assert_eq!(timestamp, 123456789),
            Ok(_) => panic!("Wrong type"),
            Err(e) => panic!("Error: {}", e),
        }
    }

    #[test]
    fn test_login() {
        let mut vec = Vec::new();
        write_header(&mut vec, 0x01);
        let uname = "alice";
        vec.push(uname.len() as u8);
        vec.extend_from_slice(uname.as_bytes());
        let hash = [0u8; 32];
        vec.extend_from_slice(&hash);
        write_footer(&mut vec);

        match parse_packet(&vec) {
            Ok(Packet::Login { username, hash: h }) => {
                assert_eq!(username, "alice");
                assert_eq!(h, hash);
            },
            _ => panic!("Expected Login"),
        }
    }

    #[test]
    fn test_order() {
        let mut vec = Vec::new();
        write_header(&mut vec, 0x02);
        vec.extend_from_slice(&999i64.to_be_bytes()); // ID
        vec.extend_from_slice("AAPL".as_bytes());
        vec.push(1); // Sell
        vec.extend_from_slice(&150.50f64.to_be_bytes());
        vec.extend_from_slice(&10u32.to_be_bytes());
        write_footer(&mut vec);
        
        match parse_packet(&vec) {
            Ok(Packet::Order { id, symbol, side, price, quantity }) => {
                assert_eq!(id, 999);
                assert_eq!(symbol, "AAPL");
                assert_eq!(side, 1);
                assert!((price - 150.50).abs() < 0.001);
                assert_eq!(quantity, 10);
            },
            _ => panic!("Expected Order"),
        }
    }

    #[test]
    fn test_invalid_magic() {
        let mut vec = Vec::new();
        vec.push(0x00);
        vec.push(1);
        vec.push(0x03);
        if parse_packet(&vec).is_ok() {
            panic!("Should fail magic");
        }
    }

    #[test]
    fn test_invalid_checksum() {
        let mut vec = Vec::new();
        write_header(&mut vec, 0x03);
        let ts: i64 = 1;
        vec.extend_from_slice(&ts.to_be_bytes());
        
        // Bad checksum
        vec.extend_from_slice(&0xDEADBEEFu32.to_be_bytes());
        
        if parse_packet(&vec).is_ok() {
            panic!("Should fail checksum");
        }
    }
}
