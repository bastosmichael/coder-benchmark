type packet = 
  | Login of { username: string; hash: bytes }
  | Order of { id: int64; symbol: string; side: int; price: float; qty: int32 }
  | Heartbeat of { timestamp: int64 }

let parse_packet (data: bytes) : (packet, string) result =
  reqtype Error "Not implemented"
