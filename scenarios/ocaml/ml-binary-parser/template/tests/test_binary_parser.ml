open BinaryParser
open Printf

let pack_u32 buf offset v =
  for i = 0 to 3 do
    Bytes.set_uint8 buf (offset + i) (Int32.to_int (Int32.logand (Int32.shift_right v ((3 - i) * 8)) 255l))
  done

let pack_u64 buf offset v =
  for i = 0 to 7 do
    Bytes.set_uint8 buf (offset + i) (Int64.to_int (Int64.logand (Int64.shift_right v ((7 - i) * 8)) 255L))
  done

let test_login () =
  let magic = 0xAF in
  let ver = 1 in 
  let typ = 1 in
  let user_len = 5 in
  let username = "admin" in
  let hash = Bytes.make 32 '\xAA' in
  
  let header_len = 3 in
  let payload_len = 1 + 5 + 32 in
  let total = header_len + payload_len + 4 in
  let buf = Bytes.create total in
  
  Bytes.set_uint8 buf 0 magic;
  Bytes.set_uint8 buf 1 ver;
  Bytes.set_uint8 buf 2 typ;
  Bytes.set_uint8 buf 3 user_len;
  Bytes.blit_string username 0 buf 4 5;
  Bytes.blit hash 0 buf 9 32;
  
  let sum = ref 0 in
  for i = 0 to header_len + payload_len - 1 do
    sum := !sum + (Bytes.get_uint8 buf i)
  done;
  
  (* Simple mod 2^32 logic actually implies wrapping sum *)
  let chk = Int32.of_int !sum in (* Assume it fits or wraps *)
  pack_u32 buf (header_len + payload_len) chk;
  
  match parse_packet buf with
  | Ok (Login {username=u; hash=h}) ->
      if u <> "admin" then failwith "Username mismatch";
      if Bytes.length h <> 32 then failwith "Hash len mismatch";
      printf "Test Login: PASSED\n"
  | Ok _ -> failwith "Wrong packet type"
  | Error e -> failwith ("Parse failed: " ^ e)

let test_order () =
  let magic = 0xAF in
  let ver = 1 in 
  let typ = 2 in
  
  let header_len = 3 in
  let id = 123456789012345L in
  let symbol = "AAPL" in (* 4 bytes *)
  let side = 1 in
  let price = 150.50 in
  let qty = 100l in
  
  let payload_len = 8 + 4 + 1 + 8 + 4 in
  let total = header_len + payload_len + 4 in
  let buf = Bytes.create total in
  
  Bytes.set_uint8 buf 0 magic;
  Bytes.set_uint8 buf 1 ver;
  Bytes.set_uint8 buf 2 typ;
  
  pack_u64 buf 3 id;
  Bytes.blit_string symbol 0 buf 11 4;
  Bytes.set_uint8 buf 15 side;
  (* Price is double. Pack logic is tricky. Let's assume user implementation handles Int64_bits of float *)
  let price_bits = Int64.bits_of_float price in
  (* Endianness: Int64.bits_of_float is host order, but prompt implies BigEndian standard protocol? 
     Usually binary parsers clarify. We'll use BigEndian for Int64. *)
     (* If host is Little Endian, bits_of_float returns LE int64. We might need to swap for BE? 
        Or just pack it as is if we expect the parser to read bits_of_float directly.
        Let's pack it as BigEndian Int64 bits. *)
  pack_u64 buf 16 price_bits; (* This puts it in BE if pack_u64 does BE *)
    
  pack_u32 buf 24 qty;
  
  let sum = ref 0 in
  for i = 0 to header_len + payload_len - 1 do
    sum := !sum + (Bytes.get_uint8 buf i)
  done;
  pack_u32 buf (header_len + payload_len) (Int32.of_int !sum);
  
  match parse_packet buf with
  | Ok (Order {id=i; symbol=s; price=p}) ->
      if i <> id then failwith "ID mismatch";
      if s <> symbol then failwith "Symbol mismatch";
      if abs_float (p -. price) > 0.001 then failwith "Price mismatch";
      printf "Test Order: PASSED\n"
  | Ok _ -> failwith "Wrong packet type"
  | Error e -> failwith ("Parse failed: " ^ e)

let () =
  try
    test_login ();
    test_order ();
    printf "All tests passed.\n"
  with Failure f ->
    printf "FAILED: %s\n" f;
    exit 1
