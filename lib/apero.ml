include Atypes
include Acommon
include Miobuf
include State
include Ordered
include Key_value
include Properties
include Uuid
include Mvar
include Json
include Identifiers
include Apath
module List = Alist
module Stringable = Stringable
module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)


let rec fast_encode_vle (v:Vle.t) buf =      
  if v <= 0x7fL then MIOBuf.put_byte (Vle.to_int v) buf
  else 
    begin 
      let c = Vle.logor (Vle.logand v 0x7fL) 0x80L in 
      MIOBuf.put_byte (Vle.to_int c) buf;
      fast_encode_vle (Vle.shift_right v  7) buf
    end 

let  fast_decode_vle buf =
  let acc = ref 0L in 
  let c = ref 0L in 
  let i = ref 0 in 
  c := Vle.of_int @@ MIOBuf.get_byte buf;
  while !c > 0x7fL do         
    let v = Vle.logand !c 0x7fL in 
    acc := Vle.logor v (Vle.shift_left !acc !i);
    c := Vle.of_int @@ MIOBuf.get_byte buf;
    i := !i + 7
  done ;  
  Vle.logor !c (Int64.shift_left !acc (!i))
  


let encode_vle ?size v buf =
  let to_char l = char_of_int @@ Int64.to_int l in
  let rec put_positive_vle_rec v' ?size' buf =
    match size', v' with
    | Some(0), _ -> raise @@ Atypes.Exception (`OutOfBounds (`Msg (Printf.sprintf "encode_vle: cannot encode %Ld as a %d bytes VLE" v (Option.get size))))
    | None, v' when v' <= Vle.byte_mask -> MIOBuf.put_char (to_char v') buf
    | Some(1), v' when v' <= Vle.byte_mask -> MIOBuf.put_char (to_char v') buf
    | _, v' ->
      let mv = Int64.logor Vle.more_bytes_flag @@ Int64.logand v' Vle.byte_mask in
      MIOBuf.put_char (to_char mv) buf;
      let sv = Int64.shift_right v' Vle.shift_len in
      let size' = Option.map size' (fun s -> s-1) in
      put_positive_vle_rec sv ?size' buf
  in
  if v < 0L then raise @@ Atypes.Exception (`OutOfRange (`Msg "encode_vle: integer to encode must be positive"))
  else put_positive_vle_rec v ?size':size buf

let decode_vle buf =
  let from_char c = Vle.of_int (int_of_char c) in
  let masked_from_char c = Vle.logand Vle.byte_mask  (Vle.of_int (int_of_char c)) in
  let merge v c n = Vle.logor v (Vle.shift_left c (n * Vle.shift_len)) in
  let rec decode_vle_rec  v n buf =
    if n < Vle.max_bytes then
      begin
        let c = MIOBuf.get_char buf in         
        if (from_char c) <= Vle.byte_mask then  (merge v (masked_from_char c) n)
        else decode_vle_rec (merge v (masked_from_char c) n) (n+1) buf              
      end
    else
      begin
        let rec skip k buf =
          let c = MIOBuf.get_char buf in           
          if from_char c <= Vle.byte_mask then raise @@ Atypes.Exception (`OutOfBounds (`Msg "vle out of bounds"))
          else skip (k+1) buf 
        in skip n buf
      end
  in decode_vle_rec 0L 0 buf

let encode_bytes src dst =
  let n = MIOBuf.available src in
  let m = MIOBuf.available dst in
  if n <= m then
    begin
      fast_encode_vle (Vle.of_int n) dst;
      MIOBuf.put_buf src dst
    end
    else      
        raise @@ Atypes.Exception (`OutOfBounds (`Msg (Printf.sprintf "encode_bytes failed because of bounds error %d < %d" n m)))    

let decode_bytes buf =
  let len = fast_decode_vle buf in 
  MIOBuf.get_buf (Vle.to_int len) buf
        
let encode_string s buf =
  let len = String.length s in
  let bs = Lwt_bytes.of_string s in
  fast_encode_vle (Vle.of_int len) buf;
  MIOBuf.blit_from_bytes bs 0 len buf
    
let decode_string buf =
  let vlen = fast_decode_vle buf in   
  let len =  Vle.to_int vlen in    
  let bs = MIOBuf.blit_to_bytes len buf in
  Lwt_bytes.to_string bs
    

let decode_seq read buf  =
  let rec get_remaining seq length buf =
    match length with
    | 0 -> seq
    | _ ->
      let value = read buf in
      get_remaining (value :: seq) (length - 1) buf
  in
  let length = fast_decode_vle buf in   
  (get_remaining  [] (Vle.to_int length) buf)

let encode_seq write seq buf =
  let rec put_remaining seq buf =
    match seq with
    | [] -> ()
    | head :: rem -> 
        write head buf;
        put_remaining rem buf
  in
    fast_encode_vle (Vle.of_int (List.length seq)) buf;
    put_remaining seq buf

let encode_seq_safe write seq buf =
  let rec put_remaining seq n buf =
    if (n = 0x3FFF) then
      (* note: 0x3FFF is the biggest length we can encode in a 2-bytes vle *)
      (buf, n, seq)
    else match seq with
    | [] -> (buf, n, [])
    | head :: rem ->
      MIOBuf.mark buf;
      try
        write head buf
        put_remaining rem (n+1) buf
      with
      | _ -> MIOBuf.reset buf; (buf, n, seq)
  in
  (* reserve space for seq length as a 2-bytes vle *)
  let length_pos = MIOBuf.position buf in
  encode_vle ~size:2 0L buf;
  let (buf, n, remain) = put_remaining seq 0 buf in 
  MIOBuf.overwrite_at length_pos (encode_vle ~size:2 (Vle.of_int n)) buf;
  (buf, remain)

let read1_spec _ p1 c (buf:MIOBuf.t) =      
  c @@ p1 buf

let read2_spec _ p1 p2 c buf =     
  let a1 = p1 buf 
  and a2 = p2 buf 
  in c a1 a2
  
let read3_spec _ p1 p2 p3 c buf = 
  let a1 = p1 buf 
  and a2 = p2 buf
  and a3 = p3 buf 
  in c a1 a2 a3
  
let read4_spec _ p1 p2 p3 p4 c buf = 
  let a1 = p1 buf 
  and a2 = p2 buf
  and a3 = p3 buf 
  and a4 = p4 buf 
  in c a1 a2 a3 a4 

let read5_spec _ p1 p2 p3 p4 p5 c buf = 
  let a1 = p1 buf 
  and a2 = p2 buf
  and a3 = p3 buf 
  and a4 = p4 buf 
  and a5 = p5 buf 
  in c a1 a2 a3 a4 a5

let read6_spec _ p1 p2 p3 p4 p5 p6 c buf = 
  let a1 = p1 buf 
  and a2 = p2 buf
  and a3 = p3 buf 
  and a4 = p4 buf 
  and a5 = p5 buf 
  and a6 = p6 buf 
  in c a1 a2 a3 a4 a5 a6
  
let lwt_of_result = function 
| Ok v -> Lwt.return v
| Error e -> Lwt.fail @@ Exception e


let failw_with_not_impl () = raise @@ Exception `NotImplemented
