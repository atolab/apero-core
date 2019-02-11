include Atypes
include Acommon
include Iobuf
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



open Result
open Result.Infix
let encode_vle ?size v buf =
  let to_char l = char_of_int @@ Int64.to_int l in
  let rec put_positive_vle_rec v' ?size' buf =
    match size', v' with
    | Some(0), _ -> fail (`OutOfBounds (`Msg (Printf.sprintf "encode_vle: cannot encode %Ld as a %d bytes VLE" v (Option.get size))))
    | None, v' when v' <= Vle.byte_mask -> IOBuf.put_char (to_char v') buf
    | Some(1), v' when v' <= Vle.byte_mask -> IOBuf.put_char (to_char v') buf
    | _, v' ->
      let mv = Int64.logor Vle.more_bytes_flag @@ Int64.logand v' Vle.byte_mask in
      let b = IOBuf.put_char (to_char mv) buf in
      let sv = Int64.shift_right v' Vle.shift_len in
      let size' = Option.map size' (fun s -> s-1) in
      b >>= put_positive_vle_rec sv ?size'
  in
  if v < 0L then fail (`OutOfRange (`Msg "encode_vle: integer to encode must be positive"))
  else put_positive_vle_rec v ?size':size buf

let decode_vle buf =
  let from_char c = Vle.of_int (int_of_char c) in
  let masked_from_char c = Vle.logand Vle.byte_mask  (Vle.of_int (int_of_char c)) in
  let merge v c n = Vle.logor v (Vle.shift_left c (n * Vle.shift_len)) in
  let rec decode_vle_rec  v n buf =
    if n < Vle.max_bytes then
      begin
        IOBuf.get_char buf
        >>= (fun (c, buf) -> 
          if (from_char c) <= Vle.byte_mask then  return ((merge v (masked_from_char c) n), buf)
          else decode_vle_rec (merge v (masked_from_char c) n) (n+1) buf
        )        
      end
    else
      begin
        let rec skip k buf =
          IOBuf.get_char buf
          >>= (fun (c, buf)  -> 
          if from_char c <= Vle.byte_mask then fail (`OutOfBounds (`Msg "vle out of bounds"))
          else skip (k+1) buf )
        in skip n buf
      end
  in decode_vle_rec 0L 0 buf

let encode_bytes src dst =
  let n = IOBuf.available src in
  let m = IOBuf.available dst in
  if n <= m then
    begin
      encode_vle (Vle.of_int n) dst 
      >>= (IOBuf.put_buf src)      
    end
    else      
        fail (`OutOfBounds (`Msg (Printf.sprintf "encode_bytes failed because of bounds error %d < %d" n m)))    

  let decode_bytes buf =
    decode_vle buf
    >>= (fun (len, buf) -> IOBuf.get_buf (Vle.to_int len) buf)
      
  

let encode_string s buf =
  let len = String.length s in
  let bs = Lwt_bytes.of_string s in
  encode_vle (Vle.of_int len) buf
  >>= (IOBuf.blit_from_bytes bs 0 len)
    
let decode_string buf =
  decode_vle buf
  >>= (fun (vlen, buf) -> 
    let len =  Vle.to_int vlen in    
    IOBuf.blit_to_bytes len buf 
    >>= (fun (bs, buf) -> return (Lwt_bytes.to_string bs, buf)))
    

let decode_seq read buf  =
  let rec get_remaining seq length buf =
    match length with
    | 0 -> return (seq, buf)
    | _ ->
      read buf 
      >>= (fun (value, buf) -> get_remaining (value :: seq) (length - 1) buf)
  in
  decode_vle buf
  >>= (fun (length, buf) ->    
    (get_remaining  [] (Vle.to_int length) buf))

let encode_seq write seq buf =
  let rec put_remaining seq buf =
    match seq with
    | [] -> return buf
    | head :: rem -> 
        write head buf 
        >>= put_remaining rem 
  in
    (encode_vle (Vle.of_int (List.length seq)) buf)
    >>= put_remaining seq

let encode_seq_safe write seq buf =
  let rec put_remaining seq n buf =
    if (n = 0x3FFF) then
      (* note: 0x3FFF is the biggest length we can encode in a 2-bytes vle *)
      (buf, n, seq)
    else match seq with
    | [] -> (buf, n, [])
    | head :: rem ->
      let buf = IOBuf.mark buf in
      match write head buf with
      | Ok buf -> put_remaining rem (n+1) buf
      | Error _ -> (IOBuf.reset buf, n, seq)
  in
  (* reserve space for seq length as a 2-bytes vle *)
  let length_pos = IOBuf.position buf in
  (encode_vle ~size:2 0L buf)
  >>> put_remaining seq 0
  >>= fun (buf, n, remain) -> IOBuf.overwrite_at length_pos (encode_vle ~size:2 (Vle.of_int n)) buf
  >>> fun buf -> (buf, remain)

let read1_spec log p1 c buf =    
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
      return ((c a1),buf))

let read2_spec log p1 p2 c buf =   
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      return ((c a1 a2),buf)))

let read3_spec log p1 p2 p3 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
      return ((c a1 a2 a3),buf))))

let read4_spec log p1 p2 p3 p4 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
        p4 buf 
        >>= (fun (a4, buf) ->
        return ((c a1 a2 a3 a4),buf)))))

let read5_spec log p1 p2 p3 p4 p5 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
        p4 buf 
        >>= (fun (a4, buf) ->
          p5 buf 
          >>= (fun (a5, buf) ->
        return ((c a1 a2 a3 a4 a5),buf))))))


let read6_spec log p1 p2 p3 p4 p5 p6 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
        p4 buf 
        >>= (fun (a4, buf) ->
          p5 buf 
          >>= (fun (a5, buf) ->
            p6 buf 
            >>= (fun (a6, buf) ->
          return ((c a1 a2 a3 a4 a5 a6),buf)))))))

let lwt_of_result = function 
| Ok v -> Lwt.return v
| Error e -> Lwt.fail @@ Exception e


let failw_with_not_impl () = fail @@ Exception `NotImplemented
