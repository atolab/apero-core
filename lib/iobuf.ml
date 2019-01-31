open Acommon.Result
open Acommon.Result.Infix
open Identifiers

module IOBuf = struct

  module Id = NumId.Make(Int64)
  module Buffer = Lwt_bytes
  type t = 
    { buffer : Buffer.t
    ; pos: int
    ; limit : int
    ; capacity: int
    ; mark : int
    ; id : Id.t
    ; grow : int
  }

  let compare a b = Id.compare a.id b.id 

  let equal a b = Id.equal a.id b.id

  let hexdump ?separator:(sep="") buf =
    let rec hexdump buf idx =
      if idx < buf.limit then 
        (Printf.sprintf "%02x%s" (int_of_char @@ Bigstringaf.get buf.buffer idx) sep ) ^ (hexdump buf (idx+1))
      else "" in 
    hexdump buf 0
    
  let to_string buf =
    "(pos: " ^ (string_of_int buf.pos) ^ ", limit: "^ (string_of_int buf.limit) ^ " content: " ^ (hexdump buf ~separator:":")

  let create ?(grow=0) len =  
    Logs.debug (fun m -> m "IOBuf.create %d " len);
    { buffer = Bigstringaf.create len;  pos = 0; limit = len; capacity = len; mark = 0; id = Id.next_id (); grow }

  let to_bytes buf = buf.buffer

  let from_bytes ?(grow=0) bs =
    let len = Bigstringaf.length bs in
    { buffer = bs; pos =  0; limit = len; capacity = len; mark = 0; id = Id.next_id (); grow }

  let expand_buf orig n = 
    let nbuf = create ~grow:n (n + orig.capacity) in 
    Bigstringaf.blit orig.buffer ~src_off:0 nbuf.buffer ~dst_off:0 ~len:orig.limit;
    {nbuf with pos = orig.pos; limit = nbuf.capacity}
  
  let flip buf = { buf with limit = buf.pos; pos = 0 }

  let clear buf = { buf with limit = buf.capacity; pos = 0 }

  let rewind buf =  { buf with pos = 0 }

  let capacity buf = buf.capacity

  let mark buf = { buf with mark = buf.pos }

  let reset buf =  { buf with pos = buf.mark; mark = 0 }

  let position buf = buf.pos

  let set_position pos buf  =
    if pos >=0 && pos <= buf.limit
    then  return { buf with pos = pos }
    else fail (`OutOfBounds (`Msg (Printf.sprintf "IOBuf.set_position with %d > %d" pos buf.limit)))

  let set_position_unsafe pos buf =  { buf with pos = pos }

  let limit buf = buf.limit

  let available buf = (buf.limit - buf.pos)

  let set_limit lim buf  =
    if lim >= buf.pos && lim <= buf.capacity
    then return { buf with limit = lim}
    else fail (`OutOfBounds (`Msg (Printf.sprintf "IOBuf.set_limit with %d > %d" lim buf.capacity)))

  
  let reset_with pos lim buf =
    set_position pos buf
    >>= (set_limit lim)

  let rec put_char c buf = 
    if buf.pos < buf.limit then
      begin
        Bigstringaf.set buf.buffer buf.pos c
      ; return { buf with pos = buf.pos + 1}
      end
    else
      match buf.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "IOBuf.put_char"))
      | n -> put_char c (expand_buf buf n)
        


  let get_char buf =
    if buf.pos < buf.limit then
      begin
        let c = Bigstringaf.get buf.buffer buf.pos in
        return (c, {buf with pos = buf.pos+1})
      end
    else 
      fail (`OutOfBounds (`Msg "IOBuf.get_char"))

  let rec blit_from_bytes bs ofs len  buf =
    if buf.pos + len < buf.limit then
      begin
        Bigstringaf.blit bs ~src_off:ofs buf.buffer ~dst_off:buf.pos ~len
      ; return { buf with pos = buf.pos + len }
      end
    else
      match buf.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "IOBuf.blit_from_bytes"))
      | n -> blit_from_bytes bs ofs len (expand_buf buf n)
      

  let blit_to_bytes n buf = 
    if n <= available buf then 
      begin 
        let bs = Bigstringaf.create n in
        Bigstringaf.blit buf.buffer ~src_off:(position buf) bs ~dst_off:0 ~len:n
        ; return (bs, { buf with pos = buf.pos + n })
      end
    else 
      fail (`OutOfBounds (`Msg "IOBuf.blit_to_bytes"))
      

    

  (** Copies  [b.limit - b.pos] bytes from the [src] into [buf]*)
  let rec put_buf src buf  =
    let len = src.limit - src.pos in
    if  len <= (available buf) then
      begin
        Bigstringaf.blit src.buffer ~src_off:src.pos buf.buffer ~dst_off:buf.pos ~len ;
        return { buf with pos = buf.pos + len}
      end
    else
     match buf.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "IOBuf.pub_buf"))
      | n -> put_buf src (expand_buf buf n)
      
    
  type io_vector = Lwt_bytes.io_vector

  let get_buf len buf = 
    if len <= available buf then 
      let dst = create len in 
      Bigstringaf.blit buf.buffer ~src_off:buf.pos dst.buffer ~dst_off:0 ~len ;
      return (dst, {buf with pos = buf.pos + len})
    else 
      fail (`OutOfBounds (`Msg "IOBuf.get_buf"))
  
  let to_io_vector buf =
    Lwt_bytes.{ iov_buffer = buf.buffer; iov_offset = buf.pos; iov_length = buf.limit; }

  let rec put_string s buf =     
    let len = String.length s in
    if len <= available buf then
      begin
        Bigstringaf.blit_from_bytes (Bytes.of_string s) ~src_off:0 buf.buffer ~dst_off:buf.pos ~len;
        return { buf with pos = buf.pos + len }
      end 
    else 
      match buf.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "IOBuf.put_sting"))
      | n -> put_string s (expand_buf buf n)
      

  
  let get_string len buf = 
    if len <= available buf then
      begin
        let s = Bytes.create len in
        Bigstringaf.blit_to_bytes buf.buffer ~src_off:buf.pos s ~dst_off:0 ~len;
        return (Bytes.to_string s, { buf with pos = buf.pos + len })
      end 
    else 
      fail (`OutOfBounds (`Msg "IOBuf.get_string"))

  let overwrite_at pos (f:t -> (t, Atypes.error) result) buf = 
    let init_pos = position buf in
    set_position pos buf >>= f >>= set_position init_pos

end
