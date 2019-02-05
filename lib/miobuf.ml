open Identifiers

module MIOBuf = struct

  module Id = NumId.Make(Int64)

  type t = 
    { buffer : Bytes.t
    ; mutable pos: int
    ; mutable limit : int
    ; capacity: int
    ; mutable mark : int
    ; id : Id.t
    ; grow : int
  }

  let compare a b = Id.compare a.id b.id 

  let equal a b = Id.equal a.id b.id

  let hexdump ?separator:(sep="") buf =
    let rec hexdump buf idx =
      if idx < buf.limit then 
        (Printf.sprintf "%02x%s" (int_of_char @@ Bytes.get buf.buffer idx) sep ) ^ (hexdump buf (idx+1)) 
      else "" in 
    hexdump buf 0
    
  let to_string buf =
    "(pos: " ^ (string_of_int buf.pos) ^ ", limit: "^ (string_of_int buf.limit) ^ " content: " ^ (hexdump buf ~separator:":")

  let create ?(grow=0) len =  
    Logs.debug (fun m -> m "IOBuf.create %d " len);
    { buffer = Bytes.create len;  pos = 0; limit = len; capacity = len; mark = 0; id = Id.next_id (); grow }

  let to_bytes buf = buf.buffer

  let from_bytes ?(grow=0) bs =
    let len = Bytes.length bs in
    { buffer = bs; pos =  0; limit = len; capacity = len; mark = 0; id = Id.next_id (); grow }

  let expand_buf orig n = 
    let nbuf = create ~grow:n (n + orig.capacity) in 
    Bytes.blit orig.buffer 0 nbuf.buffer 0 orig.limit;
    {nbuf with pos = orig.pos; limit = nbuf.capacity}
  
  let flip buf = 
    buf.limit <- buf.pos; 
    buf.pos <- 0 

  let clear buf = 
    buf.limit <- buf.capacity; 
    buf.pos <- 0 

  let rewind buf =  
    buf.pos <- 0 

  let capacity buf = buf.capacity

  let mark buf = 
    buf.mark <- buf.pos 

  let reset buf =  
    buf.pos <- buf.mark; 
    buf.mark <- 0 

  let position buf = buf.pos

  let set_position pos buf  =
    if pos >=0 && pos <= buf.limit
    then  buf.pos <- pos 
    else raise @@ Apero.Exception (`OutOfBounds (`Msg (Printf.sprintf "IOBuf.set_position with %d > %d" pos buf.limit)))

  let set_position_unsafe pos buf =  buf.pos <- pos 

  let limit buf = buf.limit

  let available buf = (buf.limit - buf.pos)

  let set_limit lim buf  =
    if lim >= buf.pos && lim <= buf.capacity
    then   buf.limit <- lim
    else raise @@ Apero.Exception (`OutOfBounds (`Msg (Printf.sprintf "IOBuf.set_limit with %d > %d" lim buf.capacity)))

  
  let reset_with pos lim buf =
    set_position pos buf ; 
    set_limit lim buf

  let rec put_char c buf = 
    if buf.pos < buf.limit then
      begin
        Bytes.set buf.buffer buf.pos c
      ;  buf.pos <- buf.pos + 1
      end
    else
      match buf.grow with 
      | 0 -> raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.put_char"))
      | n -> put_char c (expand_buf buf n)
        


  let get_char buf =
    if buf.pos < buf.limit then
      begin
        let c = Bytes.get buf.buffer buf.pos in
        buf.pos <- buf.pos + 1 ; c        
      end
    else 
      raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.get_char"))

  let rec blit_from_bytes bs ofs len  buf =
    if buf.pos + len < buf.limit then
      begin
        Bytes.blit bs ofs buf.buffer buf.pos len
      ;  buf.pos <- buf.pos + len 
      end
    else
      match buf.grow with 
      | 0 -> raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.blit_from_bytes"))
      | n -> blit_from_bytes bs ofs len (expand_buf buf n)
      

  let blit_to_bytes n buf = 
    if n <= available buf then 
      begin 
        let bs = Bytes.create n in         
        Bytes.blit buf.buffer (position buf) bs 0 n 
        ; buf.pos <-  buf.pos + n 
        ; bs
      end
    else 
      raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.blit_to_bytes"))
      

  
  (** Copies  [b.limit - b.pos] bytes from the [src] into [buf]*)
  let rec put_buf src buf  =
    let len = src.limit - src.pos in
    if  len <= (available buf) then
      begin
        Bytes.blit src.buffer src.pos buf.buffer buf.pos len ;
        buf.pos <- buf.pos + len
      end
    else
     match buf.grow with 
      | 0 -> raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.pub_buf"))
      | n -> put_buf src (expand_buf buf n)
      


  let get_buf len buf = 
    if len <= available buf then 
      let dst = create len in 
      Bytes.blit buf.buffer buf.pos dst.buffer 0 len 
      ; buf.pos <- buf.pos + len
      ; dst 
    else 
      raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.get_buf"))
  
  
  let rec put_string s buf =     
    let len = String.length s in
    if len <= available buf then
      begin
        Bytes.blit_string s 0 buf.buffer buf.pos len
        ; buf.pos <- buf.pos + len
      end 
    else 
      match buf.grow with 
      | 0 -> raise @@ Apero.Exception(`OutOfBounds (`Msg "IOBuf.put_sting"))
      | n -> put_string s (expand_buf buf n)
      

  
  let get_string len buf = 
    if len <= available buf then
      begin
        let s = Bytes.create len in
        Bytes.blit buf.buffer buf.pos s 0 len
        ; buf.pos <- buf.pos + len
        ; Bytes.to_string s
      end 
    else 
      raise @@ Apero.Exception (`OutOfBounds (`Msg "IOBuf.get_string"))

  let overwrite_at pos (f:t -> unit) buf = 
    let init_pos = position buf in
    set_position pos buf 
    ; f buf 
    ; set_position init_pos buf
    
end
