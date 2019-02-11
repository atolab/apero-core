open Identifiers

module Id = NumId.Make(Int64)

type byte = Abytes.byte
type bigstring = Abytes.bigstring
type t = { 
  id : Id.t;
  buffer : Abytes.t;
  mutable r_pos : int;
  mutable w_pos : int;
  mutable r_mark : int;
  mutable w_mark : int;
}

let compare a b = Id.compare a.id b.id 

let equal a b = Id.equal a.id b.id

let from_abytes bs =
  { 
    id = Id.next_id ();
    buffer = bs;
    r_pos = 0;
    w_pos = Abytes.capacity bs;
    r_mark = 0;
    w_mark = 0;
  }

let from_bytes ?(grow=0) bs = from_abytes (Abytes.from_bytes ~grow  bs)

let from_bigstring ?(grow=0) bs = from_abytes (Abytes.from_bigstring ~grow  bs)

let create_bigstring ?(grow=0) len =
  { 
    id = Id.next_id ();
    buffer = Abytes.create_bigstring ~grow len;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
  }

let create_bytes ?(grow=0) len =
  { 
    id = Id.next_id ();
    buffer = Abytes.create_bytes ~grow len;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
  }

let wrap ?(grow=0) bslist = 
  let abytes = List.map (fun buf -> Abytes.slice buf.r_pos (buf.w_pos - buf.r_pos) buf.buffer) bslist in
  { 
    id = Id.next_id ();
    buffer = Abytes.wrap ~grow abytes;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
  }

let slice from len buf = 
    { 
      id = Id.next_id ();
      buffer =  Abytes.slice from len buf.buffer;
      r_pos = 0;
      w_pos = 0;
      r_mark = 0;
      w_mark = 0;
    } 
  
let capacity buf = Abytes.capacity buf.buffer

let clear buf = buf.r_pos <- 0; buf.w_pos <- 0

let r_pos buf = buf.r_pos

let set_r_pos i buf = 
  if i >= 0 && i <= buf.w_pos
  then buf.r_pos <- i
  else raise @@ Apero.Exception (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.set_r_pos with %d out of (0 .. %d)" i buf.w_pos)))

let mark_r_pos buf = buf.r_mark <- buf.r_pos

let reset_r_pos buf = buf.r_pos <- buf.r_mark

let w_pos buf = buf.w_pos

let set_w_pos i buf = 
  if i >= buf.r_pos && i <= capacity buf
  then buf.w_pos <-  i
  else raise @@ Apero.Exception (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.set_w_pos with %d out of (%d .. %d)" i buf.r_pos (capacity buf))))

let mark_w_pos buf = buf.w_mark <- buf.w_pos

let reset_w_pos buf = buf.w_pos <- buf.w_mark


let readable_bytes buf = (w_pos buf) - (r_pos buf)

let readable buf = readable_bytes buf > 0

let writable_bytes buf = (capacity buf) - (w_pos buf)

let writable buf = writable_bytes buf > 0


let skip n buf = set_r_pos (buf.r_pos + n) buf 

let blit ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit ~src:src.buffer ~src_idx ~dst:dst.buffer ~dst_idx ~len

let blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit_from_bytes ~src ~src_idx ~dst:dst.buffer ~dst_idx ~len
  
let blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit_to_bytes ~src:src.buffer ~src_idx ~dst ~dst_idx ~len

let blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit_from_bigstring ~src ~src_idx ~dst:dst.buffer ~dst_idx ~len
  
let blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit_to_bigstring ~src:src.buffer ~src_idx ~dst ~dst_idx ~len

let blit_from_abytes ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit ~src ~src_idx ~dst:dst.buffer ~dst_idx ~len
  
let blit_to_abytes ~src ~src_idx ~dst ~dst_idx ~len = 
  Abytes.blit ~src:src.buffer ~src_idx ~dst ~dst_idx ~len


let read_byte buf = 
  let b = Abytes.get_byte ~at:buf.r_pos buf.buffer in 
  buf.r_pos <- buf.r_pos+1 ; b
  

let read_bytes len buf = 
  let bs = Abytes.get_bytes ~at:buf.r_pos len buf.buffer in 
  buf.r_pos <- buf.r_pos + len ; bs
 
let read_abytes len buf = 
  let bs = Abytes.get_abytes ~at:buf.r_pos len buf.buffer in 
  buf.r_pos <- buf.r_pos+len; bs
  

let read_bigstring len buf = 
  let bs = Abytes.get_bigstring ~at:buf.r_pos len buf.buffer in 
  buf.r_pos <- buf.r_pos+len ; bs
  

let read_buf len buf = 
  let bs = from_bigstring @@ Abytes.get_bigstring ~at:buf.r_pos len buf.buffer in 
  buf.r_pos <- buf.r_pos+len; bs
   

let get_byte ~at buf =
  if at + 1 <= buf.w_pos 
  then Abytes.get_byte ~at buf.buffer
  else raise @@ Apero.Exception (`OutOfBounds (`Msg "A_buf.get_byte"))

let get_bytes ~at len buf = 
  if at + len <= buf.w_pos 
  then Abytes.get_bytes ~at len buf.buffer
  else raise @@ Apero.Exception (`OutOfBounds (`Msg "A_buf.get_bytes"))

let get_abytes ~at len buf = 
  if at + len <= buf.w_pos 
  then Abytes.get_abytes ~at len buf.buffer
  else raise @@ Apero.Exception (`OutOfBounds (`Msg "A_buf.get_abytes"))

let get_bigstring ~at len buf = 
  if at + len <= buf.w_pos
  then Abytes.get_bigstring ~at len buf.buffer
  else raise @@ Apero.Exception (`OutOfBounds (`Msg "A_buf.get_bigstring"))

let get_buf ~at len buf = 
  if at + len <= buf.w_pos
  then from_bigstring @@ Abytes.get_bigstring ~at len buf.buffer     
  else raise @@ Apero.Exception (`OutOfBounds (`Msg "A_buf.get_buf"))


let write_byte b buf = 
  Abytes.set_byte b ~at:buf.w_pos buf.buffer ;
  buf.w_pos <- buf.w_pos+1
  

let write_bytes bs buf = 
  Abytes.set_bytes ~at:buf.w_pos bs buf.buffer ;
  buf.w_pos <- buf.w_pos+(Bytes.length bs)

let write_abytes bs buf = 
  Abytes.set_abytes ~at:buf.w_pos bs buf.buffer ;
  buf.w_pos <- buf.w_pos+(Abytes.capacity bs)
  
let write_bigstring bs buf = 
  Abytes.set_bigstring ~at:buf.w_pos bs buf.buffer ;
  buf.w_pos <- buf.w_pos+(Bigstringaf.length bs)
  
let write_buf bs buf = 
  Abytes.set_abytes ~at:buf.w_pos bs.buffer buf.buffer ;
  buf.w_pos <- buf.w_pos+(capacity bs)

let set_byte b ~at buf = Abytes.set_byte b ~at buf.buffer 
  
let set_bytes bs ~at buf = Abytes.set_bytes bs ~at buf.buffer 

let set_abytes bs ~at buf = Abytes.set_abytes bs ~at buf.buffer 

let set_bigstring bs ~at buf = Abytes.set_bigstring bs ~at buf.buffer 

let set_buf bs ~at buf = Abytes.set_abytes bs.buffer ~at buf.buffer 

let to_io_vecs ?(offset=0) ~append_bytes ~append_bigarray io_vecs buf = 
  Abytes.to_io_vecs ~offset:(r_pos buf + offset) ~len:(readable_bytes buf - offset) ~append_bytes ~append_bigarray io_vecs buf.buffer


let hexdump ?separator:(sep="") buf = Abytes.hexdump ~separator:sep buf.buffer
    
let to_string buf =
  "(r_pos: " ^ (string_of_int buf.r_pos) ^ ", w_pos: " ^ (string_of_int buf.w_pos) ^ " content: " ^ (hexdump buf ~separator:":")
