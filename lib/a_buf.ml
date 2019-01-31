open Acommon.Result
open Identifiers

module Id = NumId.Make(Int64)

type t = { 
  id : Id.t;
  buffer : Bigstringaf.t;
  r_pos : int;
  w_pos : int;
  r_mark : int;
  w_mark : int;
  grow : int;
}

let compare a b = Id.compare a.id b.id 

let equal a b = Id.equal a.id b.id


let from_bytes ?(grow=0) bs =
  { 
    id = Id.next_id ();
    buffer = bs;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
    grow 
  }

let create ?(grow=0) len = from_bytes ~grow (Bigstringaf.create len)

let capacity buf = Bigstringaf.length buf.buffer

let clear buf = {buf with r_pos = 0; w_pos = 0;}

let expand n buf = 
  let nbuffer = Bigstringaf.create (n + capacity buf) in 
  Bigstringaf.blit buf.buffer ~src_off:0 nbuffer ~dst_off:0 ~len:buf.w_pos;
  {buf with buffer = nbuffer}

let r_pos buf = buf.r_pos

let set_r_pos i buf = 
  if i >= 0 && i <= buf.w_pos
  then return {buf with r_pos = i}
  else fail (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.set_rr_pos with %d out of (0 .. %d)" i buf.w_pos)))

let mark_r_pos buf = {buf with r_mark = buf.r_pos}

let reset_r_pos buf = {buf with r_pos = buf.r_mark}

let w_pos buf = buf.w_pos

let set_w_pos i buf = 
  if i >= buf.r_pos && i <= capacity buf
  then return {buf with w_pos = i}
  else fail (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.set_w_pos with %d out of (%d .. %d)" i buf.r_pos (capacity buf))))

let mark_w_pos buf = {buf with w_mark = buf.w_pos}

let reset_w_pos buf = {buf with w_pos = buf.w_mark}


let readable_bytes buf = (w_pos buf) - (r_pos buf)

let readable buf = readable_bytes buf > 0

let writable_bytes buf = (capacity buf) - (w_pos buf)

let writable buf = writable_bytes buf > 0


let skip n buf = set_w_pos (buf.r_pos + n) buf |> function
  | Ok buf -> return buf
  | Error _ ->  fail (`OutOfBounds (`Msg (Printf.sprintf "A_buf.skip %d" n)))


let rec blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bigstringaf.length src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst then
        begin
          return (Bigstringaf.blit src ~src_off:src_idx dst.buffer ~dst_off:dst_idx ~len)
        end
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "IOBuf.copy_from"))
        | n -> blit_from_bytes ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "IOBuf.copy_from"))
  
let blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= src.w_pos then
    begin
      return (Bigstringaf.blit src.buffer ~src_off:src_idx dst ~dst_off:dst_idx ~len)
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.copy_to"))


let read_char buf =
  if readable buf then
    begin
      let c = Bigstringaf.get buf.buffer buf.r_pos in
      return (c, {buf with r_pos = buf.r_pos+1})
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.read_char"))

let read_chars len buf = 
  if len >= 0 && len <= readable_bytes buf then
    begin
      let s = Bytes.create len in
      Bigstringaf.blit_to_bytes buf.buffer ~src_off:buf.r_pos s ~dst_off:0 ~len;
      return (Bytes.to_string s, {buf with r_pos = buf.r_pos + len})
    end 
  else 
    fail (`OutOfBounds (`Msg "A_buf.read_chars"))

let read_bytes len buf = 
  if len >= 0 && len <= readable_bytes buf then
    begin
      let dst = Bigstringaf.create len in
      Bigstringaf.blit buf.buffer ~src_off:buf.r_pos dst ~dst_off:0 ~len ;
      return (dst, {buf with r_pos = buf.r_pos + len})
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.read_bytes"))

let read_buf len buf = read_bytes len buf |> function 
  | Ok (bs, buf) -> return (from_bytes bs, buf)
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_buf"))


let get_char ~at buf =
  if at >= 0 && at + 1 <= buf.w_pos then
    begin
      return (Bigstringaf.get buf.buffer at)
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.get_char"))

let get_chars ~at len buf = 
  if at >= 0 && len >= 0 && at + len <= buf.w_pos then
    begin
      let s = Bytes.create len in
      Bigstringaf.blit_to_bytes buf.buffer ~src_off:at s ~dst_off:0 ~len;
      return (Bytes.to_string s)
    end 
  else 
    fail (`OutOfBounds (`Msg "A_buf.get_chars"))

let get_bytes ~at len buf = 
  if at >= 0 && len >= 0 && at + len <= buf.w_pos then
    begin
      let dst = Bigstringaf.create len in 
      Bigstringaf.blit buf.buffer ~src_off:at dst ~dst_off:0 ~len ;
      return dst
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.get_bytes"))

let get_buf ~at len buf = get_bytes ~at len buf |> function 
  | Ok bs -> return (from_bytes bs)
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.get_buf"))


let rec write_char c buf = 
  if writable buf then
    begin
      Bigstringaf.set buf.buffer buf.w_pos c;
      return { buf with w_pos = buf.w_pos + 1}
    end
  else
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "IOBuf.write_char"))
    | n -> write_char c (expand n buf) 

let rec write_chars s buf =     
  let len = String.length s in
  if len <= writable_bytes buf then
    begin
      Bigstringaf.blit_from_bytes (Bytes.of_string s) ~src_off:0 buf.buffer ~dst_off:buf.w_pos ~len;
      return {buf with w_pos = buf.w_pos + len}
    end 
  else 
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "IOBuf.write_chars"))
    | n -> write_chars s (expand n buf)

let rec write_bytes src buf  =
  let len = Bigstringaf.length src in
  if len <= writable_bytes buf then
    begin
      Bigstringaf.blit src ~src_off:0 buf.buffer ~dst_off:buf.w_pos ~len ;
      return {buf with w_pos = buf.w_pos + len}
    end
  else
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "IOBuf.write_bytes"))
    | n -> write_bytes src (expand n buf)

let rec write_buf src buf  =
  let len = readable_bytes src in
  if len <= writable_bytes buf then
    begin
      Bigstringaf.blit src.buffer ~src_off:src.r_pos buf.buffer ~dst_off:buf.w_pos ~len ;
      return {buf with w_pos = buf.w_pos + len}
    end
  else
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "IOBuf.write_buf"))
    | n -> write_buf src (expand n buf)


let rec set_char c ~at buf = 
  if at >= 0 then 
    begin
      if at + 1 <= capacity buf then
        begin
          Bigstringaf.set buf.buffer at c;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "IOBuf.set_char"))
        | n -> set_char ~at c (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "IOBuf.set_char"))

let rec set_chars s ~at buf = 
  if at >= 0 then 
    begin
      let len = String.length s in
      if at + len <= capacity buf then
        begin
          Bigstringaf.blit_from_bytes (Bytes.of_string s) ~src_off:0 buf.buffer ~dst_off:buf.w_pos ~len;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "IOBuf.set_chars"))
        | n -> set_chars ~at s (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "IOBuf.set_chars"))

let rec set_bytes src ~at buf = 
  if at >= 0 then 
    begin
      let len = Bigstringaf.length src in
      if at + len <= capacity buf then
        begin
          Bigstringaf.blit src ~src_off:0 buf.buffer ~dst_off:buf.w_pos ~len;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "IOBuf.set_bytes"))
        | n -> set_bytes ~at src (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "IOBuf.set_bytes"))

let rec set_buf src ~at buf = 
  if at >= 0 then 
    begin
      let len = readable_bytes src in
      if at + len <= capacity buf then
        begin
          Bigstringaf.blit src.buffer ~src_off:src.r_pos buf.buffer ~dst_off:buf.w_pos ~len;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "IOBuf.set_buf"))
        | n -> set_buf ~at src (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "IOBuf.set_buf"))


let hexdump ?separator:(sep="") buf =
  let rec hexdump buf idx =
    if idx < buf.w_pos then 
    (Printf.sprintf "%02x%s" (int_of_char @@ Bigstringaf.get buf.buffer idx) sep ) ^ (hexdump buf (idx+1))
    else "" in 
  hexdump buf 0
    
let to_string buf =
  "(r_pos: " ^ (string_of_int buf.r_pos) ^ ", w_pos: " ^ (string_of_int buf.w_pos) ^ " content: " ^ (hexdump buf ~separator:":")
