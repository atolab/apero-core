open Acommon.Result
open Identifiers

module Id = NumId.Make(Int64)

type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t = { 
  id : Id.t;
  buffer : Bigstringaf.t;
  offset : int; 
  capacity : int;
  r_pos : int;
  w_pos : int;
  r_mark : int;
  w_mark : int;
  grow : int;
}

let compare a b = Id.compare a.id b.id 

let equal a b = Id.equal a.id b.id


let from_bigstring ?(grow=0) bs =
  { 
    id = Id.next_id ();
    buffer = bs;
    offset = 0;
    capacity = Bigstringaf.length bs;
    r_pos = 0;
    w_pos = Bigstringaf.length bs;
    r_mark = 0;
    w_mark = 0;
    grow;
  }

let from_bytes ?(grow=0) bs = 
  let len = Bytes.length bs in 
  let bigs = Bigstringaf.create len in
  Bigstringaf.blit_from_bytes bs ~src_off:0 bigs ~dst_off:0 ~len;
  from_bigstring ~grow bigs

let create ?(grow=0) len =
  { 
    id = Id.next_id ();
    buffer = Bigstringaf.create len;
    offset = 0;
    capacity = len;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
    grow;
  }

let slice from len buf = 
  if from > 0 && len > 0 && (from + len) <= buf.capacity
  then return 
    { 
      id = Id.next_id ();
      buffer = buf.buffer;
      offset = buf.offset + from;
      capacity = len;
      r_pos = 0;
      w_pos = 0;
      r_mark = 0;
      w_mark = 0;
      grow = 0;
    }
  else fail (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.slice")))

let capacity buf = buf.capacity

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


let skip n buf = set_r_pos (buf.r_pos + n) buf |> function
  | Ok buf -> return buf
  | Error _ ->  fail (`OutOfBounds (`Msg (Printf.sprintf "A_buf.skip %d" n)))


let rec blit ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst 
      then
        return (Bigstringaf.blit src.buffer ~src_off:(src.offset + src_idx) dst.buffer ~dst_off:(dst.offset + dst_idx) ~len)
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.blit"))
        | n -> blit ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "A_buf.blit"))

let rec blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bytes.length src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst 
      then
          return (Bigstringaf.blit_from_bytes src ~src_off:src_idx dst.buffer ~dst_off:(dst.offset + dst_idx) ~len)
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.blit_from_bytes"))
        | n -> blit_from_bytes ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "A_buf.blit_from_bytes"))
  
let blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= src.w_pos 
  then
    return (Bigstringaf.blit_to_bytes src.buffer ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
  else 
    fail (`OutOfBounds (`Msg "A_buf.blit_to_bytes"))

let rec blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bigstringaf.length src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst 
      then
          return (Bigstringaf.blit src ~src_off:src_idx dst.buffer ~dst_off:(dst.offset + dst_idx) ~len)
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.blit_from_bigstring"))
        | n -> blit_from_bigstring ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "A_buf.blit_from_bigstring"))
  
let blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= src.w_pos 
  then
    return (Bigstringaf.blit src.buffer ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
  else 
    fail (`OutOfBounds (`Msg "A_buf.blit_to_bigstring"))


let read_byte buf =
  if readable buf then
    begin
      let c = Bigstringaf.get buf.buffer (buf.offset + buf.r_pos) in
      return (c, {buf with r_pos = buf.r_pos+1})
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.read_byte"))

let read_bytes len buf = 
  if len >= 0 && len <= readable_bytes buf then
    begin
      let bs = Bytes.create len in
      Bigstringaf.blit_to_bytes buf.buffer ~src_off:(buf.offset + buf.r_pos) bs ~dst_off:0 ~len;
      return (bs, {buf with r_pos = buf.r_pos + len})
    end 
  else 
    fail (`OutOfBounds (`Msg "A_buf.read_bytes"))

let read_bigstring len buf = 
  if len >= 0 && len <= readable_bytes buf then
    begin
      let dst = Bigstringaf.create len in
      Bigstringaf.blit buf.buffer ~src_off:(buf.offset + buf.r_pos) dst ~dst_off:0 ~len ;
      return (dst, {buf with r_pos = buf.r_pos + len})
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.read_bigstring"))

let read_buf len buf = read_bigstring len buf |> function 
  | Ok (bs, buf) -> return (from_bigstring bs, buf)
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_buf"))


let get_byte ~at buf =
  if at >= 0 && at + 1 <= buf.w_pos then
    begin
      return (Bigstringaf.get buf.buffer (buf.offset + at))
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.get_byte"))

let get_bytes ~at len buf = 
  if at >= 0 && len >= 0 && at + len <= buf.w_pos then
    begin
      let bs = Bytes.create len in
      Bigstringaf.blit_to_bytes buf.buffer ~src_off:(buf.offset + at) bs ~dst_off:0 ~len;
      return bs
    end 
  else 
    fail (`OutOfBounds (`Msg "A_buf.get_bytes"))

let get_bigstring ~at len buf = 
  if at >= 0 && len >= 0 && at + len <= buf.w_pos then
    begin
      let dst = Bigstringaf.create len in 
      Bigstringaf.blit buf.buffer ~src_off:(buf.offset + at) dst ~dst_off:0 ~len ;
      return dst
    end
  else 
    fail (`OutOfBounds (`Msg "A_buf.get_bigstring"))

let get_buf ~at len buf = get_bigstring ~at len buf |> function 
  | Ok bs -> return (from_bigstring bs)
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.get_buf"))


let rec write_byte b buf = 
  if writable buf then
    begin
      Bigstringaf.set buf.buffer (buf.offset + buf.w_pos) b;
      return { buf with w_pos = buf.w_pos + 1}
    end
  else
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "A_buf.write_byte"))
    | n -> write_byte b (expand n buf) 

let rec write_bytes bs buf =     
  let len = Bytes.length bs in
  if len <= writable_bytes buf then
    begin
      Bigstringaf.blit_from_bytes bs ~src_off:0 buf.buffer ~dst_off:(buf.offset + buf.w_pos) ~len;
      return {buf with w_pos = buf.w_pos + len}
    end 
  else 
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "A_buf.write_bytes"))
    | n -> write_bytes bs (expand n buf)

let rec write_bigstring src buf  =
  let len = Bigstringaf.length src in
  if len <= writable_bytes buf then
    begin
      Bigstringaf.blit src ~src_off:0 buf.buffer ~dst_off:(buf.offset + buf.w_pos) ~len ;
      return {buf with w_pos = buf.w_pos + len}
    end
  else
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "A_buf.write_bigstring"))
    | n -> write_bigstring src (expand n buf)

let rec write_buf src buf  =
  let len = readable_bytes src in
  if len <= writable_bytes buf then
    begin
      Bigstringaf.blit src.buffer ~src_off:(src.offset + src.r_pos) buf.buffer ~dst_off:(buf.offset + buf.w_pos) ~len ;
      return {buf with w_pos = buf.w_pos + len}
    end
  else
    match buf.grow with 
    | 0 -> fail (`OutOfBounds (`Msg "A_buf.write_buf"))
    | n -> write_buf src (expand n buf)


let rec set_byte b ~at buf = 
  if at >= 0 then 
    begin
      if at + 1 <= capacity buf then
        begin
          Bigstringaf.set buf.buffer (buf.offset + at) b;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.set_byte"))
        | n -> set_byte ~at b (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "A_buf.set_byte"))

let rec set_bytes bs ~at buf = 
  if at >= 0 then 
    begin
      let len = Bytes.length bs in
      if at + len <= capacity buf then
        begin
          Bigstringaf.blit_from_bytes bs ~src_off:0 buf.buffer ~dst_off:(buf.offset + at) ~len;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.set_bytes"))
        | n -> set_bytes ~at bs (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "A_buf.set_bytes"))

let rec set_bigstring src ~at buf = 
  if at >= 0 then 
    begin
      let len = Bigstringaf.length src in
      if at + len <= capacity buf then
        begin
          Bigstringaf.blit src ~src_off:0 buf.buffer ~dst_off:(buf.offset + at) ~len;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.set_bigstring"))
        | n -> set_bigstring ~at src (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "A_buf.set_bigstring"))

let rec set_buf src ~at buf = 
  if at >= 0 then 
    begin
      let len = readable_bytes src in
      if at + len <= capacity buf then
        begin
          Bigstringaf.blit src.buffer ~src_off:(src.offset + src.r_pos) buf.buffer ~dst_off:(buf.offset + at) ~len;
          return buf
        end
      else
        match buf.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_buf.set_buf"))
        | n -> set_buf ~at src (expand n buf) 
    end
  else fail (`OutOfBounds (`Msg "A_buf.set_buf"))


let hexdump ?separator:(sep="") buf =
  let rec hexdump buf idx =
    if idx < buf.w_pos then 
    (Printf.sprintf "%02x%s" (int_of_char @@ Bigstringaf.get buf.buffer (buf.offset + idx)) sep ) ^ (hexdump buf (idx+1))
    else "" in 
  hexdump buf 0
    
let to_string buf =
  "(r_pos: " ^ (string_of_int buf.r_pos) ^ ", w_pos: " ^ (string_of_int buf.w_pos) ^ " content: " ^ (hexdump buf ~separator:":")
