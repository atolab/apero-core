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
    grow;
  }

let from_bytes ?(grow=0) bs = 
  let len = Bytes.length bs in 
  let bigs = Bigstringaf.create len in
  Bigstringaf.blit_from_bytes bs ~src_off:0 bigs ~dst_off:0 ~len;
  from_bigstring ~grow bigs

let create ?(grow=0) len = from_bigstring ~grow (Bigstringaf.create len)

let slice from len bs = 
  if from > 0 && len > 0 && (from + len) <= bs.capacity
  then return 
    { 
      id = Id.next_id ();
      buffer = bs.buffer;
      offset = bs.offset + from;
      capacity = len;
      grow = 0;
    }
  else fail (`OutOfBounds (`Msg (
    Printf.sprintf "A_bytes.slice")))

let capacity bs = bs.capacity

let expand n bs = 
  let nbuffer = Bigstringaf.create (n + capacity bs) in 
  Bigstringaf.blit bs.buffer ~src_off:0 nbuffer ~dst_off:0 ~len:bs.capacity;
  {bs with buffer = nbuffer}

let rec blit ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst then
        begin
          return (Bigstringaf.blit src.buffer ~src_off:(src.offset + src_idx) dst.buffer ~dst_off:(dst.offset + dst_idx) ~len)
        end
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.blit"))
        | n -> blit ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "A_bytes.blit"))

let rec blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bytes.length src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst then
        begin
          return (Bigstringaf.blit_from_bytes src ~src_off:src_idx dst.buffer ~dst_off:(dst.offset + dst_idx) ~len)
        end
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.blit_from_bytes"))
        | n -> blit_from_bytes ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "A_bytes.blit_from_bytes"))
  
let blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src then
    begin
      return (Bigstringaf.blit_to_bytes src.buffer ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
    end
  else 
    fail (`OutOfBounds (`Msg "A_bytes.blit_to_bytes")) 

let rec blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bigstringaf.length src && dst_idx >= 0 then 
    begin
      if dst_idx + len <= capacity dst then
        begin
          return (Bigstringaf.blit src ~src_off:src_idx dst.buffer ~dst_off:(dst.offset + dst_idx) ~len)
        end
      else
        match dst.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.blit_from_bigstring"))
        | n -> blit_from_bigstring ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
    end
  else fail (`OutOfBounds (`Msg "A_bytes.blit_from_bigstring"))
  
let blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src then
    begin
      return (Bigstringaf.blit src.buffer ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
    end
  else 
    fail (`OutOfBounds (`Msg "A_bytes.blit_to_bigstring")) 


let get_byte ~at bs =
  if at >= 0 && at + 1 <= capacity bs then
    begin
      return (Bigstringaf.get bs.buffer (bs.offset + at))
    end
  else 
    fail (`OutOfBounds (`Msg "A_bytes.get_byte"))

let get_bytes ~at len bs = 
  if at >= 0 && len >= 0 && at + len <= capacity bs then
    begin
      let s = Bytes.create len in
      Bigstringaf.blit_to_bytes bs.buffer ~src_off:(bs.offset + at) s ~dst_off:0 ~len;
      return s
    end 
  else 
    fail (`OutOfBounds (`Msg "A_bytes.get_bytes"))

let get_bigstring ~at len bs = 
  if at >= 0 && len >= 0 && at + len <= capacity bs then
    begin
      let dst = Bigstringaf.create len in 
      Bigstringaf.blit bs.buffer ~src_off:(bs.offset + at) dst ~dst_off:0 ~len ;
      return dst
    end
  else 
    fail (`OutOfBounds (`Msg "A_bytes.get_bigstring"))

let get_a_bytes ~at len bs = get_bigstring ~at len bs |> function 
  | Ok bigs -> return (from_bigstring bigs)
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.get_a_bytes"))


let rec set_byte c ~at bs = 
  if at >= 0 then 
    begin
      if at + 1 <= capacity bs then
        begin
          Bigstringaf.set bs.buffer (bs.offset + at) c;
          return bs
        end
      else
        match bs.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.set_byte"))
        | n -> set_byte ~at c (expand n bs) 
    end
  else fail (`OutOfBounds (`Msg "A_bytes.set_byte"))

let rec set_bytes s ~at bs = 
  if at >= 0 then 
    begin
      let len = Bytes.length s in
      if at + len <= capacity bs then
        begin
          Bigstringaf.blit_from_bytes s ~src_off:0 bs.buffer ~dst_off:(bs.offset + at) ~len;
          return bs
        end
      else
        match bs.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.set_bytes"))
        | n -> set_bytes ~at s (expand n bs) 
    end
  else fail (`OutOfBounds (`Msg "A_bytes.set_bytes"))

let rec set_bigstring src ~at bs = 
  if at >= 0 then 
    begin
      let len = Bigstringaf.length src in
      if at + len <= capacity bs then
        begin
          Bigstringaf.blit src ~src_off:0 bs.buffer ~dst_off:(bs.offset + at) ~len;
          return bs
        end
      else
        match bs.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.set_bigstring"))
        | n -> set_bigstring ~at src (expand n bs) 
    end
  else fail (`OutOfBounds (`Msg "A_bytes.set_bytes"))

let rec set_a_bytes src ~at bs = 
  if at >= 0 then 
    begin
      let len = capacity src in
      if at + len <= capacity bs then
        begin
          Bigstringaf.blit src.buffer ~src_off:(src.offset) bs.buffer ~dst_off:(bs.offset + at) ~len;
          return bs
        end
      else
        match bs.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.set_a_bytes"))
        | n -> set_a_bytes ~at src (expand n bs) 
    end
  else fail (`OutOfBounds (`Msg "A_bytes.set_a_bytes"))


let hexdump ?separator:(sep="") bs =
  let rec hexdump bs idx =
    if idx < bs.capacity then 
    (Printf.sprintf "%02x%s" (int_of_char @@ Bigstringaf.get bs.buffer (bs.offset + idx)) sep ) ^ (hexdump bs (idx+1))
    else "" in 
  hexdump bs 0
    
let to_string bs =
  "(capacity: " ^ (string_of_int bs.capacity) ^ " content: " ^ (hexdump bs ~separator:":")
