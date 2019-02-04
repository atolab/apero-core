open Acommon.Result
open Identifiers

module Id = NumId.Make(Int64)

type byte = A_bytes.byte
type bigstring = A_bytes.bigstring
type t = { 
  id : Id.t;
  buffer : A_bytes.t;
  r_pos : int;
  w_pos : int;
  r_mark : int;
  w_mark : int;
}

let compare a b = Id.compare a.id b.id 

let equal a b = Id.equal a.id b.id

let from_a_bytes bs =
  { 
    id = Id.next_id ();
    buffer = bs;
    r_pos = 0;
    w_pos = A_bytes.capacity bs;
    r_mark = 0;
    w_mark = 0;
  }

let from_bytes ?(grow=0) bs = from_a_bytes (A_bytes.from_bytes ~grow  bs)

let from_bigstring ?(grow=0) bs = from_a_bytes (A_bytes.from_bigstring ~grow  bs)

let create_bigstring ?(grow=0) len =
  { 
    id = Id.next_id ();
    buffer = A_bytes.create_bigstring ~grow len;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
  }

let create_bytes ?(grow=0) len =
  { 
    id = Id.next_id ();
    buffer = A_bytes.create_bytes ~grow len;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
  }

let wrap ?(grow=0) bslist = 
  let a_bytes = List.map (fun buf -> buf.buffer) bslist in
  { 
    id = Id.next_id ();
    buffer = A_bytes.wrap ~grow a_bytes;
    r_pos = 0;
    w_pos = 0;
    r_mark = 0;
    w_mark = 0;
  }

let slice from len buf = match A_bytes.slice from len buf.buffer with 
  | Ok bs -> return 
    { 
      id = Id.next_id ();
      buffer = bs;
      r_pos = 0;
      w_pos = 0;
      r_mark = 0;
      w_mark = 0;
    } 
  | Error _ -> fail (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.slice")))

let capacity buf = A_bytes.capacity buf.buffer

let clear buf = {buf with r_pos = 0; w_pos = 0;}

let r_pos buf = buf.r_pos

let set_r_pos i buf = 
  if i >= 0 && i <= buf.w_pos
  then return {buf with r_pos = i}
  else fail (`OutOfBounds (`Msg (
    Printf.sprintf "A_buf.set_r_pos with %d out of (0 .. %d)" i buf.w_pos)))

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


let blit ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit ~src:src.buffer ~src_idx ~dst:dst.buffer ~dst_idx ~len

let blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit_from_bytes ~src ~src_idx ~dst:dst.buffer ~dst_idx ~len
  
let blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit_to_bytes ~src:src.buffer ~src_idx ~dst ~dst_idx ~len

let blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit_from_bigstring ~src ~src_idx ~dst:dst.buffer ~dst_idx ~len
  
let blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit_to_bigstring ~src:src.buffer ~src_idx ~dst ~dst_idx ~len

let blit_from_a_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit ~src ~src_idx ~dst:dst.buffer ~dst_idx ~len
  
let blit_to_a_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  A_bytes.blit ~src:src.buffer ~src_idx ~dst ~dst_idx ~len


let read_byte buf = A_bytes.get_byte ~at:buf.r_pos buf.buffer |> function 
  | Ok b -> return (b, {buf with r_pos = buf.r_pos+1})
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_byte"))

let read_bytes len buf = A_bytes.get_bytes ~at:buf.r_pos len buf.buffer |> function 
  | Ok bs -> return (bs, {buf with r_pos = buf.r_pos+len})
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_bytes"))

let read_a_bytes len buf = A_bytes.get_a_bytes ~at:buf.r_pos len buf.buffer |> function 
  | Ok bs -> return (bs, {buf with r_pos = buf.r_pos+len})
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_a_bytes"))

let read_bigstring len buf = A_bytes.get_bigstring ~at:buf.r_pos len buf.buffer |> function 
  | Ok bs -> return (bs, {buf with r_pos = buf.r_pos+len})
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_bigstring"))

let read_buf len buf = A_bytes.get_bigstring ~at:buf.r_pos len buf.buffer |> function 
  | Ok bs -> return (from_bigstring bs, {buf with r_pos = buf.r_pos+len})
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.read_buf"))


let get_byte ~at buf =
  if at + 1 <= buf.w_pos 
  then A_bytes.get_byte ~at buf.buffer
  else fail (`OutOfBounds (`Msg "A_buf.get_byte"))

let get_bytes ~at len buf = 
  if at + len <= buf.w_pos 
  then A_bytes.get_bytes ~at len buf.buffer
  else fail (`OutOfBounds (`Msg "A_buf.get_bytes"))

let get_a_bytes ~at len buf = 
  if at + len <= buf.w_pos 
  then A_bytes.get_a_bytes ~at len buf.buffer
  else fail (`OutOfBounds (`Msg "A_buf.get_a_bytes"))

let get_bigstring ~at len buf = 
  if at + len <= buf.w_pos
  then A_bytes.get_bigstring ~at len buf.buffer
  else fail (`OutOfBounds (`Msg "A_buf.get_bigstring"))

let get_buf ~at len buf = 
  if at + len <= buf.w_pos
  then A_bytes.get_bigstring ~at len buf.buffer |> function 
    | Ok bs -> return (from_bigstring bs)
    | Error _ -> fail (`OutOfBounds (`Msg "A_buf.get_buf"))
  else fail (`OutOfBounds (`Msg "A_buf.get_buf"))


let write_byte b buf = A_bytes.set_byte b ~at:buf.w_pos buf.buffer |> function 
  | Ok _ -> return {buf with w_pos = buf.w_pos+1}
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.write_byte"))

let write_bytes bs buf = A_bytes.set_bytes ~at:buf.w_pos bs buf.buffer |> function 
  | Ok _ -> return {buf with w_pos = buf.w_pos+(Bytes.length bs)}
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.write_bytes"))

let write_a_bytes bs buf = A_bytes.set_a_bytes ~at:buf.w_pos bs buf.buffer |> function 
  | Ok _ -> return {buf with w_pos = buf.w_pos+(A_bytes.capacity bs)}
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.write_a_bytes"))

let write_bigstring bs buf = A_bytes.set_bigstring ~at:buf.w_pos bs buf.buffer |> function 
  | Ok _ -> return {buf with w_pos = buf.w_pos+(Bigstringaf.length bs)}
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.write_bigstring"))

let write_buf bs buf = A_bytes.set_a_bytes ~at:buf.w_pos bs.buffer buf.buffer |> function 
  | Ok _ -> return {buf with w_pos = buf.w_pos+(capacity bs)}
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.write_buf"))


let set_byte b ~at buf = A_bytes.set_byte b ~at buf.buffer |> function 
  | Ok _ -> return buf
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.set_byte"))

let set_bytes bs ~at buf = A_bytes.set_bytes bs ~at buf.buffer |> function 
  | Ok _ -> return buf
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.set_bytes"))

let set_a_bytes bs ~at buf = A_bytes.set_a_bytes bs ~at buf.buffer |> function 
  | Ok _ -> return buf
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.set_a_bytes"))

let set_bigstring bs ~at buf = A_bytes.set_bigstring bs ~at buf.buffer |> function 
  | Ok _ -> return buf
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.set_bigstring"))

let set_buf bs ~at buf = A_bytes.set_a_bytes bs.buffer ~at buf.buffer |> function 
  | Ok _ -> return buf
  | Error _ -> fail (`OutOfBounds (`Msg "A_buf.set_buf"))


let hexdump ?separator:(sep="") buf = A_bytes.hexdump ~separator:sep buf.buffer
    
let to_string buf =
  "(r_pos: " ^ (string_of_int buf.r_pos) ^ ", w_pos: " ^ (string_of_int buf.w_pos) ^ " content: " ^ (hexdump buf ~separator:":")
