open Acommon.Result
open Identifiers

module Id = NumId.Make(Int64)

type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type buffer = | Bytes of bytes | Bigstr of bigstring | Bufset of t list
and  t = { 
  id : Id.t;
  buffer : buffer;
  offset : int; 
  capacity : int; 
  grow : int;
}

let compare a b = Id.compare a.id b.id 

let equal a b = Id.equal a.id b.id


let from_bigstring ?(grow=0) bs =
  { 
    id = Id.next_id ();
    buffer = Bigstr bs;
    offset = 0;
    capacity = Bigstringaf.length bs;
    grow;
  }

let from_bytes ?(grow=0) bs =
  { 
    id = Id.next_id ();
    buffer = Bytes bs;
    offset = 0;
    capacity = Bytes.length bs;
    grow;
  }

let create_bigstring ?(grow=0) len = from_bigstring ~grow (Bigstringaf.create len)

let create_bytes ?(grow=0) len = from_bytes ~grow (Bytes.create len)

let capacity bs = bs.capacity

let wrap ?(grow=0) bslist = 
  { 
    id = Id.next_id ();
    buffer = Bufset bslist;
    offset = 0;
    capacity = List.fold_left (fun accu bs -> accu + capacity bs) 0 bslist;
    grow;
  }

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

let expand n bs = 
  match bs.buffer with 
  | Bytes _ -> wrap ~grow:n [bs; create_bytes n]
  | Bigstr _ -> wrap ~grow:n [bs; create_bigstring n]
  | Bufset b -> wrap ~grow:n (List.append b [create_bigstring n]) (* TODO *)

let rec blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bytes.length src && dst_idx >= 0 
  then 
    if dst_idx + len <= capacity dst 
    then
      match dst.buffer with 
      | Bytes b -> return (Bytes.blit src src_idx b (dst.offset + dst_idx) len)
      | Bigstr b -> return (Bigstringaf.blit_from_bytes src ~src_off:src_idx b ~dst_off:(dst.offset + dst_idx) ~len)
      | Bufset b -> 
        let rec blit_from_bytes_to_set ~src ~src_idx ~dst ~dst_idx ~len = 
          match dst with 
          | [] -> return ()
          | hd :: tl -> 
            if capacity hd > dst_idx 
            then 
              let hd_writable = capacity hd - dst_idx in 
              if hd_writable >= len
              then blit_from_bytes ~src ~src_idx ~dst:hd ~dst_idx ~len
              else blit_from_bytes ~src ~src_idx ~dst:hd ~dst_idx ~len:hd_writable |> function 
                    | Ok () -> blit_from_bytes_to_set ~src ~src_idx:(src_idx - hd_writable) ~dst:tl ~dst_idx:0 ~len:(len - hd_writable)
                    | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.get_bytes_from_set")) 

            else 
              blit_from_bytes_to_set ~src ~src_idx ~dst:tl ~dst_idx:(dst_idx - capacity hd) ~len
        in 
        blit_from_bytes_to_set ~src ~src_idx ~dst:b ~dst_idx ~len
    else
      match dst.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "A_bytes.blit_from_bytes"))
      | n -> blit_from_bytes ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
  else fail (`OutOfBounds (`Msg "A_bytes.blit_from_bytes"))
  
let rec blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src 
  then
    match src.buffer with 
    | Bytes b -> return (Bytes.blit b (src.offset + src_idx) dst dst_idx len)
    | Bigstr b -> return (Bigstringaf.blit_to_bytes b ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
    | Bufset b -> 
      let rec blit_set_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
        match src with 
        | [] -> return ()
        | hd :: tl -> 
          if capacity hd > src_idx 
          then 
            let hd_readable = capacity hd - src_idx in
            if hd_readable >= len
            then blit_to_bytes ~src:hd ~src_idx ~dst ~dst_idx ~len
            else blit_to_bytes ~src:hd ~src_idx ~dst ~dst_idx ~len:hd_readable |> function 
                  | Ok () -> blit_set_to_bytes ~src:tl ~src_idx:0 ~dst ~dst_idx:(dst_idx + hd_readable) ~len:(len - hd_readable)
                  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.blit_to_bytes")) 
          else 
            blit_set_to_bytes ~src:tl ~src_idx:(src_idx - capacity hd) ~dst ~dst_idx ~len
      in 
      blit_set_to_bytes ~src:b ~src_idx ~dst ~dst_idx ~len
  else 
    fail (`OutOfBounds (`Msg "A_bytes.blit_to_bytes")) 

let rec blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bigstringaf.length src && dst_idx >= 0 
  then 
    if dst_idx + len <= capacity dst 
    then
      match dst.buffer with 
      | Bytes b -> return (Bigstringaf.blit_to_bytes src ~src_off:src_idx b ~dst_off:(dst.offset + dst_idx) ~len)
      | Bigstr b -> return (Bigstringaf.blit src ~src_off:src_idx b ~dst_off:(dst.offset + dst_idx) ~len)
      | Bufset b -> 
        let rec blit_from_bigstring_to_set ~src ~src_idx ~dst ~dst_idx ~len = 
          match dst with 
          | [] -> return ()
          | hd :: tl -> 
            if capacity hd > dst_idx 
            then 
              let hd_writable = capacity hd - dst_idx in 
              if hd_writable >= len
              then blit_from_bigstring ~src ~src_idx ~dst:hd ~dst_idx ~len
              else blit_from_bigstring ~src ~src_idx ~dst:hd ~dst_idx ~len:hd_writable |> function 
                    | Ok () -> blit_from_bigstring_to_set ~src ~src_idx:(src_idx - hd_writable) ~dst:tl ~dst_idx:0 ~len:(len - hd_writable)
                    | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.blit_from_bigstring")) 
            else 
              blit_from_bigstring_to_set ~src ~src_idx ~dst:tl ~dst_idx:(dst_idx - capacity hd) ~len
        in 
        blit_from_bigstring_to_set ~src ~src_idx ~dst:b ~dst_idx ~len
    else
      match dst.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "A_bytes.blit_from_bigstring"))
      | n -> blit_from_bigstring ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
  else 
    fail (`OutOfBounds (`Msg "A_bytes.blit_from_bigstring"))
  
let rec blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src 
  then
    match src.buffer with 
    | Bytes b -> return (Bigstringaf.blit_from_bytes b ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
    | Bigstr b -> return (Bigstringaf.blit b ~src_off:(src.offset + src_idx) dst ~dst_off:dst_idx ~len)
    | Bufset b -> 
      let rec blit_set_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
        match src with 
        | [] -> return ()
        | hd :: tl -> 
          if capacity hd > src_idx 
          then 
            let hd_readable = capacity hd - src_idx in
            if hd_readable >= len
            then blit_to_bigstring ~src:hd ~src_idx ~dst ~dst_idx ~len:len
            else blit_to_bigstring ~src:hd ~src_idx ~dst ~dst_idx ~len:hd_readable |> function 
                  | Ok () -> blit_set_to_bigstring ~src:tl ~src_idx:0 ~dst ~dst_idx:(dst_idx + hd_readable) ~len:(len - hd_readable)
                  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.get_bytes_from_set")) 
          else 
            blit_set_to_bigstring ~src:tl ~src_idx:(src_idx - capacity hd) ~dst ~dst_idx ~len
      in 
      blit_set_to_bigstring ~src:b ~src_idx ~dst ~dst_idx ~len
  else 
    fail (`OutOfBounds (`Msg "A_bytes.blit_to_bigstring")) 

let rec blit ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src && dst_idx >= 0 
  then 
    if dst_idx + len <= capacity dst 
    then
      match src.buffer with 
      | Bytes b -> blit_from_bytes ~src:b ~src_idx:(src.offset + src_idx) ~dst ~dst_idx ~len
      | Bigstr b -> blit_from_bigstring ~src:b ~src_idx:(src.offset + src_idx) ~dst ~dst_idx ~len
      | Bufset b -> 
        let rec blit_fromto_set ~src ~src_idx ~dst ~dst_idx ~len = 
          match src with 
          | [] -> return ()
          | hd :: tl -> 
            if capacity hd > src_idx 
            then 
              let hd_readable = capacity hd - src_idx in
              if hd_readable >= len
              then blit ~src:hd ~src_idx ~dst ~dst_idx ~len:len
              else blit ~src:hd ~src_idx ~dst ~dst_idx ~len:hd_readable |> function 
                    | Ok () -> blit_fromto_set ~src:tl ~src_idx:0 ~dst ~dst_idx:(dst_idx + hd_readable) ~len:(len - hd_readable)
                    | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.blit")) 
            else 
              blit_fromto_set ~src:tl ~src_idx:(src_idx - capacity hd) ~dst ~dst_idx ~len
        in
        blit_fromto_set ~src:b ~src_idx ~dst ~dst_idx ~len
    else
      match dst.grow with 
      | 0 -> fail (`OutOfBounds (`Msg "A_bytes.blit"))
      | n -> blit ~src ~src_idx ~dst:(expand n dst) ~dst_idx ~len
  else fail (`OutOfBounds (`Msg "A_bytes.blit"))

let rec get_byte ~at bs =
  if at >= 0 && at + 1 <= capacity bs then
    begin
      (match bs.buffer with 
      | Bytes b -> return (Bytes.get b (bs.offset + at))
      | Bigstr b -> return (Bigstringaf.get b (bs.offset + at))
      | Bufset b -> 
        let rec get_byte_from_set at set = 
          match set with 
          | [] -> fail (`OutOfBounds (`Msg "A_bytes.get_byte"))
          | hd :: tl -> 
            if capacity hd > at 
            then get_byte ~at hd
            else get_byte_from_set (at - capacity hd) tl in 
        get_byte_from_set at b)
    end
  else 
    fail (`OutOfBounds (`Msg "A_bytes.get_byte"))

let get_bytes ~at len bs = 
  let dst = Bytes.create len in
  blit_to_bytes ~src:bs ~src_idx:at ~dst ~dst_idx:0 ~len |> function 
  | Ok () -> return dst
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.get_bytes"))

let get_bigstring ~at len bs = 
  let dst = Bigstringaf.create len in
  blit_to_bigstring ~src:bs ~src_idx:at ~dst ~dst_idx:0 ~len |> function 
  | Ok () -> return dst
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.get_bigstring"))

let get_a_bytes ~at len bs = get_bigstring ~at len bs |> function  (*TODO*)
  | Ok bigs -> return (from_bigstring bigs)
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.get_a_bytes"))


let rec set_byte c ~at bs = 
  if at >= 0 then 
    begin
      if at + 1 <= capacity bs then
        begin
          match bs.buffer with 
          | Bytes b -> Bytes.set b (bs.offset + at) c; return bs
          | Bigstr b -> Bigstringaf.set b (bs.offset + at) c; return bs
          | Bufset b -> 
            let rec set_byte_to_set at set = 
              match set with 
              | [] -> fail (`OutOfBounds (`Msg "A_bytes.set_byte"))
              | hd :: tl -> 
                if capacity hd > at 
                then set_byte c ~at hd
                else set_byte_to_set (at - capacity hd) tl in 
            set_byte_to_set at b
        end
      else
        match bs.grow with 
        | 0 -> fail (`OutOfBounds (`Msg "A_bytes.set_byte"))
        | n -> set_byte ~at c (expand n bs) 
    end
  else fail (`OutOfBounds (`Msg "A_bytes.set_byte"))

let set_bytes src ~at bs = 
  blit_from_bytes ~src ~src_idx:0 ~dst:bs ~dst_idx:at ~len:(Bytes.length src) |> function 
  | Ok () -> return bs
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.set_bytes"))

let set_bigstring src ~at bs = 
  blit_from_bigstring ~src ~src_idx:0 ~dst:bs ~dst_idx:at ~len:(Bigstringaf.length src) |> function 
  | Ok () -> return bs
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.set_bigstring"))

let set_a_bytes src ~at bs = 
  blit ~src ~src_idx:0 ~dst:bs ~dst_idx:at ~len:(capacity src) |> function 
  | Ok () -> return bs
  | Error _ -> fail (`OutOfBounds (`Msg "A_bytes.set_a_bytes"))


let hexdump ?separator:(sep="") bs =
  let rec hexdump bs idx =
    if idx < bs.capacity then 
    (Printf.sprintf "%02x%s" (get_byte ~at:idx bs |> Apero.Result.get |> int_of_char ) sep ) ^ (hexdump bs (idx+1))
    else "" in 
  hexdump bs 0
    
let to_string bs =
  "(capacity: " ^ (string_of_int bs.capacity) ^ " content: " ^ (hexdump bs ~separator:":") 
