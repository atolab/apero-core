open Atypes

type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t      

include Ordered.Comparable with type t := t

val create : ?grow:int -> int -> t
(** [create c] allocates a new A_buf of capacity [c]. *)

val from_bytes : ?grow:int -> bytes -> t 
(** [from_bytes bs] creates an A_buf by wrapping [bs].
    The capacity for the A_buf will be set to the length of [bs]. *)

val from_bigstring : ?grow:int -> Bigstringaf.t -> t 
(** [from_bigstring bs] creates an A_buf by wrapping [bs].
    The capacity for the A_buf will be set to the length of [bs]. *)

val slice : int -> int -> t -> (t, error) result 
(** [slice from len buf] creates an A_buf that wraps the subregion 
    of buffer [buf] of length [len] starting at index [from]. 
    This operation involves NO COPY. Modifications on the resulting 
    buffer will modify the original [buf] and reverse. 
    The resulting buffer is not expandable. It's reader position and 
    writer position are set to 0. *)

val capacity : t -> int
(** [capacity buf] returns the number of bytes (octets) the buffer 
    [buf] can contain. *)

val clear : t -> t
(** [clear buf] sets the reader position and writer position of [buf] to 0. *)

val r_pos : t -> int
(** [r_pos buf] returns the reader position of [buf]. *)

val set_r_pos : int -> t -> (t, error) result 
(** [set_r_pos p buf] sets the reader position of [buf] to [r]. *)

val mark_r_pos : t -> t
(** [mark_r_pos buf] marks the current reader position in the buffer [buf]. *)

val reset_r_pos : t -> t
(** [reset_r_pos buf] resets the reader position of [buf] to the marked position. 
    The marked reader position is initially set to 0. *)

val w_pos : t -> int
(** [w_pos buf] returns the writer position of [buf]. *)

val set_w_pos : int -> t -> (t, error) result 
(** [set_w_pos p buf] sets the writer position of [buf] to [p]. *)

val mark_w_pos : t -> t
(** [mark_w_pos buf] marks the current writer position in the buffer [buf]. *)

val reset_w_pos : t -> t
(** [reset_w_pos buf] resets the writer position of [buf] to the marked position. 
    The marked writer position is initially set to 0. *)

val readable : t -> bool
(** [readable buf] returns true if and only if 
    ((w_pos [buf]) - (r_pos [buf])) is greater than 0. *)

val readable_bytes : t -> int
(** [readable_bytes buf] returns the number of readable bytes of [buf] 
    which is equal to ((w_pos [buf]) - (r_pos [buf])). *)

val writable : t -> bool
(** [writable buf] returns true if and only if 
    ((capacity [buf]) - (w_pos [buf])) is greater than 0. *)

val writable_bytes : t -> int
(** [writable_bytes buf] returns the number of writable bytes of [buf] 
    which is equal to ((capacity [buf]) - (w_pos [buf])). *)


val skip : int -> t -> (t, error) result
(** [skip n buf] increases the reader position by [n] in [buf]. *)


val read_byte : t -> ((byte * t), error) result 
(** [read_byte buf] gets a byte from [buf] at reader position and 
    increases the reader position by 1 in [buf]. *)

val read_bytes : int -> t -> ((bytes * t), error) result 
(** [read_bytes n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)

val read_bigstring : int -> t -> (Bigstringaf.t * t, error) result 
(** [read_bigstring n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)

val read_buf : int -> t -> (t * t, error) result 
(** [read_bytes n buf] gets [n] bytes from [buf] at reader position and 
    increases the reader position by [n] in [buf]. *)


val get_byte : at:int -> t -> (byte, error) result 
(** [get_byte ~at buf] gets a byte from [buf] at index [at]. *)

val get_bytes : at:int -> int -> t -> (bytes, error) result 
(** [get_bytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)

val get_bigstring : at:int -> int -> t -> (Bigstringaf.t, error) result 
(** [get_bigstring ~at n buf] gets [n] bytes from [buf] at index [at]. *)

val get_buf : at:int -> int -> t -> (t, error) result 
(** [get_bytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)


val write_byte : byte -> t -> (t, error) result 
(** [write_byte b buf] sets the byte [b] in [buf] at writer position and 
    increases the writer position by 1 in [buf]. *)

val write_bytes : bytes -> t -> (t, error) result 
(** [write_bytes bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (length [bs]) in [buf]. *)

val write_bigstring : Bigstringaf.t -> t -> (t, error) result 
(** [write_bigstring bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (length [bs]) in [buf]. *)

val write_buf : t -> t -> (t, error) result 
(** [write_buf bs buf] sets the bytes [bs] in [buf] at writer position and 
    increases the writer position by (readable_bytes [bs]) in [buf]. *)


val set_byte : byte -> at:int -> t -> (t, error) result 
(** [set_byte b ~at buf] sets the byte [b] in [buf] at index [at]. *)

val set_bytes : bytes -> at:int -> t -> (t, error) result 
(** [set_bytes bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)

val set_bigstring : Bigstringaf.t -> at:int -> t -> (t, error) result 
(** [set_bigstring bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)

val set_buf : t -> at:int -> t -> (t, error) result 
(** [set_buf bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)


val blit_from_bigstring : src:Bigstringaf.t -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bigstring : src:t -> src_idx:int -> dst:Bigstringaf.t -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)


val hexdump : ?separator:string -> t -> string
(** [hexdump buf] returns an hexadecimal representation of the bytes 
    in buffer [buf] from index 0 to writer position. *)

val to_string : t -> string