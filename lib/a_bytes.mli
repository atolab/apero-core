open Atypes

type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t      

include Ordered.Comparable with type t := t

val create : ?grow:int -> int -> t
(** [create c] allocates a new A_bytes of capacity [c]. *)

val from_bytes : ?grow:int -> bytes -> t 
(** [from_bytes bs] creates an A_bytes by wrapping [bs].
    The capacity for the A_bytes will be set to the length of [bs]. *)

val from_bigstring : ?grow:int -> bigstring -> t 
(** [from_bigstring bs] creates an A_bytes by wrapping [bs].
    The capacity for the A_bytes will be set to the length of [bs]. *)

val slice : int -> int -> t -> (t, error) result 
(** [slice from len bs] creates an A_bytes that wraps the subregion 
    of A_bytes [bs] of length [len] starting at index [from]. 
    This operation involves NO COPY. Modifications on the resulting 
    A_bytes will modify the original [bs] and reverse. 
    The resulting A_bytes is not expandable. It's reader position and 
    writer position are set to 0. *)

val capacity : t -> int
(** [capacity bs] returns the number of bytes (octets) the A_bytes 
    [bs] can contain. *)

val get_byte : at:int -> t -> (byte, error) result 
(** [get_byte ~at bs] gets a byte from [bs] at index [at]. *)

val get_bytes : at:int -> int -> t -> (bytes, error) result 
(** [get_bytes ~at n bs] gets [n] bytes from [bs] at index [at]. *)

val get_bigstring : at:int -> int -> t -> (bigstring, error) result 
(** [get_bigstring ~at n bs] gets [n] bytes from [bs] at index [at]. *)

val get_a_bytes : at:int -> int -> t -> (t, error) result 
(** [get_a_bytes ~at n bs] gets [n] bytes from [bs] at index [at]. *)


val set_byte : byte -> at:int -> t -> (t, error) result 
(** [set_byte b ~at bs] sets the byte [b] in [bs] at index [at]. *)

val set_bytes : bytes -> at:int -> t -> (t, error) result 
(** [set_bytes s ~at bs] sets the bytes [s] in [bs] at index [at]. *)

val set_bigstring : bigstring -> at:int -> t -> (t, error) result 
(** [set_bigstring s ~at bs] sets the bytes [s] in [bs] at index [at]. *)

val set_a_bytes : t -> at:int -> t -> (t, error) result 
(** [set_a_bytes s ~at bs] sets the bytes [s] in [bs] at index [at]. *)


val blit : src:t -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_bytes : src:bytes -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bytes : src:t -> src_idx:int -> dst:bytes -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_bigstring : src:bigstring -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bigstring : src:t -> src_idx:int -> dst:bigstring -> dst_idx:int -> len:int -> (unit, error) result 
(** [blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)


val hexdump : ?separator:string -> t -> string
(** [hexdump bs] returns an hexadecimal representation of the bytes 
    in A_bytes [bs] from index 0 to writer position. *)

val to_string : t -> string