type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t      

include Ordered.Comparable with type t := t

val create_bytes : ?grow:int -> int -> t
(** [create_bytes c] allocates a new A_bytes of bytes and of capacity [c]. *)

val create_bigstring : ?grow:int -> int -> t
(** [create_bigstring c] allocates a new A_bytes of bigstring and of capacity [c]. *)

val from_bytes : ?grow:int -> bytes -> t 
(** [from_bytes bs] creates an A_bytes by wrapping [bs].
    The capacity for the A_bytes will be set to the length of [bs]. *)

val from_bigstring : ?grow:int -> bigstring -> t 
(** [from_bigstring bs] creates an A_bytes by wrapping [bs].
    The capacity for the A_bytes will be set to the length of [bs]. *)

val wrap : ?grow:int -> t list -> t
(** [wrap bs] creates an A_bytes by wrapping the A_bytes in the list [bs].
    The capacity for the A_bytes will be set to the sum of the capacities 
    of the A_bytes in the list [bs].
    This operation involves NO COPY. Modifications on the resulting 
    A_bytes will modify the original A_bytes in the list [bs] and reverse. *)

val slice : int -> int -> t -> t
(** [slice from len bs] creates an A_bytes that wraps the subregion 
    of A_bytes [bs] of length [len] starting at index [from]. 
    This operation involves NO COPY. Modifications on the resulting 
    A_bytes will modify the original [bs] and reverse. 
    The resulting A_bytes is not expandable. It's reader position and 
    writer position are set to 0. *)

val capacity : t -> int
(** [capacity bs] returns the number of bytes (octets) the A_bytes 
    [bs] can contain. *)

val get_byte : at:int -> t -> byte
(** [get_byte ~at bs] gets a byte from [bs] at index [at]. *)

val get_bytes : at:int -> int -> t -> bytes
(** [get_bytes ~at n bs] gets [n] bytes from [bs] at index [at]. *)

val get_bigstring : at:int -> int -> t -> bigstring
(** [get_bigstring ~at n bs] gets [n] bytes from [bs] at index [at]. *)

val get_abytes : at:int -> int -> t -> t
(** [get_abytes ~at n bs] gets [n] bytes from [bs] at index [at]. *)


val set_byte : byte -> at:int -> t -> unit
(** [set_byte b ~at bs] sets the byte [b] in [bs] at index [at]. *)

val set_bytes : bytes -> at:int -> t -> unit
(** [set_bytes s ~at bs] sets the bytes [s] in [bs] at index [at]. *)

val set_bigstring : bigstring -> at:int -> t -> unit
(** [set_bigstring s ~at bs] sets the bytes [s] in [bs] at index [at]. *)

val set_abytes : t -> at:int -> t -> unit
(** [set_abytes s ~at bs] sets the bytes [s] in [bs] at index [at]. *)


val blit : src:t -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_bytes : src:bytes -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bytes : src:t -> src_idx:int -> dst:bytes -> dst_idx:int -> len:int -> unit
(** [blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_from_bigstring : src:bigstring -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> unit
(** [blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val blit_to_bigstring : src:t -> src_idx:int -> dst:bigstring -> dst_idx:int -> len:int -> unit
(** [blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)


val hexdump : ?separator:string -> t -> string
(** [hexdump bs] returns an hexadecimal representation of the bytes 
    in A_bytes [bs] from index 0 to writer position. *)

val to_string : t -> string