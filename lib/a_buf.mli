open Atypes

type t      

include Ordered.Comparable with type t := t

val create : ?grow:int -> int -> t
(** [create c] allocates a new IOBuf of capacity [c]. *)

val from_bytes : ?grow:int -> Bigstringaf.t -> t 
(** [from_bytes bs] creates an A_buf by wrapping [bs].
    The capacity for the A_buf will be set to the length of [bs]. *)

val capacity : t -> int
(** [capacity buf] returns the number of bytes (octets) the buffer 
    [buf] can contain. *)

val clear : t -> t
(** [clear buf] sets the reader_index and writer_index of [buf] to 0. *)

val reader_index : t -> int
(** [reader_index buf] returns the reader_index of [buf]. *)

val set_reader_index : int -> t -> (t, error) result 
(** [set_reader_index i buf] sets the reader_index of [buf] to [i]. *)

val mark_reader_index : t -> t
(** [mark_reader_index buf] marks the current reader_index in the buffer [buf]. *)

val reset_reader_index : t -> t
(** [reset_reader_index buf] resets the reader_index of [buf] to the marked reader_index. 
    The marked reader_index is initially set to 0. *)

val writer_index : t -> int
(** [writer_index buf] returns the writer_index of [buf]. *)

val set_writer_index : int -> t -> (t, error) result 
(** [set_writer_index i buf] sets the writer_index of [buf] to [i]. *)

val mark_writer_index : t -> t
(** [mark_writer_index buf] marks the current writer_index in the buffer [buf]. *)

val reset_writer_index : t -> t
(** [reset_writer_index buf] resets the writer_index of [buf] to the marked writer_index. 
    The marked writer_index is initially set to 0. *)

val readable : t -> bool
(** [readable buf] returns true if and only if 
    ((writer_index [buf]) - (reader_index [buf])) is greater than 0. *)

val readable_bytes : t -> int
(** [readable_bytes buf] returns the number of readable bytes of [buf] 
    which is equal to ((writer_index [buf]) - (reader_index [buf])). *)

val writable : t -> bool
(** [readable buf] returns true if and only if 
    ((capacity [buf]) - (writer_index [buf])) is greater than 0. *)

val writable_bytes : t -> int
(** [writable_bytes buf] returns the number of writable bytes of [buf] 
    which is equal to ((capacity [buf]) - (writer_index [buf])). *)


val skip : int -> t -> (t, error) result
(** [skip n buf] increases the reader_index of [buf] of [n]. *)


val read_char : t -> ((char * t), error) result 
(** [read_char buf] gets a char from [buf] at reader_index and 
    increases the reader_index by 1 in [buf]. *)

val read_chars : int -> t -> ((string * t), error) result 
(** [read_chars n buf] gets [n] chars from [buf] at reader_index and 
    increases the reader_index by [n] in [buf]. *)

val read_bytes : int -> t -> (Bigstringaf.t * t, error) result 
(** [read_bytes n buf] gets [n] bytes from [buf] at reader_index and 
    increases the reader_index by [n] in [buf]. *)

val read_buf : int -> t -> (t * t, error) result 
(** [read_bytes n buf] gets [n] bytes from [buf] at reader_index and 
    increases the reader_index by [n] in [buf]. *)


val get_char : at:int -> t -> (char, error) result 
(** [get_char ~at buf] gets a char from [buf] at index [at]. *)

val get_chars : at:int -> int -> t -> (string, error) result 
(** [get_chars ~at n buf] gets [n] chars from [buf] at index [at]. *)

val get_bytes : at:int -> int -> t -> (Bigstringaf.t, error) result 
(** [get_bytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)

val get_buf : at:int -> int -> t -> (t, error) result 
(** [get_bytes ~at n buf] gets [n] bytes from [buf] at index [at]. *)


val write_char : char -> t -> (t, error) result 
(** [write_char c buf] sets the character [c] in [buf] at writer_index and 
    increases the writer_index by 1 in [buf]. *)

val write_chars : string -> t -> (t, error) result 
(** [write_chars s buf] sets the characters [s] in [buf] at writer_index and 
    increases the writer_index by (length [s]) in [buf]. *)

val write_bytes : Bigstringaf.t -> t -> (t, error) result 
(** [write_bytes bs buf] sets the bytes [bs] in [buf] at writer_index and 
    increases the writer_index by (length [bs]) in [buf]. *)

val write_buf : t -> t -> (t, error) result 
(** [write_buf bs buf] sets the bytes [bs] in [buf] at writer_index and 
    increases the writer_index by (readable_bytes [bs]) in [buf]. *)


val set_char : char -> at:int -> t -> (t, error) result 
(** [set_char c ~at buf] sets the character [c] in [buf] at index [at]. *)

val set_chars : string -> at:int -> t -> (t, error) result 
(** [set_chars s ~at buf] sets the characters [s] in [buf] at index [at]. *)

val set_bytes : Bigstringaf.t -> at:int -> t -> (t, error) result 
(** [set_bytes bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)

val set_buf : t -> at:int -> t -> (t, error) result 
(** [set_buf bs ~at buf] sets the bytes [bs] in [buf] at index [at]. *)


val copy_from : src:Bigstringaf.t -> src_idx:int -> dst:t -> dst_idx:int -> len:int -> (unit, error) result 
(** [copy_from ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)

val copy_to : src:t -> src_idx:int -> dst:Bigstringaf.t -> dst_idx:int -> len:int -> (unit, error) result 
(** [copy_to ~src ~src_idx ~dst ~dst_idx ~len] copies [len] bytes from [src] at index [src_idx] 
    to [dst] at index [dst_idx]. *)


val hexdump : ?separator:string -> t -> string
(** [hexdump buf] returns an hexadecimal representation of the bytes 
    in buffer [buf] from index 0 to writer_index. *)

val to_string : t -> string