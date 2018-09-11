(* open Types
open Iobuf
open Property

(** This module encodes and decodes primitive zeno types *)

val encode_vle : Vle.t -> IOBuf.t -> (IOBuf.t, error) result
val decode_vle : IOBuf.t -> ((Vle.t * IOBuf.t), error) result

val encode_bytes : IOBuf.t -> IOBuf.t -> (IOBuf.t, error) result
val decode_bytes : IOBuf.t -> (IOBuf.t * IOBuf.t, error) result

val encode_string : string -> IOBuf.t -> (IOBuf.t, error) result
val decode_string : IOBuf.t -> ((string * IOBuf.t), error) result

val encode_seq : ('a -> IOBuf.t -> (IOBuf.t, error) result) -> 'a list -> IOBuf.t -> (IOBuf.t, error) result
val decode_seq : (IOBuf.t -> ('a * IOBuf.t, error) result) ->  IOBuf.t -> ('a list * IOBuf.t, error) result

val decode_property : IOBuf.t -> ((Property.t * IOBuf.t), error) result
val encode_property : Property.t -> IOBuf.t -> (IOBuf.t, error) result *)
