open Atypes


(** The buffer used by zenoh used for I/O. This buffer has a position, a limit
    a capacity and potentially a mark. At any point in time the following
    invariant will hold 0 <= mark <= pos <= limit <= capacity.

    A buffer's limit is the index of the first element that should not
    be read or written.

    This buffer was designed for performance, additionally the only way 
    a method may fail is if it violates one of the invariants for the 
    type and in this case an exception is raised as this would cleary 
    represent a bug in the application using the buffer. *)

module IOBuf : sig  
  type t      

  include Ordered.Comparable with type t := t
  
  val create : ?grow:int -> int -> t
  (** [create] allocates a new IOBuf  of the given capacity. *)

  val to_bytes : t -> Bigstringaf.t
  (** [to_bytes] provides the [Lwt_bytes.t] representation for this buffer so that
      it can be used for I/O such as sockets, etc... This buffer should be
      considered as read-only. *)

  val from_bytes : ?grow:int -> Bigstringaf.t -> t
  (** [from_bytes] creates an IOBuf by wrapping the provided [Bigstringaf.t].
      The capacity for the IOBuf will be set to the buffer length. *)

  val flip : t -> t 
  (** [flip] sets the limit to the current position and the position to zero.
      This function is typically used after writing in a buffer, to make it ready for reading. *)

  val clear : t -> t
  (** [clear] sets the position to zero and the limit to the capacity. *)

  val rewind : t -> t
  (** [rewind] makes the buffer ready to be read again by setting the position
      to zero and keeping the limit as it is. *)

  val position : t -> int
  val set_position : int -> t -> (t, error) result 
  val set_position_unsafe : int -> t -> t

  val limit : t -> int
  val set_limit : int -> t ->  (t, error) result 

  val capacity : t -> int

  (** remaining bytes to read/write overflowing *)
  val available : t -> int

  val mark : t -> t
  val reset : t -> t
  val reset_with : int -> int -> t -> (t, error) result 

  val put_char : char -> t ->  (t, error) result 
  val get_char : t -> ((char * t), error) result 

  val blit_from_bytes : Bigstringaf.t -> int -> int ->  t -> (t, error) result

  val blit_to_bytes : int -> t ->  ((Bigstringaf.t * t), error) result

  val put_string : string -> t -> (t, error) result 
  
  val get_string : int -> t -> ((string * t), error) result

  val put_buf : t -> t -> (t, error) result 

  val get_buf : int -> t -> (t*t, error) result 

  val overwrite_at : int -> (t -> (t, error) result) -> t -> (t, error) result
  (** [overwrite_at pos f buf] temporary set the position of [buf] to [pos],
      applies [f] to [buf] and then reset its position to the initial value *)

  val hexdump : ?separator:string -> t -> string

  val to_string : t -> string

  type io_vector = Lwt_bytes.io_vector

  val to_io_vector : t -> io_vector

end
