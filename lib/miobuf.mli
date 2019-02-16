
(** The buffer used by zenoh used for I/O. This buffer has a position, a limit
    a capacity and potentially a mark. At any point in time the following
    invariant will hold 0 <= mark <= pos <= limit <= capacity.

    A buffer's limit is the index of the first element that should not
    be read or written.

    This buffer was designed for performance, additionally the only way 
    a method may fail is if it violates one of the invariants for the 
    type and in this case an exception is raised as this would cleary 
    represent a bug in the application using the buffer. *)

module MIOBuf : sig  
  type t      

  include Ordered.Comparable with type t := t
  
  val create : ?grow:int -> int -> t
  (** [create] allocates a new IOBuf  of the given capacity. *)

  val to_bytes : t -> Bytes.t
  (** [to_bytes] provides the [Lwt_bytes.t] representation for this buffer so that
      it can be used for I/O such as sockets, etc... This buffer should be
      considered as read-only. *)

  val from_bytes : ?grow:int -> Bytes.t -> t 
  (** [from_bytes] creates an IOBuf by wrapping the provided [Lwt_bytes.t].
      The capacity for the IOBuf will be set to the buffer length. *)

  val flip : t -> unit
  (** [flip] sets the limit to the current position and the position to zero.
      This function is typically used after writing in a buffer, to make it ready for reading. *)

  val clear : t -> unit
  (** [clear] sets the position to zero and the limit to the capacity. *)

  val rewind : t -> unit
  (** [rewind] makes the buffer ready to be read again by setting the position
      to zero and keeping the limit as it is. *)

  val position : t -> int
  val set_position : int -> t -> unit 
  val set_position_unsafe : int -> t -> unit

  val limit : t -> int
  val set_limit : int -> t ->  unit

  val capacity : t -> int

  (** remaining bytes to read/write overflowing *)
  val available : t -> int

  val mark : t -> unit
  val reset : t -> unit
  val reset_with : int -> int -> t -> unit

  val put_char : char -> t ->  unit
  val get_char : t -> char

  val blit_from_bytes : Lwt_bytes.t -> int -> int ->  t -> unit

  val blit_to_bytes : int -> t ->  Lwt_bytes.t

  val put_string : string -> t -> unit
  
  val get_string : int -> t -> string

  val put_buf : t -> t -> unit

  val get_buf : int -> t -> t

  val overwrite_at : int -> (t -> unit) -> t -> unit
  (** [overwrite_at pos f buf] temporary set the position of [buf] to [pos],
      applies [f] to [buf] and then reset its position to the initial value *)

  val hexdump : ?separator:string -> t -> string

  val to_string : t -> string
    
  type io_vector = Lwt_bytes.io_vector

  val to_io_vector : t -> io_vector
end
