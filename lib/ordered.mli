
module type Comparable = sig 
  type t 
  val compare : t -> t -> int      
end

module Ordered : sig 

  module type S = sig 
    type t 
    include Comparable with type t := t
    val equal : t -> t -> bool

    module Infix : sig 
      val (=) : t -> t -> bool
      val (>) : t -> t -> bool
      val (>=) : t -> t -> bool
      val (<) : t -> t -> bool
      val (<=) : t -> t -> bool
      val (<>) : t -> t -> bool
    end
  end

  module Make (C : Comparable) : S with type t = C.t

end
