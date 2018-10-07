module type S = sig 
  type t 
  val zero : t
  val one : t
  val add : t -> t -> t 
  val next_id : unit -> t
  val compare : t -> t -> int  
  val equal : t -> t -> bool
  val show : t -> string
  val to_string : t -> string
  val of_string : string -> t 
  val of_string_opt : string -> t option 
end 

module type IdSignature = sig
  type t 
  val zero : t 
  val one : t 
  val add : t -> t -> t  
  val equal : t -> t -> bool  
  val compare : t -> t -> int  
  val to_string : t -> string
  val of_string : string -> t 
  val of_string_opt : string -> t option   
end


module Make(T : IdSignature) : S