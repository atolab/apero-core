include (module type of List)

val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> ('a list) * ('b list)
val to_string : ?sep:string -> 'a list -> ('a -> string) -> string


