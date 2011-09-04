open Core.Std

type t with sexp_of
val uvar : t Univ.Variant.t

val name  : t -> string option
val code  : t -> Sexp.t option
val apply : t -> Univ.t list -> Univ.t

type proc_identifier = [`procedure of string
                       |`anonymous_procedure of Sexp.t
                       |`unknown_procedure] with sexp

val identifier : name:string option -> code:Sexp.t option -> proc_identifier

exception Argument_type_error of (proc_identifier
                                  * [`argument of int]
                                  * [`expected of string]) with sexp

exception Arity_error of (proc_identifier
                          * [`got of int] 
                          * [`expected of int]) with sexp

val create  : ?name:string -> ?code:Sexp.t
  -> (Univ.t list -> Univ.t)
  -> t

val create0 : ?name:string -> ?code:Sexp.t
  -> output:'a Univ.Variant.t
  -> (unit -> 'a) 
  -> t

val create1 : ?name:string -> ?code:Sexp.t
  -> inputs:'a Univ.Variant.t
  -> output:'b Univ.Variant.t
  -> ('a -> 'b)
  -> t

val create2 : ?name:string -> ?code:Sexp.t
  -> inputs:('a Univ.Variant.t * 'b Univ.Variant.t)
  -> output:'c Univ.Variant.t
  -> ('a -> 'b -> 'c)
  -> t

val create3 : ?name:string -> ?code:Sexp.t
  -> inputs:('a Univ.Variant.t * 'b Univ.Variant.t * 'c Univ.Variant.t)
  -> output:'d Univ.Variant.t
  -> ('a -> 'b -> 'c -> 'd)
  -> t

val create4 : ?name:string -> ?code:Sexp.t
  -> inputs:('a Univ.Variant.t * 'b Univ.Variant.t * 'c Univ.Variant.t
             * 'd Univ.Variant.t)
  -> output:'e Univ.Variant.t
  -> ('a -> 'b -> 'c -> 'd -> 'e)
  -> t

val create5 : ?name:string -> ?code:Sexp.t
  -> inputs:('a Univ.Variant.t * 'b Univ.Variant.t * 'c Univ.Variant.t
             * 'd Univ.Variant.t * 'e Univ.Variant.t)
  -> output:'f Univ.Variant.t
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> t
