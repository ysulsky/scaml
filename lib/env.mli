open Core.Std

type t with sexp_of
val uvar : t Univ.Variant.t

exception Undefined_value of string with sexp

val empty : t

val combine  : initial:t -> additional:t -> t
val of_alist : (string * Univ.t) list -> t

val add     : t -> string -> Univ.t -> t
val add_ref : t -> string -> Univ.t ref -> t
val add_proc : t -> Procedure.t -> t

val get         : t -> string -> Univ.t option
val get_exn     : t -> string -> Univ.t
val get_ref     : t -> string -> Univ.t ref option
val get_ref_exn : t -> string -> Univ.t ref

