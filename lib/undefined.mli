open Core.Std

type t with sexp_of
val undefined : t

val uvar : t Univ.Variant.t
val uval : Univ.t

