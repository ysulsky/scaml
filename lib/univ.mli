open Core.Std

type t with sexp_of

module Variant : sig
  type 'a t

  module Id : Unique_id

  val create : ?sexp_of:('a -> Sexp.t) -> string -> 'a t
  val name   : 'a t -> string
  val id     : 'a t -> Id.t
end

val get    : 'a Variant.t -> t -> 'a option
val create : 'a Variant.t -> 'a -> t
val uvar   :  t Variant.t
