open Core.Std

type t

module Variant : sig
  type 'a t

  module Id : Unique_id

  val create : string -> 'a t
  val name   : 'a t -> string
  val id     : int
end

val get : 'a Variant.t -> t -> 'a option
val create : 'a Variant.t -> 'a -> t
