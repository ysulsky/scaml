open Core.Std

exception Not_a_procedure of Sexp.t with sexp
exception Syntax_error    of Sexp.t * string with sexp
exception Type_error      of Sexp.t * [`expected of string] with sexp
exception Duplicate_argument_name 
  of [`procedure of string] * string with sexp
exception Statement_should_return_unit 
  of [`statement of Sexp.t] * [`in_body of Sexp.t] with sexp

module Primitive : sig
  val env : Env.t
end

val eval : Env.t -> Sexp.t -> Univ.t

val repl : ?prompt:string -> ?env:Env.t -> In_channel.t -> Out_channel.t -> unit

