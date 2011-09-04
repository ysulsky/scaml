open Core.Std

type t = private string

val is_valid : string -> bool
exception Invalid_identifier of string with sexp

include Identifiable.S with type identifiable := t
