open Core.Std

let is_valid = function
  | "" | ":" -> false 
  | s when s.[0] <> ':' -> false
  | _ -> true

exception Invalid_identifier of string with sexp

module T = struct
  type t = string
  let of_string s =
    if is_valid s then s else raise (Invalid_identifier s)
  let to_string = Fn.id
end

include T
include Identifiable.Of_stringable(struct include T type stringable = t end)
