open Core.Std

type t = exn with sexp_of

module Variant = struct

  module Id : Unique_id = Unique_id.Int(Unit)

  module type S = sig
    type a
    val name : string
    val id   : Id.t
    val inject  : a -> t
    val project : t -> a option
  end

  type 'a t = (module S with type a = 'a)

  let name (type s) t =
    let module V = (val t : S with type a = s) in
    V.name

  let id (type s) t =
    let module V = (val t : S with type a = s) in
    V.id

  let create (type s) ?sexp_of name =
    let sexp_of_s = 
      Option.value ~default:(const (Sexp.Atom ("<"^name^">"))) sexp_of 
    in
    let module S = struct
      type a = s with sexp_of
      let sexp_of_a = sexp_of_a

      exception E of a with sexp
      let name = name
      let id   = Id.create ()
      let inject x = E x
      let project = function
      | E x -> Some x
      | _   -> None
    end in (module S : S with type a = s)
end 

let create (type s) variant x =
  let module V = (val variant : Variant.S with type a = s) in
  V.inject x

let get (type s) variant t =
  let module V = (val variant : Variant.S with type a = s) in
  V.project t

let sexp_of_t t =
  match sexp_of_t t with
  | Sexp.List [Sexp.Atom _; x] -> x
  | _ -> assert false

let uvar = Variant.create ~sexp_of:sexp_of_t "univ"
