open Core.Std

type t = unit
let sexp_of_t () = Sexp.Atom "#<undefined>"

let undefined = ()

let uvar = Univ.Variant.create ~sexp_of:sexp_of_t "undefined"
let uval = Univ.create uvar undefined

