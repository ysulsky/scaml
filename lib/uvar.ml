open Core.Std

module V = Univ.Variant

let unit   = V.create ~sexp_of:Unit.sexp_of_t   "unit"
let bool   = V.create ~sexp_of:Bool.sexp_of_t   "bool"
let char   = V.create ~sexp_of:Char.sexp_of_t   "char"
let int    = V.create ~sexp_of:Int.sexp_of_t    "int"
let string = V.create ~sexp_of:String.sexp_of_t "string"
let float  = V.create ~sexp_of:Float.sexp_of_t  "float"

let univ_list = V.create ~sexp_of:(List.sexp_of_t Univ.sexp_of_t) "univ list"
