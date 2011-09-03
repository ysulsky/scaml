open Core.Std

type t = Univ.t ref String.Map.t with sexp_of
let uvar = Univ.Variant.create ~sexp_of:sexp_of_t "environment"

exception Undefined_value of string with sexp
let value ~ident = function Some x -> x | None -> raise (Undefined_value ident)

let empty = String.Map.empty
let of_alist = 
  List.fold ~init:empty ~f:(fun t' (ident, value) ->
    String.Map.add t' ~key:ident ~data:(ref value))

let combine ~initial ~additional =
  String.Map.fold ~init:initial ~f:String.Map.add additional

let add_ref t s v = String.Map.add t ~key:s ~data:v
let add     t s v = add_ref t s (ref v)
let add_proc t proc = 
  add t (":"^Procedure.name proc) (Univ.create Procedure.uvar proc)
 
let get_ref     t s = String.Map.find t s
let get_ref_exn t s = value ~ident:s (get_ref t s)

let get     t s = match get_ref t s with None -> None | Some r -> Some !r
let get_exn t s = value ~ident:s (get t s)
