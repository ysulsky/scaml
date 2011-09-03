open Core.Std

type t = { name : string; fn : Univ.t list -> Univ.t }
let sexp_of_t t = Sexp.Atom t.name
let uvar = Univ.Variant.create ~sexp_of:sexp_of_t "procedure"

let name  t = t.name
let apply t = t.fn


exception Argument_type_error of ([`procedure of string] 
                                  * [`argument of int]
                                  * [`expected of string]) with sexp

exception Arity_error of ([`procedure of string] 
                          * [`got of int] 
                          * [`expected of int]) with sexp

let get_arg ~name ~argnum typ arg =
  match Univ.get typ arg with
  | None -> raise (Argument_type_error (`procedure name,
                                        `argument argnum,
                                        `expected (Univ.Variant.name typ)))
  | Some x -> x

let arity_error ~name args n =
  raise (Arity_error (`procedure name, `got (List.length args), `expected n))

let create ~name fn = { name; fn }

let create0 ~name ~output f =
  create ~name (function 
    | []   -> Univ.create output (f ()) 
    | args -> arity_error ~name args 0)

let create1 ~name ~inputs ~output f =
  create ~name (function
    | [arg] -> Univ.create output (f (get_arg ~name ~argnum:1 inputs arg))
    | args  -> arity_error ~name args 1)

let create2 ~name ~inputs:(in1,in2) ~output f =
  create ~name (function
    | [arg1; arg2] -> 
      let arg1 = get_arg ~name ~argnum:1 in1 arg1 in
      let arg2 = get_arg ~name ~argnum:2 in2 arg2 in
      Univ.create output (f arg1 arg2)
    | args  -> arity_error ~name args 2)

let create3 ~name ~inputs:(in1,in2,in3) ~output f =
  create ~name (function
    | [arg1; arg2; arg3] -> 
      let arg1 = get_arg ~name ~argnum:1 in1 arg1 in
      let arg2 = get_arg ~name ~argnum:2 in2 arg2 in
      let arg3 = get_arg ~name ~argnum:3 in3 arg3 in
      Univ.create output (f arg1 arg2 arg3)
    | args  -> arity_error ~name args 3)

let create4 ~name ~inputs:(in1,in2,in3,in4) ~output f =
  create ~name (function
    | [arg1; arg2; arg3; arg4] -> 
      let arg1 = get_arg ~name ~argnum:1 in1 arg1 in
      let arg2 = get_arg ~name ~argnum:2 in2 arg2 in
      let arg3 = get_arg ~name ~argnum:3 in3 arg3 in
      let arg4 = get_arg ~name ~argnum:4 in4 arg4 in
      Univ.create output (f arg1 arg2 arg3 arg4)
    | args  -> arity_error ~name args 4)

let create5 ~name ~inputs:(in1,in2,in3,in4,in5) ~output f =
  create ~name (function
    | [arg1; arg2; arg3; arg4; arg5] -> 
      let arg1 = get_arg ~name ~argnum:1 in1 arg1 in
      let arg2 = get_arg ~name ~argnum:2 in2 arg2 in
      let arg3 = get_arg ~name ~argnum:3 in3 arg3 in
      let arg4 = get_arg ~name ~argnum:4 in4 arg4 in
      let arg5 = get_arg ~name ~argnum:5 in5 arg5 in
      Univ.create output (f arg1 arg2 arg3 arg4 arg5)
    | args  -> arity_error ~name args 5)

