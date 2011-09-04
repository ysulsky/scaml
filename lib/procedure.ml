open Core.Std

type t = { name : string option; 
           code : Sexp.t option; 
           fn   : Univ.t list -> Univ.t }
let sexp_of_t t =
  match t.name with 
  | None      -> Sexp.Atom ("#<procedure>")
  | Some name -> Sexp.Atom ("#<procedure:"^name^">")

let uvar = Univ.Variant.create ~sexp_of:sexp_of_t "procedure"

let name  t = t.name
let code  t = t.code
let apply t = t.fn

type proc_identifier = [`procedure of string
                       |`anonymous_procedure of Sexp.t
                       |`unknown_procedure] with sexp
let identifier ~name ~code =
  match name, code with
  | Some name, _    -> `procedure name
  | None, Some code -> `anonymous_procedure code
  | None, None      -> `unknown_procedure
  
exception Argument_type_error of (proc_identifier 
                                  * [`argument of int]
                                  * [`expected of string]) with sexp

exception Arity_error of (proc_identifier
                          * [`got of int] 
                          * [`expected of int]) with sexp

let get_arg ?name ?code ~argnum typ arg =
  match Univ.get typ arg with
  | None -> raise (Argument_type_error (identifier ~name ~code,
                                        `argument argnum,
                                        `expected (Univ.Variant.name typ)))
  | Some x -> x

let arity_error ?name ?code args n =
  raise (Arity_error (identifier ~name ~code, 
                      `got (List.length args), `expected n))

let create ?name ?code fn = { name; code; fn }

let create0 ?name ?code ~output f =
  create ?name ?code (function 
    | []   -> Univ.create output (f ()) 
    | args -> arity_error ?name ?code args 0)

let create1 ?name ?code ~inputs ~output f =
  create ?name ?code (function
    | [arg] -> Univ.create output (f (get_arg ?name ?code ~argnum:1 inputs arg))
    | args  -> arity_error ?name ?code args 1)

let create2 ?name ?code ~inputs:(in1,in2) ~output f =
  create ?name ?code (function
    | [arg1; arg2] -> 
      let arg1 = get_arg ?name ?code ~argnum:1 in1 arg1 in
      let arg2 = get_arg ?name ?code ~argnum:2 in2 arg2 in
      Univ.create output (f arg1 arg2)
    | args  -> arity_error ?name ?code args 2)

let create3 ?name ?code ~inputs:(in1,in2,in3) ~output f =
  create ?name ?code (function
    | [arg1; arg2; arg3] -> 
      let arg1 = get_arg ?name ?code ~argnum:1 in1 arg1 in
      let arg2 = get_arg ?name ?code ~argnum:2 in2 arg2 in
      let arg3 = get_arg ?name ?code ~argnum:3 in3 arg3 in
      Univ.create output (f arg1 arg2 arg3)
    | args  -> arity_error ?name ?code args 3)

let create4 ?name ?code ~inputs:(in1,in2,in3,in4) ~output f =
  create ?name ?code (function
    | [arg1; arg2; arg3; arg4] -> 
      let arg1 = get_arg ?name ?code ~argnum:1 in1 arg1 in
      let arg2 = get_arg ?name ?code ~argnum:2 in2 arg2 in
      let arg3 = get_arg ?name ?code ~argnum:3 in3 arg3 in
      let arg4 = get_arg ?name ?code ~argnum:4 in4 arg4 in
      Univ.create output (f arg1 arg2 arg3 arg4)
    | args  -> arity_error ?name ?code args 4)

let create5 ?name ?code ~inputs:(in1,in2,in3,in4,in5) ~output f =
  create ?name ?code (function
    | [arg1; arg2; arg3; arg4; arg5] -> 
      let arg1 = get_arg ?name ?code ~argnum:1 in1 arg1 in
      let arg2 = get_arg ?name ?code ~argnum:2 in2 arg2 in
      let arg3 = get_arg ?name ?code ~argnum:3 in3 arg3 in
      let arg4 = get_arg ?name ?code ~argnum:4 in4 arg4 in
      let arg5 = get_arg ?name ?code ~argnum:5 in5 arg5 in
      Univ.create output (f arg1 arg2 arg3 arg4 arg5)
    | args  -> arity_error ?name ?code args 5)
