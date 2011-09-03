open Core.Std

exception Not_a_procedure of Sexp.t with sexp
exception Syntax_error    of Sexp.t * string with sexp
exception Type_error      of Sexp.t * [`expected of string] with sexp
exception Duplicate_argument_name 
  of [`procedure of string] * string with sexp
exception Statement_should_return_unit 
  of [`statement of Sexp.t] * [`in_body of Sexp.t] with sexp

let is_ident s = String.length s > 0 && s.[0] = ':'

module Primitive = struct
  type t = Lambda | Begin | Let | Let_star | Let_rec | If | Set
  let uvar = Univ.Variant.create "primitive"
  let env = 
    let prims = 
      Env.of_alist (List.map ~f:(fun (x,v) -> x, Univ.create uvar v) [
        "lambda", Lambda;
        "begin",  Begin;
        "let",    Let;
        "let*",   Let_star;
        "letrec", Let_rec;
        "if",     If;
        "set!",   Set;
      ]) in
    let p2 name f inputs output = Procedure.create2 ~name ~inputs ~output f in
    List.fold ~init:prims ~f:Env.add_proc [
      p2 "%+"  ( +  ) (Uvar.int,   Uvar.int)   Uvar.int;
      p2 "%-"  ( -  ) (Uvar.int,   Uvar.int)   Uvar.int;
      p2 "%*"  ( *  ) (Uvar.int,   Uvar.int)   Uvar.int;
      p2 "%/"  ( /  ) (Uvar.int,   Uvar.int)   Uvar.int;
      p2 "%+." ( +. ) (Uvar.float, Uvar.float) Uvar.float;
      p2 "%-." ( -. ) (Uvar.float, Uvar.float) Uvar.float;
      p2 "%*." ( *. ) (Uvar.float, Uvar.float) Uvar.float;
      p2 "%/." ( /. ) (Uvar.float, Uvar.float) Uvar.float;
    ]
end

let parse_let ~prim ~sexp =
  let usage () = 
    Syntax_error (sexp, "usage: ("^prim^" ((:x1 y1) ...) body ...)") 
  in
  function
  | (Sexp.List decls) :: body1 :: body_rest ->
    List.fold_right decls ~init:[] ~f:(fun x acc ->
      match x with
      | Sexp.List [Sexp.Atom var; value] when is_ident var ->
        (var, value) :: acc
      | _ -> raise (usage ())),
    body1, body_rest
  | _ -> raise (usage ())

let lambda_counter = ref 0

let rec eval env = function
| Sexp.List (Sexp.Atom prim :: body) as sexp ->
  begin match eval_prim env prim body with
  | None -> eval_no_prim env sexp
  | Some res -> res
  end
| sexp -> eval_no_prim env sexp

and eval_no_prim env = function
(* CR ysulsky: this atom parsing is terrible *)
| Sexp.Atom var when is_ident var -> Env.get_exn env var
| Sexp.Atom ""         -> Univ.create Uvar.string ""
| Sexp.Atom "#t"       -> Univ.create Uvar.bool true
| Sexp.Atom "#f"       -> Univ.create Uvar.bool false
| Sexp.Atom "%env"     -> Univ.create Env.uvar env
| Sexp.Atom atom       ->
  (try Univ.create Uvar.int (Int.of_string atom) with _ ->
    try Univ.create Uvar.float (Float.of_string atom) with _ ->
      Univ.create Uvar.string atom)
| Sexp.List []         -> Univ.create Uvar.unit ()
| Sexp.List (fn::args) ->
  (match Univ.get Procedure.uvar (eval env fn) with
  | None    -> raise (Not_a_procedure fn)
  | Some fn -> Procedure.apply fn (List.map ~f:(eval env) args))

and eval_prim env prim body =
  let sexp = Sexp.List (Sexp.Atom prim :: body) in
  let open Option.Monad_infix in
  Env.get env prim >>= Univ.get Primitive.uvar >>| function
  | Primitive.Lambda ->
    let usage () = Syntax_error (sexp, "usage: (lambda args body ...)") in
    begin match body with
    | args :: body ->
      let name = incr lambda_counter; sprintf "<lambda#%d>" !lambda_counter in
      let proc = make_proc ~name ~usage env args body in
      Univ.create Procedure.uvar proc
    | _ -> raise (usage ())
    end
  | Primitive.Begin ->
    begin match body with
    | body1 :: body_rest -> eval_progn ~sexp env body1 body_rest
    | [] -> raise (Syntax_error (sexp, "(begin ...) cannot be empty"))
    end
  | Primitive.If ->
    begin match body with
    | [condition; then_; else_] -> eval_if env ~condition ~then_ ~else_
    | [condition; then_]        -> eval_if env ~condition ~then_
    | _ -> raise (Syntax_error (sexp, "usage: (if <condition> <then> [else])"))
    end
  | Primitive.Set ->
    begin match body with
    | [Sexp.Atom var; value] when is_ident var -> eval_set env ~var ~value
    | _ -> raise (Syntax_error (sexp, "usage: (set! :<var> <value>)"))
    end
  | Primitive.Let ->
    let decls, body1, body_rest = parse_let ~prim ~sexp body in
    let env = List.fold decls ~init:env ~f:(fun env' (var, value) ->
      Env.add env' var (eval env value))
    in eval_progn ~sexp env body1 body_rest
  | Primitive.Let_star ->
    let decls, body1, body_rest = parse_let ~prim ~sexp body in
    let env = List.fold decls ~init:env ~f:(fun env  (var, value) ->
      Env.add env  var (eval env value))
    in eval_progn ~sexp env body1 body_rest
  | Primitive.Let_rec ->
    let decls, body1, body_rest = parse_let ~prim ~sexp body in
    let env, values =
      List.fold_right decls ~init:(env, [])
        ~f:(fun (ident, value) (env, values) -> 
          let r = ref Undefined.uval in 
          Env.add_ref env ident r,
          (r, value) :: values)
    in
    List.iter values ~f:(fun (ref, value) -> ref := eval env value);
    eval_progn ~sexp env body1 body_rest

and eval_if ~condition ~then_ ?else_ env =
  match Univ.get Uvar.bool (eval env condition) with
  | None -> raise (Type_error (condition, 
                               `expected (Univ.Variant.name Uvar.bool)))
  | Some true  -> eval env then_
  | Some false ->
    match else_ with 
    | None       -> Univ.create Uvar.unit ()
    | Some else_ -> eval env else_

and eval_set env ~var ~value =
  (Env.get_ref_exn env var) := eval env value;
  Univ.create Uvar.unit ()

and eval_progn ~sexp env body1 body_rest =
  match body_rest with
  | [] -> eval env body1
  | body2::rest -> 
    match Univ.get Uvar.unit (eval env body1) with
    | Some () -> eval_progn ~sexp env body2 rest
    | None -> 
      raise (Statement_should_return_unit (`statement body1, `in_body sexp))

and make_proc env ~name ~usage args body =
  let required, rest = 
    match args with
    | Sexp.List [] ->   [], None
    | Sexp.Atom rest -> [], Some rest
    | Sexp.List args ->
      match
        List.fold args ~init:(`no_dot, []) ~f:(fun acc x ->
          match (acc, x) with
          | (_,                 Sexp.List  _ )
          | ((`have_dot, _),    Sexp.Atom ".")
          | ((`have_rest _, _), Sexp.Atom  _ ) -> raise (usage ())
          | ((`no_dot, req),    Sexp.Atom ".") -> (`have_dot, req)
          | ((`no_dot, req),    Sexp.Atom  x ) -> (`no_dot, x::req)
          | ((`have_dot, req),  Sexp.Atom  x ) -> (`have_rest x, req))
      with 
      | (`have_dot,       _  ) -> raise (usage ())
      | (`no_dot,         req) -> List.rev req, None
      | (`have_rest rest, req) -> List.rev req, Some rest
  in
  let check_arg_symbol arg = 
    if not (is_ident arg) then 
      raise (Syntax_error (args, "parameters should begin with a colon (:)"))
  in
  List.iter required ~f:check_arg_symbol;
  Option.iter rest ~f:check_arg_symbol;
  Option.iter (List.find_a_dup ((Option.to_list rest) @ required)) ~f:(fun key->
    raise (Duplicate_argument_name (`procedure name, key)));
  let body1, body_rest =
    match body with body1::body_rest -> body1, body_rest | [] -> raise (usage())
  in
  let num_required = List.length required in
  Procedure.create ~name (fun values ->
    let env =
      let req_values, rest_values = List.split_n values num_required in
      let num_req_values = List.length req_values in
      if ((num_req_values<>num_required)||(rest=None && rest_values<>[])) then
        raise (Procedure.Arity_error 
                 (`procedure name,
                  `got (num_req_values + List.length rest_values), 
                  `expected num_required));
      let env_with_req_args =
        List.fold2_exn required req_values ~init:env ~f:Env.add
      in
      match rest with
      | None      -> env_with_req_args
      | Some rest -> 
        let rest_values = Univ.create Uvar.univ_list rest_values in
        Env.add env_with_req_args rest rest_values
    in
    eval_progn ~sexp:(Sexp.List body) env body1 body_rest)

let repl ?prompt ?(env = Primitive.env) ic oc =
  let flush_after ?nl f x =
    f oc x;
    Option.iter nl ~f:(fun () -> Out_channel.newline oc);
    Out_channel.flush oc
  in
  let rec loop () =
    Option.iter ~f:(flush_after Out_channel.output_string) prompt;
    let input = Sexp.input_sexp ic in
    (try
       flush_after ~nl:()
         Sexp.output_hum (Univ.sexp_of_t (eval env input))
     with exn ->
       let msg = "*** ERROR: " in
       Out_channel.output_string oc msg;
       flush_after ~nl:()
         (Sexp.output_hum_indent (String.length msg))
         (Exn.sexp_of_t exn));
    loop ()
  in loop ()

