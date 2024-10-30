open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

let type_check expr = 
  match expr with
  Int x -> ("Int", Int x)
  |Bool x -> ("Bool", Bool x)
  |String x -> ("String", String x)
  |ID x -> ("ID", ID x)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  ID var -> lookup env var
  |Int x -> Int x
  |Bool x -> Bool x
  |String x -> String x
  |Not exp -> 
    let result = eval_expr env exp in
      (match result with
       | Bool true -> Bool false 
       | Bool false -> Bool true
       | _ -> raise (TypeError "Expected boolean in Not expression"))
  |Binop (op, exp1, exp2) -> 
    (let val1 = eval_expr env exp1 in
    let val2 = eval_expr env exp2 in
    match op with
    Add -> 
      (match (val1, val2) with
      (Int n1, Int n2) -> Int (n1 + n2)
      |_ -> raise (TypeError "Expected integers in Add"))
    |Sub -> 
       ((match (val1, val2) with
         | (Int n1, Int n2) -> Int (n1 - n2)
         | _ -> raise (TypeError "Expected integers in Sub")))
    |Mult ->
      (match (val1, val2) with
      (Int n1, Int n2) -> Int (n1 * n2)
      |_ -> raise (TypeError "Expected integers in Mult"))
    |Div -> 
      (match (val1, val2) with
      (Int n1, Int n2) -> if (n2 <> 0) then Int (n1/n2) else raise DivByZeroError
      |_ -> raise (TypeError "Expected integers in Div"))
    |Greater -> 
      (match (val1, val2) with
      (Int n1, Int n2) -> Bool (n1 > n2)
      |_ -> raise (TypeError "Expected integers in Greater"))
    |Less -> 
      (match (val1, val2) with
      (Int n1, Int n2) -> Bool (n1 < n2)
      |_ -> raise (TypeError "Expected integers in Less"))
    |GreaterEqual ->
      (match (val1, val2) with
      (Int n1, Int n2) -> Bool (n1 >= n2)
      |_ -> raise (TypeError "Expected integers in GreaterEqual"))
    |LessEqual ->
      (match (val1, val2) with
      (Int n1, Int n2) -> Bool (n1 <= n2)
      |_ -> raise (TypeError "Expected integers in LesEqual"))
    |Concat -> 
      (match (val1, val2) with
      (String n1, String n2) -> String (n1 ^ n2)
      |_ -> raise (TypeError "Expected Strings in Concat"))
    |Equal -> 
      (match (val1, val2) with
      (Int n1, Int n2) -> Bool (n1 = n2)
      |(Bool n1, Bool n2) -> Bool (n1 = n2)
      |(String n1, String n2) -> Bool (n1 = n2)
      |(ID n1, ID n2) ->  Bool (n1 = n2)
      |_ -> raise (TypeError "Expected Same types in Equal"))
    |NotEqual ->
      (match (val1, val2) with
      (Int n1, Int n2) -> Bool (n1 <> n2)
      |(Bool n1, Bool n2) -> Bool (n1 <> n2)
      |(String n1, String n2) -> Bool (n1 <> n2)
      |(ID n1, ID n2) ->  Bool (n1 <> n2)
      |_ -> raise (TypeError "Expected Same types in NotEqual"))
    |Or ->
      (match (val1, val2) with
      (Bool n1, Bool n2) -> Bool (n1 || n2)
      |_ -> raise (TypeError "Expected Bools in Or"))
    |And ->
      (match (val1, val2) with
      (Bool n1, Bool n2) -> Bool (n1 && n2)
      |_ -> raise (TypeError "Expected Bools in And")))
|If (guard, tbranch, fbranch) -> 
    (match eval_expr env guard with
    Bool true -> eval_expr env tbranch
    |Bool false -> eval_expr env fbranch
    |_ -> raise (TypeError "Expected Bool Guard exp in If"))
|Let (var, recursive, exp1, exp2) ->
    (match recursive with
    false -> 
      let val1 = eval_expr env exp1 in
      let new_env = extend env var val1 in
      let val2 = eval_expr new_env exp2 in
      val2 
    |true ->
      let new_env = extend_tmp env var in
      let val1 = eval_expr new_env exp1 in
      let _ = update new_env var val1 in
      let val2 =  eval_expr new_env exp2 in
      val2)
|Fun (var, exp) -> 
    (Closure (env, var, exp))
|App (exp1, exp2) ->
    (match eval_expr env exp1 with
      Closure (a,x,e) ->
        (let v = eval_expr env exp2 in
        let new_env = extend a x v in
        eval_expr new_env e)
      |_ -> raise (TypeError "Expected Function in App"))
|Record x -> Record x
|Select (lab, expr) ->
    (match eval_expr env expr with
    Record x ->
      (match List.assoc_opt lab x with
      Some b -> eval_expr env b
      |None -> raise (SelectError "Not Found in Record"))
    |_ -> raise (TypeError "Expected Record in App"))



(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  Def (var,exp) ->
    (
      let new_env = extend_tmp env var in
      let val1 = eval_expr new_env exp in
      let _ = update new_env var val1 in
      (new_env,Some val1)
    )
  |Expr exp ->
    (
      (env, Some (eval_expr env exp))
    )
  |NoOp ->
    (
      (env, None)
    )
