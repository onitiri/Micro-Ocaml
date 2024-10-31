open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with
  Some Tok_Let -> parse_let toks
  |Some Tok_If -> parse_if toks
  |Some Tok_Fun -> parse_fun toks
  |_-> parse_or toks

  (* LetExpr -> let Recursion Tok_ID = Expr in Expr
      Recursion -> rec | ε *)
  and parse_let toks = 
    let toks0 = match_token toks Tok_Let in  (* remove 'let' *)
    let recurse, toks1 = parse_recursion toks0 in (* Check for 'rec' and update tokens *)
    let id = 
      match lookahead toks1 with
      | Some (Tok_ID s) -> s 
      | _ -> raise (InvalidInputException "parse_let")
    in
    let toks2 = match_token toks1 (Tok_ID id) in (* remove ID token *)
    let toks3 = match_token toks2 Tok_Equal in (* remove '=' token *)
    let toks4, exp1 = parse_expr toks3 in 
    let toks5 = match_token toks4 Tok_In in (* remove 'in' token *)
    let toks6, exp2 = parse_expr toks5 in
    (toks6, Let(id, recurse, exp1, exp2))

  and parse_recursion toks =
    match lookahead toks with
    Some Tok_Rec -> (true, match_token toks Tok_Rec)
    |_ -> (false, toks)

  
  (*IfExpr -> if Expr then Expr else Expr*)
  and parse_if toks = 
    let toks0 = match_token toks Tok_If in
    let toks1, exp1 = parse_expr toks0 in
    let toks2 = match_token toks1 Tok_Then in
    let toks3, exp2 = parse_expr toks2 in
    let toks4 = match_token toks3 Tok_Else in 
    let toks5, exp3 = parse_expr toks4 in
    (toks5, If(exp1,exp2,exp3))

  and parse_fun toks = 
    let toks0 = match_token toks Tok_Fun in
    let id = 
      match lookahead toks0 with
      | Some (Tok_ID s) -> s 
      | _ -> raise (InvalidInputException "parse_fun")
    in
    let toks1 = match_many toks0 [Tok_ID id;Tok_Arrow] in (* remove ID token *)
    let toks2, exp = parse_expr toks1 in
    (toks2, Fun(id,exp))

    (*OrExpr -> AndExpr || OrExpr | AndExpr*)
    and parse_or toks = 
      let toks1, exp1 = parse_and toks in
      match lookahead toks1 with
      | Some Tok_Or -> 
          let toks2 = match_token toks1 Tok_Or in
          let toks3, exp2 = parse_or toks2 in
          (toks3, Binop(Or, exp1, exp2))
      | _ -> (toks1, exp1)  (* If no "||", return the AndExpr *)
    
    (* AndExpr -> EqualityExpr && AndExpr | EqualityExpr *)
    and parse_and toks = 
      let toks1, exp1 = parse_equality toks in
      match lookahead toks1 with
      | Some Tok_And -> 
          let toks2 = match_token toks1 Tok_And in
          let toks3, exp2 = parse_and toks2 in
          (toks3, Binop(And, exp1, exp2))
      | _ -> (toks1, exp1)  (* If no "&&", return the EqualityExpr *)

  and parse_equality toks =  
    let toks1, exp1 = parse_relational toks in
    match lookahead toks1 with
    |Some Tok_Equal -> 
      let toks2 = match_token toks1 Tok_Equal in
      let toks3, exp2 = parse_equality toks2 in
      (toks3, Binop(Equal,exp1,exp2))
    | Some Tok_NotEqual ->
      let toks2 = match_token toks1 Tok_NotEqual in
      let toks3, exp2 = parse_equality toks2 in
      (toks3, Binop(NotEqual,exp1,exp2))
    |_ -> (toks1, exp1)

  and parse_relational toks =  
    let toks1, exp1 = parse_additive toks in
    match lookahead toks1 with
    Some Tok_Less -> 
      let toks2 = match_token toks1 Tok_Less in
      let toks3, exp2 = parse_relational toks2 in
      (toks3, Binop(Less, exp1, exp2))
    |Some Tok_Greater ->
      let toks2 = match_token toks1 Tok_Greater in
      let toks3, exp2 = parse_relational toks2 in
      (toks3, Binop(Greater, exp1, exp2))
    |Some Tok_LessEqual ->
      let toks2 = match_token toks1 Tok_LessEqual in
      let toks3, exp2 = parse_relational toks2 in
      (toks3, Binop(LessEqual, exp1, exp2))
    |Some Tok_GreaterEqual ->
      let toks2 = match_token toks1 Tok_GreaterEqual in
      let toks3, exp2 = parse_relational toks2 in
      (toks3, Binop(GreaterEqual, exp1, exp2))
    |_-> (toks1, exp1)

  and parse_additive toks = 
    let toks1, exp1 = parse_multiplicative toks in
    match lookahead toks1 with
    Some Tok_Add -> 
      let toks2 = match_token toks1 Tok_Add in
      let toks3, exp2 = parse_additive toks2 in
      (toks3, Binop(Add,exp1,exp2))
    |Some Tok_Sub ->
      let toks2 = match_token toks1 Tok_Sub in
      let toks3, exp2 = parse_additive toks2 in
      (toks3, Binop(Sub,exp1,exp2))
    |_ -> (toks1, exp1)

  and parse_multiplicative toks = 
    let toks1, exp1 = parse_concat toks in
    match lookahead toks1 with
    Some Tok_Mult -> 
      let toks2 = match_token toks1 Tok_Mult in
      let toks3, exp2 = parse_multiplicative toks2 in
      (toks3, Binop(Mult, exp1, exp2))
    |Some Tok_Div ->
      let _ = Printf.sprintf "%s\n" (string_of_list string_of_token toks) in
      let toks2 = match_token toks1 Tok_Div in
      let toks3, exp2 = parse_multiplicative toks2 in
      (toks3, Binop(Div, exp1, exp2))
    |_ -> (toks1, exp1)

  and parse_concat toks = 
    let toks1, exp1 = parse_unary toks in
    match lookahead toks1 with
    Some Tok_Concat ->
      let toks2 = match_token toks1 Tok_Concat in
      let toks3, exp2 = parse_concat toks2 in
      (toks3, Binop(Concat,exp1,exp2))
    |_ -> (toks1, exp1)

  and parse_unary toks = 
    match lookahead toks with
    Some Tok_Not -> 
      let toks1 = match_token toks Tok_Not in 
      let toks2, exp1 = parse_unary toks1 in
      (toks2, Not exp1)
    |_ -> 
      let toks1, exp1 = parse_app toks in
      (toks1, exp1)

  and parse_app toks =  
    let toks1, exp1 = parse_select toks in
    match lookahead toks1 with
    | Some Tok_Int x ->
      let toks2, exp2 = parse_primary toks1 in
        (toks2, App(exp1, exp2))
    | Some Tok_Bool x ->
      let toks2, exp2 = parse_primary toks1 in
        (toks2, App(exp1, exp2))
    | Some Tok_String x ->
      let toks2, exp2 = parse_primary toks1 in
        (toks2, App(exp1, exp2))
    | Some Tok_ID x ->
      let toks2, exp2 = parse_primary toks1 in
        (toks2, App(exp1, exp2))
    | Some Tok_LParen ->
      let toks2, exp2 = parse_primary toks1 in
        (toks2, App(exp1, exp2))
    | Some Tok_LCurly ->
      let toks2, exp2 = parse_primary toks1 in
        (toks2, App(exp1, exp2))
    | _ -> (toks1, exp1)  

  and parse_select toks =  
    let toks1, exp1 = parse_primary toks in
    match lookahead toks1 with
    Some Tok_Dot ->
      let toks2 = match_token toks1 Tok_Dot in
      let id = 
        match lookahead toks2 with
        | Some (Tok_ID s) -> s 
        | _ -> raise (InvalidInputException "parse_select")
      in
      let toks3 = match_token toks2 (Tok_ID id) in
      (toks3, Select(Lab id, exp1))
    |_ -> (toks1, exp1)

  and parse_primary toks = 
    match lookahead toks with
  | Some (Tok_Int i) -> 
    (match_token toks (Tok_Int i), Int i)
  | Some (Tok_Bool b) -> (match_token toks (Tok_Bool b), Bool b)
  | Some (Tok_String s) -> (match_token toks (Tok_String s), String s)
  | Some (Tok_ID s) -> (match_token toks (Tok_ID s), ID s)
  | Some Tok_LParen ->
      let toks1 = match_token toks Tok_LParen in
      let toks2, exp1 = parse_expr toks1 in
      let toks3 = match_token toks2 Tok_RParen in
      (toks3, exp1)
  | Some Tok_LCurly -> parse_record toks
  | _ -> raise (InvalidInputException "parse_primary")

  and parse_record toks =  
    let toks1 = match_token toks Tok_LCurly in
    match lookahead toks1 with
  | Some Tok_RCurly -> (match_token toks1 Tok_RCurly, Record [])
  | _ ->
      let toks2, fields = parse_recordBody toks1 in
      let toks3 = match_token toks2 Tok_RCurly in
      (toks3, Record fields)

  and parse_recordBody toks = 
    let rec aux acc toks =
      let id = 
        match lookahead toks with
        | Some (Tok_ID s) -> s
        | _ -> raise (InvalidInputException "Expected identifier in record body")
      in
      let toks1 = match_many toks [Tok_ID id; Tok_Equal] in
      let toks2, expr = parse_expr toks1 in
      match lookahead toks2 with
      | Some Tok_Semi -> 
        aux ((Lab id, expr) :: acc) (match_token toks2 Tok_Semi)
      | _ -> ((Lab id, expr) :: acc, toks2)
    in
    let fields, toks1 = aux [] toks in
    (toks1, List.rev fields)

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  Some Tok_Def -> parse_DefMutop toks
  |_ -> parse_ExprMutop toks


and parse_DefMutop toks = 
  let toks1 = match_token toks Tok_Def in
  let id = 
    match lookahead toks1 with
    | Some (Tok_ID s) -> s 
    | _ -> raise (InvalidInputException "parse_DefMutop")
  in
  let toks2 = match_many toks1 [Tok_ID id; Tok_Equal] in
  let toks3, exp = parse_expr toks2 in
  let toks4 = match_token toks3 Tok_DoubleSemi in
  (toks4, Def (id, exp))

and parse_ExprMutop toks = 
  match lookahead toks with
  Some Tok_DoubleSemi -> ([],NoOp)
  |_ ->
  ( 
    let toks1, exp = parse_expr toks in
    let toks2 = match_token toks1 Tok_DoubleSemi in
    (toks2, Expr exp)
    )