open Types
open Str


let str_to_bool str = 
  match str with
  "true" -> true
  |"false" -> false

let re_rightparen = Str.regexp ")" 
let re_leftparen = Str.regexp "("
let re_lCurly = Str.regexp "{"
let re_rCurly = Str.regexp "}"
let re_dot = Str.regexp "\\."
let re_equal = Str.regexp "="
let re_notEqual = Str.regexp "<>"   
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterEqual = Str.regexp ">="
let re_lessEqual = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "\\bnot\\b"
let re_if = Str.regexp "\\bif\\b"
let re_then = Str.regexp "\\bthen\\b"
let re_else = Str.regexp "\\belse\\b"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "\\blet\\b"
let re_rec = Str.regexp "\\brec\\b"
let re_in = Str.regexp "\\bin\\b"
let re_def = Str.regexp "\\bdef\\b"
let re_fun = Str.regexp "fun\\b"
let re_arrow = Str.regexp "->"
let re_int = Str.regexp "[0-9]+"
let re_negint = Str.regexp "(-[0-9]+)"
let re_bool = Str.regexp "true\\|false"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_semi = Str.regexp ";"
let re_doublesemi = Str.regexp ";;"
let re_space = Str.regexp "[ \n\r\t]+"

let tokenize input = 
  let rec tok pos s =

    if pos >= String.length s then
      []
      
      else if (Str.string_match re_int s pos || Str.string_match re_negint s pos) then
        let token = Str.matched_string s in
        (Tok_Int (int_of_string (Str.global_replace re_rightparen "" (Str.global_replace re_leftparen "" token))))::(tok (pos + (String.length token)) s)
      else if (Str.string_match re_leftparen s pos) then
        Tok_LParen::(tok (pos+1) s)
      else if (Str.string_match re_rightparen s pos) then
        Tok_RParen::(tok (pos+1) s)
      else if (Str.string_match re_lCurly s pos) then
        Tok_LCurly::(tok (pos+1) s)
      else if (Str.string_match re_rCurly s pos) then
        Tok_RCurly::(tok (pos+1) s)
      else if (Str.string_match re_dot s pos) then
        Tok_Dot::(tok (pos+1) s)
      else if (Str.string_match re_equal s pos) then
        Tok_Equal::(tok (pos+1) s)
      else if (Str.string_match re_notEqual s pos) then
        Tok_NotEqual::(tok (pos+2) s)
      else if (Str.string_match re_greater s pos) then
        Tok_Greater::(tok (pos+1) s)
      else if (Str.string_match re_less s pos) then
        Tok_Less::(tok (pos+1) s)
      else if (Str.string_match re_greaterEqual s pos) then
        Tok_GreaterEqual::(tok (pos+2) s)
      else if (Str.string_match re_lessEqual s pos) then
        Tok_LessEqual::(tok (pos+2) s)
      else if (Str.string_match re_or s pos) then
        Tok_Or::(tok (pos+2) s)
      else if (Str.string_match re_and s pos) then
        Tok_And::(tok (pos+2) s)
      else if (Str.string_match re_not s pos) then
        Tok_Not::(tok (pos+3) s)
      else if (Str.string_match re_if s pos && String.length (Str.matched_string s) = 2) then
        Tok_If::(tok (pos+2) s)
      else if (Str.string_match re_then s pos && String.length (Str.matched_string s) = 4) then
        Tok_Then::(tok (pos+4) s)
      else if (Str.string_match re_else s pos && String.length (Str.matched_string s) = 4) then
        Tok_Else::(tok (pos+4) s)
        else if (Str.string_match re_arrow s pos) then
        Tok_Arrow::(tok (pos+2) s)
      else if (Str.string_match re_add s pos) then
        Tok_Add::(tok (pos+1) s)
      else if (Str.string_match re_sub s pos) then
        Tok_Sub::(tok (pos+1) s)
      else if (Str.string_match re_mult s pos) then
        Tok_Mult::(tok (pos+1) s)
      else if (Str.string_match re_div s pos) then
        Tok_Div::(tok (pos+1) s)
      else if (Str.string_match re_concat s pos) then
        Tok_Concat::(tok (pos+1) s)
      else if (Str.string_match re_let s pos) then
        Tok_Let::(tok (pos+3) s)
      else if (Str.string_match re_def s pos && String.length (Str.matched_string s) = 3) then
        Tok_Def::(tok (pos+3) s)
      else if (Str.string_match re_in s pos && String.length (Str.matched_string s) = 2) then
        Tok_In::(tok (pos+2) s)
      else if (Str.string_match re_rec s pos && String.length (Str.matched_string s) = 3) then
        Tok_Rec::(tok (pos+3) s)
      else if (Str.string_match re_fun s pos && String.length (Str.matched_string s) = 3) then
        Tok_Fun::(tok (pos+3) s)
      else if (Str.string_match re_doublesemi s pos) then
        Tok_DoubleSemi::(tok (pos+2) s)
      else if (Str.string_match re_semi s pos) then
        Tok_Semi::(tok (pos+1) s)


        
      else if (Str.string_match re_string s pos) then
        let token = Str.matched_string s in
        let tokenWithout = String.sub token 1 (String.length token - 2) in
        (Tok_String tokenWithout)::(tok (pos + (String.length token)) s)
        (* (Tok_String token)::(tok (pos + (String.length token)) s) *)



      else if (Str.string_match re_bool s pos) then
        let token = Str.matched_string s in
        (Tok_Bool (str_to_bool token))::(tok (pos + (String.length token)) s)
      else if (Str.string_match re_id s pos) then
        let token = Str.matched_string s in
        (Tok_ID token)::(tok (pos + (String.length token)) s)  
      else if (Str.string_match re_space s pos) then
        let token = Str.matched_string s in
        (tok (pos + (String.length token)) s)
      else
        raise (InvalidInputException "tokenize")
  in
  tok 0 input
