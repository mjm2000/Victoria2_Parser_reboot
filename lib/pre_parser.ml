open Lexer

open Exception

module HashSet = struct
  type 'a t = ('a, unit) Hashtbl.t

  let create () = Hashtbl.create 10

  let add set item =
    Hashtbl.replace set item ()

  let remove set item =
    Hashtbl.remove set item

  let mem set item =
    Hashtbl.mem set item
  let size set =
      Hashtbl.length set

  let list_to_set ls = 
      let tbl = Hashtbl.create (List.length ls) in
      List.iter (function x-> add tbl x) ls;
      tbl
end

type expr = 
    |LEXEM of lexem
    |ASSIGNMENT_LIST of assignment list
    |EXPR_EXCEPTION of exception_value  
and
    assignment = ASSIGNMENT of lexem * expr | ASSIGN_EXCEPTION of exception_value

let rec assignment_list lexems = 
    let rec assignment_list_r lexems out = match lexems with
        |((KEYWORD,_,_) as lh) :: ((EQ,_,_) ) :: rest  
        |((INT,_,_) as lh) :: ((EQ,_,_)) :: rest 
        |((TAG,_,_) as lh) :: ((EQ,_,_)) :: rest -> 
            let expr,rest = expression rest in
            let v = ASSIGNMENT(lh,expr) in
            assignment_list_r  rest (v::out)
        |(RB,_,_)::rest -> out,rest
        |(_,_,cords)::[] ->(ASSIGN_EXCEPTION(END_OF_FILE,cords)::out),[]
        |[] -> (ASSIGN_EXCEPTION(END_OF_FILE,(0,0))::out),[]
        |(lexem_type,str,cords)::rest -> 
            let expet = UNEXPECTED_LEXEM(str,lexem_type),cords in
            let v:assignment = ASSIGN_EXCEPTION(expet) in
            assignment_list_r rest (v::out)
 
    in
    assignment_list_r lexems []
    and 
    expression lexems = match lexems with
        |(LB,_,_) :: rest ->let v,rest = assignment_list rest in ASSIGNMENT_LIST(v),rest
        |(RB,_,cords)::rest -> EXPR_EXCEPTION(UNEXPECTED_RIGHT_BRACKET,cords),rest
        |(_,_,cords)::[] -> EXPR_EXCEPTION(END_OF_FILE,cords),[]
        |[]->EXPR_EXCEPTION(END_OF_FILE,(0,0)),[]
        |((KEYWORD,_,_) as lex) :: rest
        |((INT,_,_) as lex) :: rest 
        |((FLOAT,_,_) as lex) :: rest 
        |((STRING,_,_) as lex) :: rest 
        |((TAG,_,_) as lex) :: rest  
        |((SCOPE,_,_) as lex) :: rest 
        |((BOOL,_,_) as lex) :: rest -> LEXEM(lex),rest
        |((lexem_type,str,cords)) :: rest -> EXPR_EXCEPTION(UNEXPECTED_LEXEM(str,lexem_type),cords),rest

type param = PARAM of string*string | PARAM_ERROR of exception_value 

let verify_params a expected_as_list = 
    let expected_as = HashSet.list_to_set expected_as_list in
    let rec verify_params_r a out = match a with
    |ASSIGNMENT((lh_type,lh_value,_),LEXEM(rh_type,rh_value,_))::rest when Hashtbl.mem expected_as (lh_type,lh_value,rh_type)   ->
        verify_params_r rest (PARAM(lh_value,rh_value)::out) 
    |[]->
        out
    |rest ->
        verify_params_r rest (PARAM_ERROR(END_OF_FILE,(0,0))::out)
            
    in
    verify_params_r a out
(*let params expected assignment_list =
    let rec check_pattern e al out= match al
        |(ASSIGNMENT((kt,k,_),LEXEM(vt,v,_)) as assignment)::rs when mem e ((kt,k),vt)  -> 
            let e = remove e ((kt,k),vt) in     
            check_pattern e rs assignment::out
        |(ASSIGNMENT((kt,k,k_cords),LEXEM(vt,v,v_cords)) )::rs -> 
            let k_e= (UNEXPECTED_LEXEM(kt,k),cords) in
            let v_e= (UNEXPECTED_LEXEM(kt,k),cords) in
            check_pattern e rs PARAM_ERROR([v_e,k_e]) ::out
        |(ASSIGNMENT((kt,k,k_cords),ASSIGNMENT_LIST(_))) ::rs -> 
            let k_e= (UNEXPECTED_LEXEM(kt,k),cords) in
            check_pattern e rs (PARAM_ERROR([k_e])::out)
        |(ASSIGNMENT((kt,k,k_cords),EXPR_EXCEPTION(e) as ee)::rs -> 
            let k_e= EXPR_EXCEPTION(UNEXPECTED_LEXEM(kt,k),cords) in
            check_pattern e rs (PARAM_ERROR([ee,k_e])::out)
        |ASSIGN_EXCEPTION(e)::rs ->
            let k_e = EXPR_EXCEPTION(e) in
            check_pattern e rs (PARAM_ERROR([k_e])::out)

        |[] -> out
    in 
    check_pattern (list_to_set expected) assignment_list [] *)
