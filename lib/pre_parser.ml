open Lexer
open Type_def

let rec assignments ls = 
    let rec assignments_r out rest = match rest with 
        | [] -> (List.rev out)
        | lexems -> 
            let v, rest = assignment lexems in
            assignments_r (v :: out) rest
    in
    assignments_r [] ls

and assignment lex = match lex with
    | lh :: (EQ, _, _) :: rest -> 
        let expr, rest = expression rest in
        let v = ASSIGNMENT (lh, expr) in
        v, rest 
    |(_,_,cords):: [] -> 
        ASSIGN_EXCEPTION ((RHS [PARAM_VALUE KEYWORD], END_OF_FILE, cords)), []

    | (lex,value,cords) :: rest -> 
            (*add type *)
        ASSIGN_EXCEPTION ((RHS [PARAM_VALUE(EQ)], UNEXPECTED_LEXEM(value,lex), cords)), rest 

    | [] -> ASSIGN_EXCEPTION ((RHS [PARAM_VALUE KEYWORD], END_OF_FILE, (2, 2))), []

and assignment_list lexems = 

    let rec assignment_list_r lexems out = match lexems with
        | (RB, _, _) :: rest -> (List.rev out), rest
        | (_, _, cords) :: [] -> (List.rev (ASSIGN_EXCEPTION (RHS [PARAM_VALUE KEYWORD], END_OF_FILE, cords) :: out), [])

        | [] -> 
                (List.rev (ASSIGN_EXCEPTION (RHS [PARAM_VALUE KEYWORD], END_OF_FILE, (1, 1)) :: out), [])
        | rest -> 
            let v, rest = assignment rest in
            assignment_list_r rest (v :: out)
    in
    assignment_list_r lexems []

and lexem_list lexems = 
    let rec lexem_list_r lexems out = match lexems with
        | (RB, _, _) :: rest -> (List.rev out), rest
        | (_, _, cords) :: [] -> (List.rev (EXPR_EXCEPTION (RHS [PARAM_VALUE KEYWORD], END_OF_FILE, cords) :: out), [])

        | [] -> 
                (List.rev (EXPR_EXCEPTION (RHS [PARAM_VALUE KEYWORD], END_OF_FILE, (1, 1)) :: out), [])
        | rest -> 
            (match(expression rest) with 
            |(LEXEM(_,_,_) as v),rest -> 
                lexem_list_r rest (v :: out)
            |((ASSIGNMENT_LIST ( (ASSIGNMENT((_,_,cords),_) )::_ as al) ) ),rest ->
                let all_types = [PARAM_VALUE KEYWORD;
                PARAM_VALUE INT; 
                PARAM_VALUE STRING;
                PARAM_VALUE FLOAT] in
                let v = EXPR_EXCEPTION(RHS(all_types), UNEXPECTED_ASSIGN_LIST(al),cords) in
                lexem_list_r rest (v :: out)
            |(ASSIGNMENT_LIST (ASSIGN_EXCEPTION(ae)::_)) ,rest ->
                lexem_list_r rest (EXPR_EXCEPTION(ae) :: out) 
            |(ASSIGNMENT_LIST ([]) ), ((_,_,cords)::tail) ->
                    let v = EXPR_EXCEPTION(RHS [PARAM_VALUE KEYWORD], UNEXPECTED_ASSIGN_LIST( []), cords) in
                lexem_list_r tail (v :: out)

            |(EXPR_EXCEPTION (_) as e ,rest) ->
                lexem_list_r rest (e :: out)
            (*|( (LEXEM_LIST ((_,_,cords)::_) as ll)   ,rest) ->
                let v = EXPR_EXCEPTION(RHS [PARAM_VALUE KEYWORD],UNEXPECTED_EXPR_LIST(ll),cords) in
                lexem_list_r rest (v :: out)
            *)
            |_->List.rev out ,rest

            )
    in
    lexem_list_r lexems [] 

and expression lexems = 
    match lexems with
    | (LB, _, _) :: rest -> 
        (match rest with
        |_::(EQ,_,_)::_ ->
                let v, rest = assignment_list rest in 
                ASSIGNMENT_LIST v, rest
        |_ -> 
            let v, rest = lexem_list rest in
            LEXEM_LIST(v), rest
        )
    | (RB, _, cords) :: rest -> 
        EXPR_EXCEPTION (RHS [PARAM_VALUE LB], UNEXPECTED_RIGHT_BRACKET, cords), rest

    | (_, _, cords) :: [] -> 
        EXPR_EXCEPTION (RHS [PARAM_VALUE LB], END_OF_FILE, cords), []

    | [] -> 
        EXPR_EXCEPTION (RHS [PARAM_VALUE LB], END_OF_FILE, (3, 3)), []

    | ((KEYWORD, _, _) as lex) :: rest
    | ((INT, _, _) as lex) :: rest 
    | ((FLOAT, _, _) as lex) :: rest 
    | ((STRING, _, _) as lex) :: rest 
    | ((TAG, _, _) as lex) :: rest  
    | ((SCOPE, _, _) as lex) :: rest 
    | ((BOOL, _, _) as lex) :: rest -> LEXEM lex, rest

    | (lexem_type, str, cords) :: rest -> 
        EXPR_EXCEPTION (RHS [PARAM_VALUE KEYWORD], UNEXPECTED_LEXEM (str, lexem_type), cords), rest

and exception_iden_string exp = match exp with
    | TYPE_MISHMASH (iden, expected, received) ->
        Printf.sprintf "Incorrect Type: Text=%s Expected Type=%s Received Type=%s" 
            iden (lexem_to_str expected) (lexem_to_str received)  
    
    | END_OF_FILE -> "Unexpected EOF"  
    | UNKNOWN_IDENTIFIER string -> Printf.sprintf "Unknown Identifier: %s" string  
    | UNEXPECTED_LEXEM (iden, lexem) -> 
        Printf.sprintf "Unexpected Token: Text=%s Type=%s" iden (lexem_to_str lexem) 

    | UNEXPECTED_ASSIGNMENT (assignment) ->
        Printf.sprintf "Unexpected Assignment: %s" (string_assignment assignment)

    | UNEXPECTED_ASSIGN_LIST (e) ->
        Printf.sprintf "Unexpected Assignments:%s" (string_assignment_list e) 

    | UNEXPECTED_RIGHT_BRACKET -> "Unexpected Right Bracket"
    | UNEXPECTED_LEFT_BRACKET -> "Unexpected Left Bracket"
    | MULTIPLE_CHOICE(ls)-> 
            Printf.sprintf "Multiple Choice %s" (exception_lists_string (ls))
    | UNEXPECTED_EXPR_LIST (ls) ->
        List.fold_left (fun acc x -> Printf.sprintf "%s\n\t%s" acc (string_expr x)) "" ls
    | UNEXPECTED_EXPR (e) -> string_expr e

and exception_string (exp : exception_value) : string = 
    (*add expected value printer*)
    let ev, e, (x, y) = exp in 
    Printf.sprintf " At (%i,%i): %s, expected:%s" x y (exception_iden_string e) (string_expected_value ev)
and exceptions_string ls = 
    let rec exceptions_string_r ls out = match ls with
        | [] -> out
        | x :: rest -> 
            let x = exception_string x in
            exceptions_string_r rest (Printf.sprintf "%s\n\t%s" out x)
    in
    exceptions_string_r ls ""
and exception_lists_string ls = 
    let rec exception_lists_string_r ls out = match ls with
        | [] -> out
        | x :: rest -> 
            let x = exceptions_string x in
            exception_lists_string_r rest (Printf.sprintf "%s\n\t%s" out x)
    in
    exception_lists_string_r ls ""

and string_assignment assignment = match assignment with  
    | ASSIGNMENT ((tlh, vlh, (x,y)), rh) ->
        let string_expr = string_expr rh in
        Printf.sprintf "ASSIGNMENT(type:%s,location:(%d,%d),lhvalue:%s=rhvalue:%s)" (lexem_to_str  tlh) x y vlh string_expr 

    | ASSIGN_EXCEPTION exception_value -> exception_string exception_value

and string_assignment_list al =
    let x = List.fold_left 
        (fun acc x -> Printf.sprintf "%s\n\t%s" acc (string_assignment x)) 
        "" al 
    in 
    Printf.sprintf "ASSIGN_LIST(%s)" x

and string_expr expr = match expr with
    | LEXEM (type_val, str, (x, y)) -> 
        Printf.sprintf "LEXEM(type(%s):value(%s),location(%d,%d))" (lexem_to_str type_val) str x y 
    | LEXEM_LIST lexem_list ->
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc (string_expr x)
        ) "" lexem_list in
        Printf.sprintf "LEXEM_LIST(%s)" x
    | ASSIGNMENT_LIST assignment_list -> string_assignment_list assignment_list
    | EXPR_EXCEPTION exception_value -> exception_string exception_value
and string_rh_symbol pv = match pv with
    | PARAM_VALUE type_val -> 
        Printf.sprintf "PARAM_VALUE(type(%s))" (lexem_to_str type_val)
    | PARAM_LIST (symbol_table) -> 
        let x = Hashtbl.fold (fun k v acc -> 
            Printf.sprintf "%s\n\t%s:%s" acc (string_lh_symbol k) (string_rh_symbol v)
        ) symbol_table "" in
        Printf.sprintf "PARAM_LIST(%s)" x
    | PARAM_OPTION (options) ->
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc (string_rh_symbol x)
        ) "" options in
        Printf.sprintf "PARAM_OPTION(%s)" x
    | CHOICE_VALUE (choices) ->
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc x
        ) "" choices in
        Printf.sprintf "CHOICE_VALUE(%s)" x
    | APPEND_SYMBOLS (symbols, rh) ->
        let x = List.fold_left (fun acc (lh, rh) -> 
            Printf.sprintf "%s\n\t%s:%s" acc (string_lh_symbol lh) (string_rh_symbol rh)
        ) "" symbols in
        Printf.sprintf "APPEND_SYMBOLS(%s:%s)" x (string_rh_symbol rh)
    | VALUE_LIST (value) ->
        Printf.sprintf "%s" (string_rh_symbol value) 
    | PROVINCE_MTTH -> "PROVINCE_MTTH"
    | COUNTRY_MTTH -> "COUNTRY_MTTH"
    | PROVINCE_EFFECTS -> "PROVINCE_EFFECTS"
    | PROVINCE_CONDITIONS -> "PROVINCE_CONDITIONS"
    | COUNTRY_EFFECTS -> "COUNTRY_EFFECTS"
    | COUNTRY_CONDITIONS -> "COUNTRY_CONDITIONS"
    | POP_EFFECTS -> "POP_EFFECTS"
    | POP_CONDITIONS -> "POP_CONDITIONS"
    | STATE_CONDITIONS -> "STATE_CONDITIONS"
    | STATE_EFFECTS -> "STATE_EFFECTS"
    | PROVINCE_MODIFIERS -> "PROVINCE_MODIFIERS"
    | COUNTRY_MODIFIERS -> "COUNTRY_MODIFIERS"

and string_lh_symbol lh = match lh with
    | KEYWORD_SYMBOL str -> Printf.sprintf "KEYWORD_SYMBOL(%s)" str
    | TYPE_SYMBOL lexem -> Printf.sprintf "TYPE_SYMBOL(%s)" (lexem_to_str lexem)

and string_symbol_table symbol_table =
    let x = Hashtbl.fold (fun k v acc -> 
        Printf.sprintf "%s\n\t%s:%s" acc (string_lh_symbol k) (string_rh_symbol v)
    ) symbol_table "" in
    Printf.sprintf "SYMBOL_TABLE(%s)" x

and string_expected_value v = match v with
    | RHS(ls) -> 
        let x = List.fold_left (fun acc x -> 
            match x with
            | PARAM_LIST(_)-> Printf.sprintf "%s\n\t%s" acc "PARAM_LIST"
            |_ -> Printf.sprintf "%s\n\t%s" acc (string_rh_symbol x)
        ) "" ls in
        Printf.sprintf "RHS(%s)" x 
    | LHS(ls) -> 
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc (string_lh_symbol x)
        ) "" ls in
        Printf.sprintf "LHS(%s)" x
    | NONE -> "NONE"

let get_errors assignments = 
    let rec get_errors_r assignments out = match assignments with
    | ((ASSIGN_EXCEPTION (e)) :: rest) ->
        get_errors_r rest (e :: out)
    | ASSIGNMENT(_,EXPR_EXCEPTION e) :: rest-> 
        get_errors_r rest (e :: out)
    | ASSIGNMENT(_,ASSIGNMENT_LIST al) :: rest ->
        let assign_list__errors = get_errors_r al out in
        get_errors_r rest assign_list__errors
    | [] -> out
    | _::rest -> get_errors_r rest out
in
get_errors_r assignments []

 
