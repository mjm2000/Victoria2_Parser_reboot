 
open TypeDef
(*open Output*)
let rec assignments ls file= 
    let rec assignments_r out rest = match rest with 
        | [] -> (List.rev out)
        | lexems -> 
            let v, rest = assignment lexems file in
            assignments_r (v :: out) rest
    in
    assignments_r [] ls

and assignment lex file= match lex with
    | lh :: (EQ, _, _) :: rest -> 

        Output.print_memory_stats (Output.string_lexem lh); 
        (*add type *)
        let expr, rest = expression rest file in

        Printf.printf "Rest:%i\n" (List.length rest);
        Output.print_memory_stats (Output.string_expr expr);
        (*add type *)
        let v = ASSIGNMENT (lh, expr) in
        v, rest 
    |(_,_,cords):: [] -> 
        (*add type *)
        ASSIGN_EXCEPTION ((RHS [], END_OF_FILE, cords,file)), []


    | [] -> ASSIGN_EXCEPTION ((ExpEqual, END_OF_FILE, (2, 2),file)), []
    |rest ->
        print_endline "Assignment exception";
        (*add type *)
        let rec expr  out lexems = match lexems with
        | ((LexemValue _, _, _) as v) :: rest -> 
            let out = ((LEXEM v)::out) in
            expr out rest
        | (LB, _, _):: rest -> 
            (*add type *)
            let v = EXPR_EXCEPTION (RHS [], UNEXPECTED_LEFT_BRACKET, (1, 1),file) in
            ( v ),rest

        | (RB, _, cords)::rest -> 
            (*add type *)
            let v = EXPR_EXCEPTION (RHS [], UNEXPECTED_RIGHT_BRACKET, cords,file) in
            ( v ),rest
        | (EQ, _, cords)::rest -> 
            (*add type *)
            let v = EXPR_EXCEPTION (RHS [], UNEXPECTED_EQUAL, cords,file) in
            ( v ),rest

        | (lexem_type, str, cords)::rest -> 
            let v = EXPR_EXCEPTION (RHS [], UNEXPECTED_LEXEM (str, lexem_type), cords,file) in
             v,rest
        |[] -> 
            (*add type *)
            ((LEXEM_LIST out)),[]
    in
    let out,rest= expr [] rest in
    EXPR out,rest



and assignment_list lexems file= 

    let rec assignment_list_r lexems out = match lexems with
        | (RB, _, _) :: rest -> (List.rev out), rest
        | (_, _, cords) :: [] -> 
            print_endline "Assignment list exception";
                (List.rev (ASSIGN_EXCEPTION (RHS [], END_OF_FILE, cords,file) :: out), [])

        | [] -> 
                (List.rev (ASSIGN_EXCEPTION (RHS [], END_OF_FILE, (1, 1),file) :: out), [])
        | rest -> 
            let v, rest = assignment rest file in
            assignment_list_r rest (v :: out)
    in
    assignment_list_r lexems []

and lexem_list lexems file = 
    let rec lexem_list_r lexems out = match lexems with
        | (RB, _, _) :: rest -> (List.rev out), rest
        | (_, _, cords) :: [] -> (List.rev (EXPR_EXCEPTION (RHS [], END_OF_FILE, cords,file) :: out), [])

        | [] -> 
                (List.rev (EXPR_EXCEPTION (RHS [], END_OF_FILE, (1, 1),file) :: out), [])
        | rest -> 
            let e,rest =expression rest file in
            lexem_list_r rest (e :: out)
    in
    lexem_list_r lexems [] 

and expression lexems file = 
    match lexems with
    | (LB, _, _) :: rest -> 
        (match rest with
        |_::(EQ,_,_)::_ ->
                let v, rest = assignment_list rest file in 
                ASSIGNMENT_LIST v, rest
        |_ -> 
            let v, rest = lexem_list rest file in
            LEXEM_LIST(v), rest
        )
    | (RB, _, cords) :: rest -> 
            EXPR_EXCEPTION (ExpLeftBracket, UNEXPECTED_RIGHT_BRACKET, cords,file), rest

  (*  | (_, v, cords) :: [] -> 
        print_endline ("Expression exception trailing token:" ^ v);
        (*add type *)
        EXPR_EXCEPTION (ExpLeftBracket, END_OF_FILE, cords), []

*)
    | [] -> 
        EXPR_EXCEPTION (ExpLeftBracket, END_OF_FILE, (3, 3),file), []

    | ((LexemValue _, _, _) as lex) :: rest -> LEXEM lex, rest

    | (lexem_type, str, cords) :: rest -> 
        (*add type *)
        EXPR_EXCEPTION (RHS [], UNEXPECTED_LEXEM (str, lexem_type), cords,file), rest


 
