open SymbolTable
open TypeDef

(*This function takes a list of exceptions and returns the shortest list of exceptions*)
(*The rh is a list*)

let lookup symbol_table (symbol:lh_symbol_type) = 
    match Hashtbl.find_opt symbol_table symbol with
    | Some value -> value
    | None -> raise (Invalid_argument ("Symbol" ^ (Parser.string_lh_symbol symbol) ^"not found"))
let shortest_list lists =
  let rec find_shortest (shortest, shortest_len) = function
    | [] -> shortest
    | hd :: tl ->
        let current_length = List.length hd in
        if current_length < shortest_len then
          find_shortest (Some hd, current_length) tl
        else
          find_shortest (shortest, shortest_len) tl
  in
  find_shortest (None, max_int) lists


let split_list n ls =
    let rec split_list_r lh rh i = match rh with
    |hd::rest when i < n -> 
        split_list_r (hd::lh) rest (i+1)
    |_->
        ((List.rev lh),rh)
    in

    split_list_r [] ls 0




let type_verify (outer_symbol_table:(lh_symbol_type , rh_symbol_type) Hashtbl.t) directory home_dir=
    let rec symbol_table_from_rhv rhv = match rhv with
    | (PARAM_LIST(sub_table))->
        sub_table

    | Inherit(appended_symbols,tables) -> 
        let rhs = List.map (fun x -> lookup outer_symbol_table (Definition x) ) tables in
        let symbol_tables = List.map symbol_table_from_rhv rhs in 
        let table = combine_table_list symbol_tables in
        append_table table appended_symbols;
        table

    | _ -> 
        raise (Failure "Invalid symbol type") 
    in
let rec type_verify_r symbol_table (assignments:assignment list) (exceptions:exception_value list) scope = 
    match assignments with
    |ASSIGNMENT((lh_type,lh_value,assign_cords), LEXEM_LIST(ls))::rest ->
        let expected_rh_type = 
            match lh_type with
            |KEYWORD when (member symbol_table (KEYWORD_SYMBOL(lh_value)) )  -> 
               Some (lookup symbol_table (KEYWORD_SYMBOL(lh_value)))
            |any_type when member symbol_table (TYPE_SYMBOL(any_type)) ->  
               Some (lookup symbol_table (TYPE_SYMBOL(any_type)))
            |_-> None
        in
        let rec assign_type_check (expected_rh_type:rh_symbol_type) ls exceptions =  
            match expected_rh_type,ls with
            |_,[] -> exceptions
            |VALUE_LIST(list_erh_type),(LEXEM(rh_type,rh_value,cords)::rest)  -> 
                let cur_type = rh_type in 
                (match list_erh_type with
                |PARAM_VALUE(erh_type) when erh_type =  cur_type  -> 
                    assign_type_check expected_rh_type rest exceptions
                |PARAM_VALUE(erh_type) when erh_type !=  rh_type  ->
                    let e = (TYPE_MISHMASH(lh_value, erh_type, rh_type)) in
                    let new_exceptions=((RHS [PARAM_VALUE(erh_type)],e,cords)::exceptions) in
                    assign_type_check expected_rh_type rest new_exceptions
                |PARAM_OPTION(options) ->
                    let rec shortest_exception_list_r options = match options with 
                        |[] -> ((RHS options,UNEXPECTED_LEXEM(rh_value,rh_type),cords)::exceptions)
                        |top::rest_types ->
                            (match (assign_type_check top rest []) with
                            |[] -> exceptions 
                            |_-> shortest_exception_list_r rest_types 
                            )
                    in
                    let exceptions = shortest_exception_list_r options in
                    assign_type_check expected_rh_type rest exceptions
                |_ ->
                    let e = UNEXPECTED_LEXEM(rh_value,rh_type) in
                    let expected = (RHS [expected_rh_type]) in
                    ((expected,e,cords)::exceptions)
                )
            |_,_ -> 
                let e = UNEXPECTED_EXPR_LIST(ls) in
                let expected = (RHS [expected_rh_type]) in
                let new_exceptions =((expected,e,assign_cords)::exceptions) in
                assign_type_check expected_rh_type ls new_exceptions

        in
        (match expected_rh_type with
            |Some rhs ->  let new_exceptions = assign_type_check rhs ls exceptions in
                type_verify_r symbol_table rest new_exceptions scope
            |None -> 
                let e = (UNKNOWN_IDENTIFIER(lh_value) ) in             
                type_verify_r symbol_table rest ((scope,e,assign_cords)::exceptions) scope
            )

    |ASSIGNMENT((lh_type,lh_value,_), LEXEM((rh_type,rh_value,cords)))::rest  ->
     let expected_rh_type = 
         match lh_type with
         |KEYWORD when (member symbol_table (KEYWORD_SYMBOL(lh_value)) )  -> 
            Some (lookup symbol_table (KEYWORD_SYMBOL(lh_value)) )
         |_ when (member symbol_table (KEYWORD_SYMBOL(lh_value)))  -> 
            Some (lookup symbol_table (KEYWORD_SYMBOL(lh_value)) )
         |any_type when member symbol_table (TYPE_SYMBOL(any_type)) ->  
            Some ( lookup symbol_table (TYPE_SYMBOL(any_type)))
         |_-> 
            None
     in
     let rec assign_type_check expected_rh_type exceptions =  
        match expected_rh_type with
        |NUMBER ->
            if rh_type = FLOAT || rh_type = INT then 
                exceptions
            else 
                let e = (TYPE_MISHMASH(lh_value, FLOAT, rh_type)) in
                ((RHS [PARAM_VALUE(FLOAT)],e,cords)::exceptions)
        |PARAM_VALUE(erh_type) when erh_type = rh_type  -> 
            exceptions 
        |(PARAM_VALUE(erh_type) as epv) when erh_type !=  rh_type  ->
           let e = (TYPE_MISHMASH(lh_value, erh_type, rh_type)) in
           (((RHS [epv]),e,cords)::exceptions) 
        |PARAM_OPTION(options) ->

            let rec shortest_exception_list_r options = match options with 
                |[] -> ((RHS options,UNEXPECTED_LEXEM(rh_value,rh_type),cords)::exceptions)
                |top::rest ->
                    (match (assign_type_check top []) with
                    |[] -> exceptions 
                    |_-> shortest_exception_list_r rest 
                    )
            in
            shortest_exception_list_r options  

        |CHOICE_VALUE(choices) as rh -> 


            if List.mem rh_value choices then 
                exceptions
            else 
                let e = UNEXPECTED_LEXEM(rh_value,rh_type) in
                ((RHS [rh],e,cords)::exceptions)

        |x -> 
           let e = UNEXPECTED_LEXEM(rh_value,rh_type) in
           let expected = (RHS [x]) in
           ((expected,e,cords)::exceptions) 
    in   
    (match expected_rh_type with
    |Some rh -> 
        let exceptions = assign_type_check rh exceptions in
        type_verify_r symbol_table rest exceptions scope
    |None ->
        let e = (UNKNOWN_IDENTIFIER(lh_value) ) in             
        type_verify_r symbol_table rest ((scope,e,cords)::exceptions) scope
    )
    |ASSIGNMENT((lh_type,lh_value,cords),ASSIGNMENT_LIST(ls))::rest -> 

        let expected_rh_type = 
            match lh_type with
            |KEYWORD when (member symbol_table (KEYWORD_SYMBOL(lh_value)) )  -> 
               Some (lookup symbol_table (KEYWORD_SYMBOL(lh_value)) )
            |any_type when member symbol_table (TYPE_SYMBOL(any_type)) ->  
               Some( lookup symbol_table (TYPE_SYMBOL(any_type)) )
            |_-> None
        in

     let assignlist_type_check expected_rh_type ls exceptions= match expected_rh_type with
         | (APPEND_SYMBOLS(_) as new_scope)  ->
            let symbol_tables = symbol_table_from_rhv new_scope in
            let rec get_exceptions lst lowest_exception=
              match lst with
              | [] -> lowest_exception
              | top_table::xs -> 
                 (match (type_verify_r top_table ls exceptions (RHS([new_scope]))) with
                 |[] ->  
                         []
                 |exceptions->
                    get_exceptions xs  (exceptions@lowest_exception) 
                 )
            in
            get_exceptions [symbol_tables] []

         | (PARAM_OPTION(values) as new_scope) ->
            let symbol_tables = List.map symbol_table_from_rhv values in
            let rec get_exceptions lst lowest_exception=
              match lst with
              | [] -> 
                    (match lowest_exception with
                        |Some e -> e@exceptions
                        |None -> 
                                exceptions
                    )
              | top_table::xs -> 
                 (match (type_verify_r top_table ls [] (RHS([new_scope]))) with
                 |[] -> []
                 |exceptions->
                    get_exceptions xs (Some exceptions) 
                 )
            in
            get_exceptions symbol_tables None
        |DefinedTypeRight(type_name) as new_scope ->
            let rhs = lookup outer_symbol_table (Definition type_name) in
            let sub_table = symbol_table_from_rhv rhs in
            type_verify_r sub_table ls exceptions (RHS([new_scope])) 
        (*
        |PARAM_LIST(inner_table) as new_scope ->
            Printf.printf "PARAM_LIST\n ";
            let _ = type_verify_r inner_table ls [] (RHS([new_scope])) in
            exceptions
        *)
            
         | (x) -> 
            let e = UNEXPECTED_ASSIGN_LIST(ls) in
            ((RHS[x],e,cords)::exceptions)
        in
        (match expected_rh_type with
        |Some rh -> 
            let exceptions = assignlist_type_check rh ls exceptions in
            type_verify_r symbol_table rest exceptions scope
        |None -> 
            let e = UNKNOWN_IDENTIFIER(lh_value) in

            type_verify_r symbol_table rest ((scope,e,cords)::exceptions) scope
        )
    |(ASSIGNMENT ((_, _, cords), EXPR_EXCEPTION _) as assign) ::rest -> 
        let e = (UNEXPECTED_ASSIGNMENT (assign)) in
        type_verify_r symbol_table rest ((scope,e,cords)::exceptions) scope
    |ASSIGN_EXCEPTION((expected,exception_val,cords))::rest -> 
        type_verify_r symbol_table rest ((expected,exception_val,cords)::exceptions) scope
    | [] -> 
           exceptions
in
List.map (function  
    |filepath, (symbol_table_key:string) ->
        let current_context:rh_symbol_type = lookup outer_symbol_table (Definition symbol_table_key) in
        let table = symbol_table_from_rhv current_context in
        let lexems = Lexer.lexer (Filename.concat home_dir filepath) in
        let assigns:(assignment list) = Parser.assignments lexems in
        type_verify_r table assigns [] (RHS([DefinedTypeRight(symbol_table_key)])) 
) directory 
  
