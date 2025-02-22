open Symbol_table
open Effects
open Conditions
open Type_def
open Mtth
(*This function takes a list of exceptions and returns the shortest list of exceptions*)
(*The rh is a list*)

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

let rec symbol_table_from_rhv rhv = match rhv with
    |(PROVINCE_MTTH) ->
        [province_mtth]
    | (COUNTRY_MTTH) ->
        [country_mtth]
    | (PROVINCE_EFFECTS) ->
        [province_effects]
    | (PROVINCE_CONDITIONS) ->
        [province_conditions]
    | (COUNTRY_EFFECTS) ->
        [country_effects]
    | (COUNTRY_CONDITIONS) ->
        [country_conditions]
    | (POP_EFFECTS) ->
        [pop_effects]
    | (POP_CONDITIONS) ->
        [pop_conditions]
    | (STATE_EFFECTS) ->
        [state_effects]
    | (STATE_CONDITIONS) ->
        [state_conditions]
    | (PARAM_LIST(sub_table))->
        [sub_table]
    | PARAM_OPTION(options) ->
        List.fold_left (fun acc rh ->
            (symbol_table_from_rhv rh)@acc
        ) [] options 

    | APPEND_SYMBOLS(appended_symbols,param_value) -> 

        let param_symbol_tables = symbol_table_from_rhv param_value  
        in
        List.map (fun symbol_table -> append_table symbol_table appended_symbols) param_symbol_tables
    | _ -> []

let type_verify symbol_table assignments =
let rec type_verify_r symbol_table assignments exceptions scope = 
    match assignments with
    |ASSIGNMENT((lh_type,lh_value,_), LEXEM((rh_type,rh_value,cords)))::rest  ->
     let expected_rh_type = 
         match lh_type with
         |KEYWORD when (Hashtbl.mem symbol_table (KEYWORD_SYMBOL(lh_value)) )  -> 
            Hashtbl.find_opt symbol_table (KEYWORD_SYMBOL(lh_value)) 
         |_ when (Hashtbl.mem symbol_table (KEYWORD_SYMBOL(lh_value)))  -> 
            Hashtbl.find_opt symbol_table (KEYWORD_SYMBOL(lh_value)) 
         |any_type when Hashtbl.mem symbol_table (TYPE_SYMBOL(any_type)) ->  
            Hashtbl.find_opt symbol_table (TYPE_SYMBOL(any_type))
         |_-> 
                 print_endline (lh_value);
            None
     in
     let rec assign_type_check expected_rh_type exceptions =  
        match expected_rh_type with
        |PARAM_VALUE(erh_type) when erh_type = rh_type  -> 
            (*
            Printf.printf "%s:%d %d scope:%s\n" lh_value x y (Pre_parser.string_expected_value scope);
            *)
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
            |KEYWORD when (Hashtbl.mem symbol_table (KEYWORD_SYMBOL(lh_value)) )  -> 
               Hashtbl.find_opt symbol_table (KEYWORD_SYMBOL(lh_value)) 
            |any_type when Hashtbl.mem symbol_table (TYPE_SYMBOL(any_type)) ->  
               Hashtbl.find_opt symbol_table (TYPE_SYMBOL(any_type))
            |_-> None
        in

    
     let  assignlist_type_check expected_rh_type ls exceptions= match expected_rh_type with
         |(PROVINCE_MTTH as new_scope) 
		 | (COUNTRY_MTTH as new_scope) 
         | (PROVINCE_EFFECTS as new_scope) 
		 | (PROVINCE_CONDITIONS as new_scope) 
		 | (COUNTRY_EFFECTS as new_scope) 
		 | (COUNTRY_CONDITIONS as new_scope) 
		 | (POP_EFFECTS as new_scope) 
		 | (POP_CONDITIONS as new_scope) 
		 | (STATE_EFFECTS as new_scope) 
         | (STATE_CONDITIONS as new_scope)
		 | (PARAM_LIST(_) as new_scope)
         | (APPEND_SYMBOLS(_) as new_scope)  ->
            let symbol_tables = symbol_table_from_rhv new_scope in
            let rec get_exceptions lst lowest_exception=
              match lst with
              | [] -> lowest_exception@exceptions
              | top_table::xs -> 
                 (match (type_verify_r top_table ls [] (RHS([new_scope]))) with
                 |[] -> []
                 |exceptions->
                    print_endline ("error1");
                    get_exceptions xs  (exceptions@lowest_exception) 
                 )
            in
            get_exceptions symbol_tables []

         | (PARAM_OPTION(_) as new_scope) ->
            let symbol_tables = symbol_table_from_rhv new_scope in
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
                    print_endline ("error2");
                    get_exceptions xs (Some exceptions) 
                 )
            in
            get_exceptions symbol_tables None
            
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
List.rev (type_verify_r symbol_table assignments [] (RHS([PARAM_LIST(symbol_table)])))

  
