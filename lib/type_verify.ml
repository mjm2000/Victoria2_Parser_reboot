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
    (*
    |ASSIGNMENT ((KEYWORD,lh_value,cords), _)::rest 
     when not (Hashtbl.mem symbol_table (KEYWORD_SYMBOL(lh_value))) -> 
        let exceptions = (scope,UNKNOWN_IDENTIFIER(lh_value),cords)::exceptions in
        type_verify_r symbol_table rest exceptions scope

    *)
    |ASSIGNMENT((lh_type,lh_value,_), LEXEM((rh_type,rh_value,cords)))::rest  ->
    if not (Hashtbl.mem symbol_table (KEYWORD_SYMBOL lh_value)) then print_endline "Not found" else print_endline "Found";

     let expected_rh_type = 
         match lh_type with
         |KEYWORD when (Hashtbl.mem symbol_table (KEYWORD_SYMBOL(lh_value)) )  -> 
            Hashtbl.find_opt symbol_table (KEYWORD_SYMBOL(lh_value)) 
         |any_type when Hashtbl.mem symbol_table (TYPE_SYMBOL(any_type)) ->  
            Hashtbl.find_opt symbol_table (TYPE_SYMBOL(any_type))
         |_-> None
     in
     let rec assign_type_check expected_rh_type exceptions =  
        match expected_rh_type with
        |PARAM_VALUE(erh_type) when erh_type = rh_type  -> 

            exceptions 
        |(PARAM_VALUE(erh_type) as epv) when erh_type !=  rh_type  ->
           let e = (TYPE_MISHMASH(lh_value, erh_type, rh_type)) in
           (((RHS [epv]),e,cords)::exceptions) 
        |PARAM_OPTION(options) as rh ->
              let shortest_exception_list = (List.map (fun expected_rh_type -> 
                assign_type_check expected_rh_type [] 
              ) options)
              |> shortest_list in
              (match shortest_exception_list with 
               | Some e -> type_verify_r symbol_table rest (e@exceptions) scope
               | None -> 
                  let e = UNEXPECTED_LEXEM(rh_value,rh_type) in
                  ((RHS([rh]),e,cords)::exceptions)
              )
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
            print_endline lh_value;
            if not (Hashtbl.mem symbol_table (KEYWORD_SYMBOL lh_value)) then print_endline "Not found" else print_endline "Found";
            (print_endline (Pre_parser.string_symbol_table symbol_table));
        
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
         | (APPEND_SYMBOLS(_) as new_scope)     
         | (PARAM_OPTION(_) as new_scope) ->
            let symbol_tables = symbol_table_from_rhv new_scope in

            let exception_lists = List.map (fun symbol_table_i -> 

                    (*
                (
                if lh_value = "move_issue_percentage" then
                    print_endline "----------------------------------------";
                    Printf.printf "(Error here:%s)\n" (Pre_parser.string_symbol_table symbol_table_i);
                    Printf.printf "(Error here:%s)\n" (Pre_parser.string_assignment_list ls);
                    print_endline "----------------------------------------";

                );
                *)

                type_verify_r symbol_table_i ls [] (RHS([new_scope]))
                
            ) symbol_tables    
            in
            (match exception_lists with
            |[] -> exceptions
            |[exception_list] -> exception_list@exceptions
            |exception_lists when List.mem [] exception_lists -> 
                exceptions 
            |exception_lists -> 


                let v = (RHS([new_scope]),(MULTIPLE_CHOICE(exception_lists)),cords) in
                (v :: exceptions)
            )
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

  
