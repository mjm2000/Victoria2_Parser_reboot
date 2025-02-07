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


let rec type_verify symbol_table assignments exceptions scope = 
    match assignments with
    |ASSIGNMENT ((KEYWORD,lh_value,cords), _)::rest 
     when not (Hashtbl.mem symbol_table (KEYWORD_SYMBOL(lh_value))) -> 
         let expected = (RHS([])) in
         let exceptions = (expected,UNKNOWN_IDENTIFIER(lh_value),cords)::exceptions in
     type_verify symbol_table rest exceptions scope

    |ASSIGNMENT((lh_type,lh_value,_), LEXEM((rh_type,rh_value,cords)))::rest  ->
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
           type_verify symbol_table rest exceptions scope
        |(PARAM_VALUE(erh_type) as epv) when erh_type !=  rh_type  ->
           let e = (TYPE_MISHMASH(lh_value, erh_type, rh_type)) in
           type_verify symbol_table rest (((RHS [epv]),e,cords)::exceptions) scope
        |PARAM_OPTION(options) as rh ->
              let shortest_exception_list = (List.map (fun expected_rh_type -> 
                assign_type_check expected_rh_type [] 
              ) options)
              |> shortest_list in
              (match shortest_exception_list with 
               | Some e -> type_verify symbol_table rest (e@exceptions) scope
               | None -> 
                  let e = UNEXPECTED_LEXEM(rh_value,rh_type) in
                  type_verify symbol_table rest ((RHS([rh]),e,cords)::exceptions) scope
              )
        |x -> 
           let e = UNEXPECTED_LEXEM(rh_value,rh_type) in
           let expected = (RHS [x]) in
           type_verify symbol_table rest ((expected,e,cords)::exceptions) scope
    in   
    (match expected_rh_type with
    |Some rh -> 
        let exceptions = assign_type_check rh exceptions in
        type_verify symbol_table rest exceptions scope
    |None ->
        let e = UNKNOWN_IDENTIFIER(lh_value) in
        type_verify symbol_table rest ((scope,e,cords)::exceptions) scope
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
     let rec assignlist_type_check expected_rh_type ls exceptions= match expected_rh_type with
         |(PROVINCE_MTTH as new_scope) ->
			let exceptions = type_verify province_mtth ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (COUNTRY_MTTH as new_scope) ->
            let exceptions = type_verify province_mtth ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (PROVINCE_EFFECTS as new_scope) ->
			let exceptions= type_verify province_effects ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (PROVINCE_CONDITIONS as new_scope) ->
			let exceptions= type_verify province_conditions ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (COUNTRY_EFFECTS as new_scope) ->
			let exceptions= type_verify country_effects ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (COUNTRY_CONDITIONS as new_scope) ->
			let exceptions= type_verify country_conditions ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (POP_EFFECTS as new_scope) ->
			let exceptions= type_verify pop_effects ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (POP_CONDITIONS as new_scope) ->
			let exceptions= type_verify pop_conditions ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (STATE_EFFECTS as new_scope) ->
			let exceptions= type_verify state_effects ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (PARAM_LIST(sub_table) as new_scope)->
            let exceptions = type_verify sub_table ls exceptions (RHS[new_scope])in
			type_verify symbol_table rest exceptions (scope)
         | (APPEND_SYMBOLS(added_symbols,rest_of_expected_symbols)) ->
            let symbol_length = List.length added_symbols in
            let left_half,right_half = split_list symbol_length ls in
            let added_table = symbol_table_init added_symbols  in
            let exceptions = type_verify added_table left_half exceptions (RHS([PARAM_LIST(added_table)])) in
            let exceptions = assignlist_type_check ( (rest_of_expected_symbols)) right_half exceptions in
            type_verify symbol_table rest exceptions scope
         | (PARAM_OPTION(options)) ->
            let shortest_exception_list = (List.map (fun expected_rh_type -> 
                assignlist_type_check expected_rh_type ls []
            ) options)
            |> shortest_list in
            (match shortest_exception_list with 
             | Some e -> type_verify symbol_table rest (e@exceptions) scope
             | None -> 
                let e = UNEXPECTED_ASSIGN_LIST(ls)  in
                type_verify symbol_table rest ((scope,e,cords)::exceptions) scope
            )

         | (x) -> 
            let e = UNEXPECTED_ASSIGN_LIST(ls) in
            type_verify symbol_table rest ((RHS[x],e,cords)::exceptions) scope
        in
        (match expected_rh_type with
        |Some rh -> 
            let exceptions = assignlist_type_check rh ls exceptions in
            type_verify symbol_table rest exceptions scope
        |None -> 
            let e = UNKNOWN_IDENTIFIER(lh_value) in
            type_verify symbol_table rest ((scope,e,cords)::exceptions) scope
        )
    |(ASSIGNMENT ((_, _, cords), EXPR_EXCEPTION _) as assign) ::rest -> 
        let e = (UNEXPECTED_ASSIGNMENT (assign)) in
        type_verify symbol_table rest ((scope,e,cords)::exceptions) scope
    |ASSIGN_EXCEPTION((expected,exception_val,cords))::rest -> 
        type_verify symbol_table rest ((expected,exception_val,cords)::exceptions) scope
    | [] -> 
          exceptions

  
