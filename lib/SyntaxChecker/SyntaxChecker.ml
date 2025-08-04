open SymbolTable
open TypeDef

(*This function takes a list of exceptions and returns the shortest list of exceptions*)
(*The rh is a list*)


let lookup symbol_table (symbol:symbol_type) = 
    match Hashtbl.find_opt symbol_table symbol with
    | Some value -> value
    | None -> raise (Invalid_argument ("Symbol " ^ (Output.string_symbol symbol) ^" not found in lookup" 
    ));;
let remove_extension path =
  let re = Re2.create_exn "\\.[^.]+$" in
  Re2.replace_exn ~f:(fun _ -> "") re path
;;

let split_list n ls =
    let rec split_list_r lh rh i = match rh with
    |hd::rest when i < n -> 
        split_list_r (hd::lh) rest (i+1)
    |_->
        ((List.rev lh),rh)
    in

    split_list_r [] ls 0


(*
let get_subtypes t v = match t with
    |Decimal -> [ PositiveFloat;  NegativeFloat]
    |Integer -> [ PositiveInt; NegativeInt]
    |Number -> [PositiveFloat; NegativeFloat; PositiveInt; NegativeInt]
    |Year -> [PositiveInt]
    |Target -> [Tag;Scope]
    |WholeNumber when v != "0" -> [PositiveInt]
    |_-> [] 
*)
let remove_quotes s =
  match Re2.create "[\"']" with
  | Ok re -> Re2.replace_exn ~f:(fun _ -> "") re s
  | Error _ -> raise (Invalid_argument ("Quotes not found not found" ) )



let get_umbtypes t v = match t with
    |PositiveFloat -> [Decimal;Number;PositiveNumber]
    |NegativeFloat -> [Decimal;Number;NegativeNumber]
    |PositiveInt when String.length v = 4 -> [WholeNumber;Integer;Year;PositiveNumber;Number]
    |PositiveInt when v == "0" -> [Integer;PositiveNumber;WholeNumber;Number]
    |PositiveInt -> [Integer;Number;WholeNumber;PositiveNumber]
    |NegativeInt -> [Integer;Number;NegativeNumber]
    |Keyword -> [Identifier]
    |String -> [Identifier]
    |Tag -> [Target]
    |Scope -> [Target]
    |_ -> []

let catalog_type type_label value symbol_table = 
    
    match (Hashtbl.find_opt symbol_table (Definition(type_label))) with
    |Some TypeOption options->  
        Hashtbl.replace symbol_table (Definition(type_label)) (TypeOption (value::options));
        ()
    |Some _ -> ()
    |None -> 
        Hashtbl.add symbol_table (Definition(type_label)) (TypeOption([value])) ;
;;
let sub_catalog_type super_type sub_type (value:symbol_type) symbol_table = 
    match (Hashtbl.find_opt symbol_table (SubDefinition(super_type,sub_type))) with
    |Some TypeOption options->  
        
        Hashtbl.replace symbol_table (SubDefinition(super_type,sub_type)) (TypeOption (value::options));
        ()
    |Some _ -> ()
    |None -> 
        Hashtbl.add symbol_table (SubDefinition(super_type,sub_type)) (TypeOption([value])) ;
;;



let rec lex_symbol_eq lex_type lex_value symbol =
    match lex_type,symbol with
    |lv,Value v -> 
       lv = v
    |lv,TypeOption options -> 
        (match List.find_opt (fun v -> lex_symbol_eq lv lex_value v ) options with
        |Some _ -> true
        |None -> false)
    |lv,v when List.mem v (get_umbtypes lv lex_value) -> 
        true
    |_-> 
        false



let find_catalog_left lex_type lex_text symbol_table outer_symbol_table = 

    (* Check if the lex_type is a catalog in the symbol table *)
    (* If it is, then we can use the catalog_type function to add the value to the catalog *)
    (* If it is not, then we can check if it is a type or a value *)
    (* Check if the lex_type is a catalog in the symbol table *)
    (* Find the catalog in the symbol table that matches the lex_type *)

    
    
    
    Hashtbl.to_seq symbol_table |> Seq.find_map (fun (key,value) ->  
            
        match key with
        |Catalog(catalog_lable,v) when (lex_symbol_eq lex_type lex_text v)  -> 
           
           
            catalog_type catalog_lable (Literal lex_text) outer_symbol_table;
            Some value
        |SupCatalog(catalog_lable,v) when (lex_symbol_eq lex_type lex_text v) ->                            
            
                        
            catalog_type catalog_lable (Literal lex_text) outer_symbol_table;
            (match  value with
            |SubTable inner_table ->
                let v = ( Hashtbl.to_seq inner_table |> Seq.find_map (fun (key ,inner_value) -> match key with
                |InnerCatalog(catalog_lable,v)   when (lex_symbol_eq lex_type lex_text v) -> 
                    Some (catalog_lable,v,inner_value,inner_table)
                |_ -> None 
                 ) )
                in
                (match v with
                |Some (cl,v,inner_value,inner_table) ->
                    Hashtbl.reset inner_table; 
                    Hashtbl.add inner_table (InnerCatalog(cl,v)) inner_value;
                    Hashtbl.add inner_table (SubCatalog(lex_text,cl,v)) inner_value;

                    Some value
                |None ->
                    raise (Invalid_argument ("SupCatalog|"))
                )
         (*   Hashtbl.remove symbol_table (SupCatalog(catalog_lable,v)); 
            Hashtbl.add symbol_table (Catalog(catalog_lable,v)) (SubTable inner_table);
        *)
            

            |v->
                raise (Invalid_argument ("SupCatalog does not a have Catalog_Type:" ^ catalog_lable ^ "|with:" ^ lex_text ^ "|value:" ^ (Output.string_symbol v) ^ "| in outer symbol table"))
            )
        |SubCatalog(super_type,sub_type,v) when (lex_symbol_eq lex_type lex_text v)  -> 
            
            (* If it is not, then we can check if it is a type or a value *)
            (* Check if the lex_type is a SubCatalog in the symbol table *)
            (* Find the SubCatalog in the symbol table that matches the lex_type *)
            (*
            Printf.printf "SubCatalog->Super Type:%s SubType:%s\n" super_type sub_type;
    *)
            (* If it is not, then we can check if it is a type or a value *)
            (* Check if the lex_type is a SubCatalog in the symbol table *)
            (* Find the SubCatalog in the symbol table that matches the lex_type *)
            

            catalog_type sub_type (Literal lex_text) outer_symbol_table;
            (* If the lex_type is a SubCatalog, then we can use the sub_catalog_type function to add the value to the catalog *)
            (* If it is not, then we can check if it is a type or a value *)
            (* Check if the lex_type is a SubCatalog in the symbol table *)
            (* Find the SubCatalog in the symbol table that matches the lex_type *)
            sub_catalog_type super_type sub_type (Literal lex_text) outer_symbol_table;
            Some value
        |SupCatalog(catalog_lable,v) -> 
            (* If the lex_type is a SubCatalog, then we can use the sub_catalog_type function to add the value to the catalog *)
            (* If it is not, then we can check if it is a type or a value *)
            (* Check if the lex_type is a SubCatalog in the symbol table *)
            (* Find the SubCatalog in the symbol table that matches the lex_type *)
            raise (Invalid_argument ("SubCatalog " ^ catalog_lable ^ " not found in outer symbol table with sub type " ^  (Output.string_symbol v) ^ " and value " ^ lex_text)) 

        |_-> 
            
            
            None


    )   





let find_type_left lex_type lex_value symbol_table outer_symbol_table = 
    (* Check if the lex_type is a type in the symbol table *)
    (* If it is, then we can use the lookup function to get the value from the symbol table *)
    (* If it is not, then we can check if it is a value *)
    (* Check if the lex_type is a type in the symbol table *)
    (* Find the type in the symbol table that matches the lex_type *)
    Hashtbl.to_seq symbol_table |> Seq.find_map (fun (key,value) ->  match key with
        |Type v  -> 
             (match (Hashtbl.find_opt outer_symbol_table (Definition v)  ) with
             |Some Value v when v = lex_type -> 
                Some value
            |Some s when List.mem s (get_umbtypes lex_type lex_value)  -> 
                Some value 
            |Some TypeOption options->
                (if (List.mem (Value lex_type) options ||
                   List.mem (Literal lex_value) options ||
                   List.exists (fun v -> List.mem v (get_umbtypes lex_type lex_value) ) options
                    )
                then
                    Some value
                else
                    None
                )
            |Some s ->
            
                raise (Invalid_argument ("Type " ^ v ^ " not found in outer symbol table, found " ^ (Output.string_symbol s) ^ " instead"))

            |None ->
                None
            )
       (* |SubType (_,sub_type) ->
            (match (Hashtbl.find_opt outer_symbol_table (SubDefinition(lex_value,sub_type)) ) with
            |Some Value v when v = lex_type -> 
                Some value
            |Some s when List.mem s (get_umbtypes lex_type lex_value)  -> 
                Some value 
            |Some TypeOption options->
                (if (List.mem (Value lex_type) options ||
                   List.mem (Literal lex_value) options ||
                   List.exists (fun v -> List.mem v (get_umbtypes lex_type lex_value) ) options
                    )
                then
                    Some value
                else
                    None
                )
            |Some s ->
                raise (Invalid_argument ("SubType " ^ sub_type ^ " not found in outer symbol table, found "^ (Output.string_symbol s) ^ " instead"))

            |None ->
                (* If the sub type is not found, then we can return None *)
                None
            )
    *)
        |_-> 
            None
    )





let  left_lex_lookup lex_value lex_type symbol_table outer_symbol_table = 
        (* Check if the lex_type is a literal in the symbol table *)
        (* If it is, then we can use the lookup function to get the value from the symbol table *)
        (* If it is not, then we can check if it is a type or a value *)
    
    
    match lex_type with 
    |_ when (member symbol_table (Literal(lex_value)) ) ->
        Hashtbl.find_opt symbol_table (Literal(lex_value))
        
    |lex_type when (member symbol_table (Value(lex_type)) )  -> 
        Hashtbl.find_opt symbol_table (Value(lex_type))    

    |lex_type ->

        (* If the lex_type is a catalog, then we can use the catalog_type function to add the value to the catalog *)
        (
        match (find_catalog_left lex_type lex_value symbol_table) outer_symbol_table with
        |Some v -> 

            
                Some v
        |None ->


            let v = List.find_opt (fun t-> member symbol_table ((t))) (get_umbtypes lex_type lex_value)
            in
            (match v with
            |Some v -> (Hashtbl.find_opt symbol_table (v))
            |None -> 
                match (find_type_left lex_type lex_value symbol_table outer_symbol_table) with
                |Some v -> Some v
                |None ->None)
        )


let type_verify (outer_symbol_table:(symbol_type , symbol_type) Hashtbl.t) directory lexout astout =
    let aoc = 
       ( match astout with    
        |Some file -> 
            Some (open_out file) 
        | None -> None
       )
    in

    let rec symbol_table_from_rhv rhv = match rhv with
    | (SubTable(sub_table))->
        Some sub_table

    | Inherit(appended_symbols,tables) -> 
        let rhs = List.filter_map (fun x -> Hashtbl.find_opt outer_symbol_table (Definition x) ) tables in
        let symbol_tables = List.filter_map  symbol_table_from_rhv rhs in 

        let table = combine_table_list symbol_tables in
        append_table table appended_symbols; 
        Some table
    | Type(type_name) ->
        (match ( Hashtbl.find_opt outer_symbol_table (Definition type_name)) with
       |Some rhs ->
            symbol_table_from_rhv rhs
       |None ->
           None
        )

    | TypeOption(options) ->
        let rec find_option options= match options with  
        |[] -> None        
        |v::rest ->
                match (symbol_table_from_rhv v ) with
                |Some table -> Some table
                |None -> 
                    find_option rest


        in

        find_option options

    | _ -> None
    in



let rec type_verify_r symbol_table (assignments:assignment list) (exceptions:exception_value list) scope file= 

    match assignments with
    |ASSIGNMENT(((LexemValue lh_type),lh_value,assign_cords), LEXEM_LIST(ls))::rest ->
        let expected_rh_type =  left_lex_lookup lh_value lh_type symbol_table outer_symbol_table 

        in

        let rec assign_type_check (expected_rh_type:symbol_type) ls exceptions =  

            match expected_rh_type,ls with
            |ValueList(SubTable inner_table),((ASSIGNMENT_LIST(ls)) ::rest) -> 

                let new_scope = SubTable inner_table in
                let table_exceptions= type_verify_r inner_table ls exceptions (RHS([new_scope])) file
                in
                assign_type_check expected_rh_type rest table_exceptions
            |ValueList(list_erh_type),(LEXEM(LexemValue rh_type,rh_value,cords)::rest)  -> 

                (match list_erh_type with
                |Value(erh_type) when erh_type = rh_type  -> 
                    assign_type_check expected_rh_type rest exceptions
                |Value(erh_type) when erh_type !=  rh_type  ->
                    let e = (TYPE_MISHMASH(lh_value, erh_type, rh_type)) in
                    let new_exceptions=((RHS [Value(erh_type)],e,cords,file)::exceptions) in
                    assign_type_check expected_rh_type rest new_exceptions
                |Catalog(catalog_lable,Value ev) when ev = rh_type  -> 
                    catalog_type catalog_lable (Literal rh_value) outer_symbol_table;
                    assign_type_check expected_rh_type rest exceptions
                |Catalog(catalog_lable, ev) when List.mem ev (get_umbtypes rh_type rh_value)  ->

                    catalog_type catalog_lable (Literal rh_value) outer_symbol_table;
                    assign_type_check expected_rh_type rest exceptions
                |ev when List.mem ev (get_umbtypes rh_type rh_value)   ->
                    assign_type_check expected_rh_type rest exceptions
                |TypeOption(options)  ->

                    if List.mem (Value rh_type) options ||
                    List.mem (Literal rh_value) options ||
                    List.exists (fun v -> List.mem v (get_umbtypes rh_type rh_value) ) options
                    then
                        assign_type_check expected_rh_type rest exceptions
                    else
                        let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                        let expected = (RHS [TypeOption options]) in
                        ((expected,e,cords,file)::exceptions)                    

                |_ ->

                    let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                    let expected = (RHS [expected_rh_type]) in
                    ((expected,e,cords,file)::exceptions)

                )
            |_,[] ->                     
                    exceptions
            |_,_ -> 
                let e = UNEXPECTED_EXPR_LIST(ls) in
                let expected = (RHS [expected_rh_type]) in
                ((expected,e,assign_cords,file)::exceptions) 

        in
        (match expected_rh_type with
            |Some rhs -> 

                let new_exceptions = assign_type_check rhs ls exceptions in
                type_verify_r symbol_table rest new_exceptions scope file
            |None -> 
                (* If the left-hand side is not a value, then we can use the UNKNOWN_IDENTIFIER exception *)
                (* to indicate that the left-hand side is not a valid value for the right-hand side *)
                let e = (UNKNOWN_IDENTIFIER(lh_value) ) in             
                type_verify_r symbol_table rest ((scope,e,assign_cords,file)::exceptions) scope file
            )

    |ASSIGNMENT((LexemValue lh_type,lh_value,_), LEXEM((LexemValue rh_type,rh_value,cords) as rh_token))::rest  ->
     let expected_rh_type = left_lex_lookup lh_value lh_type symbol_table  outer_symbol_table in
     
     
     (* This function checks the type of the left-hand side of the assignment against the expected type *)
     (* If the left-hand side is a catalog, then we can use the catalog_type function to add the value to the catalog *)
     (* If the left-hand side is a type, then we can use the symbol_table_from_rhv function to get the symbol table for that type *)
     (* If the left-hand side is a value, then we can use the lookup function to get the value from the symbol table *)

     let rec assign_type_check expected_rh_type exceptions =  
        match expected_rh_type with
        |Value(erh_type) when erh_type = rh_type  -> 
            exceptions 
        |(Value(erh_type) as epv) when erh_type !=  rh_type  ->
            let e = (TYPE_MISHMASH(lh_value, erh_type, rh_type)) in
            (((RHS [epv]),e,cords,file)::exceptions) 
        |Literal v when v = rh_value ->
            exceptions
        |Type v ->

            (match Hashtbl.find_opt outer_symbol_table (Definition v) with
            |Some rhs ->
                assign_type_check rhs exceptions
            |None ->
                let cords_string = Printf.sprintf " at %s:%s" file (Output.string_lexem rh_token) in
                raise (Invalid_argument ("Type " ^ v ^ " not found in outer symbol table" ^ cords_string ))
            )

        |SubType (sup_type,sub_type) ->

            (match Hashtbl.find_opt outer_symbol_table (SubDefinition(lh_value,sub_type)) with
            |Some rhs ->
                assign_type_check rhs exceptions
            |None ->
                let x,y = cords in
                let cords_string = Printf.sprintf " at %s:(%d:%d)" file x y in
                raise (Invalid_argument ("SubType lookup| Left Hand:" ^ lh_value ^ 
                "|cords: " ^ cords_string 
                ^ "| with Subdef "  ^ Output.string_symbol (SubDefinition(lh_value,sub_type)) 
                ^ "| sup_type: " ^ sup_type  
                ^"| rh_value:"^ rh_value  ) 
                )
            )

        |Catalog(catalog_lable,Value ev) when ev = rh_type  ->
            catalog_type catalog_lable (Literal rh_value) outer_symbol_table;
            exceptions
        |Link ->
            let mod_home = lookup outer_symbol_table (Definition "mod_home_def") in
            let game_home = lookup outer_symbol_table (Definition "game_home_def") in 
            let pwd = lookup symbol_table (Definition "pwd") in
            (* Remove quotes from the rh_value *)
            let rh_value = remove_quotes rh_value in
            
            (match mod_home,game_home,pwd with
            |Dir _, Dir _, Dir cur_dir ->
                let in_mod_file =  Printf.sprintf "%s/%s"  cur_dir  rh_value in
                let in_game_file = Printf.sprintf "%s/%s"  cur_dir  rh_value in

            if Sys.file_exists in_mod_file then
                let lexems = Lexer.lexer in_mod_file in
                let symbol_table = symbol_table_from_rhv (lookup outer_symbol_table (Definition "country_def")) in
                match symbol_table with
                |Some symbol_table ->
                    let assigns:(assignment list) =  (Parser.assignments lexems in_mod_file) in
                    let exceptions = (type_verify_r symbol_table assigns exceptions (RHS([Definition "country_def"]))) in_mod_file 
                    in
                    exceptions
                |None ->
                    raise (Invalid_argument ("Symbol table for country_def not found in " ^ in_mod_file))

            else if (Sys.file_exists in_game_file) then
                let lexems = Lexer.lexer in_game_file in
                let symbol_table = symbol_table_from_rhv (lookup outer_symbol_table (Definition "country_def")) in
                let assigns:(assignment list) = List.rev (Parser.assignments lexems in_game_file) in
                match symbol_table with
                |Some symbol_table ->let exceptions = (type_verify_r symbol_table assigns exceptions (RHS([Definition "country_def"])) in_game_file)
                in
                exceptions
                |None ->
                    raise (Invalid_argument ("Symbol table for country_def not found in " ^ in_game_file))

            else
                let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                Printf.printf "Link1 not found: %s in %s or %s\n" rh_value in_mod_file in_game_file;
                let expected = (RHS [Link]) in
                ((expected,e,cords,file)::exceptions)

            |_ ->
                Printf.printf "Link2 not found: %s \n" rh_value;
                let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in


                let expected = (RHS [Link]) in
                ((expected,e,cords,file)::exceptions)
            )
            



        |erh when List.mem erh (get_umbtypes rh_type rh_value)   ->
            exceptions
        |Switch ls ->
            let rec handle_switch ls = 
                match ls with
                |(label,SubTable values)::_ when label = rh_value ->
                    Some values;
                |(label, Type v )::_ when label = rh_value ->
                    let rhs = lookup outer_symbol_table (Definition v) in
                    let sub_table = symbol_table_from_rhv rhs in
                    sub_table
                |(label,_)::_ when label = rh_value ->
                     raise (Invalid_argument "Invalid body in switch statement")
                |(_,_)::rest ->
                    handle_switch rest
                |[] -> None
            in 
            (match handle_switch ls with
            |Some sub_table ->
                Hashtbl.iter (fun k v -> Hashtbl.add symbol_table k v) sub_table;
                exceptions
            |None ->
                let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                let expected = (RHS [Switch ls]) in
                ((expected,e,cords,file)::exceptions)
            )


        |TypeOption(options) ->
            let rec shortest_exception_list_r options = match options with 
                |[] -> 
                        ((RHS options,UNEXPECTED_LEXEM(rh_value,LexemValue rh_type),cords,file)::exceptions)
                |top::rest ->
                    (match (assign_type_check top []) with
                    |[] -> exceptions 
                    |_-> shortest_exception_list_r rest 
                    )
            in
            shortest_exception_list_r options  
        |x -> 
           let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
           let expected = (RHS [x]) in
           ((expected,e,cords,file)::exceptions) 
    in   
    (match expected_rh_type with
    |Some rh -> 

        let exceptions = assign_type_check rh exceptions in
        type_verify_r symbol_table rest exceptions scope file
    |None ->
        if lh_value = "is_jingoism" then
            Printf.printf "Jingoism found in %s\n" (Output.string_symbol_table symbol_table);
            
        let e = (UNKNOWN_IDENTIFIER(lh_value) ) in             
        type_verify_r symbol_table rest ((scope,e,cords,file)::exceptions) scope file
    )
    |ASSIGNMENT((LexemValue lh_type,lh_value,cords),ASSIGNMENT_LIST(ls))::rest -> 

        let expected_rh_type = left_lex_lookup lh_value lh_type symbol_table outer_symbol_table  in
        
     let assignlist_type_check expected_rh_type ls exceptions= match expected_rh_type with
         | (Inherit(_) as new_scope)  ->
            (match symbol_table_from_rhv new_scope with
            |Some symbol_table -> 
                type_verify_r symbol_table ls exceptions (RHS([new_scope])) file
            |None ->
                raise (Invalid_argument ("Symbol table for Inherit " ^ (Output.string_symbol new_scope) ^ " not found")))


         | (TypeOption(values)) ->
            let rec get_exceptions lst (lowest_exception:symbol_type list) =
              match lst with
              | [] -> 

                    (match lowest_exception with
                    |[] -> None 
                    |v -> Some (RHS v)
                    )
              |((Inherit(_,_)) as v) ::rest
              |((Type(_)) as v )::rest  
              
              |((SubTable(_)) as v) ::rest  ->

              (match symbol_table_from_rhv v with
              |Some symbol_table -> 
                    
                    let e = type_verify_r symbol_table ls [] (RHS([v])) file in
                    (match e with
                    |[] -> None 
                    |_-> 
                        get_exceptions rest (v::lowest_exception)
                    )
              |None ->
                      let x,y = cords in
                      raise (Invalid_argument ("Symbol table for Option:" ^ (Output.string_symbol v) ^ " not found" ^ lh_value ^ " " ^ string_of_int x ^ " " ^ string_of_int y) 
                      )
              ) 
              |((TypeOption (_)) as v) ::_ ->
                     raise (Invalid_argument ("TypeOption not supported in TypeOption: " ^ (Output.string_symbol v) ^ " in " ^ lh_value))

              |v::rest -> 
                get_exceptions rest (v::lowest_exception)
              

            in
            (match (get_exceptions values [] ) with
            |Some v -> 
                    ((v,UNEXPECTED_ASSIGN_LIST(ls),cords,file)::exceptions)
            |None -> exceptions
            )

        |Type(type_name) as new_scope ->
            (match Hashtbl.find_opt outer_symbol_table (Definition type_name) with 
            |Some rhs ->
        
            (match symbol_table_from_rhv rhs with 
            |Some sub_table -> type_verify_r sub_table ls exceptions (RHS([new_scope])) file
            |None -> 
                raise (Invalid_argument ("Symbol table for  Type" ^ (Output.string_symbol new_scope) ^ " not found")))
            |None ->
                raise (Invalid_argument ("Type " ^ type_name ^ " not found in outer symbol table with assing list " ^ lh_value) 
                )
            )
        |SubTable(inner_table) as new_scope ->
             type_verify_r inner_table ls [] (RHS([new_scope]))  file
            
            
         | (x) -> 
            (* If the right-hand side is not a value, then we can use the UNEXPECTED_ASSIGN_LIST exception *)
            (* to indicate that the right-hand side is not a valid value for the left-hand side *)
            let e = UNEXPECTED_ASSIGN_LIST(ls) in
            ((RHS[x],e,cords,file)::exceptions)
        in
        (match expected_rh_type with
        |(Some rh) -> 
            
            let exceptions = assignlist_type_check rh ls exceptions in

            type_verify_r symbol_table rest exceptions scope file
        |None -> 
            let e = UNKNOWN_IDENTIFIER(lh_value) in

            type_verify_r symbol_table rest ((scope,e,cords,file)::exceptions) scope file
        )
    
    
    
    |EXPR(LEXEM_LIST ls)::rest ->
        let expected_opt = Hashtbl.find_opt symbol_table Nothing in
        let rec assign_type_check (expected_rh_type:symbol_type) ls exceptions =  
            match expected_rh_type,ls with
            |_,[] -> 
                    exceptions
            |ValueList(SubTable inner_table),((ASSIGNMENT_LIST(ls)) ::rest) -> 

                (* If the right-hand side is a subtable, then we can use the symbol_table_from_rhv function to get the symbol table for that type *)
                let new_scope = SubTable inner_table in
                let table_exceptions= type_verify_r inner_table ls exceptions (RHS([new_scope])) file
                in
                assign_type_check expected_rh_type rest table_exceptions
            |ValueList(list_erh_type),((LEXEM(LexemValue rh_type,rh_value,cords))::rest)  -> 
                (match list_erh_type with
                |Value(erh_type) when erh_type = rh_type  -> 
                    assign_type_check expected_rh_type rest exceptions
                |Value(erh_type) when erh_type !=  rh_type  ->
                    let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                    let new_exceptions=((RHS [Value(erh_type)],e,cords,file)::exceptions) in
                    assign_type_check expected_rh_type rest new_exceptions
                |Catalog(catalog_lable,Value ev) when ev = rh_type  -> 
                    catalog_type catalog_lable (Literal rh_value) outer_symbol_table;
                    assign_type_check expected_rh_type rest exceptions
                |Catalog(catalog_lable, ev) when List.mem ev (get_umbtypes rh_type rh_value)  ->

                    catalog_type catalog_lable (Literal rh_value) outer_symbol_table;
                    assign_type_check expected_rh_type rest exceptions
                |ev when List.mem ev (get_umbtypes rh_type rh_value)   ->
                    assign_type_check expected_rh_type rest exceptions
                |TypeOption(options)  ->
                    if List.mem (Value rh_type) options ||
                    List.mem (Literal rh_value) options ||
                    List.exists (fun v -> List.mem v (get_umbtypes rh_type rh_value) ) options
                    then
                        assign_type_check expected_rh_type rest exceptions
                    else
                        let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                        let expected = (RHS [TypeOption options]) in
                        ((expected,e,cords,file)::exceptions)                    
                |_ ->
                    (* If the right-hand side is not a value, then we can use the UNEXPECTED_LEXEM exception *)
                    (* to indicate that the right-hand side is not a valid value for the left-hand side *)

                    (* If the right-hand side is not a value, then we can use the UNEXPECTED_LEXEM exception *)
                    (* to indicate that the right-hand side is not a valid value for the left-hand side *)
                    let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                    let expected = (RHS [expected_rh_type]) in
                    ((expected,e,cords,file)::exceptions)
                )
            |ValueList(_),_->
                (*print list*)

                exceptions
            |_,rest -> 
                (*print list*)
                (* If the right-hand side is not a value, then we can use the UNEXPECTED_EXPR_LIST exception *)
                (* to indicate that the right-hand side is not a valid value for the left-hand side *)
                (* If the right-hand side is not a value, then we can use the UNEXPECTED_EXPR_LIST exception *)
                (* to indicate that the right-hand side is not a valid value for the left-hand side *)
                let e = UNEXPECTED_EXPR_LIST(rest) in
                let expected = (RHS [expected_rh_type]) in
                ((expected,e,(0,0),file)::exceptions) 

        in
        let exceptions = (match expected_opt with
        |Some expected ->
             assign_type_check expected ls exceptions 
        |None ->
                let x,y =match List.hd ls with
                    |LEXEM(_,_,(x,y)) ->  x,y
                    |ASSIGNMENT_LIST( ls ) ->
                        ( match List.hd ls with
                        |ASSIGNMENT((_,_,(x,y)),_) -> x,y
                        |_-> 1,1) 
                    |LEXEM_LIST (LEXEM(_,_,(x,y) )::_) -> 
                        x,y
                    |LEXEM_LIST _ -> 1,1
                    |EXPR_EXCEPTION v -> 
                        raise (Invalid_argument ("EXPR_EXCEPTION not expected in EXPR_LIST: " ^ (Output.exception_string v) ))
                in

            let v = Printf.sprintf "Expected type not found in symbol table for EXPR_LIST: %s at %s:%i,%i\n" (Output.string_expected_value scope) file x y in
            raise (Invalid_argument v)
        ) in
        type_verify_r symbol_table rest exceptions scope file
             

    |(ASSIGNMENT ((_, _, cords), EXPR_EXCEPTION _) as assign) ::rest -> 
        let e = (UNEXPECTED_ASSIGNMENT (assign)) in
        type_verify_r symbol_table rest ((scope,e,cords,file)::exceptions) scope file
    |ASSIGN_EXCEPTION((expected,exception_val,cords,file))::rest -> 
        type_verify_r symbol_table rest ((expected,exception_val,cords,file)::exceptions) scope file

    |ASSIGNMENT(_)::_ -> 
        exceptions
    |(EXPR(x) ) ::rest ->
    let e = UNEXPECTED_EXPR(x) in
    type_verify_r symbol_table rest ((scope,e,(0,0),file)::exceptions) scope file

    | [] -> 
           exceptions
in
let filename_extract path regex =

    let re = Re2.create_exn regex in 
  match Re2.find_submatches re path with
  | Ok [| _; Some name |] -> Some name
  | _ -> None 

in
List.map (function  
    |filepath, (symbol_table_key) ->


        (*
        print_memory_stats ("After compact: " ^ filepath);
        *)
        let current_context  = match  Hashtbl.find_opt outer_symbol_table (Definition symbol_table_key)  with
        |Some (CatalogFile (catalog,regex, symbol) ) ->
            (match filename_extract filepath regex with
            |Some name -> 
                catalog_type catalog (Literal name) outer_symbol_table;
                symbol
            |None -> raise (Invalid_argument ("reject not found " ^ filepath))
            )

        |Some v -> v
        |None -> raise (Invalid_argument ("Symbol table for path defined " ^ symbol_table_key ^ " not found in " ^ filepath) )
        in
        let table = symbol_table_from_rhv current_context in
        (match table with
        |Some table ->
            Hashtbl.add table (Definition "pwd") (Dir (Filename.dirname filepath));
            let lexems = Lexer.lexer filepath in
            (match lexout with
            |Some lexfile -> 
                let oc = open_out lexfile in
                Printf.fprintf oc "Lexems for %s:\n" filepath;
                List.iter (fun lexem -> Printf.fprintf oc "%s\n" (Output.string_lexem lexem)) lexems;
                close_out oc
            |None -> ()
            );
            
            let assigns:(assignment list) =  (Parser.assignments lexems filepath) in
            (match aoc with
            |Some os -> 

                Printf.fprintf oc "AST for %s:\n" filepath;
                List.iter (fun assign -> Printf.fprintf oc "%s\n" (Output.string_assignment assign)) assigns;

                close_out oc
            |None -> ()
            );




            filepath,(type_verify_r table assigns [] (RHS([Type(symbol_table_key)])) filepath)
        |None -> 
            raise (Invalid_argument ("Symbol table for  path defined " ^ symbol_table_key ^ " not found in " ^ filepath) )
        )
) directory 
  
