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
    |Some _ -> 
            raise (Invalid_argument ("Catalog_Type " ^ type_label ^ " already exists in symbol table with non-TypeOption value: " ^ (Output.string_symbol (Hashtbl.find symbol_table (Definition(type_label)))))
            )
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
                let v = ( Hashtbl.to_seq (inner_table) |> Seq.find_map (fun (key ,inner_value) -> match key with
                |InnerCatalog(catalog_lable,v)   when (lex_symbol_eq lex_type lex_text v) -> 
                    Some (catalog_lable,v,inner_value,inner_table)
                |_ -> None 
                 ) )
                in
                (match v with
                |Some (cl,v,inner_value,inner_table) ->
                    Hashtbl.reset (inner_table); 
                    Hashtbl.add (inner_table) (InnerCatalog(cl,v)) inner_value;
                    Hashtbl.add (inner_table) (SubCatalog(lex_text,cl,v)) inner_value;

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





let find_type_left (lex_type:value_type) (lex_value) (symbol_table:(symbol_type, symbol_type) Hashtbl.t) outer_symbol_table = 
    (* Check if the lex_type is a type in the symbol table *)
    (* If it is, then we can use the lookup function to get the value from the symbol table *)
    (* If it is not, then we can check if it is a value *)
    (* Check if the lex_type is a type in the symbol table *)
    (* Find the type in the symbol table that matches the lex_type *)


    Hashtbl.to_seq symbol_table |> Seq.find_map (fun (key,value) ->  match key with
        |Type v  -> 
             (match (Hashtbl.find_opt outer_symbol_table (Definition v)  ) with
            |Some (Value expr_value) when expr_value = lex_type -> 
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





let  left_lex_lookup lex_value (lex_type:value_type) symbol_table outer_symbol_table = 
        (* Check if the lex_type is a literal in the symbol table *)
        (* If it is, then we can use the lookup function to get the value from the symbol table *)
        (* If it is not, then we can check if it is a type or a value *)
    
    (* Optimized: Remove redundant member check - find_opt already handles None case *)
    match Hashtbl.find_opt symbol_table (Literal(lex_value)) with
    | Some v -> Some v
    | None -> 
        (match Hashtbl.find_opt symbol_table (Value(lex_type)) with
        | Some (CatalogLeft (str,sym_typ))  ->
            catalog_type str (Literal lex_value) outer_symbol_table;
            Some sym_typ
        | Some v -> Some v
        | None -> 
            (* If the lex_type is a catalog, then we can use the catalog_type function to add the value to the catalog *)
            (match (find_catalog_left lex_type lex_value symbol_table) outer_symbol_table with
            |Some v -> Some v
            |None ->
                (* Optimized: Find first matching type and return its value in one pass *)
                let rec find_first_match types = match types with
                    | t::rest -> (match Hashtbl.find_opt symbol_table t with
                        | Some v -> Some v
                        | None -> find_first_match rest)
                    | [] -> None
                in
                (match find_first_match (get_umbtypes lex_type lex_value) with
                | Some v -> Some v
                | None -> 
                    match (find_type_left lex_type lex_value symbol_table outer_symbol_table) with
                    |Some v -> Some v
                    |None -> None))
        )
let is_directory x = 
    Sys.file_exists x && Sys.is_directory x


let map_folder  abs_file def= Array.fold_left (fun  rest f ->
                               ((Filename.concat abs_file f),def)::rest 

                            ) [] (Sys.readdir abs_file) 

exception File_not_found of string

let currate_dirrectory replace_paths mod_home game_home og_directory =  
    List.fold_left (fun acc (path,def) ->
        let abs_mod_path = Filename.concat mod_home path in
        let abs_game_path = Filename.concat game_home path in
        if List.mem path replace_paths then
            if is_directory abs_mod_path then
                map_folder abs_mod_path def @ acc
            else if Sys.file_exists abs_mod_path then
                (abs_mod_path,def)::acc
            else
                raise (File_not_found ("corrupted game files" ^abs_game_path) )
        else  
            let mod_files = (if is_directory abs_mod_path then
                map_folder abs_mod_path def @ acc
            else if Sys.file_exists abs_mod_path then
                (abs_mod_path,def)::acc
            else
                raise (File_not_found ("corrupted game files" ^abs_game_path) )
            )in

            if is_directory abs_game_path then
                map_folder abs_game_path def @ mod_files @ acc
            else if Sys.file_exists abs_game_path then
                (abs_game_path,def)::acc
            else
                raise (File_not_found ("corrupted game files" ^abs_game_path) )


    ) [] og_directory
     

let type_verify (outer_symbol_table:(symbol_type , symbol_type) Hashtbl.t) directory lexout astout mod_file =
    let aoc = 
       ( match astout with    
        |Some file -> 
            Some (open_out file) 
        | None -> None
       )
    in
    let loc = match lexout with
        |Some file -> Some (open_out file)
        |None -> None 
    in

    let rec symbol_table_from_rhv rhv = match rhv with
    | (SubTable(sub_table))->
        Some sub_table

    | ValueList(_) as v ->
        let t = Hashtbl.create 1 in
        Hashtbl.add t Nothing v;
        Some t


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
    Printf.eprintf "File:%s\n" file;

    if List.length assignments > 0 then
        Printf.eprintf "First assignment in %s is %s\n" file (Output.string_assignment (List.hd assignments))
    else
        Printf.eprintf "No assignments in %s\n" file;
    match assignments with
    |ASSIGNMENT(((LexemValue lh_type),lh_value,assign_cords), LEXEM_LIST(ls))::rest ->
        let expected_rh_type =  left_lex_lookup lh_value lh_type symbol_table outer_symbol_table 

        in

        let rec assign_type_check (expected_rh_type:symbol_type) ls exceptions =  

            match expected_rh_type,ls with
            |ValueList(SubTable inner_table),((ASSIGNMENT_LIST(ls)) ::rest) -> 

                let new_scope = SubTable inner_table in
                let table_exceptions= type_verify_r (inner_table) ls exceptions (RHS([new_scope])) file
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
            if lh_value = "path" then
                Printf.eprintf "Path assignment found in %s with value %s\n" file rh_value;

     let expected_rh_type = left_lex_lookup lh_value lh_type symbol_table  outer_symbol_table in
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
                let cords_string = Printf.sprintf " at %s:(%d:%d)" file y x in
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
        |Catalog(catalog_lable, Link) when rh_type = String  ->
            let gh =  match lookup outer_symbol_table (Definition "game_home_def") with
            |Dir dir -> dir
            |_-> raise (Invalid_argument ("game_home_def is not a directory in catalog link assignment for " ^ lh_value ^ " with value " ^ rh_value))
            in
            let absolute_path = (if Filename.is_relative (rh_value |> remove_quotes) then

                  (Printf.eprintf "Catalog link found in with value %s\n"  rh_value;
                  Filename.concat gh rh_value |> remove_quotes 
                  )
            else
                 remove_quotes rh_value )
            in
            catalog_type catalog_lable (Literal absolute_path) outer_symbol_table;
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
                    let assigns:(assignment list) = List.rev (Parser.assignments lexems in_mod_file) in
                    let exceptions = (type_verify_r (symbol_table) assigns exceptions (RHS([Definition "country_def"]))) in_mod_file 
                    in
                    exceptions
                |None ->
                    raise (Invalid_argument ("Symbol table for country_def not found in " ^ in_mod_file))

            else if (Sys.file_exists in_game_file) then
                let lexems = Lexer.lexer in_game_file in
                let symbol_table = symbol_table_from_rhv (lookup outer_symbol_table (Definition "country_def")) in
                let assigns:(assignment list) = List.rev (Parser.assignments lexems in_game_file) in
                match symbol_table with
                |Some symbol_table ->let exceptions = (type_verify_r (symbol_table) assigns exceptions (RHS([Definition "country_def"])) in_game_file)
                in
                exceptions
                |None ->
                    raise (Invalid_argument ("Symbol table for country_def not found in " ^ in_game_file))

            else
                let e = UNEXPECTED_LEXEM(rh_value,LexemValue rh_type) in
                let expected = (RHS [Link]) in
                ((expected,e,cords,file)::exceptions)

            |_ ->
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
                Hashtbl.iter (fun k v -> Hashtbl.add symbol_table k v) (sub_table);
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
            
        let e = (UNKNOWN_IDENTIFIER(lh_value) ) in             
        type_verify_r symbol_table rest ((scope,e,cords,file)::exceptions) scope file
    )
    |ASSIGNMENT((LexemValue lh_type,lh_value,cords),ASSIGNMENT_LIST(ls))::rest -> 

        let expected_rh_type = left_lex_lookup lh_value lh_type symbol_table outer_symbol_table  in
        
     let assignlist_type_check expected_rh_type ls exceptions= match expected_rh_type with
         | (Inherit(_) as new_scope)  ->
            (match symbol_table_from_rhv new_scope with
            |Some symbol_table -> 
                type_verify_r (symbol_table) ls exceptions (RHS([new_scope])) file
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
                    
                    let e = type_verify_r (symbol_table) ls [] (RHS([v])) file in
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
            |Some sub_table -> type_verify_r (sub_table) ls exceptions (RHS([new_scope])) file
            |None -> 
                raise (Invalid_argument ("Symbol table for  Type" ^ (Output.string_symbol new_scope) ^ " not found")))
            |None ->
                raise (Invalid_argument ("Type " ^ type_name ^ " not found in outer symbol table with assing list " ^ lh_value) 
                )
            )
        |SubTable(inner_table) as new_scope ->
             type_verify_r (inner_table) ls [] (RHS([new_scope]))  file
            
            
         | (x) -> 
            (* If the right-hand side is not a value, then we can use the UNEXPECTED_ASSIGN_LIST exception *)
            (* to indicate that the right-hand side is not a valid value for the left-hand side *)
            let e = UNEXPECTED_ASSIGN_LIST(ls) in
            ((RHS[x],e,cords,file)::exceptions)
        in
        (match expected_rh_type with
        |(Some rh) -> 
            
            let exceptions = (assignlist_type_check rh ls exceptions) in

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
                let table_exceptions= type_verify_r (inner_table) ls exceptions (RHS([new_scope])) file
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
let mod_file_table = Hashtbl.find_opt outer_symbol_table (Definition "mod_file_def") in
let mod_lexems = Lexer.lexer mod_file in

let mod_assigns = Parser.assignments mod_lexems mod_file in

let mod_exps =(match mod_file_table with
 |Some (SubTable mod_file_table) -> 
         let x = type_verify_r (mod_file_table)  mod_assigns [] (RHS([Definition "mod_name"])) mod_file  in 
         (*
         let symbol_table_str=  Output.string_symbol_table outer_symbol_table in
         Printf.printf "Symbol table for mod file %s:\n%s\n" mod_file symbol_table_str; 
*)
         x
 |None -> 
         raise (Invalid_argument ("mod_file_def not found in outer symbol table for mod file: " ^ mod_file) )
 |_ ->
         raise (Invalid_argument ("mod_file_def must be a SubTable in the outer symbol table for mod file: " ^ mod_file) )
  )
in
let replace_paths = match (Hashtbl.find_opt outer_symbol_table (Definition "replace_path") ) with
|Some (TypeOption( paths)) -> paths
|Some v -> raise (Invalid_argument ("replace_path must be a ValueList of ReplacePathList in outer symbol table, found " ^ (Output.string_symbol v) ) )
|None -> [] 
in
(*
let path = Hashtbl.find_opt outer_symbol_table (Definition "path") in 
*)

let mod_home = match (Hashtbl.find outer_symbol_table (Definition "mod_home_def")) with 
    |Dir mod_home -> mod_home
    |_ -> raise (Invalid_argument "mod_home_def must be a Dir in outer symbol")
in
let game_home = match (Hashtbl.find outer_symbol_table (Definition "game_home_def")) with 
    |Dir game_home -> game_home
    |_ -> raise (Invalid_argument "game_home_def must be a Dir in outer symbol")
in

let is_directory x = 
    Sys.file_exists x && Sys.is_directory x
in



let has_file tuple_list value stem = List.exists (fun (v,_) -> (Filename.concat stem v) = value) tuple_list
in


let rec mod_paths_r acc path_list =
    (match path_list with
    |(file,def)::rest_of_paths ->
        let abs_mod_file = Filename.concat mod_home file in
             if (is_directory abs_mod_file) then 
                 if not (has_file  path_list file mod_home) then
                    mod_paths_r acc rest_of_paths 
                 else
                 (let new_paths = Array.fold_left (fun  rest f ->
                     
                    ((Filename.concat file f),def)::rest 

                 ) [] (Sys.readdir abs_mod_file) in
                 mod_paths_r acc  (new_paths @rest_of_paths) 
                 )
             else if (Sys.file_exists abs_mod_file) then
                 (
                 mod_paths_r ((abs_mod_file,def)::acc) rest_of_paths 
                 )
             else
                 mod_paths_r acc rest_of_paths
    |[] -> SymbolTable.order_requirements acc
    )
in





let rec game_paths_r (acc) path_lists  = 
                    (match path_lists with

                    |(file,def)::rest_of_paths ->
                        let abs_game_file = Filename.concat game_home file in
                        (if (false) then(
                            game_paths_r acc rest_of_paths 
                        )
                        else
                            if (is_directory abs_game_file) && not (List.mem (Literal file) replace_paths)   then
                                let new_paths = Array.fold_left (fun  rest f ->
                                    let new_path = (Filename.concat file f) in
                                     if is_directory new_path then
                                        rest 
                                     else if Sys.file_exists (Filename.concat game_home new_path) then
                                        (new_path,def)::rest 
                                    else
                                        raise (File_not_found  ("corrupted game files" ^ (Filename.concat game_home new_path) ) ) 
                                    
                                ) [] (Sys.readdir abs_game_file) in
                                game_paths_r acc  (new_paths @rest_of_paths) 

                            else if (Sys.file_exists abs_game_file) then
                                    game_paths_r ((abs_game_file,def)::acc) rest_of_paths 
                            else 
                                raise (File_not_found  ("corrupted game files" ^abs_game_file) )
                        )
                    |[] -> 
                           SymbolTable.order_requirements acc
                    )
in
let updated_directory =  (game_paths_r [] directory) @ (mod_paths_r [] directory) in 


(*
let mod_file_path = match (Hashtbl.find_opt outer_symbol_table (Definition "modpath")) 
    with
    |Some (TypeOption [(Literal path)]) ->  
            Printf.eprintf "Mod file path found in symbol table: %s\n" path;
            path
    |Some (TypeOption paths) -> raise (Invalid_argument ("Multiple paths found mod file, expected only one, found: " ^ (String.concat ", " (List.map Output.string_symbol paths)) ) )
    |Some v -> raise (Invalid_argument ("Symbol Table Curruption, found " ^ (Output.string_symbol v) ) )
    |None -> raise (Invalid_argument "Path not found in symbol table for mod file")
in



if  (Unix.realpath mod_file_path)  <> (Unix.realpath mod_home) then
    raise (Invalid_argument ("Mod file path in symbol table does not match the provided mod file path: " ^ mod_file_path ^ " vs " ^ mod_home) )
else
*)


(mod_file,mod_exps) :: 
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
            Hashtbl.add (table) (Definition "pwd") (Dir (Filename.dirname filepath));
            let lexems = Lexer.lexer filepath in
            (match loc with
            |Some oc -> 
                Printf.fprintf oc "Lexems for %s:\n" filepath;
                List.iter (fun lexem -> Printf.fprintf oc "%s\n" (Output.string_lexem lexem)) lexems;
            |None -> ()
            );
            
            let assigns:(assignment list) =  (Parser.assignments lexems filepath) in
            let parser_exceptions =
                let rec parser_assign_exceptions_r assign out =  match assign with
                |ASSIGN_EXCEPTION v :: rest -> 
                        parser_assign_exceptions_r rest (v::out)
                |ASSIGNMENT(_,v) :: rest ->
                        let out = parser_expr_exceptions_r v out in
                        parser_assign_exceptions_r rest out 
                |EXPR expression :: rest ->
                        let out = parser_expr_exceptions_r expression out in
                        parser_assign_exceptions_r rest out
                |[] -> out
                and  parser_expr_exceptions_r expr out = match expr with
                    |EXPR_EXCEPTION v -> v::out 
                    |LEXEM_LIST ls -> 
                        List.fold_left (fun acc lexem -> 
                            parser_expr_exceptions_r lexem acc 
                        ) out ls
                    |ASSIGNMENT_LIST ls ->
                        parser_assign_exceptions_r ls out
                    |LEXEM _ -> out
            in
            List.rev (parser_assign_exceptions_r assigns [])
            in


            (match aoc with
            |Some oc -> 
                Printf.fprintf oc "AST for %s:\n" filepath;
                List.iter (fun assign -> Printf.fprintf oc "%s\n" (Output.string_assignment assign))  (List.rev assigns);
            |None -> ()
            );
            
            (match parser_exceptions with
            |[] -> 
                filepath,parser_exceptions@(type_verify_r (table) assigns [] (RHS([Type(symbol_table_key)])) filepath)
            |exp->
                filepath,exp
            )
        |None -> 
            raise (Invalid_argument ("Symbol table for  path defined " ^ symbol_table_key ^ " not found in " ^ filepath) )
        )
) updated_directory 
  
