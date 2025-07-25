open SyntaxChecker
open Cmdliner
open TypeDef
exception File_not_found of string

let is_directory x = 
    Sys.file_exists x && Sys.is_directory x


let () = 

    let make_arg title shorter doc  = 
        Arg.(value &  opt (some string) None & info [title;shorter] ~doc) 
    in
    let game_value = make_arg "game" "g" "Games Choice: Victoria2, Eu4, Hoi4, imperator, CK3" in
    let mod_home = make_arg "mod-dir" "m" "Mod Home Directory" in
    let game_home = make_arg "game-dir" "h" "Game Home Directory" in
    let output_file = make_arg "output" "o" "Output File" in
    let lexoutput = make_arg "lex-output" "l" "File to output lexems" in
    let astoutput = make_arg "ast-output" "a" "File to output ast" in
    
    let game_symbols game  = match game with
    |Some "Victoria2" -> Some (Victoria2.victoria2_paths, Victoria2.victoria2_symbol_table)
    |_ -> None
    in
    let doc = "Paradox Mod Checker" in
    let info = Cmd.info "" ~doc in
(* List files in a directory and filter by glob pattern *)

    let term = Term.(const (fun game mh gh out lexoutput astoutput -> 

        match (mh,gh) with
        |(Some mod_home), (Some game_home) -> 
            ( match game_symbols game with
            |(Some (paths, symbol_table)) -> 

                Hashtbl.add symbol_table (Definition "mod_home_def") (Dir mod_home);
                Hashtbl.add symbol_table (Definition "game_home_def") (Dir game_home);
                let rec new_paths_r acc path_lists = 
                    (match path_lists with

                    |(file,def)::rest_of_paths ->
                        let abs_mod_file = Filename.concat mod_home file in

                        Printf.eprintf "%s\n" abs_mod_file;
                        let abs_game_file = Filename.concat game_home file in
                        if (is_directory abs_mod_file) then
                            let new_paths = Array.fold_left (fun  rest f ->
                               ((Filename.concat file f),def)::rest 

                            ) [] (Sys.readdir abs_mod_file) in
                            new_paths_r acc  (new_paths @rest_of_paths)
                        else if (is_directory abs_game_file) then
                            let new_paths = Array.fold_left (fun  rest f ->
                               ((Filename.concat file f),def)::rest 
                            ) [] (Sys.readdir abs_game_file) in
                            new_paths_r acc  (new_paths @rest_of_paths)

                        else
                            if (Sys.file_exists abs_mod_file) then
                                new_paths_r ((abs_mod_file,def)::acc) rest_of_paths 
                            else if (Sys.file_exists abs_game_file) then
                                new_paths_r ((abs_game_file,def)::acc) rest_of_paths 
                            else
                                raise (File_not_found ("corrupted game files" ^abs_game_file) )
                    |[] -> acc
                    )

                 
                in
                let new_paths = new_paths_r [] paths in
            
                let exceptions = type_verify symbol_table new_paths lexoutput astoutput 
                in
                let output = 
                    match out with
                    |Some file -> open_out file 
                    |None -> stdout
                in
                Printf.eprintf "Checking Files\n"; 
                (List.iter (fun (file,x) -> x
                |> List.rev
                |> Output.exceptions_string 
                |> Printf.fprintf output "%s:\n %s\n" file;)
                (List.rev exceptions) )
            |None -> Printf.printf "Game not recognized\n"
        )
        |((Some mod_home), None) -> 
                Printf.printf "No Game Provided for mod:%s\n" mod_home;
        |(None, Some game_home) -> Printf.printf "No Mod Home Provided for game:%s\n" game_home

        |(None,None) -> Printf.printf "No Game\n")  $ game_value $ mod_home $ game_home $ output_file $ lexoutput $ astoutput )
    in
    let cmd = Cmd.v info term 
    in
    Cmd.eval cmd |> Printf.printf "%i\n";


       
    
    
    
    
   (*
   let folder_exists folder_path = Sys.file_exists folder_path && Sys.is_directory folder_path
   in
    (*
   let mod_home =  
        let doc = "Path to Victoria2 Mod Files" in
        Arg.(value & opt (some String) None & info ["md"; "mod-dir"] ~doc) 
    in
    *)
    let make_arg title doc  = 
        Arg.(value & opt (some String) None & info [title] ~doc) 
    in

    (*
    let lex_output = output_file_of_arg "File to output lexems" "lex-output" in
    let ast_output = output_file_of_arg "File to output ast" "ast-output" in
    let ast_errors = output_file_of_arg "File to output ast errors" "ast-errors"
    in
    let type_errors = output_file_of_arg "File to output type errors" "type-errors"
    in
    *)
    let output_to_file file_name arg str = 
        match arg with
        |Some "stdout" -> Printf.printf "%s:%s\n" file_name str
        |Some ol -> Printf.fprintf (open_out ol) "%s:%s\n" file_name str 
        |None -> () 
    in
    let doc = "My_program does nothing but output 'cow'." in
    let info = Cmd.info "my_program" ~doc in
    let files_arg title doc =
        Arg.(value & opt (some (list String) ) None & info [title] ~doc)
    in
let filter_files raw_files selected_files = match selected_files with
    |Some files -> List.filter (fun x -> List.mem x files) raw_files
    |None -> raw_files
    in
    
let parse_env mod_home lex_output ast_output ast_errors type_errors selected_categories event_files common_files = 
    match mod_home with
    |Some path when folder_exists path ->
        let raw_entries = Array.to_list(Sys.readdir path) in
        let refined_categories = filter_files raw_entries selected_categories in 
        List.iter 
        (function
            |"events" as entry->
                let raw_events = Array.to_list (Sys.readdir (Filename.concat path entry)) in
                let refined_events = filter_files raw_events event_files in
                
                List.iter (fun event_file -> 
                    let event_file = Filename.concat (Filename.concat path entry) event_file in
                    let lexems:Lexer.lexem list= event_file |> Lexer.lexer  in
                    (
                    lexems 
                    |> Lexer.String_lexems 
                    |> output_to_file event_file lex_output;
                    );
                    if ast_output = None &&  ast_errors =None && type_errors = None then ();
                        

                    let assignments = lexems |> Pre_parser.assignments in

                    (assignments
                    |> Pre_parser.String_assignment_list  
                    |> output_to_file event_file ast_output;    
                    );

                    (assignments
                    |> Pre_parser.get_errors  
                    |>  (fun x ->     
                        if x <> [] then 
                        x 
                        |> Pre_parser.exceptions_String 
                        |> output_to_file event_file  ast_errors 
                    )
                    );
                    (assignments
                    |> Type_verify.type_verify Events.events  
                    |> (fun x -> 
                        x 
                        |> Pre_parser.exceptions_String 
                        |> output_to_file event_file type_errors;
                        ); 
                    );
                ) refined_events; 
            |"decisions" as entry ->
                let raw_decisions = Array.to_list (Sys.readdir (Filename.concat path entry)) in
                let refined_decisions = filter_files raw_decisions event_files in
                
                List.iter (fun event_file -> 
                    let event_file = Filename.concat (Filename.concat path entry) event_file in
                    let lexems:Lexer.lexem list= event_file |> Lexer.lexer  in
                    (
                    lexems 
                    |> Lexer.String_lexems 
                    |> output_to_file event_file lex_output;
                    );
                    if ast_output = None &&  ast_errors =None && type_errors = None then ();
                        
                                                                                                
                    let assignments = lexems |> Pre_parser.assignments in
                                                                                                
                    (assignments
                    |> Pre_parser.String_assignment_list  
                    |> output_to_file event_file ast_output;    
                    );
                                                                                                
                    (assignments
                    |> Pre_parser.get_errors  
                    |>  (fun x ->     
                        if x <> [] then 
                        x 
                        |> Pre_parser.exceptions_String 
                        |> output_to_file event_file  ast_errors 
                    )
                    );
                    (assignments
                    |> Type_verify.type_verify Decisions.decisions  
                    |> (fun x -> 
                        x 
                        |> Pre_parser.exceptions_String 
                        |> output_to_file event_file type_errors;
                        ); 
                    );
                ) refined_decisions; 
            |"countries"  -> ()
            |"common" as entry-> 
                let raw_common = Array.to_list (Sys.readdir (Filename.concat path entry)) in
                let refined_common = filter_files raw_common common_files in
                List.iter (fun entry -> 
                    let common_file = Filename.concat (Filename.concat path "common") entry in
                    print_endline common_file;
                    match entry with
                    |"countries" -> ()
                    |"bookmarks.txt" -> ()
                    |"buildings.txt" -> ()
                    |"cb_types.txt" -> ()
                    |"countries.txt" -> ()
                    |"cot_colors.txt" -> ()
                    |"crime.txt" -> ()
                    |"cultures.txt" -> ()
                    |"defines.lua" -> ()
                    |"event_modifiers.txt" -> ()
                    |"goods.txt" -> ()
                    |"graphicalculturetype.txt" -> ()
                    |"ideologies.txt" -> ()
                    |"issues.txt" -> ()
                    |"national_focus.txt" -> ()
                    |"nationalvalue.txt" -> ()
                    |"on_actions.txt" -> ()
                    |"pop_types.txt" -> ()
                    |"production_types.txt" -> 
                        let lexems:Lexer.lexem list= common_file |> Lexer.lexer  in
                        (
                        lexems 
                        |> Lexer.String_lexems 
                        |> output_to_file common_file lex_output;
                        );
                    if ast_output = None &&  ast_errors =None && type_errors = None then ();
                        
                                                                                                
                    let assignments = lexems |> Pre_parser.assignments in
                                                                                                
                    (assignments
                    |> Pre_parser.String_assignment_list  
                    |> output_to_file common_file ast_output;    
                    );
                    |"rebel_types.txt" -> ()
                    |"religion.txt" -> ()
                    |"static_modifiers.txt" -> ()
                    |"technology.txt" -> ()
                    |"traits.txt" -> ()
                    |"triggered_modifiers.txt" -> ()
                    |entry -> Printf.printf "File %s not recognized\n" entry
                    )
                refined_common;
                ()

            |_ -> ();
            ()
        ) refined_categories;

        |_-> ()
    in

    let term = Term.(const parse_env 
        $ (make_arg "mh" "document") 
        $ (make_arg "lo" "document")         
        $ (make_arg "ao" "document")
        $ (make_arg "ae" "document")
        $ (make_arg "te" "document")
        $ (files_arg "c" "document")
        $ (files_arg "e" "document")
        $ (files_arg "cf" "document")
    ) in
    let cmd = Cmd.v info term 
    in
    Cmd.eval cmd |> Printf.printf "%i\n";
    *) 
