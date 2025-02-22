open Vic2parser_reboot
open Cmdliner
let () = 
   let folder_exists folder_path = Sys.file_exists folder_path && Sys.is_directory folder_path
   in
    (*
   let mod_home =  
        let doc = "Path to Victoria2 Mod Files" in
        Arg.(value & opt (some string) None & info ["md"; "mod-dir"] ~doc) 
    in
    *)
    let make_arg title doc  = 
        Arg.(value & opt (some string) None & info [title] ~doc) 
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
        |Some "stdout" -> Printf.printf "%s:%s" file_name str
        |Some ol -> Printf.fprintf (open_out ol) "%s:%s" file_name str 
        |None -> () 
    in
    let doc = "My_program does nothing but output 'cow'." in
    let info = Cmd.info "my_program" ~doc in
    let files_arg title doc =
        Arg.(value & opt (some (list string) ) None & info [title] ~doc)
    in
let filter_files raw_files selected_files = match selected_files with
    |Some files -> List.filter (fun x -> List.mem x files) raw_files
    |None -> raw_files
    in
    
let parse_env mod_home lex_output ast_output ast_errors type_errors selected_categories event_files= 
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
                    |> Lexer.string_lexems 
                    |> output_to_file event_file lex_output;
                    );
                    if ast_output = None &&  ast_errors =None && type_errors = None then ();
                        
                    let assignments = lexems |> Pre_parser.assignments in

                    (assignments
                    |> Pre_parser.string_assignment_list  
                    |> output_to_file event_file ast_output;    
                    );

                    (assignments
                    |> Pre_parser.get_errors  
                    |>  (fun x -> if x <> [] then 
                        x 
                        |> Pre_parser.exceptions_string 
                        |> output_to_file event_file  ast_errors 
                    )
                    );
                    (assignments
                    |> Type_verify.type_verify Events.events  
                    |> (fun x -> if x <> [] then 
                        x 
                        |> Pre_parser.exceptions_string 
                        |> output_to_file event_file type_errors;
                        ); 
                    );
                ) refined_events; 
                

        |_-> ()
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
    ) in
    let cmd = Cmd.v info term 
    in
    Cmd.eval cmd |> Printf.printf "%i\n";
     
     
