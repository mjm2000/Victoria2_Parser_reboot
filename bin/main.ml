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
    let enable_arg title shorter doc  = 
        Arg.(value & flag & info [title;shorter] ~doc)
    in
    Printf.eprintf "Starting Paradox Mod Checker\n";
    let game_value = make_arg "game" "g" "Games Choice: Victoria2, Eu4, Hoi4, imperator, CK3" in
    let mod_home = make_arg "mod-dir" "m" "Mod Home Directory" in
    let game_home = make_arg "game-dir" "h" "Game Home Directory" in
    let mod_file = make_arg "mod-file" "mf" "Mod File to check" in
    let output_file = make_arg "output" "o" "Output File" in
    let lexoutput = make_arg "lex-output" "l" "File to output lexems" in
    let astoutput = make_arg "ast-output" "a" "File to output ast" in
    let print_files = enable_arg "print-files" "f" "Print files to stdout" in
    let save_parser = enable_arg "save-parser" "sp" "Use game saves to check mod files" in
    let mod_parser = enable_arg "mod-mode" "mp" "Use mod files to check game files" in
    let saves = make_arg "saves" "s" "find saves to use" in
    Printf.eprintf "Arguments Parsed\n";
    let game_symbols game  = match game with
    |Some "Victoria2" -> 
            Printf.eprintf "Victoria2 Selected\n";
            Some (Victoria2.victoria2_paths, Victoria2.victoria2_symbol_table)
    |Some "Eu4" ->
            Printf.eprintf "Eu4 Selected\n";
            Some (Eu4.eu4_paths, Eu4.eu4_symbol_table)
    |_ -> None
    in
    let doc = "Paradox Mod Checker" in
    let info = Cmd.info "" ~doc in
(* List files in a directory and filter by glob pattern *)
    Printf.eprintf "Setting up command line\n";  
    let term = Term.(const (fun game mh gh out lexoutput astoutput print_file save_parser mod_parser saves  mod_file-> 
        if mod_parser then (
        match (mh,gh) with
        |(Some mod_home), (Some game_home) -> 
            ( match game_symbols game with
            |(Some (paths, symbol_table)) -> 

                Hashtbl.add symbol_table (Definition "mod_home_def") (Dir mod_home);
                Hashtbl.add symbol_table (Definition "game_home_def") (Dir game_home);
                Printf.printf "mod_file: %s\n" (match mod_file with Some f -> f | None -> "None");
                let rec new_paths_r acc path_lists = 
                    (match path_lists with

                    |(file,def)::rest_of_paths ->
                        let abs_mod_file = Filename.concat mod_home file in
                        if print_file then 
                            Printf.eprintf "Checking File: %s\n" abs_mod_file;

                        let abs_game_file = Filename.concat game_home file in
                    
                        if (is_directory abs_mod_file) then
                            let new_paths = Array.fold_left (fun  rest f ->
                               ((Filename.concat file f),def)::rest 

                            ) [] (Sys.readdir abs_mod_file) in
                            Printf.eprintf "!!!!!Directory: %s\n" abs_mod_file;
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
                                raise (File_not_found  ("corrupted game files" ^abs_game_file) )
                    |[] -> acc
                    )

                 
                in
                let new_paths = new_paths_r [] paths in
            
                let exceptions = type_verify symbol_table new_paths lexoutput astoutput mod_home game_home 
                in
                let output = 
                    match out with
                    |Some file -> open_out file 
                    |None -> stdout
                in
                Printf.eprintf "Checking Files\n"; 
                (List.iter (fun (file,x) -> 
                let expr_string = x|>  Output.exceptions_string in
                if print_file then 
                        Printf.fprintf output "%s:\n%s\n" file expr_string 
                else 
                    (if x <> [] then
                        Printf.fprintf output "%s\n" expr_string
                    )
                )
                (exceptions))
            |None -> Printf.printf "Game not recognized\n"
        )
        |((Some mod_home), None) -> 
                Printf.printf "No Game Provided for mod:%s\n" mod_home;
        |(None, Some game_home) -> Printf.printf "No Mod Home Provided for game:%s\n" game_home

        |(None,None) -> Printf.printf "No Game\n")
        else if save_parser then (
            Printf.eprintf "Save Parser Mode\n";
            match (mh,gh,saves) with
            |(Some mod_home), (Some game_home), (Some saves) ->
                    Printf.printf "Mod Home: %s Game Home: %s Saves: %s\n" mod_home game_home saves;
            |_ -> Printf.printf "No Mod Home, Game Home or Saves Provided\n"

        )
        )  $ game_value $ mod_home $ game_home $ output_file $ lexoutput $ astoutput $ print_files $ save_parser $ mod_parser $ saves$ mod_file)
        
    in
    let cmd = Cmd.v info term 
    in
    Cmd.eval cmd |> Printf.printf "%i\n";
    

       
