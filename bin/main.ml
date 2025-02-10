open Vic2parser_reboot
let () = 
   let oe = open_out "output_errors.txt" in 
   let ol = open_out "output_lexems.txt" in
   let oa = open_out "output_assignments.txt" in
   let os = open_out "output_syntax.txt" in

   (List.tl (Array.to_list Sys.argv)) |>
    List.iter ( fun file -> 
        let lexems = file |> Lexer.lexer  in
        lexems |>
        Lexer.string_lexems |>
        Printf.fprintf ol "%s:%s\n" file;

        let assignments = lexems |> Pre_parser.assignments  in
        assignments 
        |> Pre_parser.string_assignment_list  
        |> Printf.fprintf oa "%s:%s\n"  file;
       
        assignments 
        |> Pre_parser.get_errors  
        |>  (fun x -> if x <> [] then 
            x 
            |> Pre_parser.exceptions_string 
            |> Printf.fprintf oe "%s:%s\n" file
        );
        assignments |>
        Type_verify.type_verify Structures.structures  
        |> (fun x -> if x <> [] then 
            x 
            |> Pre_parser.exceptions_string 
            |> Printf.fprintf os "%s:%s\n" file
        );



    )


