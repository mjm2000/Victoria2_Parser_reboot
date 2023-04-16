open Unix 

(*open Base*)
#use './types.ml'

    

let read_file_str f : char list= 
    let ic = open_in f in 
    let rec read_all file out = 
    try 
        read_all  file  (input_char ic::out)
    with eof ->  out 
    in 
    read_all ic []

let ls_to_str ls = 
    let buf = Buffer.create (List.length ls)in
    List.iter (Buffer.add_char buf) ls; 
    Buffer.contents buf

let str_to_ls str = 
     String.fold_left (fun ls chr -> (chr::ls) ) [] str 
    

let lexer file = 
    let rec lex_r (chrs:char list) (buf:char list) out : lexems list = 
        (match (chrs,buf) with
        |'='::rs,_  -> lex_r rs [] (EQ::out)  
        |' '::rs,[]|'\t'::rs,[] -> lex_r rs [] out
        |' '::rs,buf|'\t'::rs,buf-> 
            let nlex= match (ls_to_str buf) with 
            |a 
            |a -> Keyword (a) 
            
            in
            lex_r rs [] (nlex:: out)
        |'\n'::rs,[] -> lex_r rs [] out
        |'\n'::rs,buf -> lex_r rs [] (Keyword(ls_to_str buf) :: out)
        |'\"'::rs,'\"'::_ ->  lex_r rs [] (String(ls_to_str ('\"'::buf )) :: out)
        |chr::rs,'\"'::rob -> lex_r rs ('\"'::chr::rob) out 
        |'{'::rs, buf-> lex_r rs [] (LB::out)
        |'}'::rs, buf-> lex_r rs [] (RB::out)
        |a::rs, buf->  lex_r rs (a::buf) (out)
        |[],_-> out )
    in
    lex_r (read_file_str file) [] [] 

let lexem_to_str lexem = (match lexem with 
    |Keyword (x)-> Printf.sprintf "Keyword:%s"x  
    |EQ -> "=" 
    | LB->  "{"
    | RB ->  "}"
    | String (str) -> Printf.sprintf "String:%s"str
)
let print_lexem lexem = Printf.printf "%s\n" (lexem_to_str lexem)

let print_lexems lexems = List.iter print_lexem lexems;;

lexer "/home/jimmy/projects/Victoria2_Parser_reboot/src/HFM/HFM/events/ACW.txt" |> print_lexems
