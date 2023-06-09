open Types

(*open Base*)

    

let read_file_str f : char list= 
    let ic = open_in f in 
    let rec read_all file out = 
    try 

        read_all  file  (input_char ic::out)
    with  _ ->   out 
    in 
    read_all ic [] 
let ls_to_str ls = 
    let buf = Buffer.create (List.length ls)in
    List.iter (Buffer.add_char buf) ls; 
    Buffer.contents buf

let str_to_ls str = 
     String.fold_left (fun ls chr -> (chr::ls) ) [] str 
    

let match_reg reg str= Str.string_match (Str.regexp reg)  str 0 
let chars_to_lexem cl= match (ls_to_str cl) with 
          |iv when match_reg "^[0-9]+$" iv -> INT
          |fv when match_reg "^[0-9]+\\.[0-9]+$" fv -> FLOAT 
          |"NOT"|"AND"|"OR" -> CONDITION 
          |tag when match_reg  "[A-Z][A-Z][A-Z]" tag-> TAG 
          |"yes"|"no" -> BOOL 
          |"FROM"|"THIS" -> SCOPE 
          |_-> KEYWORD

let lexer file = 
    let rec lex_r (chrs:char list) (buf:char list) out (x,y) = 
        let str = ls_to_str buf in
        match (chrs,buf) with
        |'\"'::rs,'\"'::_ ->  
                let str = ls_to_str ('\"'::buf) in
                lex_r rs [] ((STRING,str,(x,y)) :: out) (x+1,y)
        |chr::rs,'\"'::rob -> lex_r rs ('\"'::chr::rob) out (x+1,y)
        |'='::rs,_  -> lex_r rs [] ((EQ,str,(x,y)) ::out)  (x+1,y)
        |'{'::rs, _-> lex_r rs [] ((LB,str,(x,y))::out) (x+1,y)
        |'}'::rs, _-> lex_r rs [] ((RB,str,(x,y))::out)        (x+1,y)
        |'\n'::rs,[] -> lex_r rs [] out(1,y+1)
        |'\n'::rs,buf -> lex_r rs [] (((chars_to_lexem buf),str,(x,y)) :: out) (1,y+1)
        |' '::rs,[]|'\t'::rs,[] -> lex_r rs [] out (x+1,y)
        |' '::rs,buf|'\t'::rs,buf-> 
            lex_r rs [] (((chars_to_lexem buf),str,(x,y)) :: out) (x+1,y)

        |chr::rs,buf ->  lex_r rs (chr::buf)  out (x+1,y)
        |[],[]->out 
        |[],buf-> ((chars_to_lexem buf),str,(x,y)) :: out
        in
        lex_r (read_file_str file)  [] [] (1,1)

let lexem_to_str lexem =match lexem with 
| FLOAT ->    "Float" 
| INT  ->     "Int"
| KEYWORD ->  "Keyword"
| TAG  ->     "Tag" 
| STRING  ->  "String"
| LB  ->      "Left Brace"
| RB  ->      "Right Brace"
| EQ ->       "Equal"
| SCOPE ->     "Scope"
| CONDITION ->"Conditional"
| BOOL ->     "Bool"      

let print_lexem lexem = Printf.printf "%s:" (lexem_to_str lexem)
let print_lexems lexems = List.iter (fun (lex,str,(x,y))->print_lexem lex; Printf.printf "%s (%i,%i)\n" str x y;) lexems;;
