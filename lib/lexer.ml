
type lexem_type =
|KEYWORD 
|BOOL  
|INT 
|FLOAT 
|STRING
|EQ
|LB
|RB
|TAG  
|SCOPE
|CONDITION
|LEX_ERROR

type lexem = lexem_type * string * (int * int)





let ls_to_str ls = 
    let buf = Buffer.create (List.length ls)in
    List.iter (Buffer.add_char buf) ls; 
    Buffer.contents buf

let match_reg reg str = 
  match Re2.create reg with
  | Ok pattern -> Re2.matches pattern str 
  | Error _ -> false  (* Handle invalid regex gracefully *)


let chars_to_lexem cl= match (cl) with 
          |iv when match_reg "-?[0-9]+$" iv -> INT
          |fv when match_reg "-?[0-9]*\\.[0-9]+$" fv -> FLOAT 
          |"NOT"|"AND"|"OR"|"not"|"and"|"or" -> CONDITION 
          |tag when match_reg  "^[A-Z][A-Z][A-Z]$" tag->
                  TAG 
          |"yes"|"no" -> BOOL 
          |"FROM"|"THIS"|"this"|"from" -> SCOPE 
          |_-> KEYWORD

let lexer f = 
    let ic = open_in f in 
    let rec read_all buf out (y,x) = 
    match (buf) with
        |'#'::rs ->
            (match input_char ic with
            |'\n' -> read_all [] out (y+1,1)
            |_ -> read_all ('#'::rs) out (y,x+1)
            |exception End_of_file -> close_in ic; out;
            )
        |'\"'::rs ->  
            (match input_char ic with
            |'\"' -> let str = ls_to_str ('\"'::List.rev buf) in                  
                read_all [] ((STRING,str,(y,x)) :: out)  (y,x+1)
            |chr-> 
                read_all ('\"'::chr::rs) out  (y,x+1)
            |exception End_of_file -> close_in ic; out;
            )
        |buf_chrs ->
            let updated_out = (match buf_chrs with
            |[]->out
            |buf_chrs -> 
                let string = ls_to_str (List.rev buf_chrs) in
                let buffer_token = (chars_to_lexem string, string,(y,x-1)) in
                buffer_token::out
            )
            in
            (match input_char ic with
            |'{' -> read_all [] ((LB,"{",(y,x))::updated_out) (y,x+1)
            |'}' -> read_all [] ((RB,"}",(y,x))::updated_out) (y,x+1)
            |'=' -> read_all [] ((EQ,"=",(y,x))::updated_out) (y,x+1)
            |'#' -> read_all ['#'] (updated_out) (y,x+1)
            |'\n' -> read_all [] (updated_out) (y+1,1)
            |'\r' -> read_all [] (updated_out) (y,x)
            |' '|'\t' -> read_all [] updated_out (y,x+1)
            |chr -> read_all (chr::buf_chrs) out (y,x+1)
            |exception End_of_file -> close_in ic; out;
            )
        |exception End_of_file -> close_in ic; out;
    in 
    List.rev (read_all [] [] (1,1)) 

let lexem_to_str lexem =match lexem with 
| FLOAT ->    "Float" 
| INT  ->     "Int"
| KEYWORD ->  "Keyword"
| TAG  ->     "Tag" 
| STRING  ->  "String"
| LB  ->      "Left Brace"
| RB  ->      "Right Brace"
| EQ ->       "Equal"
| SCOPE ->    "Scope"
| CONDITION ->"Conditional"
| BOOL ->     "Bool"      
| LEX_ERROR ->"Lex Error"


let string_lexem lex_value = 
  let (lex, str, (x, y)) = lex_value in
  Printf.sprintf "%s %s (%i,%i)" (lexem_to_str lex) str x y


let string_lexems lexems = 
  List.fold_left (fun buffer lexem -> Printf.sprintf "%s\n%s" buffer (string_lexem lexem)) "" lexems

let print_lexem lexem = 
  Printf.printf "%s: " (lexem_to_str lexem)

let print_lexems lexems = 
  List.iter (fun (lex, str, (x, y)) ->
    print_lexem lex;
    Printf.printf "%s (%i,%i)\n" str x y
  ) lexems
