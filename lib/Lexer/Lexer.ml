open TypeDef




let ls_to_str ls = 
    let buf = Buffer.create (List.length ls)in
    List.iter (Buffer.add_char buf) ls; 
    Buffer.contents buf

let match_reg reg str = 
    let regex = Re2.create_exn reg in
    Re2.matches regex str

let chars_to_lexem cl= match (cl) with 
          |iv when match_reg "^-[0-9]+$" iv -> NegativeInt
          |iv when match_reg "^[0-9]+$" iv -> PositiveInt
          |fv when match_reg "^-(0|[1-9][0-9]*)?\\.[0-9]+$" fv -> NegativeFloat 
          |fv when match_reg "^(0|[1-9][0-9]*)?\\.[0-9]+$" fv -> PositiveFloat 
            
          |"TAG"-> Keyword
          |"NOT"|"AND"|"OR"|"not"|"and"|"or" -> Condition 
          |tag when match_reg  "^[0-9]{4}\\.[0-9][0-9]?\\.[0-9][0-9]?$" tag->
              Date 
          |tag when match_reg  "^[A-Z][A-Z][A-Z]$" tag->
              Tag 
          | tag when match_reg "^[A-Z][0-9][0-9]$" tag->
              Tag
          |"yes"|"no" -> Bool 
          |"FROM"|"THIS"|"this"|"from" -> Scope 
          |_-> Keyword

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
                read_all [] ((LexemValue String,str,(y,x)) :: out)  (y,x+1)
            |chr-> 
                read_all ('\"'::chr::rs) out  (y,x+1)
            |exception End_of_file -> close_in ic; out;
            )
        |buf_chrs ->
            let updated_out = (match buf_chrs with
            |[]->out
            |buf_chrs -> 
                let string = ls_to_str (List.rev buf_chrs) in
                let buffer_token = (LexemValue (chars_to_lexem string), string,(y,x-1)) in
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


































