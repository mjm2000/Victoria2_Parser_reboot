open Stdlib
open Stdio
(*open Base*)

type lexems = 
    Keyword of string | EQ | LPARAN | RPARAN
let lexer file = 
    let fs =  In_channel.read_all file in 
    let rec lex_r pos buf out = 
        let npos = pos + 1 in
        match ((String.get fs pos),buf) with
        |'=',_  -> lex_r npos [] (EQ::out)  
        |' ',[] -> lex_r npos [] out
        |' ',buf -> lex_r npos [] (Keyword(Base.of_char_list buf):: out)
        |'\n',[] -> lex_r npos [] out
        |'\n',buf -> lex_r npos [] (Keyword(Base.of_char_list buf) :: out)
        |'\"','\"'::_ ->  lex_r npos [] (Keyword(Base.of_char_list buf) :: out)
        |chr,'\"'::rob -> lex_r npos ('\"'::chr::rob) out 
        |'{', buf-> lex_r npos [] (LPARAN::out)
        |'}', buf-> lex_r npos [] (RPARAN::out)
        |a , buf->  lex_r npos (a::buf) (out)

    
    in
    lex_r  0 [] []
