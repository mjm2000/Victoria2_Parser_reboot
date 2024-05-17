open Lexer

type exception_type = 
    |TYPE_MISHMASH of string * lexem_type * lexem_type 
    |END_OF_FILE 
    |UNKNOWN_IDENTIFIER of string 
    |UNEXPECTED_LEXEM of string * lexem_type 
    |UNEXPECTED_ASSIGNMENT of string * lexem_type *  string * lexem_type 
    |UNEXPECTED_ASSIGN_LIST of string * lexem_type  
    |UNEXPECTED_RIGHT_BRACKET
    |UNEXPECTED_LEFT_BRACKET


    

type exception_value = exception_type *  (int * int)


let exception_iden_string exp =  match exp with
    |TYPE_MISHMASH(iden,expected,received)->
    Printf.sprintf "Incorrect Type: Text=%s Expected Type=%s Received Type=%s" iden (lexem_to_str expected) (lexem_to_str received)  

    |END_OF_FILE -> 
            Printf.sprintf "Unexpected EOF"  
    |UNKNOWN_IDENTIFIER(string)->
        Printf.sprintf "Unknown Identifier:%s" string  
    |UNEXPECTED_LEXEM(iden,lexem)->
        Printf.sprintf "Unexpected Token: Text=%s Type=%s" iden (lexem_to_str lexem) 
    |UNEXPECTED_ASSIGNMENT(iden1,lex1,iden2,lex2) ->
        
        Printf.sprintf "Unexpected ASSIGNMENT: %s:%s=%s:%s " iden1 (lexem_to_str lex1) iden2 (lexem_to_str lex2)
    |UNEXPECTED_RIGHT_BRACKET -> "Unexpected Right Bracket"
    |UNEXPECTED_LEFT_BRACKET -> "Unexpected Left Bracket"

let exception_string (exp:exception_value) : string= 
    let e,(x,y)= exp in 
    Printf.sprintf "At (%i,%i):%s" x y (exception_iden_string e) 
            
    
