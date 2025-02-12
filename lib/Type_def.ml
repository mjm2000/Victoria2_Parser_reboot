open Lexer
type expr = 
    |LEXEM of lexem
    |ASSIGNMENT_LIST of assignment list
    |EXPR_EXCEPTION of exception_value  
and assignment = 
    |ASSIGNMENT of lexem * expr 
    |ASSIGN_EXCEPTION of exception_value

and expected_value =
    |RHS of rh_symbol_type list 
    |LHS of lh_symbol_type list 
    | NONE 

and exception_type = 
    |TYPE_MISHMASH of string  * lexem_type * lexem_type 
    |END_OF_FILE 
    |UNKNOWN_IDENTIFIER of string 
    |UNEXPECTED_LEXEM of string * lexem_type 
    |UNEXPECTED_ASSIGNMENT of assignment 
    |UNEXPECTED_ASSIGN_LIST of assignment list 
    |UNEXPECTED_RIGHT_BRACKET
    |UNEXPECTED_LEFT_BRACKET
    |MULTIPLE_CHOICE of exception_value list list
and lh_symbol_type = 
    |KEYWORD_SYMBOL of string 
    |TYPE_SYMBOL of lexem_type
(*add label*)
and rh_symbol_type = 
    |PARAM_LIST of (lh_symbol_type,rh_symbol_type) Hashtbl.t
    |PARAM_VALUE of lexem_type 
    |PARAM_OPTION of rh_symbol_type list
    |CHOICE_VALUE of string list 
    |APPEND_SYMBOLS of (lh_symbol_type * rh_symbol_type) list * rh_symbol_type 
    |PROVINCE_MTTH
    |COUNTRY_MTTH
    |PROVINCE_EFFECTS
    |PROVINCE_CONDITIONS
    |COUNTRY_EFFECTS
    |COUNTRY_CONDITIONS
    |POP_EFFECTS
    |POP_CONDITIONS
    |STATE_EFFECTS
    |STATE_CONDITIONS



and exception_value = expected_value * exception_type * (int * int)


            
    

let rec string_of_symbol_table symbol_table =
    let rec string_rh (v: rh_symbol_type ): string= match v with 
        |PARAM_VALUE(lex) -> Printf.sprintf "PARAM_VALUE(Type:%s)" (lexem_to_str lex)
        |PARAM_OPTION(rh_options) -> 
                let options = List.fold_left (fun buffer rh ->
                    let rh_string = string_rh rh in
                    Printf.sprintf "%s%s\n" buffer rh_string
                ) "" rh_options  in
                Printf.sprintf "PARAM_OPTION(%s)" options 
        |CHOICE_VALUE(choices) -> 
                let string_choices = List.fold_left (fun buffer choice ->
                Printf.sprintf "%s%s\n" buffer choice
                ) "" choices in
                Printf.sprintf "CHOICE_VALUE(%s)" string_choices
        |APPEND_SYMBOLS(append_list,rh_value) ->
                let append_list = List.fold_left (fun buffer (lh,rh) ->
                let lh_string = string_lh lh in
                let rh_string = string_rh rh in
                Printf.sprintf "%s%s:%s\n" buffer lh_string rh_string
                ) "" append_list in
                let rh_string = string_rh rh_value in
                Printf.sprintf "APPEND_SYMBOLS(%s:%s)" append_list rh_string
        |PROVINCE_MTTH -> "PROVINCE_MTTH"
        |COUNTRY_MTTH -> "COUNTRY_MTTH"
        |PROVINCE_EFFECTS -> "PROVINCE_EFFECTS"
        |PROVINCE_CONDITIONS -> "PROVINCE_CONDITIONS"
        |STATE_CONDITIONS -> "STATE_CONDITIONS"
        |COUNTRY_EFFECTS -> "COUNTRY_EFFECTS"
        |COUNTRY_CONDITIONS -> "COUNTRY_CONDITIONS"
        |POP_EFFECTS -> "POP_EFFECTS"
        |POP_CONDITIONS -> "POP_CONDITIONS"
        |STATE_EFFECTS -> "STATE_EFFECTS"
        |PARAM_LIST(list) -> string_of_symbol_table list
    and string_lh v = match v with 
        |KEYWORD_SYMBOL(str) -> Printf.sprintf "KEYWORD_SYMBOL(%s)" str
        |TYPE_SYMBOL(type_lex) -> 
                Printf.sprintf "TYPE_SYMBOL(%s)" (lexem_to_str type_lex)
    
    in
    let v:string = Hashtbl.fold (fun  key value buffer ->
        let key_string:string = string_lh key in
        let value_string:string = string_rh value in
        Printf.sprintf "%s%s:%s\n" buffer key_string value_string 
    ) symbol_table "" in
    v

