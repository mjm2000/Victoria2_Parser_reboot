open Lexer
type expr = 
    |LEXEM of lexem
    |LEXEM_LIST of expr list
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
    |UNEXPECTED_EXPR_LIST of expr list
    |UNEXPECTED_EXPR of expr
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
    |VALUE_LIST of rh_symbol_type 
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
    |PROVINCE_MODIFIERS
    |COUNTRY_MODIFIERS



and exception_value = expected_value * exception_type * (int * int)


