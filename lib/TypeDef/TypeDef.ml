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
    |KeywordLiteral of string 
    |TYPE_SYMBOL of lexem_type
    |DefinedTypeLeft of string
    |CatalogLeft of string * lh_symbol_type
    |Definition of string
(*add label*)
and symbol_amount =
    |Required of int
    |Optional 

and rh_symbol_type = 
    |DefinedTypeRight of string
    |Catalog_Right of string * rh_symbol_type
    |PARAM_LIST of (lh_symbol_type,rh_symbol_type) Hashtbl.t
    |PARAM_VALUE of lexem_type 
    |LINK
    |Inherit of (lh_symbol_type * rh_symbol_type) list * string list
    |Literal of string
    |NUMBER
    |WholeNumber
    |Integer
    |PositiveDecimal
    |NegativeDecimal
    |Decimal
    |PROVINCE_ID
    |PARAM_OPTION of rh_symbol_type list
    |VALUE_LIST of rh_symbol_type 
    |CHOICE_VALUE of string list 
    |APPEND_SYMBOLS of (lh_symbol_type * rh_symbol_type) list * rh_symbol_type 
    |SubTable of string
   



and exception_value = expected_value * exception_type * (int * int)
