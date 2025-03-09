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
and symbol_amount =
    |Required of int
    |Optional 

and rh_symbol_type = 
    |PARAM_LIST of (lh_symbol_type,rh_symbol_type) Hashtbl.t
    |PARAM_VALUE of lexem_type 
    |NUMBER
    |PROVINCE_ID
    |PARAM_OPTION of rh_symbol_type list
    |VALUE_LIST of rh_symbol_type 
    |CHOICE_VALUE of string list 
    |APPEND_SYMBOLS of (lh_symbol_type * rh_symbol_type) list * rh_symbol_type 
    |SubTable of string
   



and exception_value = expected_value * exception_type * (int * int)


module FunctionalHashTable : sig
  type ('k, 'v) t
  val empty : ('k, 'v) t
  val add : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
  val find : ('k, 'v) t -> 'k -> 'v option
  val remove : ('k, 'v) t -> 'k -> ('k, 'v) t
  end = struct
  module M = Map.Make(String) (* Change String to any comparable type *)

  type ('k, 'v) t = 'v M.t

  let empty = M.empty

  let add table key value = M.add key value table

  let find table key = 
    try Some (M.find key table) 
    with Not_found -> None

  let remove table key = M.remove key table
end
