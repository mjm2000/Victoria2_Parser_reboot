type value_type =
|Keyword 
|Bool  
|PositiveInt
|NegativeInt
|NegativeFloat 
|PositiveFloat
|String
|Tag  
|Scope
|Condition
|Date
|InvalidString



type lexem_type =
|EQ
|LB
|RB
|LexemValue of value_type







type lexem = lexem_type * string * (int * int)

type expr = 
    |LEXEM of lexem
    |LEXEM_LIST of expr list
    |ASSIGNMENT_LIST of assignment list
    |EXPR_EXCEPTION of exception_value  
and assignment = 
    |ASSIGNMENT of lexem * expr 
    |EXPR of expr
    |ASSIGN_EXCEPTION of exception_value

and expected_value =
    |ExpEqual 
    |ExpLeftBracket
    |ExpRightBracket
    |ExpAssignment
    |ExpValue
    |ExpExprList
    |RHS of symbol_type list 
    |LHS of symbol_type list 
    |NONE 

and exception_type = 
    |TYPE_MISHMASH of string  * value_type * value_type 
    |END_OF_FILE 
    |UNKNOWN_IDENTIFIER of string 
    |UNEXPECTED_LEXEM of string * lexem_type    
    |UNEXPECTED_ASSIGNMENT of assignment 
    |UNEXPECTED_ASSIGN_LIST of assignment list 
    |UNEXPECTED_EXPR_LIST of expr list
    |UNEXPECTED_EXPR of expr
    |UNEXPECTED_EQUAL
    |UNEXPECTED_RIGHT_BRACKET
    |UNEXPECTED_LEFT_BRACKET
    |MULTIPLE_CHOICE of exception_value list list



(*add label*)
and symbol_amount =
    |Required of int
    |Optional 

and symbol_type = 
    (*found from type*)
    |Definition of string
    (*catalog_location, regex, actual body*)
    |CatalogFile of string  * string * symbol_type 
    |Type of string
    |Catalog of string * symbol_type
    |CatalogLeft of string * symbol_type
    |SubTable of  (symbol_type,symbol_type)  Hashtbl.t
    |Link
    |Inherit of (symbol_type * symbol_type) list * string list
    |Literal of string
    |Value of value_type 
    |Identifier
    |Decimal
    |Integer
    |PositiveNumber
    |NegativeNumber
    |Number
    |Target
    |WholeNumber
    |Year
    |TypeOption of symbol_type list
    |ValueList of symbol_type 
    |Switch of (string * symbol_type) list 
    |SubCatalog of string * string * symbol_type
    |SupCatalog of string * symbol_type
    |InnerCatalog of string * symbol_type
    |SubDefinition of string * string 
    |SubType of string * string 
    |SupType of string 
    |Dir of string
    |Nothing
    |IsoSubType of string * string
    |SameSubType of string * string
    |Single of symbol_type
    |Multiple of symbol_type
    |RhLookup of symbol_type


   



and exception_value = expected_value * exception_type * (int * int) * string
