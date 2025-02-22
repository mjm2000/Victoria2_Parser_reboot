open Lexer
open Type_def
let decisions = symbol_table_init [
    (KEYWORD_SYMBOL,"political_decisions",PARAM_LIST());
] 
