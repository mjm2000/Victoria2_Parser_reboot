open Symbol_table
open Type_def

let bookmarks = symbol_table_init [
    (KEYWORD_SYMBOL, "bookmark",symbol_table_init [
        (KEYWORD_SYMBOL, "name", PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL, "desc", PARAM_VALUE(STRING)); 
        (KEYWORD_SYMBOL, "date", PARAM_VALUE(KEYWORD_SYMBOL));
        (KEYWORD_SYMBOL, "cameraX", PARAM_VALUE(INT));
        (KEYWORD_SYMBOL, "cameraY", PARAM_VALUE(INT));
    ]);


]
