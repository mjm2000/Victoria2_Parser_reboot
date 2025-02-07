open Symbol_table
open Type_def


let stuctures = symbol_table_init [
    (KEYWORD_SYMBOL("country_event"), PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("id"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("title"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("desc"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("picture"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("major"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("mean_time_to_happen"), COUNTRY_MTTH);
        (KEYWORD_SYMBOL("is_triggered_only"), PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("option"), COUNTRY_EFFECTS);
        (KEYWORD_SYMBOL("immediate"), COUNTRY_EFFECTS);
    ]));
    (KEYWORD_SYMBOL("province_event"), PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("id"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("title"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("desc"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("picture"), PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("major"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("mean_time_to_happen"), PROVINCE_MTTH);
        (KEYWORD_SYMBOL("is_triggered_only"), PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("option"), APPEND_SYMBOLS(
            [(KEYWORD_SYMBOL("name"),PARAM_VALUE(STRING))],
            PROVINCE_EFFECTS)
        );
        (KEYWORD_SYMBOL("immediate"), PROVINCE_EFFECTS);
    ]));

]
    
