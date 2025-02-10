open Symbol_table
open Type_def
let country_mtth = symbol_table_init [
    (KEYWORD_SYMBOL("year"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("months"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("days"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("modifier"),APPEND_SYMBOLS([
        (KEYWORD_SYMBOL("factor"),PARAM_VALUE(FLOAT));
    ],COUNTRY_CONDITIONS));
]
let province_mtth = symbol_table_init [
    (KEYWORD_SYMBOL("year"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("months"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("days"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("modifier"),APPEND_SYMBOLS([
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION[
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]);
    ],PROVINCE_CONDITIONS));
]
