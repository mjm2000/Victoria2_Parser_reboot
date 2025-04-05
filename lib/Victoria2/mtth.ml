open SymbolTable
open TypeDef
let country_mtth = symbol_table_init [
    (KEYWORD_SYMBOL("years"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("year"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("months"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("days"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("modifier"),APPEND_SYMBOLS([
       (KEYWORD_SYMBOL("factor"),PARAM_OPTION[
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]);
    ],DefinedTypeRight "country_conditions_def"));
]
let province_mtth = symbol_table_init [
    (KEYWORD_SYMBOL("years"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("year"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("months"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("days"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("modifier"),APPEND_SYMBOLS([
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION[
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]);
    ],DefinedTypeRight "province_conditions_def"));
]
