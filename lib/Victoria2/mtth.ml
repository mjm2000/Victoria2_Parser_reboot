open SymbolTable
open TypeDef
let country_mtth = symbol_table_init [
    (KeywordLiteral("years"),PARAM_VALUE(INT));
    (KeywordLiteral("year"),PARAM_VALUE(INT));
    (KeywordLiteral("months"),PARAM_VALUE(INT));
    (KeywordLiteral("days"),PARAM_VALUE(INT));
    (KeywordLiteral("modifier"),APPEND_SYMBOLS([
       (KeywordLiteral("factor"),PARAM_OPTION[
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]);
    ],DefinedTypeRight "country_conditions_def"));
]
let province_mtth = symbol_table_init [
    (KeywordLiteral("years"),PARAM_VALUE(INT));
    (KeywordLiteral("year"),PARAM_VALUE(INT));
    (KeywordLiteral("months"),PARAM_VALUE(INT));
    (KeywordLiteral("days"),PARAM_VALUE(INT));
    (KeywordLiteral("modifier"),APPEND_SYMBOLS([
        (KeywordLiteral("factor"),PARAM_OPTION[
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]);
    ],DefinedTypeRight "province_conditions_def"));
]
