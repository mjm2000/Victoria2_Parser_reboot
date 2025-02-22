open Type_def
open Symbol_table
let decisions = symbol_table_init [
    (KEYWORD_SYMBOL("political_decisions"),PARAM_LIST(symbol_table_init [
        (TYPE_SYMBOL(KEYWORD),PARAM_LIST(symbol_table_init[
            (KEYWORD_SYMBOL "potential",COUNTRY_CONDITIONS);
            (KEYWORD_SYMBOL "allow",COUNTRY_CONDITIONS);
            (KEYWORD_SYMBOL "effect",COUNTRY_EFFECTS);
            (KEYWORD_SYMBOL("ai_chance"),PARAM_LIST(symbol_table_init [
                (KEYWORD_SYMBOL("factor"),
                    PARAM_OPTION([
                        PARAM_VALUE(INT);
                        PARAM_VALUE(FLOAT);
                ]));
                (KEYWORD_SYMBOL("modifier"),APPEND_SYMBOLS([
                    (KEYWORD_SYMBOL("factor"),
                        PARAM_OPTION([
                            PARAM_VALUE(INT);
                            PARAM_VALUE(FLOAT);
                    ]));
                ],COUNTRY_CONDITIONS));
            ]));
            
        ]));

    ])
    );
] 
