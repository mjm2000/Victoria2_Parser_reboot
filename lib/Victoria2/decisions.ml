open TypeDef
open SymbolTable
let decisions = symbol_table_init [
    (KEYWORD_SYMBOL("political_decisions"),PARAM_LIST(symbol_table_init [
        (TYPE_SYMBOL(KEYWORD),PARAM_LIST(symbol_table_init[
            (KEYWORD_SYMBOL "potential",DefinedTypeRight "country_conditions_def");
            (KEYWORD_SYMBOL "allow",DefinedTypeRight "country_conditions_def");
            (KEYWORD_SYMBOL "effect",DefinedTypeRight "country_effects_def");
            (KEYWORD_SYMBOL "news",PARAM_VALUE(BOOL));
            (KEYWORD_SYMBOL "news_title",PARAM_VALUE(STRING));
            (KEYWORD_SYMBOL "news_desc_short",PARAM_VALUE(STRING));
            (KEYWORD_SYMBOL "news_desc_medium",PARAM_VALUE(STRING));
            (KEYWORD_SYMBOL "news_desc_long",PARAM_VALUE(STRING));
            (KEYWORD_SYMBOL "news_desc_image",PARAM_VALUE(STRING));
            (KEYWORD_SYMBOL("picture"), PARAM_OPTION([
                PARAM_VALUE(STRING);
                PARAM_VALUE(KEYWORD)
            ]));
            (KEYWORD_SYMBOL "alert",PARAM_VALUE(BOOL));
            (KEYWORD_SYMBOL("ai_will_do"),PARAM_LIST(symbol_table_init [
                (KEYWORD_SYMBOL("factor"),
                    PARAM_OPTION([
                        PARAM_VALUE(INT);
                        PARAM_VALUE(FLOAT);
                ]));
                (KEYWORD_SYMBOL("modifier"),Inherit([
                    (KEYWORD_SYMBOL("factor"),
                        PARAM_OPTION([
                            PARAM_VALUE(INT);
                            PARAM_VALUE(FLOAT);
                    ]));
            ],["country_conditions_def"]));
            ]));
            
        ]));

    ])
    );
] 
