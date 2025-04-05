open TypeDef
open SymbolTable
let decisions = symbol_table_init [
    (KeywordLiteral("political_decisions"),PARAM_LIST(symbol_table_init [
        (TYPE_SYMBOL(KEYWORD),PARAM_LIST(symbol_table_init[
            (KeywordLiteral "potential",DefinedTypeRight "country_conditions_def");
            (KeywordLiteral "allow",DefinedTypeRight "country_conditions_def");
            (KeywordLiteral "effect",DefinedTypeRight "country_effects_def");
            (KeywordLiteral "news",PARAM_VALUE(BOOL));
            (KeywordLiteral "news_title",PARAM_VALUE(STRING));
            (KeywordLiteral "news_desc_short",PARAM_VALUE(STRING));
            (KeywordLiteral "news_desc_medium",PARAM_VALUE(STRING));
            (KeywordLiteral "news_desc_long",PARAM_VALUE(STRING));
            (KeywordLiteral "news_desc_image",PARAM_VALUE(STRING));
            (KeywordLiteral("picture"), PARAM_OPTION([
                PARAM_VALUE(STRING);
            ]));
            (KeywordLiteral "alert",PARAM_VALUE(BOOL));
            (KeywordLiteral("ai_will_do"),PARAM_LIST(symbol_table_init [
                (KeywordLiteral("factor"),
                    PARAM_OPTION([
                        PARAM_VALUE(INT);
                        PARAM_VALUE(FLOAT);
                ]));
            ]));
            (KeywordLiteral("modifier"),Inherit([
                (KeywordLiteral("factor"),
                    PARAM_OPTION([
                        PARAM_VALUE(INT);
                        PARAM_VALUE(FLOAT);
                    ]));
            ],["country_conditions_def"]));
        ]));
    ])
    );
] 
