open SymbolTable
open TypeDef


let events = symbol_table_init [
    (KEYWORD_SYMBOL("country_event"), PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("id"), Catalog_Right ("country_event_id",PARAM_VALUE(INT)) );
        (KEYWORD_SYMBOL("title"), PARAM_OPTION([
            PARAM_VALUE(STRING);
            PARAM_VALUE(KEYWORD)
        ]));
        (KEYWORD_SYMBOL("desc"), PARAM_OPTION([
            PARAM_VALUE(STRING);
            PARAM_VALUE(KEYWORD)
        ]));
        (KEYWORD_SYMBOL("picture"), PARAM_OPTION([
            PARAM_VALUE(STRING);
            PARAM_VALUE(KEYWORD)
        ]));
        (KEYWORD_SYMBOL("major"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("election"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("issue_group"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("allow_multiple_instances"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("mean_time_to_happen"), DefinedTypeRight "country_mtth");
        (KEYWORD_SYMBOL("is_triggered_only"), PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("fire_only_once"), PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("news"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("news_title"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("news_desc_short"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("news_desc_medium"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("news_desc_long"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("trigger"), DefinedTypeRight "country_conditions_def");
        (KEYWORD_SYMBOL("option"), APPEND_SYMBOLS([
            (KEYWORD_SYMBOL("name"),PARAM_OPTION([
                PARAM_VALUE(STRING);
                PARAM_VALUE(KEYWORD)
            ]));
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
                ],DefinedTypeRight "country_conditions_def"));
            ]));
            ],
            DefinedTypeRight "country_effects_def")
        );
        (KEYWORD_SYMBOL("immediate"), DefinedTypeRight "country_effects_def");
    ]));
    (KEYWORD_SYMBOL("province_event"), PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("id"), Catalog_Right ("province_event",PARAM_VALUE INT) );
        (KEYWORD_SYMBOL("title"), PARAM_OPTION([
            PARAM_VALUE(STRING);
            PARAM_VALUE(KEYWORD)
        ]));
        (KEYWORD_SYMBOL("desc"), PARAM_OPTION([
            PARAM_VALUE(STRING);
            PARAM_VALUE(KEYWORD)
        ]));
         (KEYWORD_SYMBOL("picture"), PARAM_OPTION([
            PARAM_VALUE(STRING);
            PARAM_VALUE(KEYWORD)
        ]));
        (KEYWORD_SYMBOL("allow_multiple_instances"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("news"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("news_desc_short"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("news_desc_medium"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("news_desc_long"),PARAM_VALUE(STRING));
        (KEYWORD_SYMBOL("fire_only_once"), PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("major"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("trigger"), DefinedTypeRight "province_conditions_def");
        (KEYWORD_SYMBOL("mean_time_to_happen"), DefinedTypeRight "province_mtth");
        (KEYWORD_SYMBOL("is_triggered_only"), PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("option"), APPEND_SYMBOLS([
            (KEYWORD_SYMBOL("name"),PARAM_OPTION([
                PARAM_VALUE(STRING);
                PARAM_VALUE(KEYWORD)
            ]));
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
                ],DefinedTypeRight "province_conditions_def"));
            ]));
            ],
            DefinedTypeRight "province_effects_def")
        );
        (KEYWORD_SYMBOL("immediate"), DefinedTypeRight "province_effects_def");
    ]));

]
    
