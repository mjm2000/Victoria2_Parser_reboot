open SymbolTable
open TypeDef


let events = symbol_table_init [
    (KeywordLiteral("country_event"), PARAM_LIST(symbol_table_init [
        (KeywordLiteral("id"), Catalog_Right ("country_event_id",PARAM_VALUE(INT)) );
        (KeywordLiteral("title"), PARAM_OPTION([
            PARAM_VALUE(STRING);
        ]));
        (KeywordLiteral("desc"), PARAM_OPTION([
            PARAM_VALUE(STRING);
        ]));
        (KeywordLiteral("picture"), PARAM_OPTION([
            PARAM_VALUE(STRING);
        ]));
        (KeywordLiteral("major"),PARAM_VALUE(BOOL));
        (KeywordLiteral("election"),PARAM_VALUE(BOOL));
        (KeywordLiteral("issue_group"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("allow_multiple_instances"),PARAM_VALUE(BOOL));
        (KeywordLiteral("mean_time_to_happen"), DefinedTypeRight "country_mtth");
        (KeywordLiteral("is_triggered_only"), PARAM_VALUE(BOOL));
        (KeywordLiteral("fire_only_once"), PARAM_VALUE(BOOL));
        (KeywordLiteral("news"),PARAM_VALUE(BOOL));
        (KeywordLiteral("news_title"),PARAM_VALUE(STRING));
        (KeywordLiteral("news_desc_short"),PARAM_VALUE(STRING));
        (KeywordLiteral("news_desc_medium"),PARAM_VALUE(STRING));
        (KeywordLiteral("news_desc_long"),PARAM_VALUE(STRING));
        (KeywordLiteral("trigger"), DefinedTypeRight "country_conditions_def");
        (KeywordLiteral("option"), APPEND_SYMBOLS([
            (KeywordLiteral("name"),PARAM_OPTION([
                PARAM_VALUE(STRING);
            ]));
            (KeywordLiteral("ai_chance"),PARAM_LIST(symbol_table_init [
                (KeywordLiteral("factor"),
                    PARAM_OPTION([
                        PARAM_VALUE(INT);
                        PARAM_VALUE(FLOAT);
                ]));
                (KeywordLiteral("modifier"),APPEND_SYMBOLS([
                    (KeywordLiteral("factor"),
                        PARAM_OPTION([
                            PARAM_VALUE(INT);
                            PARAM_VALUE(FLOAT);
                    ]));
                ],DefinedTypeRight "country_conditions_def"));
            ]));
            ],
            DefinedTypeRight "country_effects_def")
        );
        (KeywordLiteral("immediate"), DefinedTypeRight "country_effects_def");
    ]));
    (KeywordLiteral("province_event"), PARAM_LIST(symbol_table_init [
        (KeywordLiteral("id"), Catalog_Right ("province_event",PARAM_VALUE INT) );
        (KeywordLiteral("title"), PARAM_OPTION([
            PARAM_VALUE(STRING);
        ]));
        (KeywordLiteral("desc"), PARAM_OPTION([
            PARAM_VALUE(STRING);
        ]));
         (KeywordLiteral("picture"), PARAM_OPTION([
            PARAM_VALUE(STRING);
        ]));
        (KeywordLiteral("allow_multiple_instances"),PARAM_VALUE(BOOL));
        (KeywordLiteral("news"),PARAM_VALUE(BOOL));
        (KeywordLiteral("news_desc_short"),PARAM_VALUE(STRING));
        (KeywordLiteral("news_desc_medium"),PARAM_VALUE(STRING));
        (KeywordLiteral("news_desc_long"),PARAM_VALUE(STRING));
        (KeywordLiteral("fire_only_once"), PARAM_VALUE(BOOL));
        (KeywordLiteral("major"),PARAM_VALUE(BOOL));
        (KeywordLiteral("trigger"), DefinedTypeRight "province_conditions_def");
        (KeywordLiteral("mean_time_to_happen"), DefinedTypeRight "province_mtth");
        (KeywordLiteral("is_triggered_only"), PARAM_VALUE(BOOL));
        (KeywordLiteral("option"), APPEND_SYMBOLS([
            (KeywordLiteral("name"),PARAM_OPTION([
                PARAM_VALUE(STRING);
            ]));
            (KeywordLiteral("ai_chance"),PARAM_LIST(symbol_table_init [
                (KeywordLiteral("factor"),
                    PARAM_OPTION([
                        PARAM_VALUE(INT);
                        PARAM_VALUE(FLOAT);
                ]));
                (KeywordLiteral("modifier"),APPEND_SYMBOLS([
                    (KeywordLiteral("factor"),
                        PARAM_OPTION([
                            PARAM_VALUE(INT);
                            PARAM_VALUE(FLOAT);
                    ]));
                ],DefinedTypeRight "province_conditions_def"));
            ]));
            ],
            DefinedTypeRight "province_effects_def")
        );
        (KeywordLiteral("immediate"), DefinedTypeRight "province_effects_def");
    ]));

]
    
