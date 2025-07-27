open SymbolTable
open TypeDef


let events = symbol_table_init [
    (Literal("country_event"), SubTable(symbol_table_init [
        (Literal("id"), Catalog ("country_event_id",Value PositiveInt) );
        (Literal("title"), TypeOption([
            Value(String);
            Value(Keyword)
        ]));
        (Literal("desc"), TypeOption([
            Value(String);
            Value(Keyword)
        ]));
        (Literal("picture"), TypeOption([
            Value(String);
            Value(Keyword)
        ]));
        (Literal("major"),Value(Bool));
        (Literal("election"),Value(Bool));
        (Literal("issue_group"),Value(Keyword));
        (Literal("allow_multiple_instances"),Value(Bool));
        (Literal("mean_time_to_happen"), Type "country_mtth");
        (Literal("is_triggered_only"), Value(Bool));
        (Literal("fire_only_once"), Value(Bool));
        (Literal("news"),Value(Bool));
        (Literal("news_title"),Value(String));
        (Literal("news_desc_short"),Value(String));
        (Literal("news_desc_medium"),Value(String));
        (Literal("news_desc_long"),Value(String));
        (Literal("trigger"), Type "country_conditions_def");
        (Literal("option"), Inherit([
            (Literal("name"),TypeOption([
                Value(String);
                Value(Keyword)
            ]));
            (Literal("ai_chance"),SubTable(symbol_table_init [
                (Literal("factor"),
                    Number);
                (Literal("modifier"),Inherit([
                    (Literal("factor"),
                        Number);
                ],[ "country_conditions_def"]));
            ]));
            ],
            ["country_effects_def"])
        );
        (Literal("immediate"), Type "country_effects_def");
    ]));
    (Literal("province_event"), SubTable(symbol_table_init [
        (Literal("id"), Catalog ("province_event_id",Value PositiveInt) );
        (Literal("title"), TypeOption([
            Value(String);
            Value(Keyword)
        ]));
        (Literal("desc"), TypeOption([
            Value(String);
            Value(Keyword)
        ]));
         (Literal("picture"), TypeOption([
            Value(String);
            Value(Keyword)
        ]));
        (Literal("allow_multiple_instances"),Value(Bool));
        (Literal("news"),Value(Bool));
        (Literal("news_desc_short"),Value(String));
        (Literal("news_desc_medium"),Value(String));
        (Literal("news_desc_long"),Value(String));
        (Literal("fire_only_once"), Value(Bool));
        (Literal("major"),Value(Bool));
        (Literal("trigger"), Type "province_conditions_def");
        (Literal("mean_time_to_happen"), Type "province_mtth");
        (Literal("is_triggered_only"), Value(Bool));
        (Literal("option"), Inherit([
            (Literal("name"),TypeOption([
                Value(String);
                Value(Keyword)
            ]));
            (Literal("ai_chance"),SubTable(symbol_table_init [
                (Literal("factor"),
                    Number);
                (Literal("modifier"),Inherit([
                    (Literal("factor"),Number);
                ],[ "province_conditions_def"]));
            ]));
            ],
            ["province_effects_def"])
        );
        (Literal("immediate"), Type "province_effects_def");
    ]));

]
    
