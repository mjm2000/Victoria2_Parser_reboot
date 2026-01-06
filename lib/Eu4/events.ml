open SymbolTable
open TypeDef

let events = symbol_table_init [
    (Literal "namespace", Value Keyword);
    (Literal("country_event"), SubTable(symbol_table_init [
        (Literal("id"), Catalog ("country_event_id", Value PositiveInt));
        (Literal("title"), TypeOption([
            Value(String);
            Value(Keyword);
        ]));
        (Literal("desc"), TypeOption([
            Value(String);
            Value(Keyword);
        ]));
        (Literal("picture"), TypeOption([
            Value(String);
            Value(Keyword);
        ]));
        (Literal("hidden"), Value(Bool));
        (Literal("major"), Value(Bool));
        (Literal("is_triggered_only"), Value(Bool));
        (Literal("fire_only_once"), Value(Bool));
        (Literal("mean_time_to_happen"), Type "country_mtth");
        (Literal("trigger"), Type "country_conditions_def");
        (Literal("option"), Inherit([
            (Literal("name"), TypeOption([
                Value(String);
                Value(Keyword);
            ]));
            (Literal("ai_chance"), SubTable(symbol_table_init [
                (Literal("factor"), Number);
                (Literal("modifier"), Inherit([
                    (Literal("factor"), Number);
                ], ["country_conditions_def"]));
            ]));
        ],
        ["country_effects_def"])
        );
        (Literal("immediate"), Type "country_effects_def");
    ]));
    (Literal("province_event"), SubTable(symbol_table_init [
        (Literal("id"), Catalog ("province_event_id", Value PositiveInt));
        (Literal("title"), TypeOption([
            Value(String);
            Value(Keyword);
        ]));
        (Literal("desc"), TypeOption([
            Value(String);
            Value(Keyword);
        ]));
        (Literal("picture"), TypeOption([
            Value(String);
            Value(Keyword);
        ]));
        (Literal("major"), Value(Bool));
        (Literal("is_triggered_only"), Value(Bool));
        (Literal("fire_only_once"), Value(Bool));
        (Literal("mean_time_to_happen"), Type "province_mtth");
        (Literal("trigger"), Type "province_conditions_def");
        (Literal("option"), Inherit([
            (Literal("name"), TypeOption([
                Value(String);
                Value(Keyword);
            ]));
            (Literal("ai_chance"), SubTable(symbol_table_init [
                (Literal("factor"), Number);
                (Literal("modifier"), Inherit([
                    (Literal("factor"), Number);
                ], ["province_conditions_def"]));
            ]));
        ],
        ["province_effects_def"])
        );
        (Literal("immediate"), Type "province_effects_def");
    ]));
]
