open TypeDef
open SymbolTable

let commons = [
    (
      Definition "bookmarks_def",
        SubTable (symbol_table_init [
            (Literal "bookmark",
            SubTable (symbol_table_init [
                Literal "name", Value String;
                Literal "desc", Value String;
                Literal "date", Value Date;
                Literal "country", ValueList (Value Tag);
                Literal "effect", Type "country_effects_def";
            ])
            )
        ])
    );

    (
      Definition "countries_def",
        SubTable (symbol_table_init [
            (Catalog("country_tag",Value Tag), Link);
        ])
    );

    (
      Definition "country_def",
        SubTable (symbol_table_init [
            Literal "color", ValueList (Value PositiveInt);
            Literal "graphical_culture", Value Keyword;
            Literal "preferred_religion", Value Keyword;
            Literal "historical_idea_groups", ValueList (Value Keyword);
            Literal "historical_units", ValueList (Value Keyword);
            Literal "monarch_names", SubTable (symbol_table_init [
                Value Keyword, Number
            ]);
            Literal "leader_names", ValueList (TypeOption [Value Keyword; Value String]);
            Literal "ship_names", ValueList (TypeOption [Value Keyword; Value String]);
            Literal "army_names", ValueList (Value String);
            Literal "fleet_names", ValueList (Value String);
            Literal "revolutionary_colors", ValueList (Value PositiveInt);
        ])
    );

    (
      Definition "static_modifiers_def",
        SubTable (symbol_table_init [
            (Value Keyword, Inherit (
                [Literal "icon", TypeOption [Value Keyword; Integer]],
                ["country_modifiers_def";"province_modifiers_def"]
            ) 
            );
        ])
    );

    (
      Definition "cb_types_def",
        SubTable (symbol_table_init [
            (Catalog("cb_type",Value Keyword), SubTable (symbol_table_init [
                Literal "valid_for_subject", Value Bool;
                Literal "is_triggered_only", Value Bool;
                Literal "months", Value PositiveInt;
                Literal "prerequisites_self", Type "country_conditions_def";
                Literal "prerequisites", Type "country_conditions_def";
                Literal "war_goal", Value Keyword;
                Literal "can_use", Type "country_conditions_def";
                Literal "on_success", Type "country_effects_def";
                Literal "on_fail", Type "country_effects_def";
                Literal "peace_cost_factor", Decimal;
                Literal "badboy_factor", Decimal;
                Literal "prestige_factor", Decimal;
                Literal "break_truce_prestige_factor", Decimal;
                Literal "break_truce_stability_factor", Decimal;
                Literal "no_opinion_hit", Value Bool;
            ]))
        ])
    );

    (
      Definition "religions_def",
        SubTable (symbol_table_init [
            (Catalog("religion",Value Keyword), SubTable  (symbol_table_init [
                Literal "defender_of_faith", Value Bool;
                Literal "can_form_personal_unions", Value Bool;
                Literal "center_of_religion", Value PositiveInt;
                Literal "flags_with_emblem_percentage", Value PositiveInt;
                Literal "flag_emblem_index_range", ValueList (Value PositiveInt);
                (Catalog ("religion",Value Keyword), SubTable (symbol_table_init [
                    Literal "icon", Value PositiveInt;
                    Literal "color", ValueList (Number);
                    Literal "country", Type "country_modifiers_def";
                    Literal "country_as_secondary", Type "country_modifiers_def";
                    Literal "province", Type "province_modifiers_def";
                    Literal "allowed_conversion", ValueList (Value Keyword);
                    Literal "hre_religion", Value Bool;
                    Literal "on_convert", Type "country_effects_def";
                    Literal "heretic", ValueList (Value Keyword);
                    Literal "papacy", SubTable (symbol_table_init [
                        Literal "papal_tag", Value Tag;
                        Literal "election_cost", Number;
                        Literal "seat_of_papacy", Value PositiveInt;
                    ]);
                ]))
            ]));
        ])
    );

    (
      Definition "cultures_def",
        SubTable (symbol_table_init [
            (Value Keyword, SubTable (symbol_table_init [
                Literal "graphical_culture", Value Keyword;
                Literal "male_names", ValueList (Value String);
                (Catalog ("culture", Value Keyword), SubTable (symbol_table_init [
                    Literal "primary", Value Tag;
                    Literal "male_names", ValueList (Value String);
                    Literal "female_names", ValueList (Value String);
                    Literal "dynasty_names", ValueList (Value String);
                ]));
            ]));
        ])
    );

    (
      Definition "idea_groups_def",
        SubTable (symbol_table_init [
            (Catalog("idea_group",Value Keyword), SubTable (symbol_table_init [
                Literal "category", Value Keyword;
                Literal "bonus", Type "country_modifiers_def";
                Literal "trigger", Type "country_conditions_def";

               (Literal ("ai_will_do"),SubTable(symbol_table_init [
                (Literal("factor"),
                    Number);
                (Literal ("modifier"),Inherit([
                    (Literal ("factor"),Number);
                ],["country_conditions_def"]));
                ]));

                Literal "colonial", Value Bool;
                (Catalog("idea",Value Keyword), Inherit ( [
                    Literal "effect", Type "country_effects_def";
                    Literal "removed_effect", Type "country_effects_def";
                ],["country_modifiers_def"])
                );
            ]))
        ])
    );

    (
      Definition "government_types_def",
        SubTable (symbol_table_init [
            (Catalog("government_type",Value Keyword), SubTable (symbol_table_init [
                Literal "reform_levels", SubTable (symbol_table_init [
                    (Catalog("reform_level",Value Keyword), SubTable (symbol_table_init [
                        Literal "reforms", ValueList (Value Keyword);
                    ]))
                ]);
                Literal "monarchy_names", ValueList (Value String);
                Literal "republican_names", ValueList (Value String);
                Literal "reform_names", ValueList (Value String);
                Literal "modifiers", Type "country_modifiers_def";
            ]))
        ])
    );

    (
      Definition "trade_goods_def",
        SubTable (symbol_table_init [
            (Catalog("trade_good",Value Keyword), SubTable (symbol_table_init [
                Literal "color", ValueList (Number);
                Literal "modifier", Type "country_modifiers_def";
                Literal "province", Type "province_modifiers_def";
                Literal "chance", SubTable (symbol_table_init [
                    Literal "factor", Number;
                    Literal "modifier", Inherit ( [
                        Literal "factor", Number;
                     ], ["country_conditions_def"]);
                ]);
            ]))
        ])
    );

    (
      Definition "buildings_def",
        SubTable (symbol_table_init [
            (Catalog("building",Value Keyword), SubTable (symbol_table_init [
                Literal "cost", Value PositiveInt;
                Literal "time", Value PositiveInt;
                Literal "modifier", Type "province_modifiers_def";
                Literal "trigger", Type "province_conditions_def";
                Literal "one_per_country", Value Bool;
                Literal "manufactory", ValueList (Value Keyword);
                Literal "onmap", Value Bool;
                Literal "on_built", Type "province_effects_def";
                Literal "on_destroyed", Type "province_effects_def";
                Literal "on_construction_started", Type "province_effects_def";
                Literal "on_construction_canceled", Type "province_effects_def";
                Literal "on_obsolete", Type "province_effects_def";
                (Literal ("ai_will_do"),SubTable(symbol_table_init [
                (Literal("factor"),
                    Number);
                (Literal ("modifier"),Inherit([
                    (Literal ("factor"),Number);
                ],["country_conditions_def"]));
            ]));
            ]))
        ])
    );

    (
      Definition "advisor_types_def",
        SubTable (symbol_table_init [
            (Catalog("advisor_type",Value Keyword), SubTable (symbol_table_init [
                Literal "monarch_power", Value Keyword;
                Literal "cost", Value PositiveInt;
                Literal "allow", Type "country_conditions_def";
                Literal "allow_only_male", Value Bool;
                Literal "allow_only_female", Value Bool;
                Literal "modifiers", Type "country_modifiers_def";
                Literal "skill_scaled_modifier", SubTable (symbol_table_init [
                    Literal "trigger", Type "country_conditions_def";
                    Literal "modifier", Type "country_modifiers_def";
                ]);
                (Literal ("ai_will_do"),SubTable(symbol_table_init [
                    (Literal("factor"),
                        Number);
                    (Literal ("modifier"),Inherit([
                        (Literal ("factor"),Number);
                    ],["country_conditions_def"]));
                ]));
            ]))
        ])
    );

    (
      Definition "estate_privileges_def",
        SubTable (symbol_table_init [
            (Catalog("estate_privilege",Value Keyword), SubTable (symbol_table_init [
                Literal "icon", Value Keyword;
                Literal "land_share", Number;
                Literal "max_absolutism", Number;
                Literal "conditional_modifier", SubTable (symbol_table_init [
                    Literal "trigger", Type "country_conditions_def";
                    Literal "modifier", Type "country_modifiers_def";
                ]);
                Literal "loyalty", Decimal;
                Literal "influence", Decimal;
                Literal "can_select", Type "country_conditions_def";
                Literal "on_granted", Type "country_effects_def";
                Literal "modifier_by_land_ownership", Type "country_modifiers_def";
                Literal "penalties", Type "country_modifiers_def";
                Literal "benefits", Type "country_modifiers_def";
                Literal "modifiers", Type "country_modifiers_def";
                Literal "allow", Type "country_conditions_def";

                (Literal ("ai_will_do"),SubTable(symbol_table_init [
                    (Literal("factor"),
                        Number);
                    (Literal ("modifier"),Inherit([
                        (Literal ("factor"),Number);
                    ],["country_conditions_def"]));
                ]));
            ]))
        ])
    );

    (
      Definition "institutions_def",
        SubTable (symbol_table_init [
            (Catalog("institution",Value Keyword), SubTable (symbol_table_init [
                Literal "bonus", Type "country_modifiers_def";
                Literal "trade_company_efficiency", Decimal;
                Literal "history", Type "province_conditions_def";
                Literal "can_emerge", Type "province_conditions_def";
                Literal "effect", Type "country_effects_def";
            ]))
        ])
    );

    (
      Definition "graphical_culture_type_def", ValueList (Catalog ("graphic_culture", Value Keyword));
    );

    (
      Definition "event_modifiers_def",
        SubTable (symbol_table_init [
            (Value Keyword, Inherit ([
                (Literal "trigger", Type "country_conditions_def");
                (Literal "icon", TypeOption [Value Keyword; Integer; Value PositiveInt]);
            ], ["country_modifiers_def"; "province_modifiers_def"]));
        ])
    );

    (
      Definition "triggered_modifiers_def",
        SubTable (symbol_table_init [
            (Catalog ("triggered_modifier", Value Keyword), Inherit ([
                (Literal "trigger", Type "country_conditions_def");
                (Literal "icon", TypeOption [Value Keyword; Integer; Value PositiveInt]);
            ], ["country_modifiers_def"]));
        ])
    );
]
