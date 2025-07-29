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
                Literal "cameraX", Value PositiveInt;
                Literal "cameraY", Value PositiveInt;
            ])
            )
        ])
    );

    (
      Definition "buildings",
        SubTable (symbol_table_init [
            (Catalog("factory", Value Keyword), Type "factory_def");
            (Catalog("building", Literal "naval_base"), Type "naval_base_def");
            (Catalog("building", Literal "railroad"), Type "rail_def";)
        ])
    );

    (
      Definition "building_def",SubTable (symbol_table_init [
            (Literal "goods_cost", SubTable (symbol_table_init [
                (Type "good", Integer);
            ]));
            (Literal "time", Value PositiveInt);
            (Literal "visibility", Value Bool);
            (Literal "onmap", Value Bool);
            (Literal "province", Value Bool);
            (Literal "cost", Value PositiveInt);
            (Literal "max_level", Value PositiveInt);
            (Literal "pop_build_factory", Value Bool);
            (Literal "one_per_state", Value Bool);
        ])
    );
    
    
    (
      Definition "fort",
        Inherit ([
            (Literal "type", Literal "fort");
            (Literal "fort_level", Value PositiveInt);
        ], ["province_modifier"; "building_def"])
    );

    (
      Definition "factory_def",
        Inherit ([
            (Literal "type", Value Keyword);
            (Literal "production_type", Type "production_type");
            (Literal "completion_size", Decimal);
            (Literal "on_completion", Value Keyword);
            (Literal "default_enabled", Value Bool);
            (Literal "sail", Value Bool);
            (Literal "steam", Value Bool);
            (Literal "strategic_factory", Value Bool);
            (Literal "advanced_factory", Value Bool);
        ], ["building_def"]
        )
    );

    (
      Definition "naval_base_def",
        Inherit ([
            (Literal "type", Literal "naval_base");
            (Literal "port", Value Bool);
            (Literal "capital", Value Bool);
            (Literal "naval_capacity", Value PositiveInt);
            (Literal "colonial_range", Value PositiveInt);
        ], ["province_modifier"; "building_def"])
    );

    (
      Definition "rail_def",
        (Inherit ([
            (Literal "type", Literal "railroad");
            (Literal "infrastructure", Value PositiveFloat);
            (Literal "spawn_railroad_track", Value Bool);
        ], ["province_modifier"; "building_def"]))
    );

    (
      Definition "cb_types_def",
        SubTable (symbol_table_init [
            (Literal "peace_order", ValueList (Value Keyword));
            (Catalog("cb_type",Value Keyword), SubTable (symbol_table_init [
                (Literal "constructing_cb", Value Bool);
                (Literal "sprite_index", Value PositiveInt);
                (Literal "is_triggered_only", Value Bool);
                (Literal "months", Value PositiveInt);
                (Literal "crisis", Value Bool);
                (Literal "construction_speed", Decimal);
                (Literal "badboy_factor", Value PositiveInt);
                (Literal "prestige_factor", Value PositiveInt);
                (Literal "peace_cost_factor", Value PositiveInt);
                (Literal "penalty_factor", Value PositiveInt);
                (Literal "always", Value Bool);
                (Literal "break_truce_prestige_factor", Value PositiveInt);
                (Literal "break_truce_infamy_factor", Value PositiveInt);
                (Literal "break_truce_militancy_factor", Value PositiveInt);
                (Literal "truce_months", Value PositiveInt);
                (Literal "good_relation_prestige_factor", Value PositiveInt);
                (Literal "good_relation_infamy_factor", Value PositiveInt);
                (Literal "good_relation_militancy_factor", Value PositiveInt);
                (Literal "can_use", Type "country_conditions_def");
                (Literal "allowed_states", Type "country_conditions_def");
                (Literal "on_po_accepted", Type "country_effects_def");
                (Literal "po_disarmament", Value Bool);
                (Literal "po_reparations", Value Bool);
                (Literal "war_name", Value Keyword);
                (Literal "po_remove_cores", Value Bool);
                (Literal "po_transfer_provinces", Value Bool);
                (Literal "po_demand_state", Value Bool);
                (Literal "po_add_to_sphere", Value Bool);
                (Literal "po_remove_prestige", Value Bool);
                (Literal "po_make_puppet", Value Bool);
                (Literal "po_release_puppet", Value Bool);
                (Literal "po_status_quo", Value Bool);
                (Literal "po_install_communist_gov_type", Value Bool);
                (Literal "po_uninstall_communist_gov_type", Value Bool);
                (Literal "po_colony", Value Bool);
                (Literal "po_destroy_forts", Value Bool);
                (Literal "po_destroy_naval_bases", Value Bool);
                (Literal "po_clear_union_sphere", Value Bool);
                (Literal "po_gunboat", Value Bool);
                (Literal "po_demand_states", Value Bool);
                (Literal "po_annex", Value Bool);
                (Literal "great_war_obligatory", Value Bool);
                (Literal "mutual", Value Bool);
                (Literal "on_add", Inherit( [
                    (Literal "add_war_goal", SubTable (symbol_table_init [
                        (Literal "casus_belli", Type "cb_type");
                    ]));
                ],
                ["country_effects_def"]
                ));
            ]))
        ])
    );

    (
      Definition "countries_def",
        SubTable (symbol_table_init [
            (*Catalog later*)
            (Value Tag, Link);
        ])
    );

    (
      Definition "country_def",
        SubTable (symbol_table_init [
            (Literal "color", ValueList (Value PositiveInt));
            (Literal "graphical_culture", Type "graphic_culture");
            (Literal "unit_names",SubTable ( symbol_table_init[
            Value Keyword,ValueList (TypeOption [
                Value Keyword; 
                Value String;
            ]
            )]));
            (Literal "party", Type "party_def");
            (Type "government", Value PositiveInt);
        ])
    );
    (
    Definition "graphical_culture_type_def",SubTable(symbol_table_init[
        (Nothing,ValueList(Catalog ("graphic_culture",Value Keyword)));
        ]
    ));
    (
      Definition "party_def",
            ( SubTable (symbol_table_init [
                (Literal "name", TypeOption[Value Keyword; Value String]);
                (Literal "ideology", Type "ideology");
                (Literal "start_date", Value Date);
                (Literal "end_date", Value Date);
                (Type "policy_type", SubType ("policy_type","reform")  );
            ]))
        
    );

    (
      Definition "country_colors",
        SubTable (symbol_table_init [
            (Value Tag, SubTable (symbol_table_init [
                (Literal "color", ValueList (Value PositiveInt));
                (Literal "color2", ValueList (Value PositiveInt));
                (Literal "color3", ValueList (Value PositiveInt));
            ]));
        ])
    );

    (
      Definition "crime",
        SubTable (symbol_table_init [
            (Value Keyword, Inherit([
                (Literal "trigger", Type "pop_conditions_def");
            ], [ "province_modifiers_def"]));
        ])
    );
    (
      Definition "event_modifiers_def",
        SubTable (symbol_table_init [
            (Value Keyword, Inherit ([
                (Literal "trigger", Type "country_conditions_def");
                (Literal "icon", Value PositiveInt);
            ], ["country_modifiers_def"; "province_modifiers_def"]))
        ])
    );

    (
      Definition "culture_groups_def",
        SubTable (symbol_table_init [
            (Value Keyword, SubTable (symbol_table_init [
                (Literal "leader", Value Keyword);
                (Literal "unit", Value Keyword);
                (Literal "union", Value Tag);
                (Catalog ("culture", Value Keyword), Type "culture_def");
                (Literal "radicalism", Number);
                (Literal "primary", Value Tag);
            ]));
        ])
    );

    (
      Definition "culture_def",
        SubTable (symbol_table_init [
            (Literal "color", ValueList (Value PositiveInt));
            (Literal "first_names", ValueList (TypeOption [Value String; Value Keyword]));
            (Literal "last_names", ValueList (TypeOption [Value String; Value Keyword]));
            (Literal "radicalism", Number);
            (Literal "primary", Value Tag);
        ])
    );

    (
      Definition "goods_def",
        SubTable (symbol_table_init [
            (Value Keyword, SubTable (symbol_table_init [
                (Catalog("good", Value Keyword), SubTable (symbol_table_init [
                    (Literal "cost", Decimal);
                    (Literal "color", ValueList (Value PositiveInt));
                    (Literal "available_from_start", Value Bool);
                    (Literal "oversees_penalty", Value Bool);
                    (Literal "tradeable", Value Bool);
                    (Literal "money", Value Bool);
                ]));
            ]));
        ])
    );

    (
      Definition "ideologies_def",
        SubTable (symbol_table_init [
            (Value Keyword, SubTable (symbol_table_init [
                (Catalog("ideology", Value Keyword), SubTable (symbol_table_init [
                    (Literal "civilized", Value Bool);
                    (Literal "uncivilized", Value Bool);
                    (Literal "date", Value Date);
                    (Literal "color", ValueList (Value PositiveInt));
                    (Literal "can_reduce_militancy", Value Bool);
                    (Literal "remove_political_reform", Type "reform_action"); 
                    (Literal "add_political_reform", Type "reform_action");
                    (Literal "add_social_reform",  Type "reform_action");
                    (Literal "remove_social_reform", Type "reform_action"); 
                    (Literal "add_military_reform", Type "reform_action");
                    (Literal "remove_military_reform", Type "reform_action");
                    (Literal "add_economic_reform", Type "reform_action");
                    (Literal "remove_economic_reform", Type "reform_action");
                ]));
            ]));
        ])
    );
    (Definition "reform_action",SubTable (symbol_table_init [
             (Literal "base", Number);
             (Literal "group", SubTable (symbol_table_init [
                 (Literal "modifier", Inherit ([
                     (Literal "factor", TypeOption ([Decimal; Integer]));
                 ],  [ "pop_conditions_def";  "country_conditions_def";  "province_conditions_def"]));
             ]));
            (Literal "modifier", Inherit ([
                (Literal "factor", TypeOption ([Decimal; Integer]));
            ],  [ "pop_conditions_def";  "country_conditions_def";  "province_conditions_def"]));





    ]) );

    (
      Definition "issues_file_def",
        SubTable (symbol_table_init [
            (Literal "party_issues", SubTable (symbol_table_init [
                (SupCatalog("policy_type", Value Keyword), SubTable (symbol_table_init [
                    (InnerCatalog("reform", Value Keyword), Type "issue_def");
                ]));
            ]));
            (Literal "political_reforms", SubTable (symbol_table_init [
                (Literal "administrative",Value Bool);
                (Literal "next_step_only", Value Bool);
                (SupCatalog("policy_type", Value Keyword), SubTable (symbol_table_init [
                    (InnerCatalog("reform", Value Keyword), Type "reform_def");
                ]));
            ]));
            (Literal "social_reforms", SubTable (symbol_table_init [
                (Literal "administrative",Value Bool);
                (Literal "next_step_only", Value Bool);
                (SupCatalog("policy_type", Value Keyword), SubTable (symbol_table_init [
                    (InnerCatalog("reform", Value Keyword), Type "reform_def");
                ]));
            ]));
            (Literal "military_reforms", SubTable (symbol_table_init [
                (Literal "administrative",Value Bool);
                (Literal "next_step_only", Value Bool);
                (SupCatalog("policy_type", Value Keyword), SubTable (symbol_table_init [
                    (InnerCatalog("reform", Value Keyword), Type "reform_def");
                ]));
            ]));
            (Literal "economic_reforms", SubTable (symbol_table_init [
                (Literal "administrative",Value Bool);
                (Literal "next_step_only", Value Bool);
                (SupCatalog("policy_type", Value Keyword), SubTable (symbol_table_init [
                    (InnerCatalog("reform", Value Keyword), Type "reform_def");
                ]));
            ]));
        ])
    );

    (
      Definition "issue_def",
        (Inherit ([
            (Literal "rules", SubTable (symbol_table_init [
                (Value Keyword, Value Bool);
            ]));
            (Literal "on_execute", SubTable (symbol_table_init [
                (Literal "effect", Type "country_effects_def");
            ]));
            (Literal "max_tariff", Number);
            (Literal "min_tariff", Number);
            (Literal "max_tax", Number);
            (Literal "min_tax", Number);
            (*error here*)
            (Literal "is_jingoism", Value Bool);
            (Literal "minimum_wage", Decimal);
            (Literal "administrative_multiplier", Value PositiveFloat);
            (Literal "factory_maintenance", Value NegativeFloat);
            (Literal "pension_level", Value PositiveFloat);
            (Literal "unemployment_benefit", Value PositiveFloat);
        ], [ "country_modifiers_def"]))
    );

    (
      Definition "reform_def",
        Inherit ([
            (Literal "allow", Type "country_conditions_def");
        ], [ "issue_def"])
    );

    (
      Definition "national_focus_group_def",
        SubTable (symbol_table_init [
            (Catalog ("national_focus",Value Keyword) , SubTable (symbol_table_init [
                (Value Keyword, Inherit ([
                    (Literal "icon", Value PositiveInt);
                    (Literal "railroads", Number);
                    (Literal "uncolonized_province", Value Bool);
                    (Literal "colonial_validity_check", Value Bool);
                    (Literal "limit", Type "province_conditions_def");
                    (Literal "own_provinces", Value Bool);
                    (Literal "has_flashpoint", Value Bool);
                    (Literal "ideology", Type "ideology");
                    (Literal "loyalty", Decimal);
                    (Literal "flashpoint_tension", Decimal);
                ], [ "province_modifiers_def"]));
            ]));
        ])
    );

    (
      Definition "nationalvalue",
        SubTable (symbol_table_init [
            (Value Keyword, Type "country_modifiers_def");
        ])
    );

    (
      Definition "on_actions_def",
        SubTable (symbol_table_init [
            (Literal "on_election_tick", SubTable (symbol_table_init [
                (Integer, TypeOption [Type "country_event_id"]);
            ]));
            (Literal "on_colony_to_state", SubTable (symbol_table_init [
                (Integer, TypeOption [Type "country_event_id"]);
            ]));
            (Literal "on_state_conquest", SubTable (symbol_table_init [
                (Integer, TypeOption [Type "country_event_id"]);
            ]));
            (Literal "on_colony_to_state_free_slave", SubTable (symbol_table_init [
                (Integer, TypeOption [Type "country_event_id"]);
            ]));
            (Literal "on_quarterly_pulse", SubTable (symbol_table_init [
                (Integer, TypeOption [Type "country_event_id"; Type "province_event_id"]);
            ]));
            (Value Keyword, SubTable (symbol_table_init [
                (Integer, TypeOption [Type "country_event_id"; Type "province_event_id"]);
            ]));
        ])
    );

    (
      Definition "poptypes_def",
        SubTable (symbol_table_init [
            (Literal "promotion_chance", Type "pop_type_def");
            (Literal "demotion_chance", Type "pop_type_def");
            (Literal "migration_chance", Type "pop_type_def");
            (Literal "colonialmigration_chance", Type "pop_type_def");
            (Literal "emigration_chance", Type "pop_type_def");
            (Literal "assimilation_chance", Type "pop_type_def");
            (Literal "conversion_chance", Type "pop_type_def");
        ])
    );

    (
      Definition "pop_type_def",
        SubTable (symbol_table_init [
            (Literal "factor", Number);
            (Literal "modifier", Inherit ([
                (Literal "factor", Number);
            ],  [ "pop_conditions_def";  "country_conditions_def";  "province_conditions_def"]));
            (Literal "group", SubTable (symbol_table_init [
                (Literal "modifier", Inherit ([
                    (Literal "factor", Number);
                ], ["pop_conditions_def";  "country_conditions_def"; "province_conditions_def"]));
            ]));
        ])
    );

    (
      Definition "production_type_def",
        SubTable (symbol_table_init [
            (Catalog ("production_type",Value Keyword), SubTable (symbol_table_init [
                (Literal "efficiency", SubTable (symbol_table_init [
                    (Value Keyword, Decimal);
                ]));
                (Literal "owner", SubTable (symbol_table_init [
                    (Literal "poptype", Value Keyword);
                    (Literal "effect", Value Keyword);
                    (Literal "effect_multiplier", Decimal);
                ]));
                (Literal "employees", ValueList (SubTable (symbol_table_init [
                    (Literal "poptype", Value Keyword);
                    (Literal "effect", Value Keyword);
                    (Literal "amount", Decimal);
                    (Literal "effect_multiplier", Decimal);
                ])));
                (Literal "type", TypeOption [
                    Literal "rgo";
                    Literal "artisan";
                    Literal "factory";
                ]);
                (Literal "workforce", TypeOption [Decimal; Value PositiveInt]);
                (Literal "value", Decimal);
                (Literal "input_goods", SubTable (symbol_table_init [
                    (Value Keyword, Number);
                ]));
                (Literal "output_goods", Value Keyword);
                (Literal "bonus", SubTable (symbol_table_init [
                    (Literal "type", Value Keyword);
                    (Literal "value", Number);
                ]));
                (Literal "farm", Value Bool);
                (Literal "mine", Value Bool);
                (Literal "is_coastal", Value Bool);
                (Literal "type", TypeOption [Literal "rgo"; Literal "artisan"; Literal "factory"]);
            ]));
        ])
    );
    (Definition "governments_def",SubTable (symbol_table_init [
        Catalog ("government",Value Keyword),SubTable (symbol_table_init[ 
            Type "ideology", Value Bool;
            Literal "appoint_ruling_party", Value Bool;
            Literal "flagType", Value Keyword;
            Literal "duration", WholeNumber;
        ])
    ])
    );

    (
  Definition "rebel_type_def",
    SubTable (symbol_table_init [
        Catalog("rebel_type", Value Keyword), SubTable(symbol_table_init [
            (Literal "icon", Value PositiveInt);
            (Literal "area", Value Keyword);
            (Literal "break_alliance_on_win", Value Bool);
            (Literal "government", SubTable (symbol_table_init [
                (Type "government", Type "government");
            ]));
            (Literal "defection", Value Keyword);
            (Literal "independence", Value Keyword);
            (Literal "defect_delay", Value PositiveInt);
            (Literal "ideology", Type "ideology");
            (Literal "allow_all_cultures", Value Bool);
            (Literal "allow_all_religions", Value Bool);
            (Literal "allow_all_ideologies", Value Bool);
            (Literal "resilient", Value Bool);
            (Literal "reinforcing", Value Bool);
            (Literal "general", Value Bool);
            (Literal "smart", Value Bool);
            (Literal "unit_transfer", Value Bool);
            (Literal "occupation_mult", Decimal);
            (Literal "will_rise", SubTable (symbol_table_init [
                (Literal "factor", Number);
                (Literal "modifier", Inherit([(Literal "factor", Number)], 
                [ "country_conditions_def"]));
            ]));
            (Literal "spawn_chance", SubTable (symbol_table_init [
                (Literal "factor", Number);
                (Literal "modifier", Inherit([(Literal "factor", Number)], 
                ["pop_conditions_def"]));
            ]));
            (Literal "movement_evaluation", SubTable (symbol_table_init [
                (Literal "factor", Number);
                (Literal "modifier", Inherit([(Literal "factor", Number)], 
                [ "province_conditions_def"]));
            ]));
            (Literal "siege_won_trigger", Type "province_conditions_def");
            (Literal "siege_won_effect", Type "province_effects_def");
            (Literal "demands_enforced_trigger", Type "country_conditions_def");
            (Literal "demands_enforced_effect", Type "country_effects_def");
        ])
    ])
    );
    (
      Definition "religions_def",
        SubTable (symbol_table_init [
            (Value Keyword, SubTable  (symbol_table_init [
                (Catalog ("religion",Value Keyword), SubTable (symbol_table_init [
                    (Literal "icon", Value PositiveInt);
                    (Literal "color", ValueList (Number));
                    (Literal "pagan", Value Bool);
                ]))
            ]));
        ])
    );

    (
      Definition "static_modifiers_def",
        SubTable (symbol_table_init [
            (Value Keyword, Inherit (
                [(Literal "icon", Value Keyword)],
                ["country_modifiers_def";"province_modifiers_def"]
            ) 
            );
        ])
    );

    (
      Definition "technology_group_def",
        SubTable (symbol_table_init [
            (Literal "schools", SubTable (symbol_table_init [
                (Catalog ("tech_school",Value Keyword), SubTable (symbol_table_init [
                    (Literal "army_tech_research_bonus", Number);
                    (Literal "commerce_tech_research_bonus", Number);
                    (Literal "culture_tech_research_bonus", Number);
                    (Literal "industry_tech_research_bonus", Number);
                    (Literal "navy_tech_research_bonus", Number);
                    (Literal "unciv_economic_modifier", Number);
                    (Literal "unciv_military_modifier", Number);
                ]));
            ]));
            (Literal "folders", SubTable (symbol_table_init [
                (Value Keyword, ValueList (Catalog ("tech_area",Value Keyword)));
            ]));
        ])
    );

    (
      Definition "trait_file_def",
        SubTable (symbol_table_init [
            (Literal "personality", SubTable (symbol_table_init [
                (Catalog("personality", Value Keyword), SubTable (symbol_table_init [
                    (Literal "attack", Number);
                    (Literal "defence", Number);
                    (Literal "morale", Number);
                    (Literal "organisation", Number);
                    (Literal "reconnaissance", Number);
                    (Literal "speed", Number);
                    (Literal "attrition", Number);
                    (Literal "experience", Number);
                    (Literal "reliability", Number);
                ]))
            ]));
            (Literal "background", SubTable (symbol_table_init [
                (Catalog("background", Value Keyword), SubTable (symbol_table_init [
                    (Literal "attack", Number);
                    (Literal "defence", Number);
                    (Literal "morale", Number);
                    (Literal "organisation", Number);
                    (Literal "reconnaissance", Number);
                    (Literal "speed", Number);
                    (Literal "attrition", Number);
                    (Literal "experience", Number);
                    (Literal "reliability", Number);
                ]))
            ]))
        ])
    );

    (
      Definition "triggered_modifiers_def",
        SubTable (symbol_table_init [
            (Catalog("trigger_modifiers", Value Keyword), Inherit ([
                (Literal "trigger", Type "country_conditions_def");
                (Literal "icon", Integer);
            ], [ "country_modifiers_def"])
            );
        ])
    );
]
