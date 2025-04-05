open TypeDef
open SymbolTable

let commons = [
    (
      Definition "bookmarks_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "bookmark",
            PARAM_LIST (symbol_table_init [
                KeywordLiteral "name", PARAM_VALUE STRING;
                KeywordLiteral "desc", PARAM_VALUE STRING;
                KeywordLiteral "date", PARAM_VALUE KEYWORD;
                KeywordLiteral "cameraX", PARAM_VALUE INT;
                KeywordLiteral "cameraY", PARAM_VALUE INT;
            ])
            )
        ])
    );

    (
      Definition "buildings",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft("factory", TYPE_SYMBOL KEYWORD), DefinedTypeRight "factory_def");
            (CatalogLeft("building", KeywordLiteral "naval_base"), DefinedTypeRight "naval_base_def");
            (CatalogLeft("building", KeywordLiteral "railroad"), DefinedTypeRight "rail_def";)
        ])
    );

    (
      Definition "building_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "goods_cost", PARAM_LIST (symbol_table_init [
                (CatalogLeft("goods", TYPE_SYMBOL KEYWORD), PARAM_VALUE INT);
            ]));
            (KeywordLiteral "time", PARAM_VALUE INT);
            (KeywordLiteral "visibility", PARAM_VALUE BOOL);
            (KeywordLiteral "onmap", PARAM_VALUE BOOL);
            (KeywordLiteral "province", PARAM_VALUE BOOL);
            (KeywordLiteral "cost", PARAM_VALUE INT);
            (KeywordLiteral "max_level", PARAM_VALUE INT);
            (KeywordLiteral "pop_build_factory", PARAM_VALUE BOOL);
            (KeywordLiteral "one_per_state", PARAM_VALUE BOOL);
        ])
    );

    (
      Definition "fort",
        Inherit ([
            (KeywordLiteral "type", Literal "fort");
            (KeywordLiteral "fort_level", PARAM_VALUE INT);
        ], ["province_modifier"; "building_def"])
    );

    (
      Definition "factory_def",
        APPEND_SYMBOLS ([
            (KeywordLiteral "type", PARAM_VALUE KEYWORD);
            (KeywordLiteral "production_type", DefinedTypeRight "production_type");
            (KeywordLiteral "completion_size", Decimal);
            (KeywordLiteral "on_completion", PARAM_VALUE KEYWORD);
            (KeywordLiteral "default_enabled", PARAM_VALUE BOOL);
            (KeywordLiteral "sail", PARAM_VALUE BOOL);
            (KeywordLiteral "steam", PARAM_VALUE BOOL);
            (KeywordLiteral "strategic_factory", PARAM_VALUE BOOL);
            (KeywordLiteral "advanced_factory", PARAM_VALUE BOOL);
        ], DefinedTypeRight "building_def")
    );

    (
      Definition "naval_base_def",
        Inherit ([
            (KeywordLiteral "type", Literal "naval_base");
            (KeywordLiteral "port", PARAM_VALUE BOOL);
            (KeywordLiteral "capital", PARAM_VALUE BOOL);
            (KeywordLiteral "naval_capacity", PARAM_VALUE INT);
            (KeywordLiteral "colonial_range", PARAM_VALUE INT);
        ], ["province_modifier"; "building_def"])
    );

    (
      Definition "rail_def",
        (Inherit ([
            (KeywordLiteral "type", Literal "railroad");
            (KeywordLiteral "infrastructure", PositiveDecimal);
            (KeywordLiteral "spawn_railroad_track", PARAM_VALUE BOOL);
        ], ["province_modifier"; "building_def"]))
    );

    (
      Definition "cb_types_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "peace_order", VALUE_LIST (PARAM_VALUE KEYWORD));
            (CatalogLeft("cb_type",TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                (KeywordLiteral "sprite_index", PARAM_VALUE INT);
                (KeywordLiteral "is_triggered_only", PARAM_VALUE BOOL);
                (KeywordLiteral "months", PARAM_VALUE INT);
                (KeywordLiteral "crisis", PARAM_VALUE BOOL);
                (KeywordLiteral "construction_speed", PARAM_VALUE FLOAT);
                (KeywordLiteral "badboy_factor", PARAM_VALUE INT);
                (KeywordLiteral "prestige_factor", PARAM_VALUE INT);
                (KeywordLiteral "peace_cost_factor", PARAM_VALUE INT);
                (KeywordLiteral "penalty_factor", PARAM_VALUE INT);
                (KeywordLiteral "always", PARAM_VALUE BOOL);
                (KeywordLiteral "break_truce_prestige_factor", PARAM_VALUE INT);
                (KeywordLiteral "break_truce_infamy_factor", PARAM_VALUE INT);
                (KeywordLiteral "break_truce_militancy_factor", PARAM_VALUE INT);
                (KeywordLiteral "truce_months", PARAM_VALUE INT);
                (KeywordLiteral "good_relation_prestige_factor", PARAM_VALUE INT);
                (KeywordLiteral "good_relation_infamy_factor", PARAM_VALUE INT);
                (KeywordLiteral "good_relation_militancy_factor", PARAM_VALUE INT);
                (KeywordLiteral "can_use", DefinedTypeRight "country_conditions_def");
                (KeywordLiteral "on_add", DefinedTypeRight "country_effects_def");
                (KeywordLiteral "allowed_states", DefinedTypeRight "country_conditions_def");
                (KeywordLiteral "on_po_accepted", DefinedTypeRight "country_effects_def");
                (KeywordLiteral "po_disarmament", PARAM_VALUE BOOL);
                (KeywordLiteral "po_reparations", PARAM_VALUE BOOL);
                (KeywordLiteral "war_name", PARAM_VALUE KEYWORD);
                (KeywordLiteral "po_remove_cores", PARAM_VALUE BOOL);
                (KeywordLiteral "po_transfer_provinces", PARAM_VALUE BOOL);
                (KeywordLiteral "po_demand_state", PARAM_VALUE BOOL);
                (KeywordLiteral "po_add_to_sphere", PARAM_VALUE BOOL);
                (KeywordLiteral "po_remove_prestige", PARAM_VALUE BOOL);
                (KeywordLiteral "po_make_puppet", PARAM_VALUE BOOL);
                (KeywordLiteral "po_release_puppet", PARAM_VALUE BOOL);
                (KeywordLiteral "po_status_quo", PARAM_VALUE BOOL);
                (KeywordLiteral "po_install_communist_gov_type", PARAM_VALUE BOOL);
                (KeywordLiteral "po_uninstall_communist_gov_type", PARAM_VALUE BOOL);
                (KeywordLiteral "po_colony", PARAM_VALUE BOOL);
                (KeywordLiteral "po_destroy_forts", PARAM_VALUE BOOL);
                (KeywordLiteral "po_destroy_naval_bases", PARAM_VALUE BOOL);
                (KeywordLiteral "po_clear_union_sphere", PARAM_VALUE BOOL);
                (KeywordLiteral "po_gunboat", PARAM_VALUE BOOL);
                (KeywordLiteral "po_demand_states", PARAM_VALUE BOOL);
                (KeywordLiteral "po_annex", PARAM_VALUE BOOL);
                (KeywordLiteral "great_war_obligatory", PARAM_VALUE BOOL);
                (KeywordLiteral "mutual", PARAM_VALUE BOOL);
            ]))
        ])
    );

    (
      Definition "countries_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft("country",TYPE_SYMBOL TAG), LINK);
        ])
    );

    (
      Definition "country_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "color", VALUE_LIST (PARAM_VALUE INT));
            (KeywordLiteral "graphical_culture", DefinedTypeRight "graphic_culture");
            (KeywordLiteral "unit_names",PARAM_LIST ( symbol_table_init[
            TYPE_SYMBOL KEYWORD,VALUE_LIST (PARAM_OPTION [
                PARAM_VALUE STRING; 
                PARAM_VALUE KEYWORD             
            ]
            )]));
            (DefinedTypeLeft "government", PARAM_VALUE INT);
        ])
    );

    (
      Definition "graphic_culture_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "color", VALUE_LIST (Catalog_Right ("graphic_culture", PARAM_VALUE KEYWORD)));
        ])
    );

    (
      Definition "party_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (KeywordLiteral "name", PARAM_VALUE STRING);
                (KeywordLiteral "ideology", DefinedTypeRight "ideology");
                (KeywordLiteral "start_date", PARAM_VALUE DATE);
                (KeywordLiteral "end_date", PARAM_VALUE DATE);
                (DefinedTypeLeft "policy_type", DefinedTypeRight "issue");
            ]))
        ])
    );

    (
      Definition "country_colors",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL TAG, PARAM_LIST (symbol_table_init [
                (KeywordLiteral "color", VALUE_LIST (PARAM_VALUE INT));
                (KeywordLiteral "color2", VALUE_LIST (PARAM_VALUE INT));
                (KeywordLiteral "color3", VALUE_LIST (PARAM_VALUE INT));
            ]));
        ])
    );

    (
      Definition "crime",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, APPEND_SYMBOLS([
                (KeywordLiteral "trigger", DefinedTypeRight "pop_conditions_def");
            ], DefinedTypeRight "province_modifiers_def"));
        ])
    );
    (
      Definition "event_modifiers_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, Inherit ([
                (KeywordLiteral "trigger", DefinedTypeRight "country_conditions_def");
                (KeywordLiteral "icon", PARAM_VALUE KEYWORD);
            ], ["country_modifiers_def"; "province_modifiers_def"]))
        ])
    );

    (
      Definition "culture_groups_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (KeywordLiteral "leader", PARAM_VALUE KEYWORD);
                (KeywordLiteral "unit", PARAM_VALUE KEYWORD);
                (KeywordLiteral "union", PARAM_VALUE TAG);
                (CatalogLeft ("culture", TYPE_SYMBOL KEYWORD), DefinedTypeRight "culture_def");
                (KeywordLiteral "radicalism", NUMBER);
                (KeywordLiteral "primary", PARAM_VALUE TAG);
            ]));
        ])
    );

    (
      Definition "culture_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "color", VALUE_LIST (PARAM_VALUE INT));
            (KeywordLiteral "first_names", VALUE_LIST (PARAM_OPTION [PARAM_VALUE STRING; PARAM_VALUE KEYWORD]));
            (KeywordLiteral "last_names", VALUE_LIST (PARAM_OPTION [PARAM_VALUE STRING; PARAM_VALUE KEYWORD]));
            (KeywordLiteral "radicalism", NUMBER);
            (KeywordLiteral "primary", PARAM_VALUE TAG);
        ])
    );

    (
      Definition "goods_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (CatalogLeft("goods", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "cost", PARAM_VALUE FLOAT);
                    (KeywordLiteral "color", VALUE_LIST (PARAM_VALUE INT));
                    (KeywordLiteral "availiable_from_start", PARAM_VALUE BOOL);
                    (KeywordLiteral "oversees_penalty", PARAM_VALUE BOOL);
                    (KeywordLiteral "tradeable", PARAM_VALUE BOOL);
                    (KeywordLiteral "money", PARAM_VALUE BOOL);
                ]));
            ]));
        ])
    );

    (
      Definition "ideologies_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (CatalogLeft("ideology", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "date", PARAM_VALUE DATE);
                    (KeywordLiteral "color", VALUE_LIST (PARAM_VALUE INT));
                    (KeywordLiteral "can_reduce_militancy", PARAM_VALUE BOOL);
                    (KeywordLiteral "remove_political_reform", PARAM_LIST (symbol_table_init [
                        (KeywordLiteral "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KeywordLiteral "group", PARAM_LIST (symbol_table_init [
                            (KeywordLiteral "modifier", APPEND_SYMBOLS ([
                                (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                    (KeywordLiteral "add_political_reform", PARAM_LIST (symbol_table_init [
                        (KeywordLiteral "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KeywordLiteral "group", PARAM_LIST (symbol_table_init [
                            (KeywordLiteral "modifier", APPEND_SYMBOLS ([
                                (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                    (KeywordLiteral "add_social_reform", PARAM_LIST (symbol_table_init [
                        (KeywordLiteral "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KeywordLiteral "group", PARAM_LIST (symbol_table_init [
                            (KeywordLiteral "modifier", APPEND_SYMBOLS ([
                                (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                    (KeywordLiteral "remove_social_reform", PARAM_LIST (symbol_table_init [
                        (KeywordLiteral "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KeywordLiteral "group", PARAM_LIST (symbol_table_init [
                            (KeywordLiteral "modifier", APPEND_SYMBOLS ([
                                (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                ]));
            ]));
        ])
    );

    (
      Definition "issues",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "party_issues", PARAM_LIST (symbol_table_init [
                (CatalogLeft("policy_type", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (CatalogLeft("issue", TYPE_SYMBOL KEYWORD), DefinedTypeRight "issue_def");
                ]));
            ]));
            (KeywordLiteral "political_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
            (KeywordLiteral "social_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
            (KeywordLiteral "military_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
            (KeywordLiteral "economic_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
        ])
    );

    (
      Definition "issue_def",
        (APPEND_SYMBOLS ([
            (KeywordLiteral "rules", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_VALUE BOOL);
            ]));
            (KeywordLiteral "on_execute", PARAM_LIST (symbol_table_init [
                (KeywordLiteral "effect", DefinedTypeRight "country_effects_def");
            ]));
            (KeywordLiteral "max_tariff", NUMBER);
            (KeywordLiteral "min_tariff", NUMBER);
            (KeywordLiteral "max_tax", NUMBER);
            (KeywordLiteral "min_tax", NUMBER);
            (KeywordLiteral "is_jingoism", PARAM_VALUE BOOL);
            (KeywordLiteral "minimum_wage", Decimal);
            (KeywordLiteral "administrative_multiplier", PositiveDecimal);
            (KeywordLiteral "factory_maintenance", NegativeDecimal);
            (KeywordLiteral "pension_level", PositiveDecimal);
            (KeywordLiteral "unemployment_benefit", PositiveDecimal);
        ], DefinedTypeRight "country_modifiers_def"))
    );

    (
      Definition "reform_def",
        APPEND_SYMBOLS ([
            (KeywordLiteral "allow", DefinedTypeRight "country_conditions_def");
        ], DefinedTypeRight "issue")
    );

    (
      Definition "national_focus_group_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft ("national_focus",TYPE_SYMBOL KEYWORD) , PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, APPEND_SYMBOLS ([
                    (KeywordLiteral "icon", PARAM_VALUE INT);
                    (KeywordLiteral "railroads", NUMBER);
                    (KeywordLiteral "uncolonized_province", PARAM_VALUE BOOL);
                    (KeywordLiteral "colonial_validity_check", PARAM_VALUE BOOL);
                    (KeywordLiteral "limit", DefinedTypeRight "province_conditions_def");
                    (KeywordLiteral "own_provinces", PARAM_VALUE BOOL);
                    (KeywordLiteral "has_flashpoint", PARAM_VALUE BOOL);
                    (KeywordLiteral "ideology", DefinedTypeRight "ideology");
                    (KeywordLiteral "loyalty", PARAM_VALUE FLOAT);
                    (KeywordLiteral "flashpoint_tension", PARAM_VALUE FLOAT);
                ], DefinedTypeRight "province_modifiers_def"));
            ]));
        ])
    );

    (
      Definition "nationalvalue",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, DefinedTypeRight "country_modifiers_def");
        ])
    );

    (
      Definition "on_actions_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "on_election_tick", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KeywordLiteral "on_colony_to_state", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KeywordLiteral "on_state_conquest", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KeywordLiteral "on_colony_to_state_free_slave", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KeywordLiteral "on_quarterly_pulse", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"; DefinedTypeRight "province_event_id"]);
            ]));
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"; DefinedTypeRight "province_event_id"]);
            ]));
        ])
    );

    (
      Definition "poptypes_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "promotion_chance", DefinedTypeRight "pop_type");
            (KeywordLiteral "demotion_chance", DefinedTypeRight "pop_type");
            (KeywordLiteral "migration_chance", DefinedTypeRight "pop_type");
            (KeywordLiteral "colonialmigration_chance", DefinedTypeRight "pop_type");
            (KeywordLiteral "emigration_chance", DefinedTypeRight "pop_type");
            (KeywordLiteral "assimilation_chance", DefinedTypeRight "pop_type");
            (KeywordLiteral "conversion_chance", DefinedTypeRight "pop_type");
        ])
    );

    (
      Definition "pop_type_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
            (KeywordLiteral "modifier", APPEND_SYMBOLS ([
                (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
            (KeywordLiteral "group", PARAM_LIST (symbol_table_init [
                (KeywordLiteral "modifier", APPEND_SYMBOLS ([
                    (KeywordLiteral "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
            ]));
        ])
    );

    (
      Definition "production_type_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (KeywordLiteral "efficiency", PARAM_LIST (symbol_table_init [
                    (TYPE_SYMBOL KEYWORD, PARAM_VALUE FLOAT);
                ]));
                (KeywordLiteral "owner", PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "poptype", PARAM_VALUE KEYWORD);
                    (KeywordLiteral "effect", PARAM_VALUE KEYWORD);
                    (KeywordLiteral "effect_multiplier", PARAM_VALUE FLOAT);
                ]));
                (KeywordLiteral "employees", VALUE_LIST (PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "poptype", PARAM_VALUE KEYWORD);
                    (KeywordLiteral "effect", PARAM_VALUE KEYWORD);
                    (KeywordLiteral "amount", PARAM_VALUE FLOAT);
                    (KeywordLiteral "effect_multiplier", PARAM_VALUE FLOAT);
                ])));
                (KeywordLiteral "type", PARAM_VALUE KEYWORD);
                (KeywordLiteral "workforce", PARAM_OPTION [PARAM_VALUE FLOAT; PARAM_VALUE INT]);
                (KeywordLiteral "value", PARAM_VALUE FLOAT);
                (KeywordLiteral "input_goods", PARAM_LIST (symbol_table_init [
                    (TYPE_SYMBOL KEYWORD, NUMBER);
                ]));
                (KeywordLiteral "output_goods", PARAM_VALUE KEYWORD);
                (KeywordLiteral "bonus", PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "type", PARAM_VALUE KEYWORD);
                    (KeywordLiteral "value", NUMBER);
                ]));
                (KeywordLiteral "farm", PARAM_VALUE BOOL);
                (KeywordLiteral "mine", PARAM_VALUE BOOL);
                (KeywordLiteral "is_coastal", PARAM_VALUE BOOL);
                (KeywordLiteral "type", CHOICE_VALUE ["rgo"; "artisan"; "factory"]);
            ]));
        ])
    );
    (Definition "governments_def",PARAM_LIST (symbol_table_init [
        CatalogLeft ("government",TYPE_SYMBOL KEYWORD),PARAM_LIST (symbol_table_init[ 
            DefinedTypeLeft "ideology", PARAM_VALUE BOOL;
            KeywordLiteral "appoint_ruling_party", PARAM_VALUE BOOL;
            KeywordLiteral "flagType", PARAM_VALUE STRING;
            KeywordLiteral "duration", WholeNumber;
        ])
    ])
    );

    (
  Definition "rebel_type_def",
    PARAM_LIST (symbol_table_init [
        CatalogLeft("rebel_type", TYPE_SYMBOL KEYWORD), PARAM_LIST(symbol_table_init [
            (KeywordLiteral "icon", PARAM_VALUE INT);
            (KeywordLiteral "area", PARAM_VALUE KEYWORD);
            (KeywordLiteral "break_alliance_on_win", PARAM_VALUE BOOL);
            (KeywordLiteral "government", PARAM_LIST (symbol_table_init [
                (DefinedTypeLeft "government", DefinedTypeRight "government");
            ]));
            (KeywordLiteral "defection", PARAM_VALUE KEYWORD);
            (KeywordLiteral "independence", PARAM_VALUE KEYWORD);
            (KeywordLiteral "defect_delay", PARAM_VALUE INT);
            (KeywordLiteral "ideology", DefinedTypeRight "ideology");
            (KeywordLiteral "allow_all_cultures", PARAM_VALUE BOOL);
            (KeywordLiteral "allow_all_religions", PARAM_VALUE BOOL);
            (KeywordLiteral "allow_all_ideologies", PARAM_VALUE BOOL);
            (KeywordLiteral "resilient", PARAM_VALUE BOOL);
            (KeywordLiteral "reinforcing", PARAM_VALUE BOOL);
            (KeywordLiteral "general", PARAM_VALUE BOOL);
            (KeywordLiteral "smart", PARAM_VALUE BOOL);
            (KeywordLiteral "unit_transfer", PARAM_VALUE BOOL);
            (KeywordLiteral "occupation_mult", PARAM_VALUE FLOAT);
            (KeywordLiteral "will_rise", PARAM_LIST (symbol_table_init [
                (KeywordLiteral "factor", PARAM_VALUE FLOAT);
                (KeywordLiteral "modifier", APPEND_SYMBOLS([(KeywordLiteral "factor", PARAM_VALUE BOOL)], DefinedTypeRight "country_conditions_def"));
            ]));
            (KeywordLiteral "spawn_chance", PARAM_LIST (symbol_table_init [
                (KeywordLiteral "factor", PARAM_VALUE FLOAT);
                (KeywordLiteral "modifier", APPEND_SYMBOLS([(KeywordLiteral "factor", PARAM_VALUE BOOL)], DefinedTypeRight "pop_conditions_def"));
            ]));
            (KeywordLiteral "movement_evaluation", PARAM_LIST (symbol_table_init [
                (KeywordLiteral "factor", PARAM_VALUE FLOAT);
                (KeywordLiteral "modifier", APPEND_SYMBOLS([(KeywordLiteral "factor", PARAM_VALUE BOOL)], DefinedTypeRight "province_conditions_def"));
            ]));
            (KeywordLiteral "siege_won_trigger", DefinedTypeRight "province_conditions_def");
            (KeywordLiteral "siege_won_effect", DefinedTypeRight "province_effects_def");
            (KeywordLiteral "demands_enforced_trigger", DefinedTypeRight "country_conditions_def");
            (KeywordLiteral "demands_enforced_effect", DefinedTypeRight "country_effects_def");
        ])
    ])
    );
    (
      Definition "religions_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft ("religion",TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                (KeywordLiteral "icon", PARAM_VALUE INT);
                (KeywordLiteral "color", VALUE_LIST (NUMBER));
                (KeywordLiteral "pagan", PARAM_VALUE BOOL);
            ]))
        ])
    );

    (
      Definition "static_modifiers_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, DefinedTypeRight "country_modifiers_def");
        ])
    );

    (
      Definition "technologies_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "schools", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "army_tech_research_bonus", NUMBER);
                    (KeywordLiteral "commerce_tech_research_bonus", NUMBER);
                    (KeywordLiteral "culture_tech_research_bonus", NUMBER);
                    (KeywordLiteral "industry_tech_research_bonus", NUMBER);
                    (KeywordLiteral "navy_tech_research_bonus", NUMBER);
                    (KeywordLiteral "unciv_economic_modifier", NUMBER);
                    (KeywordLiteral "unciv_military_modifier", NUMBER);
                ]));
            ]));
            (KeywordLiteral "folders", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, VALUE_LIST (Catalog_Right ("tech",PARAM_VALUE KEYWORD)));
            ]));
        ])
    );

    (
      Definition "trait_file_def",
        PARAM_LIST (symbol_table_init [
            (KeywordLiteral "personality", PARAM_LIST (symbol_table_init [
                (CatalogLeft("personality", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "attack", NUMBER);
                    (KeywordLiteral "defence", NUMBER);
                    (KeywordLiteral "morale", NUMBER);
                    (KeywordLiteral "organisation", NUMBER);
                    (KeywordLiteral "reconnaissance", NUMBER);
                    (KeywordLiteral "speed", NUMBER);
                    (KeywordLiteral "attrition", NUMBER);
                    (KeywordLiteral "experience", NUMBER);
                    (KeywordLiteral "reliability", NUMBER);
                ]))
            ]));
            (KeywordLiteral "background", PARAM_LIST (symbol_table_init [
                (CatalogLeft("background", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KeywordLiteral "attack", NUMBER);
                    (KeywordLiteral "defence", NUMBER);
                    (KeywordLiteral "morale", NUMBER);
                    (KeywordLiteral "organisation", NUMBER);
                    (KeywordLiteral "reconnaissance", NUMBER);
                    (KeywordLiteral "speed", NUMBER);
                    (KeywordLiteral "attrition", NUMBER);
                    (KeywordLiteral "experience", NUMBER);
                    (KeywordLiteral "reliability", NUMBER);
                ]))
            ]))
        ])
    );

    (
      Definition "triggered_modifiers_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft("trigger_modifiers", TYPE_SYMBOL KEYWORD), APPEND_SYMBOLS ([
                (KeywordLiteral "trigger", DefinedTypeRight "country_conditions_def");
                (KeywordLiteral "icon", DefinedTypeRight "country_modifiers_def");
            ], DefinedTypeRight "country_modifiers_def")
            );
        ])
    );
]
