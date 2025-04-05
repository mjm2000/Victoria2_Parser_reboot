open TypeDef
open SymbolTable

let commons = [
    (
      Definition "bookmarks_def",
        PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "bookmark",
            PARAM_LIST (symbol_table_init [
                KEYWORD_SYMBOL "name", PARAM_VALUE STRING;
                KEYWORD_SYMBOL "desc", PARAM_VALUE STRING;
                KEYWORD_SYMBOL "date", PARAM_VALUE KEYWORD;
                KEYWORD_SYMBOL "cameraX", PARAM_VALUE INT;
                KEYWORD_SYMBOL "cameraY", PARAM_VALUE INT;
            ])
            )
        ])
    );

    (
      Definition "buildings",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft("factory", TYPE_SYMBOL KEYWORD), DefinedTypeRight "factory_def");
            (CatalogLeft("building", KEYWORD_SYMBOL "naval_base"), DefinedTypeRight "naval_base_def");
            (CatalogLeft("building", KEYWORD_SYMBOL "railroad"), DefinedTypeRight "rail_def";)
        ])
    );

    (
      Definition "building_def",
        PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "goods_cost", PARAM_LIST (symbol_table_init [
                (CatalogLeft("goods", TYPE_SYMBOL KEYWORD), PARAM_VALUE INT);
            ]));
            (KEYWORD_SYMBOL "time", PARAM_VALUE INT);
            (KEYWORD_SYMBOL "visibility", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "onmap", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "province", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "cost", PARAM_VALUE INT);
            (KEYWORD_SYMBOL "max_level", PARAM_VALUE INT);
            (KEYWORD_SYMBOL "pop_build_factory", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "one_per_state", PARAM_VALUE BOOL);
        ])
    );

    (
      Definition "fort",
        Inherit ([
            (KEYWORD_SYMBOL "type", Literal "fort");
            (KEYWORD_SYMBOL "fort_level", PARAM_VALUE INT);
        ], ["province_modifier"; "building_def"])
    );

    (
      Definition "factory_def",
        APPEND_SYMBOLS ([
            (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
            (KEYWORD_SYMBOL "production_type", DefinedTypeRight "production_type");
            (KEYWORD_SYMBOL "completion_size", Decimal);
            (KEYWORD_SYMBOL "on_completion", PARAM_VALUE KEYWORD);
            (KEYWORD_SYMBOL "default_enabled", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "sail", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "steam", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "strategic_factory", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "advanced_factory", PARAM_VALUE BOOL);
        ], DefinedTypeRight "building_def")
    );

    (
      Definition "naval_base_def",
        Inherit ([
            (KEYWORD_SYMBOL "type", Literal "naval_base");
            (KEYWORD_SYMBOL "port", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "capital", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "naval_capacity", PARAM_VALUE INT);
            (KEYWORD_SYMBOL "colonial_range", PARAM_VALUE INT);
        ], ["province_modifier"; "building_def"])
    );

    (
      Definition "rail_def",
        (Inherit ([
            (KEYWORD_SYMBOL "type", Literal "railroad");
            (KEYWORD_SYMBOL "infrastructure", PositiveDecimal);
            (KEYWORD_SYMBOL "spawn_railroad_track", PARAM_VALUE BOOL);
        ], ["province_modifier"; "building_def"]))
    );

    (
      Definition "cb_types_def",
        PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "peace_order", VALUE_LIST (PARAM_VALUE KEYWORD));
            (CatalogLeft("cb_type",TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "sprite_index", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "is_triggered_only", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "months", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "crisis", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "construction_speed", PARAM_VALUE FLOAT);
                (KEYWORD_SYMBOL "badboy_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "prestige_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "peace_cost_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "penalty_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "always", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "break_truce_prestige_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "break_truce_infamy_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "break_truce_militancy_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "truce_months", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "good_relation_prestige_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "good_relation_infamy_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "good_relation_militancy_factor", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "can_use", DefinedTypeRight "country_conditions_def");
                (KEYWORD_SYMBOL "on_add", DefinedTypeRight "country_effects_def");
                (KEYWORD_SYMBOL "allowed_states", DefinedTypeRight "country_conditions_def");
                (KEYWORD_SYMBOL "on_po_accepted", DefinedTypeRight "country_effects_def");
                (KEYWORD_SYMBOL "po_disarmament", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_reparations", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "war_name", PARAM_VALUE KEYWORD);
                (KEYWORD_SYMBOL "po_remove_cores", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_transfer_provinces", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_demand_state", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_add_to_sphere", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_remove_prestige", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_make_puppet", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_release_puppet", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_status_quo", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_install_communist_gov_type", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_uninstall_communist_gov_type", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_colony", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_destroy_forts", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_destroy_naval_bases", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_clear_union_sphere", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_gunboat", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_demand_states", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "po_annex", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "great_war_obligatory", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "mutual", PARAM_VALUE BOOL);
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
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "graphical_culture", DefinedTypeRight "graphic_culture");
            (KEYWORD_SYMBOL "unit_names",PARAM_LIST ( symbol_table_init[
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
            (KEYWORD_SYMBOL "color", VALUE_LIST (Catalog_Right ("graphic_culture", PARAM_VALUE KEYWORD)));
        ])
    );

    (
      Definition "party_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "name", PARAM_VALUE STRING);
                (KEYWORD_SYMBOL "ideology", DefinedTypeRight "ideology");
                (KEYWORD_SYMBOL "start_date", PARAM_VALUE DATE);
                (KEYWORD_SYMBOL "end_date", PARAM_VALUE DATE);
                (DefinedTypeLeft "policy_type", DefinedTypeRight "issue");
            ]))
        ])
    );

    (
      Definition "country_colors",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL TAG, PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
                (KEYWORD_SYMBOL "color2", VALUE_LIST (PARAM_VALUE INT));
                (KEYWORD_SYMBOL "color3", VALUE_LIST (PARAM_VALUE INT));
            ]));
        ])
    );

    (
      Definition "crime",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, APPEND_SYMBOLS([
                (KEYWORD_SYMBOL "trigger", DefinedTypeRight "pop_conditions_def");
            ], DefinedTypeRight "province_modifiers_def"));
        ])
    );
    (
      Definition "event_modifiers_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, Inherit ([
                (KEYWORD_SYMBOL "trigger", DefinedTypeRight "country_conditions_def");
                (KEYWORD_SYMBOL "icon", PARAM_VALUE KEYWORD);
            ], ["country_modifiers_def"; "province_modifiers_def"]))
        ])
    );

    (
      Definition "culture_groups_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "leader", PARAM_VALUE KEYWORD);
                (KEYWORD_SYMBOL "unit", PARAM_VALUE KEYWORD);
                (KEYWORD_SYMBOL "union", PARAM_VALUE TAG);
                (CatalogLeft ("culture", TYPE_SYMBOL KEYWORD), DefinedTypeRight "culture_def");
                (KEYWORD_SYMBOL "radicalism", NUMBER);
                (KEYWORD_SYMBOL "primary", PARAM_VALUE TAG);
            ]));
        ])
    );

    (
      Definition "culture_def",
        PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "first_names", VALUE_LIST (PARAM_OPTION [PARAM_VALUE STRING; PARAM_VALUE KEYWORD]));
            (KEYWORD_SYMBOL "last_names", VALUE_LIST (PARAM_OPTION [PARAM_VALUE STRING; PARAM_VALUE KEYWORD]));
            (KEYWORD_SYMBOL "radicalism", NUMBER);
            (KEYWORD_SYMBOL "primary", PARAM_VALUE TAG);
        ])
    );

    (
      Definition "goods_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (CatalogLeft("goods", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "cost", PARAM_VALUE FLOAT);
                    (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
                    (KEYWORD_SYMBOL "availiable_from_start", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "oversees_penalty", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "tradeable", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "money", PARAM_VALUE BOOL);
                ]));
            ]));
        ])
    );

    (
      Definition "ideologies_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (CatalogLeft("ideology", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "date", PARAM_VALUE DATE);
                    (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
                    (KEYWORD_SYMBOL "can_reduce_militancy", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "remove_political_reform", PARAM_LIST (symbol_table_init [
                        (KEYWORD_SYMBOL "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                            (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                                (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                    (KEYWORD_SYMBOL "add_political_reform", PARAM_LIST (symbol_table_init [
                        (KEYWORD_SYMBOL "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                            (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                                (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                    (KEYWORD_SYMBOL "add_social_reform", PARAM_LIST (symbol_table_init [
                        (KEYWORD_SYMBOL "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                            (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                                (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
                        ]));
                    ]));
                    (KEYWORD_SYMBOL "remove_social_reform", PARAM_LIST (symbol_table_init [
                        (KEYWORD_SYMBOL "base", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                        (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                            (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                                (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
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
            (KEYWORD_SYMBOL "party_issues", PARAM_LIST (symbol_table_init [
                (CatalogLeft("policy_type", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (CatalogLeft("issue", TYPE_SYMBOL KEYWORD), DefinedTypeRight "issue_def");
                ]));
            ]));
            (KEYWORD_SYMBOL "political_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
            (KEYWORD_SYMBOL "social_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
            (KEYWORD_SYMBOL "military_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
            (KEYWORD_SYMBOL "economic_reforms", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (CatalogLeft("political_reform", TYPE_SYMBOL KEYWORD), DefinedTypeRight "reform_def");
                ]));
            ]));
        ])
    );

    (
      Definition "issue_def",
        (APPEND_SYMBOLS ([
            (KEYWORD_SYMBOL "rules", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_VALUE BOOL);
            ]));
            (KEYWORD_SYMBOL "on_execute", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "effect", DefinedTypeRight "country_effects_def");
            ]));
            (KEYWORD_SYMBOL "max_tariff", NUMBER);
            (KEYWORD_SYMBOL "min_tariff", NUMBER);
            (KEYWORD_SYMBOL "max_tax", NUMBER);
            (KEYWORD_SYMBOL "min_tax", NUMBER);
            (KEYWORD_SYMBOL "is_jingoism", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "minimum_wage", Decimal);
            (KEYWORD_SYMBOL "administrative_multiplier", PositiveDecimal);
            (KEYWORD_SYMBOL "factory_maintenance", NegativeDecimal);
            (KEYWORD_SYMBOL "pension_level", PositiveDecimal);
            (KEYWORD_SYMBOL "unemployment_benefit", PositiveDecimal);
        ], DefinedTypeRight "country_modifiers_def"))
    );

    (
      Definition "reform_def",
        APPEND_SYMBOLS ([
            (KEYWORD_SYMBOL "allow", DefinedTypeRight "country_conditions_def");
        ], DefinedTypeRight "issue")
    );

    (
      Definition "national_focus_group_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft ("national_focus",TYPE_SYMBOL KEYWORD) , PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, APPEND_SYMBOLS ([
                    (KEYWORD_SYMBOL "icon", PARAM_VALUE INT);
                    (KEYWORD_SYMBOL "railroads", NUMBER);
                    (KEYWORD_SYMBOL "uncolonized_province", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "colonial_validity_check", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "limit", DefinedTypeRight "province_conditions_def");
                    (KEYWORD_SYMBOL "own_provinces", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "has_flashpoint", PARAM_VALUE BOOL);
                    (KEYWORD_SYMBOL "ideology", DefinedTypeRight "ideology");
                    (KEYWORD_SYMBOL "loyalty", PARAM_VALUE FLOAT);
                    (KEYWORD_SYMBOL "flashpoint_tension", PARAM_VALUE FLOAT);
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
            (KEYWORD_SYMBOL "on_election_tick", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KEYWORD_SYMBOL "on_colony_to_state", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KEYWORD_SYMBOL "on_state_conquest", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KEYWORD_SYMBOL "on_colony_to_state_free_slave", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL INT, PARAM_OPTION [DefinedTypeRight "country_event_id"]);
            ]));
            (KEYWORD_SYMBOL "on_quarterly_pulse", PARAM_LIST (symbol_table_init [
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
            (KEYWORD_SYMBOL "promotion_chance", DefinedTypeRight "pop_type");
            (KEYWORD_SYMBOL "demotion_chance", DefinedTypeRight "pop_type");
            (KEYWORD_SYMBOL "migration_chance", DefinedTypeRight "pop_type");
            (KEYWORD_SYMBOL "colonialmigration_chance", DefinedTypeRight "pop_type");
            (KEYWORD_SYMBOL "emigration_chance", DefinedTypeRight "pop_type");
            (KEYWORD_SYMBOL "assimilation_chance", DefinedTypeRight "pop_type");
            (KEYWORD_SYMBOL "conversion_chance", DefinedTypeRight "pop_type");
        ])
    );

    (
      Definition "pop_type_def",
        PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
            (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
            ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
            (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                    (KEYWORD_SYMBOL "factor", PARAM_OPTION ([PARAM_VALUE FLOAT; PARAM_VALUE INT]));
                ], PARAM_OPTION [DefinedTypeRight "pop_conditions_def"; DefinedTypeRight "country_conditions_def"; DefinedTypeRight "province_conditions_def"]));
            ]));
        ])
    );

    (
      Definition "production_type_def",
        PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "efficiency", PARAM_LIST (symbol_table_init [
                    (TYPE_SYMBOL KEYWORD, PARAM_VALUE FLOAT);
                ]));
                (KEYWORD_SYMBOL "owner", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "poptype", PARAM_VALUE KEYWORD);
                    (KEYWORD_SYMBOL "effect", PARAM_VALUE KEYWORD);
                    (KEYWORD_SYMBOL "effect_multiplier", PARAM_VALUE FLOAT);
                ]));
                (KEYWORD_SYMBOL "employees", VALUE_LIST (PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "poptype", PARAM_VALUE KEYWORD);
                    (KEYWORD_SYMBOL "effect", PARAM_VALUE KEYWORD);
                    (KEYWORD_SYMBOL "amount", PARAM_VALUE FLOAT);
                    (KEYWORD_SYMBOL "effect_multiplier", PARAM_VALUE FLOAT);
                ])));
                (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
                (KEYWORD_SYMBOL "workforce", PARAM_OPTION [PARAM_VALUE FLOAT; PARAM_VALUE INT]);
                (KEYWORD_SYMBOL "value", PARAM_VALUE FLOAT);
                (KEYWORD_SYMBOL "input_goods", PARAM_LIST (symbol_table_init [
                    (TYPE_SYMBOL KEYWORD, NUMBER);
                ]));
                (KEYWORD_SYMBOL "output_goods", PARAM_VALUE KEYWORD);
                (KEYWORD_SYMBOL "bonus", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
                    (KEYWORD_SYMBOL "value", NUMBER);
                ]));
                (KEYWORD_SYMBOL "farm", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "mine", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "is_coastal", PARAM_VALUE BOOL);
                (KEYWORD_SYMBOL "type", CHOICE_VALUE ["rgo"; "artisan"; "factory"]);
            ]));
        ])
    );
    (Definition "governments_def",PARAM_LIST (symbol_table_init [
        CatalogLeft ("government",TYPE_SYMBOL KEYWORD),PARAM_LIST (symbol_table_init[ 
            DefinedTypeLeft "ideology", PARAM_VALUE BOOL;
            KEYWORD_SYMBOL "appoint_ruling_party", PARAM_VALUE BOOL;
            KEYWORD_SYMBOL "flagType", PARAM_VALUE STRING;
            KEYWORD_SYMBOL "duration", WholeNumber;
        ])
    ])
    );

    (
  Definition "rebel_type_def",
    PARAM_LIST (symbol_table_init [
        CatalogLeft("rebel_type", TYPE_SYMBOL KEYWORD), PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL "icon", PARAM_VALUE INT);
            (KEYWORD_SYMBOL "area", PARAM_VALUE KEYWORD);
            (KEYWORD_SYMBOL "break_alliance_on_win", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "government", PARAM_LIST (symbol_table_init [
                (DefinedTypeLeft "government", DefinedTypeRight "government");
            ]));
            (KEYWORD_SYMBOL "defection", PARAM_VALUE KEYWORD);
            (KEYWORD_SYMBOL "independence", PARAM_VALUE KEYWORD);
            (KEYWORD_SYMBOL "defect_delay", PARAM_VALUE INT);
            (KEYWORD_SYMBOL "ideology", DefinedTypeRight "ideology");
            (KEYWORD_SYMBOL "allow_all_cultures", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "allow_all_religions", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "allow_all_ideologies", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "resilient", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "reinforcing", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "general", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "smart", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "unit_transfer", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "occupation_mult", PARAM_VALUE FLOAT);
            (KEYWORD_SYMBOL "will_rise", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "factor", PARAM_VALUE FLOAT);
                (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS([(KEYWORD_SYMBOL "factor", PARAM_VALUE BOOL)], DefinedTypeRight "country_conditions_def"));
            ]));
            (KEYWORD_SYMBOL "spawn_chance", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "factor", PARAM_VALUE FLOAT);
                (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS([(KEYWORD_SYMBOL "factor", PARAM_VALUE BOOL)], DefinedTypeRight "pop_conditions_def"));
            ]));
            (KEYWORD_SYMBOL "movement_evaluation", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "factor", PARAM_VALUE FLOAT);
                (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS([(KEYWORD_SYMBOL "factor", PARAM_VALUE BOOL)], DefinedTypeRight "province_conditions_def"));
            ]));
            (KEYWORD_SYMBOL "siege_won_trigger", DefinedTypeRight "province_conditions_def");
            (KEYWORD_SYMBOL "siege_won_effect", DefinedTypeRight "province_effects_def");
            (KEYWORD_SYMBOL "demands_enforced_trigger", DefinedTypeRight "country_conditions_def");
            (KEYWORD_SYMBOL "demands_enforced_effect", DefinedTypeRight "country_effects_def");
        ])
    ])
    );
    (
      Definition "religions_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft ("religion",TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "icon", PARAM_VALUE INT);
                (KEYWORD_SYMBOL "color", VALUE_LIST (NUMBER));
                (KEYWORD_SYMBOL "pagan", PARAM_VALUE BOOL);
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
            (KEYWORD_SYMBOL "schools", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "army_tech_research_bonus", NUMBER);
                    (KEYWORD_SYMBOL "commerce_tech_research_bonus", NUMBER);
                    (KEYWORD_SYMBOL "culture_tech_research_bonus", NUMBER);
                    (KEYWORD_SYMBOL "industry_tech_research_bonus", NUMBER);
                    (KEYWORD_SYMBOL "navy_tech_research_bonus", NUMBER);
                    (KEYWORD_SYMBOL "unciv_economic_modifier", NUMBER);
                    (KEYWORD_SYMBOL "unciv_military_modifier", NUMBER);
                ]));
            ]));
            (KEYWORD_SYMBOL "folders", PARAM_LIST (symbol_table_init [
                (TYPE_SYMBOL KEYWORD, VALUE_LIST (Catalog_Right ("tech",PARAM_VALUE KEYWORD)));
            ]));
        ])
    );

    (
      Definition "trait_file_def",
        PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "personality", PARAM_LIST (symbol_table_init [
                (CatalogLeft("personality", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "attack", NUMBER);
                    (KEYWORD_SYMBOL "defence", NUMBER);
                    (KEYWORD_SYMBOL "morale", NUMBER);
                    (KEYWORD_SYMBOL "organisation", NUMBER);
                    (KEYWORD_SYMBOL "reconnaissance", NUMBER);
                    (KEYWORD_SYMBOL "speed", NUMBER);
                    (KEYWORD_SYMBOL "attrition", NUMBER);
                    (KEYWORD_SYMBOL "experience", NUMBER);
                    (KEYWORD_SYMBOL "reliability", NUMBER);
                ]))
            ]));
            (KEYWORD_SYMBOL "background", PARAM_LIST (symbol_table_init [
                (CatalogLeft("background", TYPE_SYMBOL KEYWORD), PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "attack", NUMBER);
                    (KEYWORD_SYMBOL "defence", NUMBER);
                    (KEYWORD_SYMBOL "morale", NUMBER);
                    (KEYWORD_SYMBOL "organisation", NUMBER);
                    (KEYWORD_SYMBOL "reconnaissance", NUMBER);
                    (KEYWORD_SYMBOL "speed", NUMBER);
                    (KEYWORD_SYMBOL "attrition", NUMBER);
                    (KEYWORD_SYMBOL "experience", NUMBER);
                    (KEYWORD_SYMBOL "reliability", NUMBER);
                ]))
            ]))
        ])
    );

    (
      Definition "triggered_modifiers_def",
        PARAM_LIST (symbol_table_init [
            (CatalogLeft("trigger_modifiers", TYPE_SYMBOL KEYWORD), APPEND_SYMBOLS ([
                (KEYWORD_SYMBOL "trigger", DefinedTypeRight "country_conditions_def");
                (KEYWORD_SYMBOL "icon", DefinedTypeRight "country_modifiers_def");
            ], DefinedTypeRight "country_modifiers_def")
            );
        ])
    );
]
