open Symbol_table
open Type_def

let bookmarks = symbol_table_init [
    KEYWORD_SYMBOL "bookmark",PARAM_LIST (symbol_table_init [
        KEYWORD_SYMBOL "name", PARAM_VALUE STRING;
        KEYWORD_SYMBOL "desc", PARAM_VALUE STRING; 
        KEYWORD_SYMBOL "date", PARAM_VALUE KEYWORD;
        KEYWORD_SYMBOL "cameraX", PARAM_VALUE INT;
        KEYWORD_SYMBOL "cameraY", PARAM_VALUE INT;
        ]
    ); 
]



let buildings = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "on_completion", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "completion_size", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "max_level", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "goods_cost", PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_VALUE INT);
        ]
        ));
        (KEYWORD_SYMBOL "time", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "visibility", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "onmap", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "production_type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "pop_build_factory", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "advanced_factory", PARAM_VALUE BOOL);
    ]));
]




let cb_types = symbol_table_init [
    (KEYWORD_SYMBOL "peace_order", VALUE_LIST (PARAM_VALUE KEYWORD));
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
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
        (KEYWORD_SYMBOL "can_use", COUNTRY_CONDITIONS);
        (KEYWORD_SYMBOL "on_add", COUNTRY_EFFECTS);
        (KEYWORD_SYMBOL "allowed_states", COUNTRY_CONDITIONS);
        (KEYWORD_SYMBOL "on_po_accepted", COUNTRY_EFFECTS);
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
        (KEYWORD_SYMBOL "po_install_communist_gov_type",PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_uninstall_communist_gov_type",PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_colony", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_destroy_forts", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_destroy_naval_bases", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_clear_union_sphere", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_gunboat", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_demand_states", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_annex", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_make_puppet", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_release_puppet", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "great_war_obligatory", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "mutual", PARAM_VALUE BOOL);
    ]
    ))]

let countries = symbol_table_init [
    (TYPE_SYMBOL TAG,PARAM_VALUE STRING);
]
let country_colors = symbol_table_init [
    (TYPE_SYMBOL TAG, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
        (KEYWORD_SYMBOL "color2", VALUE_LIST (PARAM_VALUE INT));
        (KEYWORD_SYMBOL "color3", VALUE_LIST (PARAM_VALUE INT));
    ]));
]
let crime = symbol_table_init [
    (TYPE_SYMBOL KEYWORD,APPEND_SYMBOLS([
        (KEYWORD_SYMBOL "trigger", POP_CONDITIONS);
    ],PROVINCE_MODIFIERS));

]

let event_modifiers = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_OPTION [COUNTRY_MODIFIERS;PROVINCE_MODIFIERS]);
]

let goods = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL "cost", PARAM_VALUE FLOAT);
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "availiable_from_start", PARAM_VALUE BOOL);
        ]));
    ]));
]


let ideologies = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL "cost", PARAM_VALUE FLOAT);
            (KEYWORD_SYMBOL "date", PARAM_VALUE DATE);
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "can_reduce_militancy", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "remove_political_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));
            (KEYWORD_SYMBOL "add_political_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));
            (KEYWORD_SYMBOL "add_social_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));
            (KEYWORD_SYMBOL "remove_social_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));


            

        ]));
    ]));
]
(*
{

	trade_policy = {

		protectionism = {
			max_tariff = 1
			min_tariff = -0.25
		}

		free_trade = {
			max_tariff = 0.25
			min_tariff = -1
		}
	}

	economic_policy = {

		laissez_faire = {
			max_tax = 0.75
			factory_owner_cost = 0.25
			factory_output = 0.1
			import_cost = -0.25
			#factory_owner_cost = 5.0

			rules = {
				build_factory = no
				build_bank = yes
				build_university = yes
				expand_factory = no
				open_factory = no
				destroy_factory = no
				build_railway = no
				factory_priority = no
				can_subsidise = no
				pop_build_factory = yes
				pop_expand_factory = yes
				pop_open_factory = yes
				delete_factory_if_no_input = yes
				pop_build_factory_invest = yes
				pop_expand_factory_invest = yes
				open_factory_invest = yes
				allow_foreign_investment = yes
				build_railway_invest = yes
				can_invest_in_pop_projects = no
			}
		}

		interventionism = {
			factory_owner_cost = 0.5
			import_cost = 0.25

			rules = {
				build_factory = no
				build_bank = yes
				build_university = yes
				expand_factory = yes
				open_factory = yes
				destroy_factory = yes
				build_railway = yes
				factory_priority = yes
				can_subsidise = yes
				pop_build_factory = yes
				pop_expand_factory = yes
				pop_open_factory = yes
				delete_factory_if_no_input = no
				pop_build_factory_invest = yes
				pop_expand_factory_invest = yes
				open_factory_invest = yes
				build_factory_invest = yes
				expand_factory_invest = yes
				build_railway_invest = yes
				allow_foreign_investment = yes
				can_invest_in_pop_projects = yes
			}
		}

		state_capitalism = {
			import_cost = 0.5
			min_tax = 0.25
			factory_owner_cost = 0.8

			rules = {
				build_factory = yes
				build_bank = yes
				build_university = yes
				expand_factory = yes
				open_factory = yes
				destroy_factory = yes
				build_railway = yes
				factory_priority = yes
				can_subsidise = yes
				pop_build_factory = yes
				pop_expand_factory = no
				pop_open_factory = yes
				delete_factory_if_no_input = no
				build_factory_invest = yes
				expand_factory_invest = yes
				build_railway_invest = yes
				can_invest_in_pop_projects = yes
			}
		}

		planned_economy = {
			import_cost = 0.75
			min_tax = 0.5
			factory_throughput = 0.25
			factory_owner_cost = 0.8	#-1 negative costs, can't build.

			rules = {
				build_factory = yes
				build_bank = yes
				build_university = yes
				expand_factory = yes
				open_factory = yes
				destroy_factory = yes
				build_railway = yes
				factory_priority = yes
				can_subsidise = yes
				pop_build_factory = yes
				pop_expand_factory = yes
				pop_open_factory = yes
				delete_factory_if_no_input = no
				build_factory_invest = yes
				expand_factory_invest = yes
				build_railway_invest = yes
				can_invest_in_pop_projects = yes
			}
		}
	}

	religious_policy = {

		pro_atheism = {
			#no effect
		}

		secularized = {
			#no effect
		}

		pluralism = {
			#no effect
		}

		moralism = {
			#no effect
		}
	}

	citizenship_policy = { # TODO there was talk of reworking this to increase immigration but reduce assimilation instead

		residency = {
		}

		limited_citizenship = {
			global_assimilation_rate = 0.05
		}

		full_citizenship = {
			global_assimilation_rate = 0.1
		}
	}

	war_policy = {

		jingoism = {
			max_military_spending = 1.0
			supply_consumption = 0.25
			war_exhaustion_effect = 0.5
			is_jingoism = yes
			cb_generation_speed_modifier = 0.3
			mobilization_impact = 4
			org_regain = 0.5
			war_exhaustion = -0.2
			reinforce_speed = 0.5
		}

		pro_military = {
			max_military_spending = 1.0
			war_exhaustion_effect = 0.7
			supply_consumption = 0.1
			cb_generation_speed_modifier = 0.2
			mobilization_impact = 3
			org_regain = 0.25
			war_exhaustion = -0.1
			reinforce_speed = 0.25
		}

		anti_military = {
			max_military_spending = 0.5
			supply_consumption = -0.1
			war_exhaustion_effect = 1.2
			cb_generation_speed_modifier = -0.2
			mobilization_impact = 2
			org_regain = -0.25
			reinforce_speed = -0.25
			assimilation_rate = 0.15
			research_points_modifier = 0.1
			war_exhaustion = 0.1
			global_pop_militancy_modifier = -0.005
		}

		pacifism = {
			max_military_spending = 0.4
			supply_consumption = -0.25
			war_exhaustion_effect = 1.5
			cb_generation_speed_modifier = -0.4
			mobilization_impact = 1
			org_regain = -0.5
			reinforce_speed = -0.5
			assimilation_rate = 0.3
			research_points_modifier = 0.2
			war_exhaustion = 0.2
			global_pop_militancy_modifier = -0.02
		}
	}

	social_policy = {

		no_state_interference = {
			# Audax Validator "." Ignore_NEXT
			max_social_spending = 0.5
		}

		colonial_spending_policy = {
			# Audax Validator "." Ignore_NEXT
			max_social_spending = 0.01
		}

		no_position_set = {
			# Audax Validator "." Ignore_NEXT
			min_social_spending = 0.0
			# Audax Validator "." Ignore_NEXT
			max_social_spending = 1.0
		}

		populist_welfare = {
			# Audax Validator "." Ignore_NEXT
			min_social_spending = 0.15
		}

		welfare_state = {
			# Audax Validator "." Ignore_NEXT
			min_social_spending = 0.33
		}
	}
}




 *)
let issues = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, (APPEND_SYMBOLS ([
                (KEYWORD_SYMBOL "rules",PARAM_LIST (symbol_table_init [
                    (TYPE_SYMBOL KEYWORD, PARAM_VALUE BOOL); 
                ]));
                (KEYWORD_SYMBOL "allow", COUNTRY_CONDITIONS);
                (KEYWORD_SYMBOL "on_execute", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "effect", COUNTRY_EFFECTS);
                ]));

                ],COUNTRY_MODIFIERS);
            ));
        ]));
    ]));
]
let national_focus =  symbol_table_init [
    (TYPE_SYMBOL KEYWORD, (PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD,(APPEND_SYMBOLS ([(KEYWORD_SYMBOL "limit",COUNTRY_CONDITIONS)]),PROVINCE_MODIFIERS)); 
    ])));
     
]
    
