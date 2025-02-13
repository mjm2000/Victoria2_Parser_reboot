open Symbol_table
open Lexer
open Type_def

let country_effects =  symbol_table_init [
(KEYWORD_SYMBOL("world_wars_enabled"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("dominant_issue"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("value"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
    ]));
(KEYWORD_SYMBOL("random_pop"),POP_EFFECTS);
(KEYWORD_SYMBOL("random_owned"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("any_country"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("random_country"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("random_state"),STATE_EFFECTS);
(KEYWORD_SYMBOL("capital"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("leadership"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("treasury"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("small_arms"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("cotton"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("ammunition"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("canned_food"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("artillery"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("wine"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("liquor"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("country_event"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("id"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("days"),PARAM_VALUE(INT));
]));

(KEYWORD_SYMBOL("change_variable"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("which"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
]));

(KEYWORD_SYMBOL("set_global_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("clr_global_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("activate_technology"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("add_accepted_culture"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("remove_accepted_culture"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("add_country_modifier"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KEYWORD_SYMBOL("duration"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("kill_leader"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("remove_country_modifier"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("add_crisis_interest"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("add_crisis_temperature"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("badboy"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("build_factory_in_capital_state"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("capital"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("civilized"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("nationalvalue"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("plurality"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("prestige"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("prestige_factor"),PARAM_VALUE(FLOAT));
(KEYWORD_SYMBOL("primary_culture"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("religion"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("slaves"),POP_EFFECTS);
(KEYWORD_SYMBOL("research_points"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("war_exhaustion"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("years_of_research"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
]));
(KEYWORD_SYMBOL("nationalize"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("economic_reform"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("election"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("government"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("military_reform"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("political_reform"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("ruling_party_ideology"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("social_reform"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("upper_house"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("ideology"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("value"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
]));
(KEYWORD_SYMBOL("add_casus_belli"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("target"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
    (KEYWORD_SYMBOL("type"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("months"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("annex_to"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("casus_belli"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("target"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
    (KEYWORD_SYMBOL("type"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("months"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("create_alliance"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("create_vassal"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("diplomatic_influence"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("who"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
    ]);
    );
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("end_military_access"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("end_war"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("inherit"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("leave_alliance"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("military_access"),PARAM_OPTION([
PARAM_VALUE(TAG);
PARAM_VALUE(SCOPE)
]));
(KEYWORD_SYMBOL("neutrality"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("relation"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("who"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
    ]));
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("release"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("release_vassal"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
    ]));
(KEYWORD_SYMBOL("war"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("war"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("target"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
    ]));
    (KEYWORD_SYMBOL("attacker_goal"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("casus_belli"),PARAM_VALUE(KEYWORD));
    ]));
    (KEYWORD_SYMBOL("defender_goal"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("casus_belli"),PARAM_VALUE(KEYWORD));
    ]));
    (KEYWORD_SYMBOL("call_ally"),PARAM_VALUE(BOOL));
]));
(KEYWORD_SYMBOL("province_event"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("id"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("add_tax_relative_income"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("treasury"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("change_tag"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("change_tag_no_core_switch"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("clr_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("set_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("great_wars_enabled"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("tag"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("any_pop"),POP_EFFECTS);
(KEYWORD_SYMBOL("any_owned"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("all_core"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("any_core"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("any_greater_power"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("any_neighbor_country"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("any_owned_province"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("any_sphere_member"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("any_state"),STATE_EFFECTS);
(KEYWORD_SYMBOL("any_substate"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("capital_scope"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("country_tag"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("cultural_union"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("overlord"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("region_name"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("sphere_owner"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("war_countries"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("random"),APPEND_SYMBOLS([(KEYWORD_SYMBOL("chance"),PARAM_VALUE(INT))],COUNTRY_EFFECTS));
(KEYWORD_SYMBOL("limit"),COUNTRY_CONDITIONS);
(KEYWORD_SYMBOL("random_list"),PARAM_LIST(symbol_table_init [
    (TYPE_SYMBOL(INT),COUNTRY_EFFECTS)
]));
(KEYWORD_SYMBOL("set_variable"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("which"),PARAM_VALUE(KEYWORD)); 
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT))
]));

(TYPE_SYMBOL(TAG),COUNTRY_EFFECTS);
(TYPE_SYMBOL(SCOPE),COUNTRY_EFFECTS);
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        PARAM_VALUE(FLOAT);
        PARAM_VALUE(INT);
        PARAM_VALUE(KEYWORD);
        POP_EFFECTS;
        PROVINCE_EFFECTS;
    ]));
(TYPE_SYMBOL(INT),PROVINCE_EFFECTS);
]
let province_effects = symbol_table_init [ 
(*potential errors*)
(KEYWORD_SYMBOL("world_wars_enabled"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("add_country_modifier"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KEYWORD_SYMBOL("duration"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("relation"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("who"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
    ]
    ));
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("any_country"),COUNTRY_EFFECTS);
(*end errors*)
(KEYWORD_SYMBOL("set_global_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("random_country"),COUNTRY_EFFECTS);
(KEYWORD_SYMBOL("random_state"),STATE_EFFECTS);
(KEYWORD_SYMBOL("random_pop"),POP_EFFECTS);
(KEYWORD_SYMBOL("clr_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("set_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("state_scope"),STATE_EFFECTS);
(KEYWORD_SYMBOL("province_event"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("id"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("any_pop"),POP_EFFECTS);
(KEYWORD_SYMBOL("aristocrats"),POP_EFFECTS);
(KEYWORD_SYMBOL("artisans"),POP_EFFECTS);
(KEYWORD_SYMBOL("bureaucrats"),POP_EFFECTS);
(KEYWORD_SYMBOL("capitalists"),POP_EFFECTS);
(KEYWORD_SYMBOL("clergymen"),POP_EFFECTS);
(KEYWORD_SYMBOL("clerks"),POP_EFFECTS);
(KEYWORD_SYMBOL("craftsmen"),POP_EFFECTS);
(KEYWORD_SYMBOL("farmers"),POP_EFFECTS);
(KEYWORD_SYMBOL("labourers"),POP_EFFECTS);
(KEYWORD_SYMBOL("officers"),POP_EFFECTS);
(KEYWORD_SYMBOL("slaves"),POP_EFFECTS);
(KEYWORD_SYMBOL("soldiers"),POP_EFFECTS);
(KEYWORD_SYMBOL("poor_strata"),POP_EFFECTS);
(KEYWORD_SYMBOL("middle_strata"),POP_EFFECTS);
(KEYWORD_SYMBOL("rich_strata"),POP_EFFECTS);
(KEYWORD_SYMBOL("random_list"),PARAM_LIST(symbol_table_init [
    (TYPE_SYMBOL(INT),COUNTRY_EFFECTS)
]));
(KEYWORD_SYMBOL("limit"),PROVINCE_CONDITIONS);
(KEYWORD_SYMBOL("assimilate"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("add_core"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("add_province_modifier"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KEYWORD_SYMBOL("duration"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("duration"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("change_controller"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("fort"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("infrastructure"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("life_rating"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("naval_base"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("remove_core"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("remove_province_modifier"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("RGO_size"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("secede_province"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KEYWORD_SYMBOL("sub_unit"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("type"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(KEYWORD));
]));
(KEYWORD_SYMBOL("trade_goods"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("clr_province_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("set_province_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("change_province_name"),PARAM_VALUE(STRING));
(KEYWORD_SYMBOL("change_variable"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("which"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("owner"),COUNTRY_EFFECTS);
]

let state_effects = symbol_table_init [
(KEYWORD_SYMBOL("any_pop"),POP_EFFECTS);
(KEYWORD_SYMBOL("change_region_name"),PARAM_VALUE(STRING));
(KEYWORD_SYMBOL("flashpoint_tension"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("is_slave"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("is_colony"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("average_militancy"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("average_consciousness"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("has_pop_type"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("any_owned"),PROVINCE_EFFECTS);
(KEYWORD_SYMBOL("limit"),STATE_CONDITIONS);
(KEYWORD_SYMBOL("add_province_modifier"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KEYWORD_SYMBOL("duration"),PARAM_VALUE(INT));
]));
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        POP_EFFECTS;
        PROVINCE_EFFECTS;
    ]));
]

let pop_effects = symbol_table_init [
    (KEYWORD_SYMBOL("assimilate"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("consciousness"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (KEYWORD_SYMBOL("militancy"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (KEYWORD_SYMBOL("dominant_issue"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("value"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
    ]));
    (KEYWORD_SYMBOL("ideology"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("value"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
    ]));
    (KEYWORD_SYMBOL("literacy"),PARAM_VALUE(FLOAT));
    (KEYWORD_SYMBOL("money"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (*move issue percent*)
    (KEYWORD_SYMBOL("move_issue_percent"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("from"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("to"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
    ]));
    (*move_pop,int*)
    (KEYWORD_SYMBOL("move_pop"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("pop_type"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("reduce_pop"),PARAM_VALUE(FLOAT));
    (KEYWORD_SYMBOL("scaled_consciousness"),PARAM_OPTION([
        PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("ideology"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
        ]);
        PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL("issue"),PARAM_VALUE(KEYWORD));
            (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
                PARAM_VALUE(FLOAT);
                PARAM_VALUE(INT);
            ]));
        ]);
    ]));
    (KEYWORD_SYMBOL("scaled_militancy"),PARAM_OPTION([
        PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("ideology"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
        ]);
        PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL("issue"),PARAM_VALUE(KEYWORD));
            (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
                PARAM_VALUE(FLOAT);
                PARAM_VALUE(INT);
            ]));
        ]);
    ]));
    (KEYWORD_SYMBOL("limit"),POP_CONDITIONS);
    (KEYWORD_SYMBOL("pop_type"),PARAM_VALUE(KEYWORD));
]
