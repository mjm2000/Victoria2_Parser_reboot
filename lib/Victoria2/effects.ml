open SymbolTable
open TypeDef

let country_effects =  symbol_table_init [


(*potential errors*)

(KEYWORD_SYMBOL("country"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("move_issue_percentage"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("to"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("from"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("value"),PARAM_OPTION([
        PARAM_VALUE(FLOAT);
        PARAM_VALUE(INT)
    ]));
]));

(KEYWORD_SYMBOL("change_tag"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(KEYWORD)
]));
(KEYWORD_SYMBOL("is_slave"),PARAM_VALUE(BOOL));
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

(KEYWORD_SYMBOL("world_wars_enabled"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("dominant_issue"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("value"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
    ]));
(KEYWORD_SYMBOL("random_pop"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("random_owned"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("any_country"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("random_country"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("random_state"),DefinedTypeRight "state_effects_def");
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
(KEYWORD_SYMBOL("country_event"),PARAM_OPTION(
    [PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("id"),PARAM_VALUE(INT));
        (KEYWORD_SYMBOL("days"),PARAM_VALUE(INT));
    ]);
    PARAM_VALUE(INT)
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
(KEYWORD_SYMBOL("add_country_modifier"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KEYWORD_SYMBOL("duration"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("define_general"),PARAM_LIST( symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_VALUE(STRING));
    (KEYWORD_SYMBOL("personality"),PARAM_VALUE(KEYWORD););
    (KEYWORD_SYMBOL("background"),PARAM_VALUE(KEYWORD));
]));
(KEYWORD_SYMBOL("define_admiral"),PARAM_LIST( symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_VALUE(STRING));
    (KEYWORD_SYMBOL("personality"),PARAM_VALUE(KEYWORD););
    (KEYWORD_SYMBOL("background"),PARAM_VALUE(KEYWORD));
]));

(KEYWORD_SYMBOL("kill_leader"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("remove_country_modifier"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("add_crisis_interest"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("add_crisis_temperature"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("badboy"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT)
]));
(KEYWORD_SYMBOL("build_factory_in_capital_state"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("in_whole_capital_state"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("limit_to_world_greatest_level"),PARAM_VALUE(BOOL););
    ]);
    ])
);
(KEYWORD_SYMBOL("build_factory_in_capital"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("in_whole_capital_state"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("limit_to_world_greatest_level"),PARAM_VALUE(BOOL););
    ]);
    ])
);
(KEYWORD_SYMBOL("build_railway_in_capital"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("in_whole_capital_state"),PARAM_VALUE(BOOL));
        (KEYWORD_SYMBOL("limit_to_world_greatest_level"),PARAM_VALUE(BOOL););
    ]);
    ])
);
(KEYWORD_SYMBOL("capital"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("civilized"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("nationalvalue"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("plurality"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("prestige"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT)
]));
(KEYWORD_SYMBOL("prestige_factor"),PARAM_VALUE(FLOAT));
(KEYWORD_SYMBOL("primary_culture"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KEYWORD_SYMBOL("religion"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("slaves"),DefinedTypeRight "pop_effects_def");
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
    (KEYWORD_SYMBOL("state_province_id"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("create_alliance"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KEYWORD_SYMBOL("create_vassal"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
    ]));
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
(KEYWORD_SYMBOL("end_war"),PARAM_OPTION(
    [PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE)
]
));
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
(KEYWORD_SYMBOL("war"),PARAM_OPTION([
    PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
    ]);
    PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("target"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
    ]));
    (KEYWORD_SYMBOL("attacker_goal"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("casus_belli"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("country"),PARAM_OPTION([
            PARAM_VALUE(TAG);
            PARAM_VALUE(SCOPE);
        ]));
        (KEYWORD_SYMBOL("state_province_id"),PARAM_VALUE(INT));
    ]));
    (KEYWORD_SYMBOL("defender_goal"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("casus_belli"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("country"),PARAM_OPTION([
            PARAM_VALUE(TAG);
            PARAM_VALUE(SCOPE);
        ]));
        (KEYWORD_SYMBOL("state_province_id"),PARAM_VALUE(INT));
    ]));
    (KEYWORD_SYMBOL("call_ally"),PARAM_VALUE(BOOL));
    ])
]));
(KEYWORD_SYMBOL("province_event"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("id"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("add_tax_relative_income"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT)
]));
(KEYWORD_SYMBOL("treasury"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("change_tag_no_core_switch"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("clr_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("set_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("great_wars_enabled"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("tag"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("any_pop"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("any_owned"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("all_core"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("any_core"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("any_greater_power"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("any_neighbor_country"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("any_owned_province"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("any_sphere_member"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("any_state"),DefinedTypeRight "state_effects_def");
(KEYWORD_SYMBOL("any_substate"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("capital_scope"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("country_tag"),PARAM_VALUE(TAG));
(KEYWORD_SYMBOL("cultural_union"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("overlord"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("region_name"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("sphere_owner"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("war_countries"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("random"),APPEND_SYMBOLS([(KEYWORD_SYMBOL("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));
(KEYWORD_SYMBOL("limit"),DefinedTypeRight "country_conditions_def");
(KEYWORD_SYMBOL("random_list"),PARAM_LIST(symbol_table_init [
    (TYPE_SYMBOL(INT),DefinedTypeRight "country_effects_def")
]));
(KEYWORD_SYMBOL("set_variable"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("which"),PARAM_VALUE(KEYWORD)); 
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT))
]));

(TYPE_SYMBOL(TAG),DefinedTypeRight "country_effects_def");
(TYPE_SYMBOL(SCOPE),DefinedTypeRight "country_effects_def");
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        PARAM_VALUE(FLOAT);
        PARAM_VALUE(INT);
        PARAM_VALUE(KEYWORD);
        DefinedTypeRight "pop_effects_def";
        DefinedTypeRight "province_effects_def";
        DefinedTypeRight "state_effects_def";
    ]));
(TYPE_SYMBOL(INT),DefinedTypeRight "province_effects_def");
]
let province_effects = symbol_table_init [ 
(TYPE_SYMBOL(INT),DefinedTypeRight "province_effects_def");
(TYPE_SYMBOL(TAG),DefinedTypeRight "country_effects_def");
(TYPE_SYMBOL(SCOPE),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("country"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("add_crime"),PARAM_VALUE(KEYWORD));
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
(KEYWORD_SYMBOL("any_country"),DefinedTypeRight "country_effects_def");
(*end errors*)
(KEYWORD_SYMBOL("set_global_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("random_country"),DefinedTypeRight "country_effects_def");
(KEYWORD_SYMBOL("random_state"),DefinedTypeRight "state_effects_def");
(KEYWORD_SYMBOL("random_pop"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("clr_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("set_country_flag"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("state_scope"),DefinedTypeRight "state_effects_def");
(KEYWORD_SYMBOL("province_event"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("id"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("any_pop"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("aristocrats"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("artisans"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("bureaucrats"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("capitalists"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("clergymen"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("clerks"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("craftsmen"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("farmers"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("labourers"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("officers"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("slaves"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("soldiers"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("poor_strata"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("middle_strata"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("rich_strata"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("random_list"),PARAM_LIST(symbol_table_init [
    (TYPE_SYMBOL(INT),DefinedTypeRight "province_effects_def")
]));
(KEYWORD_SYMBOL("limit"),DefinedTypeRight "province_conditions_def");
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
(KEYWORD_SYMBOL("change_controller"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
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
(KEYWORD_SYMBOL("change_province_name"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(STRING);
]));
(KEYWORD_SYMBOL("change_variable"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("which"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("value"),PARAM_VALUE(INT));
]));
(KEYWORD_SYMBOL("owner"),DefinedTypeRight "country_effects_def");
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        PARAM_VALUE(FLOAT);
        PARAM_VALUE(INT);
        PARAM_VALUE(KEYWORD);
        DefinedTypeRight "state_effects_def";
        DefinedTypeRight "pop_effects_def";
        DefinedTypeRight "province_effects_def";
]));
(KEYWORD_SYMBOL("random"),APPEND_SYMBOLS([(KEYWORD_SYMBOL("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));
]

let state_effects = symbol_table_init [
(KEYWORD_SYMBOL("infrastructure"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("fort"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("remove_core"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
	]));
(KEYWORD_SYMBOL("remove_province_modifier"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("any_pop"),DefinedTypeRight "pop_effects_def");
(KEYWORD_SYMBOL("change_region_name"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(STRING)
]));
(KEYWORD_SYMBOL("add_core"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KEYWORD_SYMBOL("flashpoint_tension"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("is_slave"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("is_colony"),PARAM_VALUE(BOOL));
(KEYWORD_SYMBOL("average_militancy"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("average_consciousness"),PARAM_VALUE(INT));
(KEYWORD_SYMBOL("has_pop_type"),PARAM_VALUE(KEYWORD));
(KEYWORD_SYMBOL("any_owned"),DefinedTypeRight "province_effects_def");
(KEYWORD_SYMBOL("limit"),DefinedTypeRight "state_conditions_def");
(KEYWORD_SYMBOL("add_province_modifier"),PARAM_LIST(symbol_table_init [
    (KEYWORD_SYMBOL("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KEYWORD_SYMBOL("duration"),PARAM_VALUE(INT));
]));
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        DefinedTypeRight "pop_effects_def";
        DefinedTypeRight "province_effects_def";
    ]));

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
(KEYWORD_SYMBOL("random"),APPEND_SYMBOLS([(KEYWORD_SYMBOL("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));

]

let pop_effects = symbol_table_init [
    (KEYWORD_SYMBOL("location"),DefinedTypeRight "province_effects_def");
    (KEYWORD_SYMBOL("random_list"),PARAM_LIST(symbol_table_init [
        (TYPE_SYMBOL(INT),DefinedTypeRight "pop_effects_def")
    ]));

    (KEYWORD_SYMBOL("any_pop"),DefinedTypeRight "pop_effects_def");
    (KEYWORD_SYMBOL("random"),APPEND_SYMBOLS([(KEYWORD_SYMBOL("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));
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
    (KEYWORD_SYMBOL("move_issue_percentage"),PARAM_LIST(symbol_table_init [
        (KEYWORD_SYMBOL("from"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("to"),PARAM_VALUE(KEYWORD));
        (KEYWORD_SYMBOL("value"),PARAM_OPTION([
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT)
        ]));
    ]));
    (*move_pop,int*)
    (KEYWORD_SYMBOL("move_pop"),PARAM_VALUE(INT));
    (KEYWORD_SYMBOL("pop_type"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("reduce_pop"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
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
    (KEYWORD_SYMBOL("limit"),DefinedTypeRight "pop_conditions_def");
    (KEYWORD_SYMBOL("pop_type"),PARAM_VALUE(KEYWORD));
    (KEYWORD_SYMBOL("country"),DefinedTypeRight "country_effects_def");

]
