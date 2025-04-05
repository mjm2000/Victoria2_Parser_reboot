open SymbolTable
open TypeDef

let country_effects = symbol_table_init [
(KeywordLiteral("country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("move_issue_percentage"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("to"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("from"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("value"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
]);
(KeywordLiteral("change_tag"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
]);
(KeywordLiteral("is_slave"),PARAM_VALUE(BOOL));
(KeywordLiteral("scaled_consciousness"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("factor"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT);
    ]);
    ]);
]);
(KeywordLiteral("issue"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("factor"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
]);
(KeywordLiteral("scaled_militancy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("factor"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT);
    ]);
    ]);
]);
(KeywordLiteral("issue"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("factor"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
]);
(KeywordLiteral("world_wars_enabled"),PARAM_VALUE(BOOL));
(KeywordLiteral("dominant_issue"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("value"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("factor"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
]);
(KeywordLiteral("random_pop"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("random_owned"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("any_country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("random_country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("random_state"),DefinedTypeRight "state_effects_def");
(KeywordLiteral("capital"),PARAM_VALUE(INT));
(KeywordLiteral("leadership"),PARAM_VALUE(INT));
(KeywordLiteral("treasury"),PARAM_VALUE(INT));
(KeywordLiteral("small_arms"),PARAM_VALUE(INT));
(KeywordLiteral("cotton"),PARAM_VALUE(INT));
(KeywordLiteral("ammunition"),PARAM_VALUE(INT));
(KeywordLiteral("canned_food"),PARAM_VALUE(INT));
(KeywordLiteral("artillery"),PARAM_VALUE(INT));
(KeywordLiteral("wine"),PARAM_VALUE(INT));
(KeywordLiteral("liquor"),PARAM_VALUE(INT));
(KeywordLiteral("country_event"),PARAM_OPTION(
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("id"),PARAM_VALUE(INT));
    (KeywordLiteral("days"),PARAM_VALUE(INT));
    ]);
));
(KeywordLiteral("change_variable"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("which"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("value"),PARAM_VALUE(INT));
]);
(KeywordLiteral("set_global_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("clr_global_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("activate_technology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("add_accepted_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("remove_accepted_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("add_country_modifier"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("name"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
]);
(KeywordLiteral("duration"),PARAM_VALUE(INT));
]);
(KeywordLiteral("add_country_modifier"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("name"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
]);
(KeywordLiteral("duration"),PARAM_VALUE(INT));
]);
(KeywordLiteral("define_general"),PARAM_LIST( symbol_table_init [
(KeywordLiteral("name"),PARAM_VALUE(STRING));
(KeywordLiteral("personality"),PARAM_VALUE(KEYWORD););
(KeywordLiteral("background"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("define_admiral"),PARAM_LIST( symbol_table_init [
(KeywordLiteral("name"),PARAM_VALUE(STRING));
(KeywordLiteral("personality"),PARAM_VALUE(KEYWORD););
(KeywordLiteral("background"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("kill_leader"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("remove_country_modifier"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("add_crisis_interest"),PARAM_VALUE(BOOL));
(KeywordLiteral("add_crisis_temperature"),PARAM_VALUE(INT));
(KeywordLiteral("badboy"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
(KeywordLiteral("build_factory_in_capital_state"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("in_whole_capital_state"),PARAM_VALUE(BOOL));
    (KeywordLiteral("limit_to_world_greatest_level"),PARAM_VALUE(BOOL););
    ]);
]);
(KeywordLiteral("build_factory_in_capital"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("in_whole_capital_state"),PARAM_VALUE(BOOL));
    (KeywordLiteral("limit_to_world_greatest_level"),PARAM_VALUE(BOOL););
    ]);
]);
(KeywordLiteral("build_railway_in_capital"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("in_whole_capital_state"),PARAM_VALUE(BOOL));
    (KeywordLiteral("limit_to_world_greatest_level"),PARAM_VALUE(BOOL););
    ]);
]);
(KeywordLiteral("capital"),PARAM_VALUE(INT));
(KeywordLiteral("civilized"),PARAM_VALUE(BOOL));
(KeywordLiteral("nationalvalue"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("plurality"),PARAM_VALUE(INT));
(KeywordLiteral("prestige"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
(KeywordLiteral("prestige_factor"),PARAM_VALUE(FLOAT));
(KeywordLiteral("primary_culture"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("value"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("factor"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT);
    ]);
    ]);
]);
(KeywordLiteral("religion"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("slaves"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("research_points"),PARAM_VALUE(INT));
(KeywordLiteral("war_exhaustion"),PARAM_VALUE(INT));
(KeywordLiteral("years_of_research"),PARAM_OPTION([
    PARAM_VALUE(INT);
]);
(KeywordLiteral("nationalize"),PARAM_VALUE(BOOL));
(KeywordLiteral("economic_reform"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("election"),PARAM_VALUE(BOOL));
(KeywordLiteral("government"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("military_reform"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("political_reform"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("ruling_party_ideology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("social_reform"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("upper_house"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("value"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]);
]);
(KeywordLiteral("add_casus_belli"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("target"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
]);
(KeywordLiteral("type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("months"),PARAM_VALUE(INT));
]);
(KeywordLiteral("annex_to"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("casus_belli"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("target"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
]);
(KeywordLiteral("type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("months"),PARAM_VALUE(INT));
(KeywordLiteral("state_province_id"),PARAM_VALUE(INT));
]);
(KeywordLiteral("create_alliance"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("create_vassal"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("diplomatic_influence"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("who"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]);
(KeywordLiteral("value"),PARAM_VALUE(INT));
]);
(KeywordLiteral("end_military_access"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("end_war"),PARAM_OPTION(
    PARAM_LIST(symbol_table_init [
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("inherit"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("leave_alliance"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("military_access"),PARAM_OPTION([
PARAM_VALUE(KEYWORD);
PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("neutrality"),PARAM_VALUE(BOOL));
(KeywordLiteral("relation"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("who"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]);
(KeywordLiteral("value"),PARAM_VALUE(INT));
]);
(KeywordLiteral("release"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("release_vassal"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("war"),PARAM_OPTION([
    PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(SCOPE);
    ]);
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("target"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(SCOPE);
    ]));
    (KeywordLiteral("attacker_goal"),PARAM_LIST(symbol_table_init [
        (KeywordLiteral("casus_belli"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("country"),PARAM_OPTION([
            PARAM_VALUE(KEYWORD);
            PARAM_VALUE(SCOPE);
        ]));
        (KeywordLiteral("state_province_id"),PARAM_VALUE(INT));
    ]));
    (KeywordLiteral("defender_goal"),PARAM_LIST(symbol_table_init [
        (KeywordLiteral("casus_belli"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("country"),PARAM_OPTION([
            PARAM_VALUE(KEYWORD);
            PARAM_VALUE(SCOPE);
        ]));
        (KeywordLiteral("state_province_id"),PARAM_VALUE(INT));
    ]));
    (KeywordLiteral("call_ally"),PARAM_VALUE(BOOL));
    ]));
]);
(KeywordLiteral("province_event"),PARAM_LIST(symbol_table_init [
(KeywordLiteral("id"),PARAM_VALUE(INT));
]));
(KeywordLiteral("add_tax_relative_income"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("treasury"),PARAM_VALUE(INT));
(KeywordLiteral("change_tag_no_core_switch"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("clr_country_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("set_country_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("great_wars_enabled"),PARAM_VALUE(BOOL));
(KeywordLiteral("tag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("any_pop"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("any_owned"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("all_core"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("any_core"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("any_greater_power"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("any_neighbor_country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("any_owned_province"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("any_sphere_member"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("any_state"),DefinedTypeRight "state_effects_def");
(KeywordLiteral("any_substate"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("capital_scope"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("country_tag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("cultural_union"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("overlord"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("region_name"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("sphere_owner"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("war_countries"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("random"),APPEND_SYMBOLS([(KeywordLiteral("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));
(KeywordLiteral("limit"),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("random_list"),PARAM_LIST(symbol_table_init [
    (TYPE_SYMBOL(INT),DefinedTypeRight "country_effects_def")
]));
(KeywordLiteral("set_variable"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("which"),PARAM_VALUE(KEYWORD)); 
    (KeywordLiteral("value"),PARAM_VALUE(INT))
]));
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
(TYPE_SYMBOL(KEYWORD),DefinedTypeRight "country_effects_def");
(TYPE_SYMBOL(SCOPE),DefinedTypeRight "country_effects_def");
(KeywordLiteral("country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("add_crime"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("world_wars_enabled"),PARAM_VALUE(BOOL));
(KeywordLiteral("add_country_modifier"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KeywordLiteral("duration"),PARAM_VALUE(INT));
]));
(KeywordLiteral("relation"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("who"),PARAM_OPTION([
		PARAM_VALUE(KEYWORD);
		PARAM_VALUE(SCOPE)
    ]
    ));
    (KeywordLiteral("value"),PARAM_VALUE(INT));
]));
(KeywordLiteral("any_country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("set_global_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("random_country"),DefinedTypeRight "country_effects_def");
(KeywordLiteral("random_state"),DefinedTypeRight "state_effects_def");
(KeywordLiteral("random_pop"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("clr_country_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("set_country_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("state_scope"),DefinedTypeRight "state_effects_def");
(KeywordLiteral("province_event"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("id"),PARAM_VALUE(INT));
]));
(KeywordLiteral("any_pop"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("aristocrats"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("artisans"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("bureaucrats"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("capitalists"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("clergymen"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("clerks"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("craftsmen"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("farmers"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("labourers"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("officers"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("slaves"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("soldiers"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("poor_strata"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("middle_strata"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("rich_strata"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("random_list"),PARAM_LIST(symbol_table_init [
    (TYPE_SYMBOL(INT),DefinedTypeRight "province_effects_def")
]));
(KeywordLiteral("limit"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("assimilate"),PARAM_VALUE(BOOL));
(KeywordLiteral("add_core"),PARAM_OPTION([
		PARAM_VALUE(KEYWORD);
		PARAM_VALUE(SCOPE)
	]));
(KeywordLiteral("add_province_modifier"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KeywordLiteral("duration"),PARAM_VALUE(INT));
]));
(KeywordLiteral("duration"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("change_controller"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("fort"),PARAM_VALUE(INT));
(KeywordLiteral("infrastructure"),PARAM_VALUE(INT));
(KeywordLiteral("life_rating"),PARAM_VALUE(INT));
(KeywordLiteral("naval_base"),PARAM_VALUE(INT));
(KeywordLiteral("remove_core"),PARAM_OPTION([
		PARAM_VALUE(KEYWORD);
		PARAM_VALUE(SCOPE)
	]));
(KeywordLiteral("remove_province_modifier"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("RGO_size"),PARAM_VALUE(INT));
(KeywordLiteral("secede_province"),PARAM_OPTION([
		PARAM_VALUE(KEYWORD);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("sub_unit"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("type"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("value"),PARAM_VALUE(KEYWORD));
]));
(KeywordLiteral("trade_goods"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("clr_province_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("set_province_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("change_province_name"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(STRING);
]));
(KeywordLiteral("change_variable"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("which"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("value"),PARAM_VALUE(INT));
]));
(KeywordLiteral("owner"),DefinedTypeRight "country_effects_def");
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        PARAM_VALUE(FLOAT);
        PARAM_VALUE(INT);
        PARAM_VALUE(KEYWORD);
        DefinedTypeRight "state_effects_def";
        DefinedTypeRight "pop_effects_def";
        DefinedTypeRight "province_effects_def";
]));
(KeywordLiteral("random"),APPEND_SYMBOLS([(KeywordLiteral("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));
]

let state_effects = symbol_table_init [
(KeywordLiteral("infrastructure"),PARAM_VALUE(INT));
(KeywordLiteral("fort"),PARAM_VALUE(INT));
(KeywordLiteral("remove_core"),PARAM_OPTION([
		PARAM_VALUE(KEYWORD);
		PARAM_VALUE(SCOPE)
	]));
(KeywordLiteral("remove_province_modifier"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("any_pop"),DefinedTypeRight "pop_effects_def");
(KeywordLiteral("change_region_name"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(STRING)
]));
(KeywordLiteral("add_core"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("flashpoint_tension"),PARAM_VALUE(INT));
(KeywordLiteral("is_slave"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_colony"),PARAM_VALUE(BOOL));
(KeywordLiteral("average_militancy"),PARAM_VALUE(INT));
(KeywordLiteral("average_consciousness"),PARAM_VALUE(INT));
(KeywordLiteral("has_pop_type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("any_owned"),DefinedTypeRight "province_effects_def");
(KeywordLiteral("limit"),DefinedTypeRight "state_conditions_def");
(KeywordLiteral("add_province_modifier"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("name"),PARAM_OPTION([
        PARAM_VALUE(KEYWORD);
        PARAM_VALUE(STRING)
    ]);
    );
    (KeywordLiteral("duration"),PARAM_VALUE(INT));
]));
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        DefinedTypeRight "pop_effects_def";
        DefinedTypeRight "province_effects_def";
    ]));

(KeywordLiteral("scaled_consciousness"),PARAM_OPTION([
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("factor"),PARAM_OPTION([
		PARAM_VALUE(FLOAT);
		PARAM_VALUE(INT);
	]));
    ]);
    PARAM_LIST(symbol_table_init [
        (KeywordLiteral("issue"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("factor"),PARAM_OPTION([
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]));
    ]);
]));
(KeywordLiteral("scaled_militancy"),PARAM_OPTION([
    PARAM_LIST(symbol_table_init [
    (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("factor"),PARAM_OPTION([
		PARAM_VALUE(FLOAT);
		PARAM_VALUE(INT);
	]));
    ]);
    PARAM_LIST(symbol_table_init [
        (KeywordLiteral("issue"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("factor"),PARAM_OPTION([
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT);
        ]));
    ]);
]));
(KeywordLiteral("random"),APPEND_SYMBOLS([(KeywordLiteral("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));

]

let pop_effects = symbol_table_init [
    (KeywordLiteral("location"),DefinedTypeRight "province_effects_def");
    (KeywordLiteral("random_list"),PARAM_LIST(symbol_table_init [
        (TYPE_SYMBOL(INT),DefinedTypeRight "pop_effects_def")
    ]));

    (KeywordLiteral("any_pop"),DefinedTypeRight "pop_effects_def");
    (KeywordLiteral("random"),APPEND_SYMBOLS([(KeywordLiteral("chance"),PARAM_VALUE(INT))],DefinedTypeRight "country_effects_def"));
    (KeywordLiteral("assimilate"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("consciousness"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (KeywordLiteral("militancy"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (KeywordLiteral("dominant_issue"),PARAM_LIST(symbol_table_init [
        (KeywordLiteral("value"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
    ]));
    (KeywordLiteral("ideology"),PARAM_LIST(symbol_table_init [
        (KeywordLiteral("value"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
    ]));

    (KeywordLiteral("literacy"),PARAM_VALUE(FLOAT));
    (KeywordLiteral("money"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (KeywordLiteral("move_issue_percentage"),PARAM_LIST(symbol_table_init [
        (KeywordLiteral("from"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("to"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("value"),PARAM_OPTION([
            PARAM_VALUE(FLOAT);
            PARAM_VALUE(INT)
        ]));
    ]));
    (KeywordLiteral("move_pop"),PARAM_VALUE(INT));
    (KeywordLiteral("pop_type"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("reduce_pop"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
    (KeywordLiteral("scaled_consciousness"),PARAM_OPTION([
        PARAM_LIST(symbol_table_init [
        (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
        ]);
        PARAM_LIST(symbol_table_init [
            (KeywordLiteral("issue"),PARAM_VALUE(KEYWORD));
            (KeywordLiteral("factor"),PARAM_OPTION([
                PARAM_VALUE(FLOAT);
                PARAM_VALUE(INT);
            ]));
        ]);
    ]));
    (KeywordLiteral("scaled_militancy"),PARAM_OPTION([
        PARAM_LIST(symbol_table_init [
        (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
        (KeywordLiteral("factor"),PARAM_OPTION([
			PARAM_VALUE(FLOAT);
			PARAM_VALUE(INT);
		]));
        ]);
        PARAM_LIST(symbol_table_init [
            (KeywordLiteral("issue"),PARAM_VALUE(KEYWORD));
            (KeywordLiteral("factor"),PARAM_OPTION([
                PARAM_VALUE(FLOAT);
                PARAM_VALUE(INT);
            ]));
        ]);
    ]));
    (KeywordLiteral("limit"),DefinedTypeRight "pop_conditions_def");
    (KeywordLiteral("pop_type"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("country"),DefinedTypeRight "country_effects_def");

]
