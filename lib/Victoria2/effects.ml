open SymbolTable
open TypeDef

let country_effects =  symbol_table_init [
    (Literal "enable_canal",Number);
(Literal "add_core",Type "land_provid");
(Literal "tech_school",Type "tech_school");
(Type "region",Type "state_effects_def");
(Literal("poor_strata"),Type "pop_effects_def");
(Literal("middle_strata"),Type "pop_effects_def");
(Literal("rich_strata"),Type "pop_effects_def");
(Literal("enable_ideology"), Type "ideology");
(Type "good",Number);
(Literal "money",Number);
(Type "pop_type",Type "pop_effects_def");
(Type "ideology",Value PositiveInt);
(Literal("country"),Type "country_effects_def");
(Literal("move_issue_percentage"),SubTable(symbol_table_init [
    (Literal("to"),Value(Keyword));
    (Literal("from"),Value(Keyword));
    (Literal("value"),Number);
]));
(Literal "militancy",Number);
(Literal "consciousness",Number);
(Type "land_provid",Type "province_effects_def");
(Literal("change_tag"),TypeOption([
        Value(Tag);
        Value(Keyword)
]));
(Literal("is_slave"),Value(Bool));
(Literal("scaled_consciousness"),TypeOption([
    SubTable(symbol_table_init [
    (Literal("ideology"),Value(Keyword));
    (Literal("factor"),Number);
    ]);
    SubTable(symbol_table_init [
        (Literal("issue"),Value(Keyword));
        (Literal("factor"),Number);
    ]);
]));
(Literal("scaled_militancy"),TypeOption([
    SubTable(symbol_table_init [
    (Literal("ideology"),Value(Keyword));
    (Literal("factor"),Number);
    ]);
    SubTable(symbol_table_init [
        (Literal("issue"),Value(Keyword));
        (Literal("factor"),Number);
    ]);
]));

(Literal("world_wars_enabled"),Value(Bool));
(Literal("dominant_issue"),SubTable(symbol_table_init [
        (Literal("value"),Value(Keyword));
        (Literal("factor"),Number);
    ]));
(Literal("random_pop"),Type "pop_effects_def");
(Literal("random_owned"),Type "province_effects_def");
(Literal("any_country"),Type "country_effects_def");
(Literal("random_country"),Type "country_effects_def");
(Literal("random_state"),Type "state_effects_def");
(Literal("capital"),Integer);
(Literal("leadership"),Integer);
(Literal("treasury"),Integer);
(Literal("small_arms"),Integer);
(Literal("cotton"),Integer);
(Literal("ammunition"),Integer);
(Literal("canned_food"),Integer);
(Literal("artillery"),Integer);
(Literal("wine"),Integer);
(Literal("liquor"),Integer);
(Literal("country_event"),TypeOption(
    [SubTable(symbol_table_init [
        (Literal("id"),Integer);
        (Literal("days"),Integer);
    ]);
    Integer
]));

(Literal("change_variable"),SubTable(symbol_table_init [
    (Literal("which"),Value(Keyword));
    (Literal("value"),Integer);
]));

(Literal("set_global_flag"),Value(Keyword));
(Literal("clr_global_flag"),Value(Keyword));
(Literal("activate_technology"),Value(Keyword));
(Literal("add_accepted_culture"),Value(Keyword));
(Literal("remove_accepted_culture"),Value(Keyword));
(Literal("add_country_modifier"),SubTable(symbol_table_init [
    (Literal("name"),TypeOption([
        Value(Keyword);
        Value(String)
    ]);
    );
    (Literal("duration"),Integer);
]));
(Literal("add_country_modifier"),SubTable(symbol_table_init [
    (Literal("name"),TypeOption([
        Value(Keyword);
        Value(String)
    ]);
    );
    (Literal("duration"),Integer);
]));
(Literal("define_general"),SubTable( symbol_table_init [
    (Literal("name"),Value(String));
    (Literal("personality"),Value(Keyword););
    (Literal("background"),Value(Keyword));
]));
(Literal("define_admiral"),SubTable( symbol_table_init [
    (Literal("name"),Value(String));
    (Literal("personality"),Value(Keyword););
    (Literal("background"),Value(Keyword));
]));

(Literal("kill_leader"),Value(Keyword));
(Literal("remove_country_modifier"),Value(Keyword));
(Literal("add_crisis_interest"),Value(Bool));
(Literal("add_crisis_temperature"),Integer);
(Literal("badboy"),Number);
(Literal("build_factory_in_capital_state"),TypeOption([
    Value(Keyword);
    SubTable(symbol_table_init [
        (Literal("in_whole_capital_state"),Value(Bool));
        (Literal("limit_to_world_greatest_level"),Value(Bool););
    ]);
    ])
);
(Literal("build_factory_in_capital"),TypeOption([
    Value(Keyword);
    SubTable(symbol_table_init [
        (Literal("in_whole_capital_state"),Value(Bool));
        (Literal("limit_to_world_greatest_level"),Value(Bool););
    ]);
    ])
);
(Literal("build_railway_in_capital"),TypeOption([
    Value(Keyword);
    SubTable(symbol_table_init [
        (Literal("in_whole_capital_state"),Value(Bool));
        (Literal("limit_to_world_greatest_level"),Value(Bool););
    ]);
    ])
);
(Literal("capital"),Integer);
(Literal("civilized"),Value(Bool));
(Literal("nationalvalue"),Value(Keyword));
(Literal("plurality"),Integer);
(Literal("prestige"),Number);
(Literal("prestige_factor"),Decimal);
(Literal("primary_culture"),TypeOption([
        Value(Keyword);
		Value(Tag);
		Value(Scope)
]));
(Literal("religion"),Value(Keyword));
(Literal("slaves"),Type "pop_effects_def");
(Literal("research_points"),Integer);
(Literal("war_exhaustion"),Number);
(Literal("years_of_research"),Number);
(Literal("nationalize"),Value(Bool));
(Literal("economic_reform"),Value(Keyword));
(Literal("election"),Value(Bool));
(Literal("government"),Value(Keyword));
(Literal("military_reform"),Value(Keyword));
(Literal("political_reform"),Value(Keyword));
(Literal("ruling_party_ideology"),Value(Keyword));
(Literal("social_reform"),Value(Keyword));
(Literal("upper_house"),SubTable(symbol_table_init [
    (Literal("ideology"),Value(Keyword));
    (Literal("value"),Number);
]));
(Literal("add_casus_belli"),SubTable(symbol_table_init [
    (Literal("target"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
    (Literal("type"),Value(Keyword));
    (Literal("months"),Integer);
]));
(Literal("annex_to"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("casus_belli"),SubTable(symbol_table_init [
    (Literal("target"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
    (Literal("type"),Value(Keyword));
    (Literal("months"),Integer);
    (Literal("state_province_id"),Integer);
]));
(Literal("create_alliance"),TypeOption([
        Value(Tag);
        Value(Scope)
]));
(Literal("create_vassal"),TypeOption([
        Value(Tag);
        Value(Scope)
    ]));
(Literal("diplomatic_influence"),SubTable(symbol_table_init [
    (Literal("who"),TypeOption([
        Value(Tag);
        Value(Scope);
    ]);
    );
    (Literal("value"),Integer);
]));
(Literal("end_military_access"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("end_war"),TypeOption(
    [Value(Tag);
    Value(Scope)
]
));
(Literal("inherit"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("leave_alliance"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("military_access"),TypeOption([
Value(Tag);
Value(Scope)
]));
(Literal("neutrality"),Value(Bool));
(Literal("relation"),SubTable(symbol_table_init [
    (Literal("who"),TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal("value"),Integer);
]));
(Literal("release"),Value(Tag));
(Literal("release_vassal"),TypeOption([
        Value(Tag);
        Value(Scope)
    ]));
(Literal("war"),TypeOption([
    Value(Tag);
    SubTable(symbol_table_init [
    (Literal("target"),TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal("attacker_goal"),SubTable(symbol_table_init [
        (Literal("casus_belli"),Value(Keyword));
        (Literal("country"),TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal("state_province_id"),Integer);
    ]));
    (Literal("defender_goal"),SubTable(symbol_table_init [
        (Literal("casus_belli"),Value(Keyword));
        (Literal("country"),TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal("state_province_id"),Integer);
    ]));
    (Literal("call_ally"),Value(Bool));
    ])
]));
(Literal("province_event"),SubTable(symbol_table_init [
    (Literal("id"),Integer);
]));
(Literal("add_tax_relative_income"),Number);
(Literal("treasury"),Integer);
(Literal("change_tag_no_core_switch"),Value(Tag));
(Literal("clr_country_flag"),Value(Keyword));
(Literal("set_country_flag"),Value(Keyword));
(Literal("great_wars_enabled"),Value(Bool));
(Literal("tag"),TypeOption [Value(Tag);Value(Scope)]);
(Literal("any_pop"),Type "pop_effects_def");
(Literal("any_owned"),Type "province_effects_def");
(Literal("all_core"),Type "province_effects_def");
(Literal("any_core"),Type "province_effects_def");
(Literal("any_greater_power"),Type "country_effects_def");
(Literal("any_neighbor_country"),Type "country_effects_def");
(Literal("any_owned_province"),Type "province_effects_def");
(Literal("any_sphere_member"),Type "country_effects_def");
(Literal("any_state"),Type "state_effects_def");
(Literal("any_substate"),Type "country_effects_def");
(Literal("capital_scope"),Type "province_effects_def");
(Literal("country_tag"),Value(Tag));
(Literal("cultural_union"),Type "country_effects_def");
(Literal("overlord"),Type "country_effects_def");
(Literal("region_name"),Value(Keyword));
(Literal("sphere_owner"),Type "country_effects_def");
(Literal("war_countries"),Type "country_effects_def");
(Literal("random"),Inherit([(Literal("chance"),Integer)],[ "country_effects_def"]));
(Literal("limit"),Type "country_conditions_def");
(Literal("random_list"),SubTable(symbol_table_init [
    (Integer,Type "country_effects_def")
]));
(Literal("set_variable"),SubTable(symbol_table_init [
    (Literal("which"),Value(Keyword)); 
    (Literal("value"),Integer)
]));

(Type "country_tag",Type "country_effects_def");
(Value(Scope),Type "country_effects_def");
(Integer,Type "province_effects_def");
]
let province_effects = symbol_table_init [ 
(Literal "party_loyalty",SubTable(symbol_table_init [
    (Literal("ideology"),Type "ideology");
    (Literal("loyalty_value"),Number);
]));
(Integer,Type "province_effects_def");
(Type "country_tag",Type "country_effects_def");
(Value(Scope),Type "country_effects_def");
(Literal("country"),Type "country_effects_def");
(Literal("add_crime"),Value(Keyword));
(*potential errors*)
(Literal("world_wars_enabled"),Value(Bool));
(Literal("add_country_modifier"),SubTable(symbol_table_init [
    (Literal("name"),TypeOption([
        Value(Keyword);
        Value(String)
    ]);
    );
    (Literal("duration"),Integer);
]));
(Literal("relation"),SubTable(symbol_table_init [
    (Literal("who"),TypeOption([
		Value(Tag);
		Value(Scope)
    ]
    ));
    (Literal("value"),Integer);
]));
(Literal("any_country"),Type "country_effects_def");
(*end errors*)
(Literal("set_global_flag"),Value(Keyword));
(Literal("random_country"),Type "country_effects_def");
(Literal("random_state"),Type "state_effects_def");
(Literal("random_pop"),Type "pop_effects_def");
(Literal("clr_country_flag"),Value(Keyword));
(Literal("set_country_flag"),Value(Keyword));
(Literal("state_scope"),Type "state_effects_def");
(Literal("province_event"),SubTable(symbol_table_init [
    (Literal("id"),Integer);
]));
(Literal("any_pop"),Type "pop_effects_def");
(Literal("aristocrats"),Type "pop_effects_def");
(Literal("artisans"),Type "pop_effects_def");
(Literal("bureaucrats"),Type "pop_effects_def");
(Literal("capitalists"),Type "pop_effects_def");
(Literal("clergymen"),Type "pop_effects_def");
(Literal("clerks"),Type "pop_effects_def");
(Literal("craftsmen"),Type "pop_effects_def");
(Literal("farmers"),Type "pop_effects_def");
(Literal("labourers"),Type "pop_effects_def");
(Literal("officers"),Type "pop_effects_def");
(Literal("slaves"),Type "pop_effects_def");
(Literal("soldiers"),Type "pop_effects_def");
(Literal("poor_strata"),Type "pop_effects_def");
(Literal("middle_strata"),Type "pop_effects_def");
(Literal("rich_strata"),Type "pop_effects_def");
(Literal("random_list"),SubTable(symbol_table_init [
    (Integer,Type "province_effects_def")
]));
(Literal("limit"),Type "province_conditions_def");
(Literal("assimilate"),Value(Bool));
(Literal("add_core"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("add_province_modifier"),SubTable(symbol_table_init [
    (Literal("name"),TypeOption([
        Value(Keyword);
        Value(String)
    ]);
    );
    (Literal("duration"),Integer);
]));
(Literal("duration"),Value(Keyword));
(Literal("change_controller"),TypeOption([
        Value(Tag);
        Value(Scope)
]));
(Literal("fort"),Integer);
(Literal("infrastructure"),Integer);
(Literal("life_rating"),Integer);
(Literal("naval_base"),Integer);
(Literal("remove_core"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("remove_province_modifier"),Value(Keyword));
(Literal("RGO_size"),Integer);
(Literal("secede_province"),TypeOption([
		Value(Tag);
		Value(Scope)
]));
(Literal("sub_unit"),SubTable(symbol_table_init [
    (Literal("type"),Value(Keyword));
    (Literal("value"),Value(Keyword));
]));
(Literal("trade_goods"),Value(Keyword));
(Literal("clr_province_flag"),Value(Keyword));
(Literal("set_province_flag"),Value(Keyword));
(Literal("change_province_name"),TypeOption([
    Value(Keyword);
    Value(String);
]));
(Literal("change_variable"),SubTable(symbol_table_init [
    (Literal("which"),Value(Keyword));
    (Literal("value"),Integer);
]));
(Literal("owner"),Type "country_effects_def");
(*
(Value(Keyword),TypeOption([
        Decimal;
        Integer;
        Value(Keyword);
        Type "state_effects_def";
        Type "pop_effects_def";
        Type "province_effects_def";
]));
*)
(Literal("random"),Inherit([(Literal("chance"),Integer)],[ "province_effects_def"]));
]

let state_effects = symbol_table_init [
(literal "owner",Type "country_effects_def");
(Type "land_provid",Type "province_conditions_def");
(Literal("infrastructure"),Integer);
(Literal("fort"),Integer);
(Literal("remove_core"),TypeOption([
		Value(Tag);
		Value(Scope)
	]));
(Literal("remove_province_modifier"),Value(Keyword));
(Literal("any_pop"),Type "pop_effects_def");
(Literal("change_region_name"),TypeOption([
    Value(Keyword);
    Value(String)
]));
(Literal("add_core"),TypeOption([
        Value(Tag);
        Value(Scope)
]));
(Literal "secede_province",TypeOption([
        Type "country_tag";
        Value (Scope);
    ]));
(Literal("flashpoint_tension"),Integer);
(Literal("is_slave"),Value(Bool));
(Literal("is_colony"),Value(Bool));
(Literal("average_militancy"),Integer);
(Literal("average_consciousness"),Integer);
(Literal("has_pop_type"),Value(Keyword));
(Literal("any_owned"),Type "province_effects_def");
(Literal("limit"),Type "state_conditions_def");
(Literal("add_province_modifier"),SubTable(symbol_table_init [
    (Literal("name"),TypeOption([
        Value(Keyword);
        Value(String)
    ]);
    );
    (Literal("duration"),Integer);
]));
(Value(Keyword),TypeOption([
        Type "pop_effects_def";
        Type "province_effects_def";
    ]));

(Literal("scaled_consciousness"),TypeOption([
    SubTable(symbol_table_init [
    (Literal("ideology"),Value(Keyword));
    (Literal("factor"),Number);
    ]);
    SubTable(symbol_table_init [
        (Literal("issue"),Value(Keyword));
        (Literal("factor"),Number);
    ]);
]));
(Literal("scaled_militancy"),TypeOption([
    SubTable(symbol_table_init [
    (Literal("ideology"),Value(Keyword));
    (Literal("factor"),Number);
    ]);
    SubTable(symbol_table_init [
        (Literal("issue"),Value(Keyword));
        (Literal("factor"),Number);
    ]);
]));
(Literal("random"),Inherit([(Literal("chance"),Integer)],[ "country_effects_def"]));

]

let pop_effects = symbol_table_init [
    (Literal("location"),Type "province_effects_def");
    (Literal("random_list"),SubTable(symbol_table_init [
        (Integer,Type "pop_effects_def")
    ]));

    (Literal("any_pop"),Type "pop_effects_def");
    (Literal("random"),Inherit([(Literal("chance"),Integer)],
        ["country_effects_def"]));
    (Literal("assimilate"),Value(Keyword));
    (Literal("consciousness"),Number);
    (Literal("militancy"),Number);
    (Literal("dominant_issue"),SubTable(symbol_table_init [
        (Literal("value"),Value(Keyword));
        (Literal("factor"),Number);
    ]));
    (Literal("ideology"),SubTable(symbol_table_init [
        (Literal("value"),Value(Keyword));
        (Literal("factor"),Number);
    ]));

    (Literal("literacy"),Decimal);
    (Literal("money"),Number);
    (*move issue percent*)
    (Literal("move_issue_percentage"),SubTable(symbol_table_init [
        (Literal("from"),Value(Keyword));
        (Literal("to"),Value(Keyword));
        (Literal("value"),Number);
    ]));
    (*move_pop,int*)
    (Literal("move_pop"),Integer);
    (Literal("pop_type"),Value(Keyword));
    (Literal("reduce_pop"),Number);
    (Literal("scaled_consciousness"),TypeOption([
        SubTable(symbol_table_init [
        (Literal("ideology"),Value(Keyword));
        (Literal("factor"),Number);
        ]);
        SubTable(symbol_table_init [
            (Literal("issue"),Value(Keyword));
            (Literal("factor"),Number);
        ]);
    ]));
    (Literal("scaled_militancy"),TypeOption([
        SubTable(symbol_table_init [
        (Literal("ideology"),Value(Keyword));
        (Literal("factor"),Number);
        ]);
        SubTable(symbol_table_init [
            (Literal("issue"),Value(Keyword));
            (Literal("factor"),Number);
        ]);
    ]));
    (Literal("limit"),Type "pop_conditions_def");
    (Literal("pop_type"),Value(Keyword));
    (Literal("country"),Type "country_effects_def");

]
