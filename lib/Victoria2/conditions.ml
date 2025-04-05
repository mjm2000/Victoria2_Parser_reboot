open SymbolTable
open TypeDef

let pop_conditions = symbol_table_init [
(KeywordLiteral("is_canal_enabled"),PARAM_VALUE(INT));
(KeywordLiteral("agree_with_ruling_party"),PARAM_VALUE(FLOAT));
(KeywordLiteral("cash_reserves"),PARAM_VALUE(INT));
(KeywordLiteral("consciousness"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("everyday_needs"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("continent"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_pop_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_pop_religion"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("is_primary_culture"),PARAM_OPTION([
    PARAM_VALUE(KEYWORD);
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
    PARAM_VALUE(BOOL);
]));
(KeywordLiteral("always"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_accepted_culture"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_culture_group"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
    PARAM_VALUE(KEYWORD)
]));
(KeywordLiteral("is_state_religion"),PARAM_VALUE(BOOL));
(KeywordLiteral("life_needs"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("literacy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("luxury_needs"),PARAM_VALUE(INT));
(KeywordLiteral("militancy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("money"),PARAM_VALUE(INT));
(KeywordLiteral("political_movement"),PARAM_VALUE(BOOL));
(KeywordLiteral("political_reform_want"),PARAM_VALUE(FLOAT));
(KeywordLiteral("pop_majority_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_majority_ideology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_majority_issue"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_majority_religion"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("religion"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("social_movement"),PARAM_VALUE(BOOL));
(KeywordLiteral("social_reform_want"),PARAM_VALUE(FLOAT));
(KeywordLiteral("strata"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("unemployment"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("location"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("country"),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("cultural_union"),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("pop_type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_global_flag"),PARAM_VALUE(KEYWORD));
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
    PARAM_VALUE(INT);
]));
(TYPE_SYMBOL(CONDITION), DefinedTypeRight "pop_conditions_def");
(TYPE_SYMBOL(SCOPE),DefinedTypeRight "country_conditions_def");
]
let country_conditions = symbol_table_init [
(KeywordLiteral("capital_scope"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("always"),PARAM_VALUE(BOOL));
(KeywordLiteral("overlord"),DefinedTypeRight "country_conditions_def");
(*maybe errors*)

(KeywordLiteral("produces"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("world_wars_enabled"),PARAM_VALUE(BOOL));
(TYPE_SYMBOL(SCOPE),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("any_owned_province"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("any_state"),DefinedTypeRight "state_conditions_def");
(KeywordLiteral("any_core"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("all_core"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("any_pop"),DefinedTypeRight "pop_conditions_def");
(KeywordLiteral("controller"),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("sea_zone"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("state_scope"),DefinedTypeRight "state_conditions_def");
(KeywordLiteral("year"),PARAM_VALUE(INT));
(KeywordLiteral("month"),PARAM_VALUE(INT));
(KeywordLiteral("allow_multiple_instances"),PARAM_VALUE(BOOL));
(KeywordLiteral("fire_only_once"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_triggered_only"),PARAM_VALUE(BOOL));
(KeywordLiteral("major"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("immediate"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("check_variable"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("which"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("value"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT);
    ]));
]));
(KeywordLiteral("has_global_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("is_canal_enabled"),PARAM_VALUE(INT));
(KeywordLiteral("administration_spending"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("ai"),PARAM_VALUE(BOOL));
(KeywordLiteral("AI"),PARAM_VALUE(BOOL));
(KeywordLiteral("alliance_with"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("average_consciousness"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("average_militancy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("badboy"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("big_producer"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("blockade"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("brigades_compare"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("can_build_factory_in_capital_state"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("crime_higher_than_education"),PARAM_VALUE(BOOL));
(KeywordLiteral("can_nationalize"),PARAM_VALUE(BOOL));
(KeywordLiteral("can_create_vassals"),PARAM_VALUE(BOOL));
(KeywordLiteral("capital"),PARAM_VALUE(INT));
(KeywordLiteral("casus_belli"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("citizenship_policy"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("civilization_progress"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("civilized"),PARAM_VALUE(BOOL));
(KeywordLiteral("colonial_nation"),PARAM_VALUE(BOOL));
(KeywordLiteral("constructing_cb_progress"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("constructing_cb_type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("controls"),PARAM_VALUE(INT));
(KeywordLiteral("crime_fighting"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("crisis_exist"),PARAM_VALUE(BOOL));
(KeywordLiteral("culture_has_union_tag"),PARAM_VALUE(BOOL));
(KeywordLiteral("diplomatic_influence"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("who"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
    ]));
    (KeywordLiteral("value"),PARAM_VALUE(INT));
]));
(KeywordLiteral("economic_policy"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("economic_reform_name"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("education_spending"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("election"),PARAM_VALUE(BOOL));
(KeywordLiteral("exists"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(BOOL);
]));
(KeywordLiteral("government"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("great_wars_enabled"),PARAM_VALUE(BOOL));
(KeywordLiteral("have_core_in"),PARAM_VALUE(TAG));
(KeywordLiteral("has_country_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_country_modifier"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_cultural_sphere"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_leader"),PARAM_OPTION([
    PARAM_VALUE(STRING);
    PARAM_VALUE(KEYWORD);
]));
(KeywordLiteral("has_recently_lost_war"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_unclaimed_cores"),PARAM_VALUE(BOOL));
(KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("industrial_score"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("in_sphere"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("in_default"),PARAM_OPTION([
    PARAM_VALUE(BOOL);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("invention"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("involved_in_crisis"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_claim_crisis"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_colonial_crisis"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_core"),PARAM_VALUE(INT));
(KeywordLiteral("is_cultural_union"),PARAM_OPTION([
		PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
        PARAM_VALUE(BOOL);
]));
(KeywordLiteral("is_culture_group"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
        PARAM_VALUE(BOOL);
        PARAM_VALUE(KEYWORD)

])
);
(KeywordLiteral("is_disarmed"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_greater_power"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_ideology_enabled"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("is_independant"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_liberation_crisis"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_mobilised"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_next_reform"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("is_our_vassal"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("is_possible_vassal"),PARAM_VALUE(TAG));
(KeywordLiteral("is_secondary_power"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_sphere_leader_of"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("is_vassal"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_substate"),PARAM_VALUE(BOOL));
(KeywordLiteral("literacy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("lost_national"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("middle_strata_everyday_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("middle_strata_life_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("middle_strata_luxury_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("middle_tax"),PARAM_VALUE(INT));
(KeywordLiteral("military_access"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("military_reform_name"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("military_score"),PARAM_OPTION([
        PARAM_VALUE(INT);
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("military_spending"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("money"),PARAM_VALUE(INT));
(KeywordLiteral("nationalvalue"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("national_provinces_occupied"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("neighbour"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("num_of_allies"),PARAM_VALUE(INT));
(KeywordLiteral("num_of_cities"),PARAM_VALUE(INT));
(KeywordLiteral("num_of_ports"),PARAM_VALUE(INT));
(KeywordLiteral("num_of_revolts"),PARAM_VALUE(INT));
(KeywordLiteral("number_of_states"),PARAM_VALUE(INT));
(KeywordLiteral("num_of_substates"),PARAM_VALUE(INT));
(KeywordLiteral("num_of_vassals"),PARAM_VALUE(INT));
(KeywordLiteral("num_of_vassals_no_substates"),PARAM_VALUE(INT));
(KeywordLiteral("owns"),PARAM_VALUE(INT));
(KeywordLiteral("part_of_sphere"),PARAM_VALUE(BOOL));
(KeywordLiteral("political_movement_strength"),PARAM_OPTION ([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("political_reform_name"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("political_reform_want"),PARAM_VALUE(FLOAT));
(KeywordLiteral("poor_strata_everyday_needs"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("poor_strata_life_needs"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("poor_strata_luxury_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("poor_tax"),PARAM_VALUE(INT));
(KeywordLiteral("pop_majority_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_majority_ideology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_majority_religion"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_militancy"),PARAM_VALUE(INT));
(KeywordLiteral("prestige"),PARAM_VALUE(INT));
(KeywordLiteral("primary_culture"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
    PARAM_VALUE(KEYWORD);
]));
(KeywordLiteral("accepted_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("rank"),PARAM_VALUE(INT));
(KeywordLiteral("rebel_power_fraction"),PARAM_VALUE(INT));
(KeywordLiteral("recruited_percentage"),PARAM_VALUE(INT));
(KeywordLiteral("relation"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("who"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
    ]
    ));
    (KeywordLiteral("value"),PARAM_VALUE(INT));
]));
(KeywordLiteral("religious_policy"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("revolt_percentage"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("rich_strata_everyday_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("rich_strata_life_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("rich_strata_luxury_needs"),PARAM_VALUE(FLOAT));
(KeywordLiteral("rich_tax"),PARAM_VALUE(INT));
(KeywordLiteral("ruling_party"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("ruling_party_ideology"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("slavery"),CHOICE_VALUE(["yes_slavery";"no_slavery";"freedom_of_womb"]));
(KeywordLiteral("social_movement_strength"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("social_reform_name"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("social_reform_want"),PARAM_VALUE(FLOAT));
(KeywordLiteral("social_spending"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("stronger_army_than"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("substate_of"),PARAM_OPTION([
		PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
    ];
));
(KeywordLiteral("tag"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("TAG"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("this_culture_union"),PARAM_OPTION([
	PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
    PARAM_VALUE(KEYWORD);
]));
(KeywordLiteral("total_amount_of_divisions"),PARAM_VALUE(INT));
(KeywordLiteral("total_amount_of_ships"),PARAM_VALUE(INT));
(KeywordLiteral("total_defensives"),PARAM_VALUE(INT));
(KeywordLiteral("total_num_of_ports"),PARAM_VALUE(INT));
(KeywordLiteral("total_offensives"),PARAM_VALUE(INT));
(KeywordLiteral("total_of_ours_sunk"),PARAM_VALUE(INT));
(KeywordLiteral("total_sea_battles"),PARAM_VALUE(INT));
(KeywordLiteral("total_sunk_by_us"),PARAM_VALUE(INT));
(KeywordLiteral("trade_policy"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("truce_with"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("unemployment"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("unit_has_leader"),PARAM_VALUE(BOOL));
(KeywordLiteral("unit_in_battle"),PARAM_VALUE(BOOL));
(KeywordLiteral("upper_house"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("ideology"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("value"),PARAM_VALUE(FLOAT));
]));
(KeywordLiteral("vassal_of"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("war"),PARAM_VALUE(BOOL));
(KeywordLiteral("war_exhaustion"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("war_policy"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("war_score"),PARAM_VALUE(INT));
(KeywordLiteral("war_with"),PARAM_OPTION([
        PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE)
]));
(TYPE_SYMBOL(TAG), DefinedTypeRight "country_conditions_def");
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
        PARAM_VALUE(FLOAT);
        PARAM_VALUE(INT);
        PARAM_VALUE(KEYWORD);
]));
(TYPE_SYMBOL(CONDITION),DefinedTypeRight "country_conditions_def");
(TYPE_SYMBOL(INT),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("is_primary_culture"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
    PARAM_VALUE(BOOL);
]));
]

let province_conditions = symbol_table_init [
(TYPE_SYMBOL(SCOPE),DefinedTypeRight "country_conditions_def");
(TYPE_SYMBOL(INT),DefinedTypeRight "province_conditions_def");
(TYPE_SYMBOL(TAG),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("can_build_in_province"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("building"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("limit_to_world_greatest_level"),PARAM_VALUE(BOOL));
]));
(KeywordLiteral("has_global_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("state_scope"),DefinedTypeRight "state_conditions_def");
(KeywordLiteral("any_neighbor_province"),DefinedTypeRight "province_conditions_def");
(KeywordLiteral("owner"),DefinedTypeRight "country_conditions_def");
(KeywordLiteral("any_pop"),DefinedTypeRight "pop_conditions_def");
(KeywordLiteral("year"),PARAM_VALUE(INT));
(KeywordLiteral("month"),PARAM_VALUE(INT));
(KeywordLiteral("allow_multiple_instances"),PARAM_VALUE(BOOL));
(KeywordLiteral("fire_only_once"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_triggered_only"),PARAM_VALUE(BOOL));
(KeywordLiteral("major"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("average_consciousness"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("average_militancy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("can_build_factory"),PARAM_VALUE(BOOL));
(KeywordLiteral("continent"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("controlled_by"),PARAM_OPTION([
		PARAM_VALUE(TAG);
        PARAM_VALUE(SCOPE);
        CHOICE_VALUE(["owner";"sphere_owner";"any_country"]);
	]));
(KeywordLiteral("controlled_by_rebels"),PARAM_VALUE(BOOL));
(KeywordLiteral("country_units_in_province"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("country_units_in_state"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("crime_fighting"),PARAM_OPTION ([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("education_spending"), PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("empty"),PARAM_VALUE(BOOL));
(KeywordLiteral("flashpoint_tension"),PARAM_VALUE(INT));
(KeywordLiteral("has_building"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_crime"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_culture_core"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_empty_adjacent_province"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_empty_adjacent_state"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_factories"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_flashpoint"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_national_minority"),PARAM_VALUE(BOOL));
(KeywordLiteral("has_pop_type"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_province_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_province_modifier"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_recent_imigration"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT);
]));
(KeywordLiteral("is_accepted_culture"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_blockaded"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_capital"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_coastal"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_colonial"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_core"),PARAM_OPTION([
	PARAM_VALUE(TAG);
	PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("is_ideology_enabled"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("is_overseas"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_primary_culture"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
    PARAM_VALUE(BOOL);
]));
(KeywordLiteral("is_state_capital"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_slave"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_state_religion"),PARAM_VALUE(BOOL));
(KeywordLiteral("life_rating"),PARAM_VALUE(INT));
(KeywordLiteral("literacy"),PARAM_OPTION([
    PARAM_VALUE(FLOAT);
    PARAM_VALUE(INT);
]));
(KeywordLiteral("military_spending"),PARAM_VALUE(INT));
(KeywordLiteral("minorities"),PARAM_VALUE(BOOL));
(KeywordLiteral("owned_by"),PARAM_OPTION([
	PARAM_VALUE(TAG);
	PARAM_VALUE(SCOPE)
]));
(KeywordLiteral("pop_militancy"),PARAM_VALUE(INT));
(KeywordLiteral("port"),PARAM_VALUE(BOOL));
(KeywordLiteral("province_control_days"),PARAM_VALUE(INT));
(KeywordLiteral("province_id"),PARAM_VALUE(INT));
(KeywordLiteral("region"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("state_id"),PARAM_VALUE(INT));
(KeywordLiteral("terrain"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("trade_goods"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("total_pops"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT)
]));
(KeywordLiteral("unemployment"),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT)
]));
(KeywordLiteral("unemployment_by_type"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("type"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("value"),PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT)
    ]));
]));
(KeywordLiteral("units_in_province"),PARAM_VALUE(INT));
(KeywordLiteral("work_available"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("worker"),PARAM_VALUE(KEYWORD));
]));
(KeywordLiteral("exists"),PARAM_OPTION([
    PARAM_VALUE(TAG);
    PARAM_VALUE(BOOL);
]));
(TYPE_SYMBOL(TAG),DefinedTypeRight "country_conditions_def");
(TYPE_SYMBOL(CONDITION),  DefinedTypeRight "province_conditions_def");
(TYPE_SYMBOL(KEYWORD),PARAM_OPTION([
    PARAM_VALUE(INT);
    PARAM_VALUE(FLOAT);
    (*error maybe*)
    PARAM_VALUE(KEYWORD);
]));

(KeywordLiteral("has_province_modifier"),PARAM_VALUE(KEYWORD));
(*maybe errors*)
(KeywordLiteral("is_greater_power"),PARAM_VALUE(BOOL));
(KeywordLiteral("civilized"),PARAM_VALUE(BOOL));
(KeywordLiteral("produces"),PARAM_VALUE(KEYWORD));
 (KeywordLiteral("nationalvalue"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("world_wars_enabled"),PARAM_VALUE(BOOL));
(KeywordLiteral("culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_country_flag"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_country_modifier"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("has_pop_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("pop_majority_culture"),PARAM_VALUE(KEYWORD));
(KeywordLiteral("relation"),PARAM_LIST(symbol_table_init [
    (KeywordLiteral("who"),PARAM_OPTION([
		PARAM_VALUE(TAG);
		PARAM_VALUE(SCOPE)
    ]
    ));
    (KeywordLiteral("value"),PARAM_VALUE(INT));
]));
(*pop scope*)
(KeywordLiteral("is_primary_culture"),PARAM_OPTION([
    PARAM_VALUE(BOOL);
    PARAM_VALUE(TAG);
    PARAM_VALUE(SCOPE);
]));
(KeywordLiteral("unit_in_battle"),PARAM_VALUE(BOOL));
(KeywordLiteral("is_canal_enabled"),PARAM_VALUE(INT));
]

let state_conditions = symbol_table_init [
    (KeywordLiteral("continent"),PARAM_VALUE(KEYWORD));

    (KeywordLiteral("is_canal_enabled"),PARAM_VALUE(INT));
    (KeywordLiteral("infrastructure"),PARAM_VALUE(KEYWORD)); 
    (KeywordLiteral("produces"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("is_slave"),PARAM_VALUE(BOOL));
    (KeywordLiteral("is_colonial"),PARAM_VALUE(BOOL));
    (KeywordLiteral("has_flashpoint"),PARAM_VALUE(BOOL));
    (KeywordLiteral("average_militancy"),PARAM_VALUE(INT));
    (KeywordLiteral("average_consciousness"),PARAM_VALUE(INT));
    (KeywordLiteral("remove_province_modifier"),PARAM_VALUE(KEYWORD));
    (KeywordLiteral("any_pop"),DefinedTypeRight "pop_conditions_def");

    (KeywordLiteral("has_pop_type"),PARAM_VALUE(KEYWORD));
    (TYPE_SYMBOL(KEYWORD), PARAM_OPTION([
        PARAM_VALUE(INT);
        PARAM_VALUE(FLOAT);
    ]));
    (TYPE_SYMBOL(CONDITION), DefinedTypeRight "state_conditions_def");
    (KeywordLiteral("any_owned_province"),DefinedTypeRight "province_conditions_def");
    (*error maybe*)
    (KeywordLiteral("has_building"),PARAM_VALUE(KEYWORD));
]
