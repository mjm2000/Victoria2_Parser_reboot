open SymbolTable
open TypeDef

let pop_conditions = symbol_table_init [
(Literal ("is_canal_enabled"),Integer);
(Literal ("agree_with_ruling_party"),TypeOption([
    Decimal;
    Value Bool;
]));
(Literal ("cash_reserves"),Integer);
(Literal ("consciousness"),Number);
(Literal ("culture"),Value(Keyword));
(Literal ("everyday_needs"),Number);
(Literal ("continent"),Value(Keyword));
(Literal ("has_pop_culture"),Value(Keyword));
(Literal ("has_pop_religion"),Value(Keyword));
(Literal ("is_primary_culture"),TypeOption([
    Value(Keyword);
    Value(Tag);
    Value(Scope);
    Value(Bool);
]));
(Literal ("always"),Value(Bool));
(Literal ("is_accepted_culture"),Value(Bool));
(Literal ("is_culture_group"),TypeOption([
    Value(Tag);
    Value(Scope);
    Value(Keyword)
]));
(Literal ("is_state_religion"),Value(Bool));
(Literal ("life_needs"),Number);
(Literal ("literacy"),Number);
(Literal ("luxury_needs"),Integer);
(Literal ("militancy"),Number);
(Literal ("money"),Integer);
(Literal ("political_movement"),Value(Bool));
(Literal ("political_reform_want"),Decimal);
(Literal ("pop_majority_culture"),Value(Keyword));
(Literal ("pop_majority_ideology"),Value(Keyword));
(Literal ("pop_majority_issue"),Value(Keyword));
(Literal ("pop_majority_religion"),Value(Keyword));
(Literal ("religion"),Value(Keyword));
(Literal ("social_movement"),Value(Bool));
(Literal ("social_reform_want"),Decimal);
(Literal ("strata"),Value(Keyword));
(Literal ("type"),Value(Keyword));
(Literal ("unemployment"),Number);
(Literal ("location"),Type "province_conditions_def");
(Literal ("country"),Type "country_conditions_def");
(Literal ("cultural_union"),Type "country_conditions_def");
(Literal ("any_greater_power"),Type "country_conditions_def");

(Literal ("pop_type"),Value(Keyword));
(Literal ("has_global_flag"),Value(Keyword));

(Value(Condition), Type "pop_conditions_def");
(Value(Scope),Type "country_conditions_def");
]
let country_conditions = symbol_table_init [
(Literal "militancy",Number);
(Type "region", Type "state_conditions_def");
(Literal ("poor_strata"),Type "pop_conditions_def");
(Literal ("middle_strata"),Type "pop_conditions_def");
(Literal ("rich_strata"),Type "pop_conditions_def");

(Literal ("plurality"),Number);
(Literal "total_pops",Integer);
(Literal ("any_greater_power"),Type "country_conditions_def");
(Literal ("cultural_union"),Type "country_conditions_def");
(Type "pop_type",Number);
(Literal ("money"),Number);
(Type "good",Integer);
(Literal ("has_pop_type"),Value(Keyword));
(Literal ("owner"),Type "country_conditions_def");
(Type "reform",Value PositiveInt);
(Type "ideology",Value PositiveInt);
(Literal ("any_neighbor_country"),Type "country_conditions_def");
(Literal ("any_owned_country"),Type "country_conditions_def");
(Type "technology",WholeNumber);
(Type "policy_type",(SubType ("policy_type","reform")) );
(Literal ("capital_scope"),Type "province_conditions_def");
(Literal ("always"),Value(Bool));
(Literal ("overlord"),Type "country_conditions_def");
(Literal "tech_school",Type "tech_school");
(Literal ("produces"),Value(Keyword));
(Literal ("world_wars_enabled"),Value(Bool));
(Value(Scope),Type "country_conditions_def");
(Literal ("any_owned_province"),Type "province_conditions_def");
(Literal ("any_state"),Type "state_conditions_def");
(Literal ("any_core"),Type "province_conditions_def");
(Literal ("all_core"),Type "province_conditions_def");
(Literal ("any_pop"),Type "pop_conditions_def");
(Literal ("controller"),Type "country_conditions_def");
(Literal ("sea_zone"),Type "province_conditions_def");
(Literal ("state_scope"),Type "state_conditions_def");
(Literal ("year"),Integer);
(Literal ("month"),Integer);
(Literal ("allow_multiple_instances"),Value(Bool));
(Literal ("fire_only_once"),Value(Bool));
(Literal ("is_triggered_only"),Value(Bool));
(Literal ("major"),Value(Keyword));
(Literal ("exists"),TypeOption[Target;Value(Bool)]);
(Literal ("immediate"),Value(Keyword));
(Literal ("check_variable"),SubTable(symbol_table_init [
    (Literal ("which"),Value(Keyword));
    (Literal ("value"),TypeOption([
        Integer;
        Decimal;
    ]));
]));
(Literal ("has_global_flag"),Value(Keyword));
(Literal ("is_canal_enabled"),Integer);
(Literal ("administration_spending"),Number);
(Literal ("ai"),Value(Bool));
(Literal ("AI"),Value(Bool));
(Literal ("alliance_with"),Target);
(Literal ("average_consciousness"),Number);
(Literal ("average_militancy"),Number);
(Literal ("badboy"),Number);
(Literal ("big_producer"),Value(Keyword));
(Literal ("blockade"),Number);
(Literal ("brigades_compare"),Number);
(Literal ("can_build_factory_in_capital_state"),Value(Keyword));
(Literal ("crime_higher_than_education"),Value(Bool));
(Literal ("can_nationalize"),Value(Bool));
(Literal ("can_create_vassals"),Value(Bool));
(Literal ("capital"),Integer);
(Literal ("casus_belli"),Number);
(Literal ("citizenship_policy"),Value(Keyword));
(Literal ("civilization_progress"),Number);
(Literal ("civilized"),Value(Bool));
(Literal ("colonial_nation"),Value(Bool));
(Literal ("constructing_cb_progress"),Number);
(Literal ("constructing_cb_type"),Value(Keyword));
(Literal ("controls"),Integer);
(Literal ("crime_fighting"),Number);
(Literal ("crisis_exist"),Value(Bool));
(Literal ("culture_has_union_tag"),Value(Bool));
(Literal ("diplomatic_influence"),SubTable(symbol_table_init [
    (Literal ("who"),TypeOption([
		Value(Tag);
		Value(Scope)
    ]));
    (Literal ("value"),Integer);
]));
(Literal ("economic_policy"),Value(Keyword));
(Literal ("economic_reform_name"),Value(Keyword));
(Literal ("education_spending"),Number);
(Literal ("election"),Value(Bool));
(Literal ("government"),Value(Keyword));
(Literal ("great_wars_enabled"),Value(Bool));
(Literal ("have_core_in"),Value(Tag));
(Literal ("has_country_flag"),Value(Keyword));
(Literal ("has_country_modifier"),Value(Keyword));
(Literal ("has_cultural_sphere"),Value(Bool));
(Literal ("has_leader"),Number);
(Literal ("has_recently_lost_war"),Value(Bool));
(Literal ("has_unclaimed_cores"),Value(Bool));
(Literal ("ideology"),Value(Keyword));
(Literal ("industrial_score"),TypeOption([
    Integer;
    Value(Tag);
    Value(Scope)
]));
(Literal ("in_sphere"),Target);
(Literal ("in_default"),TypeOption([
        Value(Tag);
        Value(Scope);
]));
(Literal ("invention"),Value(Keyword));
(Literal ("involved_in_crisis"),Value(Bool));
(Literal ("is_claim_crisis"),Value(Bool));
(Literal ("is_colonial_crisis"),Value(Bool));
(Literal ("is_core"),Integer);
(Literal ("is_cultural_union"),TypeOption([
		Value(Tag);
        Value(Scope);
        Value(Bool);
]));
(Literal ("is_culture_group"),TypeOption([
        Value(Tag);
        Value(Scope);
        Value(Bool);
        Value(Keyword)

])
);
(Literal ("is_disarmed"),Value(Bool));
(Literal ("is_greater_power"),Value(Bool));
(Literal ("is_ideology_enabled"),Value(Keyword));
(Literal ("is_independant"),Value(Bool));
(Literal ("is_liberation_crisis"),Value(Bool));
(Literal ("is_mobilised"),Value(Bool));
(Literal ("is_next_reform"),Value(Keyword));
(Literal ("is_our_vassal"),TypeOption([
        Value(Tag);
        Value(Scope);
]));
(Literal ("is_possible_vassal"),Value(Tag));
(Literal ("is_secondary_power"),Value(Bool));
(Literal ("is_sphere_leader_of"),Target);
(Literal ("is_vassal"),Value(Bool));
(Literal ("is_substate"),Value(Bool));
(Literal ("literacy"),Number);
(Literal ("lost_national"),Number);
(Literal ("middle_strata_everyday_needs"),Decimal);
(Literal ("middle_strata_life_needs"),Decimal);
(Literal ("middle_strata_luxury_needs"),Decimal);
(Literal ("middle_tax"),Integer);
(Literal ("military_access"),Number);
(Literal ("military_reform_name"),Value(Keyword));
(Literal ("military_score"),TypeOption([
        Integer;
		Value(Tag);
		Value(Scope)
]));
(Literal ("military_spending"),Number);
(Literal ("money"),Integer);
(Literal ("nationalvalue"),Value(Keyword));
(Literal ("national_provinces_occupied"),Number);
(Literal ("neighbour"),Target);
(Literal ("num_of_allies"),Integer);
(Literal ("num_of_cities"),Integer);
(Literal ("num_of_ports"),Integer);
(Literal ("num_of_revolts"),Integer);
(Literal ("number_of_states"),Integer);
(Literal ("num_of_substates"),Integer);
(Literal ("num_of_vassals"),Integer);
(Literal ("num_of_vassals_no_substates"),Integer);
(Literal ("owns"),Integer);
(Literal ("part_of_sphere"),Value(Bool));
(Literal ("political_movement_strength"),TypeOption ([
    Decimal;
    Integer;
]));
(Literal ("political_reform_name"),Value(Keyword));
(Literal ("political_reform_want"),Decimal);
(Literal ("poor_strata_everyday_needs"),Number);
(Literal ("poor_strata_life_needs"),Number);
(Literal ("poor_strata_luxury_needs"),Decimal);
(Literal ("poor_tax"),Integer);
(Literal ("pop_majority_culture"),Value(Keyword));
(Literal ("pop_majority_ideology"),Value(Keyword));
(Literal ("pop_majority_religion"),Value(Keyword));
(Literal ("pop_militancy"),Integer);
(Literal ("prestige"),Integer);
(Literal ("primary_culture"),TypeOption([
    Value(Tag);
    Value(Scope);
    Value(Keyword);
]));
(Literal ("accepted_culture"),Value(Keyword));
(Literal ("rank"),Integer);
(Literal ("rebel_power_fraction"),Integer);
(Literal ("recruited_percentage"),Integer);
(Literal ("relation"),SubTable(symbol_table_init [
    (Literal ("who"),TypeOption([
		Value(Tag);
		Value(Scope)
    ]
    ));
    (Literal ("value"),Integer);
]));
(Literal ("religious_policy"),Value(Keyword));
(Literal ("revolt_percentage"),Number);
(Literal ("rich_strata_everyday_needs"),Decimal);
(Literal ("rich_strata_life_needs"),Decimal);
(Literal ("rich_strata_luxury_needs"),Decimal);
(Literal ("rich_tax"),Integer);
(Literal ("ruling_party"),Value(Keyword));
(Literal ("ruling_party_ideology"),Value(Keyword));
(Literal ("slavery"),TypeOption(
    [Literal "yes_slavery";Literal "no_slavery"; Literal"freedom_of_womb"]));
(Literal ("social_movement_strength"),Value(Keyword));
(Literal ("social_reform_name"),Value(Keyword));
(Literal ("social_reform_want"),Decimal);
(Literal ("social_spending"),Number);
(Literal ("stronger_army_than"),Number);
(Literal ("substate_of"),TypeOption([
		Value(Tag);
        Value(Scope);
    ];
));

(Literal("tag"),TypeOption [Value(Tag);Value(Scope)]);

(Literal ("Tag"),Value(Tag));
(Literal ("this_culture_union"),TypeOption([
	Value(Tag);
    Value(Scope);
    Value(Keyword);
]));
(Literal ("total_amount_of_divisions"),Integer);
(Literal ("total_amount_of_ships"),Integer);
(Literal ("total_defensives"),Integer);
(Literal ("total_num_of_ports"),Integer);
(Literal ("total_offensives"),Integer);
(Literal ("total_of_ours_sunk"),Integer);
(Literal ("total_sea_battles"),Integer);
(Literal ("total_sunk_by_us"),Integer);
(Literal ("trade_policy"),Value(Keyword));
(Literal ("truce_with"),Target);
(Literal ("unemployment"),Number);
(Literal ("unit_has_leader"),Value(Bool));
(Literal ("unit_in_battle"),Value(Bool));
(Literal ("upper_house"),SubTable(symbol_table_init [
    (Literal ("ideology"),Value(Keyword));
    (Literal ("value"),Decimal);
]));
(Literal ("vassal_of"),TypeOption([
        Value(Tag);
        Value(Scope);
    ]
));
(Literal ("war"),Value(Bool));
(Literal ("war_exhaustion"),Number);
(Literal ("war_policy"),Value(Keyword));
(Literal ("war_score"),Integer);
(Literal ("war_with"),Target);
(Value(Tag), Type "country_conditions_def");
(Value(Condition),Type "country_conditions_def");
(Integer,Type "province_conditions_def");
(Literal ("is_primary_culture"),TypeOption([
    Value(Tag);
    Value(Scope);
    Value(Bool);
]));
]

let province_conditions = symbol_table_init [
(Literal ("poor_strata"),Type "pop_conditions_def");
(Literal ("middle_strata"),Type "pop_conditions_def");
(Literal ("rich_strata"),Type "pop_conditions_def");
(Value(Scope),Type "country_conditions_def");
(Type "land_provid",Type "province_conditions_def");
(Value(Tag),Type "country_conditions_def");
(Literal ("can_build_in_province"),SubTable(symbol_table_init [
    (Literal ("building"),Value(Keyword));
    (Literal ("limit_to_world_greatest_level"),Value(Bool));
]));
(Literal ("has_global_flag"),Value(Keyword));
(Literal ("state_scope"),Type "state_conditions_def");
(Literal ("any_neighbor_province"),Type "province_conditions_def");
(Literal ("owner"),Type "country_conditions_def");
(Literal ("any_pop"),Type "pop_conditions_def");
(Literal ("year"),Integer);
(Literal ("month"),Integer);
(Literal ("allow_multiple_instances"),Value(Bool));
(Literal ("fire_only_once"),Value(Bool));
(Literal ("is_triggered_only"),Value(Bool));
(Literal ("major"),Value(Keyword));
(Literal ("average_consciousness"),Number);
(Literal ("average_militancy"),Number);
(Literal ("can_build_factory"),Value(Bool));
(Literal ("continent"),Value(Keyword));
(Literal ("controlled_by"),TypeOption([
		Value(Tag);
        Value(Scope);
        Literal "owner";
        Literal "sphere_owner";
        Literal "any_country"
        ]
	));
(Literal ("controlled_by_rebels"),Value(Bool));
(Literal ("country_units_in_province"),Target);
(Literal ("country_units_in_state"),Target);
(Literal ("crime_fighting"),Number);
(Literal ("education_spending"), Number);
(Literal ("empty"),Value(Bool));
(Literal ("flashpoint_tension"),Integer);
(Literal ("has_building"),Value(Keyword));
(Literal ("has_crime"),Value(Keyword));
(Literal ("has_culture_core"),Value(Bool));
(Literal ("has_empty_adjacent_province"),Value(Bool));
(Literal ("has_empty_adjacent_state"),Value(Bool));
(Literal ("has_factories"),Value(Bool));
(Literal ("has_flashpoint"),Value(Bool));
(Literal ("has_national_minority"),Value(Bool));
(Literal ("has_pop_type"),Value(Keyword));
(Literal ("has_province_flag"),Value(Keyword));
(Literal ("has_province_modifier"),Value(Keyword));
(Literal ("has_recent_imigration"),Number);
(Literal ("is_accepted_culture"),Value(Bool));
(Literal ("is_blockaded"),Value(Bool));
(Literal ("is_capital"),Value(Bool));
(Literal ("is_coastal"),Value(Bool));
(Literal ("is_colonial"),Value(Bool));
(Literal ("is_core"),Value Tag);
(Literal ("is_ideology_enabled"),Value(Keyword));
(Literal ("is_overseas"),Value(Bool));
(Literal ("is_primary_culture"),TypeOption([
    Value(Tag);
    Value(Scope);
    Value(Bool);
]));
(Literal ("is_state_capital"),Value(Bool));
(Literal ("is_slave"),Value(Bool));
(Literal ("is_state_religion"),Value(Bool));
(Literal ("life_rating"),Integer);
(Literal ("literacy"),Number);
(Literal ("military_spending"),Integer);
(Literal ("minorities"),Value(Bool));
(Literal ("owned_by"),TypeOption([
    Value(Tag);
    Value(Scope);
    Literal "owner";
    Literal "sphere_owner";
    Literal "any_country"
]));
(Literal ("pop_militancy"),Integer);
(Literal ("port"),Value(Bool));
(Literal ("province_control_days"),Integer);
(Literal ("province_id"),Integer);
(Literal ("region"),Value(Keyword));
(Literal ("state_id"),Integer);
(Literal ("terrain"),Value(Keyword));
(Literal ("trade_goods"),Value(Keyword));
(Literal ("total_pops"),Number);
(Literal ("unemployment"),Number);
(Literal ("unemployment_by_type"),SubTable(symbol_table_init [
    (Literal ("type"),Value(Keyword));
    (Literal ("value"),Number);
]));
(Literal ("units_in_province"),Integer);
(Literal ("work_available"),SubTable(symbol_table_init [
    (Literal ("worker"),Value(Keyword));
]));
(Literal ("exists"),Target);
(Value(Tag),Type "country_conditions_def");
(Value(Condition),  Type "province_conditions_def");
(Value(Keyword),TypeOption([
    Number;
    Value(Keyword);
]));

(Literal ("has_province_modifier"),Value(Keyword));
(*maybe errors*)
(Literal ("is_greater_power"),Value(Bool));
(Literal ("civilized"),Value(Bool));
(Literal ("produces"),Value(Keyword));
 (Literal ("nationalvalue"),Value(Keyword));
(Literal ("world_wars_enabled"),Value(Bool));
(Literal ("culture"),Value(Keyword));
(Literal ("has_country_flag"),Value(Keyword));
(Literal ("has_country_modifier"),Value(Keyword));
(Literal ("has_pop_culture"),Value(Keyword));
(Literal ("pop_majority_culture"),Value(Keyword));
(Literal ("relation"),SubTable(symbol_table_init [
    (Literal ("who"),TypeOption([
		Value(Tag);
		Value(Scope)
    ]
    ));
    (Literal ("value"),Integer);
]));
(*pop scope*)
(Literal ("is_primary_culture"),TypeOption([
    Value(Bool);
    Value(Tag);
    Value(Scope);
]));
(Literal ("unit_in_battle"),Value(Bool));
(Literal ("is_canal_enabled"),Integer);
]

let state_conditions = symbol_table_init [
    (Literal ("continent"),Value(Keyword));

    (Literal ("is_canal_enabled"),Integer);
    (Literal ("infrastructure"),Value(Keyword)); 
    (Literal ("produces"),Value(Keyword));
    (Literal ("is_slave"),Value(Bool));
    (Literal ("is_colonial"),Value(Bool));
    (Literal ("has_flashpoint"),Value(Bool));
    (Literal ("average_militancy"),Integer);
    (Literal ("average_consciousness"),Integer);
    (Literal ("remove_province_modifier"),Value(Keyword));
    (Literal "owned_by",TypeOption([
        Target;
    ]));
    (Literal ("any_pop"),Type "pop_conditions_def");

    (Literal ("has_pop_type"),Value(Keyword));
    (Value(Keyword), Number);
    (Value(Condition), Type "state_conditions_def");
    (Literal ("any_owned_province"),Type "province_conditions_def");
    (*error maybe*)
    (Literal ("has_building"),Value(Keyword));
]
