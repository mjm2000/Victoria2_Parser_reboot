open SymbolTable
open TypeDef

let country_conditions = symbol_table_init [
    (Literal ("country"), Type "country_conditions_def");
    (Literal ("always"), Value(Bool));
    (Literal ("year"), Integer);
    (Literal ("month"), Integer);
    (Literal ("day"), Integer);
    (Literal ("check_variable"), SubTable(symbol_table_init [
        (Literal ("which"), Value(Keyword));
        (Literal ("value"), TypeOption([
            Integer;
            Decimal;
        ]));
    ]));
    (Literal ("has_reform"), Value(Keyword)); 
    (Literal ("has_global_flag"), Value(Keyword));
    (Literal ("has_country_flag"), Value(Keyword));
    (Literal ("has_country_modifier"), Value(Keyword));
    (Literal ("ai"), Value(Bool));
    (Literal ("AI"), Value(Bool));
    (Literal ("alliance_with"), Type "target");
    (Literal ("at_war"), Value(Bool));
    (Literal ("badboy"), Number);
    (Literal ("base_production"), Number);
    (Literal ("base_tax"), Number);
    (Literal ("capital"), Integer);
    (Literal ("cash"), Number);
    (Literal ("casus_belli"), SubTable(symbol_table_init [
        (Literal ("target"), TypeOption([
            Value(Tag);
            Type "scope";
        ]));
        (Literal ("type"), Value(Keyword));
    ]));
    (Literal ("controls"), Integer);
    (Literal ("core_claim"), Integer);
    (Literal ("culture"), Value(Keyword));
    (Literal ("culture_group"), Value(Keyword));
    (Literal ("diplomatic_reputation"), Number);
    (Literal ("diplomatic_upkeep"), Integer);
    (Literal ("dynasty"), Value(Keyword));
    (Literal ("election"), Value(Bool));
    (Literal ("exists"), TypeOption[Type "target"; Value(Bool)]);
    (Literal ("government"), Value(Keyword));
    (Literal ("government_rank"), Integer);
    (Literal ("has_advisor"), Value(Keyword));
    (Literal ("has_discovered"), Integer);
    (Literal ("has_idea"), Value(Keyword));
    (Literal ("has_idea_group"), Value(Keyword));
    (Literal ("has_institution"), Value(Keyword));
    (Literal ("has_leader"), Number);
    (Literal ("has_opinion"), SubTable(symbol_table_init [
        (Literal ("who"), TypeOption([
            Value(Tag);
            Type "scope";
        ]));
        (Literal ("value"), Number);
    ]));
    (Literal ("has_regency"), Value(Bool));
    (Literal ("heir_adm"), Integer);
    (Literal ("heir_dip"), Integer);
    (Literal ("heir_mil"), Integer);
    (Literal ("imperial_influence"), Number);
    (Literal ("in_default"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("is_at_war"), Value(Bool));
    (Literal ("is_colonial_nation"), Value(Bool));
    (Literal ("is_custom_nation"), Value(Bool));
    (Literal ("is_elector"), Value(Bool));
    (Literal ("is_emperor"), Value(Bool));
    (Literal ("is_free_or_tributary_trigger"), Value(Bool));
    (Literal ("is_former_colonial_nation"), Value(Bool));
    (Literal ("is_league_enemy"), Value(Bool));
    (Literal ("is_league_leader"), Value(Bool));
    (Literal ("is_league_member"), Value(Bool));
    (Literal ("is_lesser_in_union"), Value(Bool));
    (Literal ("is_march"), Value(Bool));
    (Literal ("is_neighbor_of"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("is_overseas"), Integer);
    (Literal ("is_partial_liberty"), Value(Bool));
    (Literal ("is_subject"), Value(Bool));
    (Literal ("is_subject_of"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("is_tributary_of"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("is_vassal"), Value(Bool));
    (Literal ("is_vassal_of"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("legitimacy"), Number);
    (Literal ("legitimacy_equivalent"), Number);
    (Literal ("liberty_desire"), Number);
    (Literal ("manpower"), Number);
    (Literal ("mercantilism"), Number);
    (Literal ("mil_power"), Number);
    (Literal ("mil_tech"), Integer);
    (Literal ("military_power"), Number);
    (Literal ("monthly_income"), Number);
    (Literal ("national_unrest"), Number);
    (Literal ("num_of_allies"), Integer);
    (Literal ("num_of_cities"), Integer);
    (Literal ("num_of_ports"), Integer);
    (Literal ("num_of_revolts"), Integer);
    (Literal ("num_of_trade_companies"), Integer);
    (Literal ("num_of_trading_bonuses"), Integer);
    (Literal ("num_of_trading_companies"), Integer);
    (Literal ("num_of_vassals"), Integer);
    (Literal ("owns"), Integer);
    (Literal ("owns_core_province"), Integer);
    (Literal ("prestige"), Number);
    (Literal ("primary_culture"), Value(Keyword));
    (Literal ("religion"), TypeOption([
        Type "religion";
        Type "scope";
    ]));
    (Literal ("religious_unity"), Number);
    (Literal ("republican_tradition"), Number);
    (Literal ("ruler_age"), Integer);
    (Literal ("ruler_adm"), Integer);
    (Literal ("ruler_dip"), Integer);
    (Literal ("ruler_mil"), Integer);
    (Literal ("ruler_has_personality"), Value(Keyword));
    (Literal ("stability"), Number);
    (Literal ("subjects_of_subject"), Integer);
    (Literal ("tag"), TypeOption [Value(Tag); Type "scope"]);
    (Literal ("total_base_tax"), Number);
    (Literal ("total_development"), Integer);
    (Literal ("trade_company_investment"), Integer);
    (Literal ("trade_income_percentage"), Number);
    (Literal ("trade_steering"), Number);
    (Literal ("trading_bonus"), Number);
    (Literal ("truce_with"), Type "target");
    (Literal ("war_exhaustion"), Number);
    (Literal ("war_score"), Integer);
    (Literal ("war_with"), Type "target");
    (Literal ("years_of_income"), Number);
    (Type "country_tag", Type "country_conditions_def");
    (Value(Condition), Type "country_conditions_def");
    (Type "scope", Type "country_conditions_def");
    (Integer, Type "province_conditions_def");
    (Literal ("any_owned_province"), Type "province_conditions_def");
    (Literal ("any_subject"), Type "country_conditions_def");
    (Literal ("any_vassal"), Type "country_conditions_def");
    (Literal ("any_neighbor_country"), Type "country_conditions_def");
    (Literal ("any_ally"), Type "country_conditions_def");
    (Literal ("any_enemy_country"), Type "country_conditions_def");
    (Literal ("any_rival_country"), Type "country_conditions_def");
    (Literal ("any_known_country"), Type "country_conditions_def");
    (Literal ("any_core"), Type "province_conditions_def");
    (Literal ("all_core"), Type "province_conditions_def");
    (Literal ("controller"), Type "country_conditions_def");
    (Literal ("owner"), Type "country_conditions_def");
    (Literal ("overlord"), Type "country_conditions_def");
]

let province_conditions = symbol_table_init [
    (Literal ("controller"), Type "country_conditions_def");
    (Literal ("owner"), Type "country_conditions_def");
    (Type "scope", Type "country_conditions_def");
    (Type "land_provid", Type "province_conditions_def");
    (Type "country_tag", Type "country_conditions_def");
    (Literal ("has_global_flag"), Value(Keyword));
    (Literal ("has_province_flag"), Value(Keyword));
    (Literal ("has_province_modifier"), Value(Keyword));
    (Literal ("year"), Integer);
    (Literal ("month"), Integer);
    (Literal ("day"), Integer);
    (Literal ("always"), Value(Bool));
    (Literal ("base_production"), Number);
    (Literal ("base_tax"), Number);
    (Literal ("base_manpower"), Number);
    (Literal ("blockaded"), Value(Bool));
    (Literal ("continent"), Value(Keyword));
    (Literal ("controlled_by"), TypeOption([
        Value(Tag);
        Type "scope";
        Literal "owner";
        Literal "any_country";
    ]));
    (Literal ("core_claim"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("culture"), Value(Keyword));
    (Literal ("culture_group"), Value(Keyword));
    (Literal ("development"), Integer);
    (Literal ("discovered_by"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("has_building"), Value(Keyword));
    (Literal ("has_center_of_trade"), Value(Bool));
    (Literal ("has_estate"), Value(Keyword));
    (Literal ("has_estate_privilege"), Value(Keyword));
    (Literal ("has_fort"), Value(Bool));
    (Literal ("has_institution"), Value(Keyword));
    (Literal ("has_port"), Value(Bool));
    (Literal ("has_revolt"), Value(Bool));
    (Literal ("has_trade_company_investment"), Value(Keyword));
    (Literal ("is_capital"), Value(Bool));
    (Literal ("is_city"), Value(Bool));
    (Literal ("is_coastal"), Value(Bool));
    (Literal ("is_core"), TypeOption([
        Value(Tag);
        Type "scope";
        Value(Bool);
    ]));
    (Literal ("is_empty"), Value(Bool));
    (Literal ("is_overseas"), Value(Bool));
    (Literal ("is_territorial_core"), TypeOption([
        Value(Tag);
        Type "scope";
    ]));
    (Literal ("is_wasteland"), Value(Bool));
    (Literal ("manpower"), Number);
    (Literal ("province_id"), Integer);
    (Literal ("religion"), Value(Keyword));
    (Literal ("religion_group"), Value(Keyword));
    (Literal ("trade_goods"), Value(Keyword));
    (Literal ("trade_power"), Number);
    (Literal ("trade_value"), Number);
    (Literal ("unrest"), Number);
    (Literal ("any_neighbor_province"), Type "province_conditions_def");
    (Literal ("exists"), Type "target");
    (Value(Condition), Type "province_conditions_def");
]
