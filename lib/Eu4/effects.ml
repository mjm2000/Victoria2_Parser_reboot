open SymbolTable
open TypeDef

let country_effects = symbol_table_init [
    (Literal "every_province"), Inherit( 
        [(Literal "limit"),Type "province_conditions"],["province_effects_def"];
    );
    (Literal ("country"), Type "country_effects_def");
    (Type "land_provid", Type "province_effects_def");
    (Literal ("money"), Number);
    (Literal ("add_core"), Type "land_provid");
    (Literal ("remove_core"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("add_claim"), TypeOption([
        Integer;
        SubTable(symbol_table_init [
            (Literal ("province"), Integer);
            (Literal ("who"), TypeOption([
                Value(Tag);
                Value(Scope);
            ]));
        ]);
    ]));
    (Literal ("remove_claim"), TypeOption([
        Integer;
        SubTable(symbol_table_init [
            (Literal ("province"), Integer);
            (Literal ("who"), TypeOption([
                Value(Tag);
                Value(Scope);
            ]));
        ]);
    ]));
    (Literal ("change_tag"), TypeOption([
        Value(Tag);
        Value(Keyword);
    ]));
    (Literal ("set_global_flag"), Value(Keyword));
    (Literal ("clr_global_flag"), Value(Keyword));
    (Literal ("set_country_flag"), Value(Keyword));
    (Literal ("clr_country_flag"), Value(Keyword));
    (Literal ("add_country_modifier"), SubTable(symbol_table_init [
        (Literal ("name"), TypeOption([
            Value(Keyword);
            Value(String);
        ]));
        (Literal ("duration"), Integer);
    ]));
    (Literal ("remove_country_modifier"), Value(Keyword));
    (Literal ("add_prestige"), Number);
    (Literal ("add_stability"), Number);
    (Literal ("add_legitimacy"), Number);
    (Literal ("add_republican_tradition"), Number);
    (Literal ("add_mercantilism"), Number);
    (Literal ("add_war_exhaustion"), Number);
    (Literal ("add_years_of_income"), Number);
    (Literal ("add_adm_power"), Number);
    (Literal ("add_dip_power"), Number);
    (Literal ("add_mil_power"), Number);
    (Literal ("add_manpower"), Number);
    (Literal ("add_years_of_income"), Number);
    (Literal ("change_religion"), Value(Keyword));
    (Literal ("change_primary_culture"), Value(Keyword));
    (Literal ("change_culture"), SubTable(symbol_table_init [
        (Literal ("province"), Integer);
        (Literal ("culture"), Value(Keyword));
    ]));
    (Literal ("change_religion"), SubTable(symbol_table_init [
        (Literal ("province"), Integer);
        (Literal ("religion"), Value(Keyword));
    ]));
    (Literal ("add_idea"), Value(Keyword));
    (Literal ("remove_idea"), Value(Keyword));
    (Literal ("enable_idea_group"), Value(Keyword));
    (Literal ("disable_idea_group"), Value(Keyword));
    (Literal ("add_institution"), Value(Keyword));
    (Literal ("embrace_institution"), Value(Keyword));
    (Literal ("add_government_reform"), Value(Keyword));
    (Literal ("remove_government_reform"), Value(Keyword));
    (Literal ("set_government_rank"), Integer);
    (Literal ("change_government"), Value(Keyword));
    (Literal ("add_trust"), SubTable(symbol_table_init [
        (Literal ("who"), TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal ("value"), Number);
    ]));
    (Literal ("add_opinion"), SubTable(symbol_table_init [
        (Literal ("who"), TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal ("modifier"), Value(Keyword));
    ]));
    (Literal ("improve_relation"), SubTable(symbol_table_init [
        (Literal ("who"), TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal ("value"), Number);
    ]));
    (Literal ("deteriorate_relation"), SubTable(symbol_table_init [
        (Literal ("who"), TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal ("value"), Number);
    ]));
    (Literal ("create_alliance"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("break_alliance"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("create_vassal"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("create_union"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("create_personal_union"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("release"), Value(Tag));
    (Literal ("annex_to"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("inherit"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("integrate"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("add_casus_belli"), SubTable(symbol_table_init [
        (Literal ("target"), TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal ("type"), Value(Keyword));
        (Literal ("months"), Integer);
    ]));
    (Literal ("declare_war_with_cb"), SubTable(symbol_table_init [
        (Literal ("target"), TypeOption([
            Value(Tag);
            Value(Scope);
        ]));
        (Literal ("casus_belli"), Value(Keyword));
        (Literal ("war_goal_province"), Integer);
    ]));
    (Literal ("country_event"), TypeOption(
        [SubTable(symbol_table_init [
            (Literal ("id"), Integer);
            (Literal ("days"), Integer);
        ]);
        Integer]
    ));
    (Literal ("province_event"), SubTable(symbol_table_init [
        (Literal ("id"), Value Keyword);
        (Literal ("days"), Integer);
    ]));
    (Literal ("random_owned_province"), Type "province_effects_def");
    (Literal ("random_owned_controlled_by"), Type "province_effects_def");
    (Literal ("any_owned_province"), Type "province_effects_def");
    (Literal ("any_core"), Type "province_effects_def");
    (Literal ("all_core"), Type "province_effects_def");
    (Literal ("any_subject"), Type "country_effects_def");
    (Literal ("any_vassal"), Type "country_effects_def");
    (Literal ("any_neighbor_country"), Type "country_effects_def");
    (Literal ("any_ally"), Type "country_effects_def");
    (Literal ("any_enemy_country"), Type "country_effects_def");
    (Literal ("any_rival_country"), Type "country_effects_def");
    (Literal ("any_known_country"), Type "country_effects_def");
    (Literal ("overlord"), Type "country_effects_def");
    (Literal ("limit"), Type "country_conditions_def");
    (Literal ("random"), Inherit([(Literal ("chance"), Integer)], ["country_effects_def"]));
    (Literal ("random_list"), SubTable(symbol_table_init [
        (Number, Type "country_effects_def");
    ]));
    (Literal ("set_variable"), SubTable(symbol_table_init [
        (Literal ("which"), Value(Keyword));
        (Literal ("value"), Integer);
    ]));
    (Literal ("change_variable"), SubTable(symbol_table_init [
        (Literal ("which"), Value(Keyword));
        (Literal ("value"), Integer);
    ]));
    (Type "country_tag", Type "country_effects_def");
    (Value(Scope), Type "country_effects_def");
    (Integer, Type "province_effects_def");
]

let province_effects = symbol_table_init [
    (Integer, Type "province_effects_def");
    (Literal "province_event", SubTable(symbol_table_init [
        (Literal ("id"), Value Keyword);
        (Literal ("days"), Integer);
    ]));
    (Type "country_tag", Type "country_effects_def");
    (Value(Scope), Type "country_effects_def");
    (Literal ("country"), Type "country_effects_def");
    (Literal ("set_province_flag"), Value(Keyword));
    (Literal ("clr_province_flag"), Value(Keyword));
    (Literal ("add_province_modifier"), SubTable(symbol_table_init [
        (Literal ("name"), TypeOption([
            Value(Keyword);
            Value(String);
        ]));
        (Literal ("duration"), Integer);
    ]));
    (Literal ("remove_province_modifier"), Value(Keyword));
    (Literal ("change_controller"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("change_owner"), TypeOption([
        Value(Tag);
        Value(Scope);
    ]));
    (Literal ("change_culture"), Value(Keyword));
    (Literal ("change_religion"), Value(Keyword));
    (Literal ("add_base_tax"), Number);
    (Literal ("add_base_production"), Number);
    (Literal ("add_base_manpower"), Number);
    (Literal ("add_development"), SubTable(symbol_table_init [
        (Literal ("adm"), Integer);
        (Literal ("dip"), Integer);
        (Literal ("mil"), Integer);
    ]));
    (Literal ("add_building"), Value(Keyword));
    (Literal ("remove_building"), Value(Keyword));
    (Literal ("add_trade_company_investment"), Value(Keyword));
    (Literal ("remove_trade_company_investment"), Value(Keyword));
    (Literal ("add_center_of_trade_level"), Integer);
    (Literal ("remove_center_of_trade"), Value(Bool));
    (Literal ("set_trade_goods"), Value(Keyword));
    (Literal ("add_unrest"), Number);
    (Literal ("spawn_rebels"), SubTable(symbol_table_init [
        (Literal ("type"), Value(Keyword));
        (Literal ("size"), Integer);
        (Literal ("leader"), Value(Keyword));
    ]));
    (Literal ("kill_rebels"), SubTable(symbol_table_init [
        (Literal ("type"), Value(Keyword));
    ]));
    (Literal ("owner"), Type "country_effects_def");
    (Literal ("controller"), Type "country_effects_def");
    (Literal ("limit"), Type "province_conditions_def");
    (Literal ("random"), Inherit([(Literal ("chance"), Integer)], ["province_effects_def"]));
    (Literal ("random_list"), SubTable(symbol_table_init [
        (Integer, Type "province_effects_def");
    ]));
    (Literal ("any_neighbor_province"), Type "province_effects_def");
    (Literal ("set_variable"), SubTable(symbol_table_init [
        (Literal ("which"), Value(Keyword));
        (Literal ("value"), Integer);
    ]));
    (Literal ("change_variable"), SubTable(symbol_table_init [
        (Literal ("which"), Value(Keyword));
        (Literal ("value"), Integer);
    ]));
]
