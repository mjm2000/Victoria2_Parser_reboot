open SymbolTable
open TypeDef

let country_modifiers = symbol_table_init [
    (KEYWORD_SYMBOL "administrative_efficiency_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "badboy", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "cb_generation_speed_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "core_pop_militancy_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "core_pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "diplomatic_points_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "education_efficiency_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "factory_cost", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "factory_input", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "factory_output", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "factory_owner_cost", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "factory_throughput", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "global_assimilation_rate", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KEYWORD_SYMBOL "global_immigrant_attract", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "global_pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "global_pop_militancy_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "global_population_growth", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]); 
    (KEYWORD_SYMBOL "import_cost", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "influence_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "issue_change_speed", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "land_organisation", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "land_unit_start_experience", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "leadership_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "loan_interest", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "max_loan_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "max_military_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "max_social_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "max_tariff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "max_tax", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "min_military_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "min_social_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "min_tariff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "min_tax", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "mobilisation_economy_impact", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "mobilization_impact", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "mobilisation_size", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "naval_organisation", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "naval_unit_start_experience", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "non_accepted_pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "non_accepted_pop_militancy_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "org_regain", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "political_reform_desire", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "prestige", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "research_points", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "research_points_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "research_points_on_conquer", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "rgo_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "rgo_throughput", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "farm_rgo_size", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "mine_rgo_size", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "ruling_party_support", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "social_reform_desire", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "suppression_points_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "supply_consumption", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "poor_vote", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "middle_vote", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]); 
    (KEYWORD_SYMBOL "rich_vote", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "tax_efficiency", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "industry_tech_research_bonus", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "culture_tech_research_bonus", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "commerce_tech_research_bonus", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);

    (KEYWORD_SYMBOL "unit_start_experience", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "war_exhaustion", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
] 
let province_modifiers = symbol_table_init [
    (KEYWORD_SYMBOL "assimilation_rate", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "immigrant_attract", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "immigrant_push", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "life_rating", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_artisan_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_factory_input", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_factory_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_factory_throughput", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_repair", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_RGO_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_RGO_throughput", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_ruling_party_support", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "local_ship_build", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "pop_militancy_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "population_growth", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "farm_rgo_eff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "mine_rgo_eff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "goods_demand", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "poor_income_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "middle_income_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "rich_income_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);

    (KEYWORD_SYMBOL "poor_life_needs", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "middle_life_needs", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KEYWORD_SYMBOL "rich_life_needs", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
];
