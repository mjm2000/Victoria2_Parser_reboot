open SymbolTable
open TypeDef

let country_modifiers = symbol_table_init [
    (KeywordLiteral "administrative_efficiency_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "badboy", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "cb_generation_speed_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "core_pop_militancy_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "core_pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "diplomatic_points_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "education_efficiency_modifier", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "factory_cost", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "factory_input", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "factory_output", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "factory_owner_cost", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "factory_throughput", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "global_assimilation_rate", PARAM_OPTION[
		(PARAM_VALUE FLOAT);
		(PARAM_VALUE INT);
	]);
    (KeywordLiteral "global_immigrant_attract", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "global_pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "global_pop_militancy_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "global_population_growth", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]); 
    (KeywordLiteral "import_cost", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "influence_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "issue_change_speed", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "land_organisation", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "land_unit_start_experience", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "leadership_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "loan_interest", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "max_loan_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "max_military_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "max_social_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "max_tariff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "max_tax", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "min_military_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "min_social_spending", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "min_tariff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "min_tax", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "mobilisation_economy_impact", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "mobilization_impact", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "mobilisation_size", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "naval_organisation", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "naval_unit_start_experience", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "non_accepted_pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "non_accepted_pop_militancy_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "org_regain", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "political_reform_desire", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "prestige", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "research_points", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "research_points_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "research_points_on_conquer", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "rgo_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "rgo_throughput", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "farm_rgo_size", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "mine_rgo_size", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "ruling_party_support", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "social_reform_desire", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "suppression_points_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "supply_consumption", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "poor_vote", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "middle_vote", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]); 
    (KeywordLiteral "rich_vote", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "tax_efficiency", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "industry_tech_research_bonus", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "culture_tech_research_bonus", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "commerce_tech_research_bonus", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);

    (KeywordLiteral "unit_start_experience", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "war_exhaustion", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
] 
let province_modifiers = symbol_table_init [
    (KeywordLiteral "assimilation_rate", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "immigrant_attract", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "immigrant_push", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "life_rating", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_artisan_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_factory_input", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_factory_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_factory_throughput", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_repair", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_RGO_output", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_RGO_throughput", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_ruling_party_support", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "local_ship_build", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "pop_consciousness_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "pop_militancy_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "population_growth", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "farm_rgo_eff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "mine_rgo_eff", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "goods_demand", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "poor_income_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "middle_income_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "rich_income_modifier", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);

    (KeywordLiteral "poor_life_needs", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "middle_life_needs", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
    (KeywordLiteral "rich_life_needs", PARAM_OPTION[
        (PARAM_VALUE FLOAT);
        (PARAM_VALUE INT);
    ]);
];
