open SymbolTable
open TypeDef
(*

Optional = { Left = "min_social_spending" Right = Double }
Optional = { Left = "max_social_spending" Right = Double }
Optional = { Left = "min_education_spending" Right = Double }
Optional = { Left = "max_education_spending" Right = Double }
Optional = { Left = "min_military_spending" Right = Double }
Optional = { Left = "max_military_spending" Right = Double }
Optional = { Left = "min_administration_spending" Right = Double }
Optional = { Left = "max_administration_spending" Right = Double }
                                                                               
Optional = { Left = "prestige" Right = NzDbl }
Optional = { Left = "badboy" Right = NzDbl }
Optional = { Left = "diplomatic_points_modifier" Right = NzDbl }

Optional = { Left = "factory_output" Right = NzDbl }
Optional = { Left = "rgo_output" Right = NzDbl }
Optional = { Left = { "RGO_throughput" "rgo_throughput" } Right = NzDbl }
Optional = { Left = "factory_owner_cost" Right = NzDbl }

Optional = { Left = "poor_vote" Right = Double }
Optional = { Left = "middle_vote" Right = Double }
Optional = { Left = "rich_vote" Right = Double }

Optional = { Left = "rich_income_modifier" Right = NzDbl }

Optional = { Left = "global_pop_consciousness_modifier" Right = NzDbl }
Optional = { Left = "core_pop_consciousness_modifier" Right = NzDbl }
Optional = { Left = "global_pop_militancy_modifier" Right = NzDbl }
Optional = { Left = "core_pop_militancy_modifier" Right = NzDbl }
Optional = { Left = "non_accepted_pop_militancy_modifier" Right = NzDbl }
Optional = { Left = "non_accepted_pop_consciousness_modifier" Right = NzDbl }

Optional = { Left = "tax_efficiency" Right = NzDbl }
Optional = { Left = "max_tariff" Right = NzDbl }
Optional = { Left = "research_points_modifier" Right = NzDbl }
Optional = { Left = "research_points" Right = NzDbl }
Optional = { Left = "loan_interest" Right = NzDbl }
Optional = { Left = "import_cost" Right = NzDbl }

Optional = { Left = "min_military_spending" Right = NzDbl }
Optional = { Left = "max_military_spending" Right = NzDbl }
Optional = { Left = "war_exhaustion" Right = NzDbl }
Optional = { Left = "org_regain" Right = NzDbl }
Optional = { Left = "leadership" Right = NzDbl }
Optional = { Left = "leadership_modifier" Right = NzDbl }
Optional = { Left = "land_organisation" Right = NzDbl }
Optional = { Left = "naval_organisation" Right = NzDbl }
Optional = { Left = "unit_start_experience" Right = NzDbl }
Optional = { Left = "supply_consumption" Right = NzDbl }
Optional = { Left = "mobilisation_economy_impact" Right = NzDbl }
Optional = { Left = "mobilisation_size" Right = NzDbl }
Optional = { Left = "supply_limit" Right = NzDbl }
Optional = { Left = "max_attrition" Right = NzDbl }
Optional = { Left = "max_war_exhaustion" Right = NnDbl }

Optional = { Left = "global_population_growth" Right = NzDbl }
Optional = { Left = "global_immigrant_attract" Right = NzDbl }
Optional = { Left = "assimilation_rate" Right = NzDbl }
Optional = { Left = "global_assimilation_rate" Right = NzDbl }
Optional = { Left = "ruling_party_support" Right = NzDbl }
Optional = { Left = "war_exhaustion_effect" Right = NzDbl }
Optional = { Left = "factory_throughput" Right = NzDbl }
Optional = { Left = "social_reform_desire" Right = NzDbl }
Optional = { Left = "political_reform_desire" Right = NzDbl }
Optional = { Left = "issue_change_speed" Right = NzDbl }
Optional = { Left = "literacy_con_impact" Right = NzDbl }

Optional = { Left = "navy_tech_research_bonus" Right = NzDbl }
Optional = { Left = "army_tech_research_bonus" Right = NzDbl }
Optional = { Left = "commerce_tech_research_bonus" Right = NzDbl }
Optional = { Left = "culture_tech_research_bonus" Right = NzDbl }
Optional = { Left = "industry_tech_research_bonus" Right = NzDbl }

Optional = { Left = "land_attrition" Right = Double }
Optional = { Left = "naval_attrition" Right = Double }	
Optional = { Left = "colonial_life_rating" Right = Double }	
Optional = { Left = "soldier_to_pop_loss" Right = Double }	
Optional = { Left = "colonial_prestige" Right = Double }	
Optional = { Left = "colonial_migration" Right = Double }	
Optional = { Left = "education_efficiency" Right = Double }	
Optional = { Left = "diplomatic_points" Right = Double }	
Optional = { Left = "seperatism" Right = Double }	
(Ch N Y) = { Left = "factory_goods_output" Right = {
	Required = { Left = { Type = Goods } Right = Double }
} }
(Ch N Y) = { Left = "factory_goods_throughput" Right = {
	Required = { Left = { Type = Goods } Right = Double }
} }
(Ch N Y) = { Left = "rgo_goods_throughput" Right = {
	Required = { Left = { Type = Goods } Right = Double }
} }
(Ch N Y) = { Left = "rgo_goods_output" Right = {
	Required = { Left = { Type = Goods } Right = Double }
} }
(Ch N Y) = { Left = "rgo_size" Right = {
	Required = { Left = { Type = Goods } Right = Double }
} }
(Ch N Y) = { Left = "rebel_org_gain" Right = {
	Single = { Left = "faction" Right = { Type = RebelType Literal = "all" } }
	Single = { Left = "value" Right = Double }
} }
Optional = { Left = "factory_input" Right = Double }
Optional = { Left = "factory_cost" Right = Double }
Optional = { Left = "pop_growth" Right = Double }
Optional = { Left = "morale" Right = Double }
Optional = { Left = "tax_eff" Right = Double }
	
(If If = Vic2Ahd) = {
	Optional = { Left = "cb_generation_speed_modifier" Right = NzDbl }
	Optional = { Left = "land_unit_start_experience" Right = NzDbl }
	Optional = { Left = "self_unciv_economic_modifier" Right = NzDbl }
	Optional = { Left = "self_unciv_military_modifier" Right = NzDbl }
	Optional = { Left = "reinforce_speed" Right = NzDbl }
	Optional = { Left = "mobilization_impact" Right = NzDbl }
	Optional = { Left = "suppression_points_modifier" Right = NzDbl }
	Optional = { Left = "education_efficiency_modifier" Right = NzDbl }
	Optional = { Left = "naval_defense_modifier" Right = NzDbl }
	Optional = { Left = "naval_attack_modifier" Right = NzDbl }
	Optional = { Left = "naval_unit_start_experience" Right = NzDbl }
	Optional = { Left = "land_defense_modifier" Right = NzDbl }
	Optional = { Left = "max_loan_modifier" Right = NzDbl }
	Optional = { Left = "administrative_efficiency_modifier" Right = NzDbl }
	Optional = { Left = "tariff_efficiency_modifier" Right = NzDbl }
		Optional = { Left = "poor_life_needs" Right = NzDbl }
	Optional = { Left = "poor_everyday_needs" Right = NzDbl }
	Optional = { Left = "poor_luxury_needs" Right = NzDbl }
	Optional = { Left = "middle_life_needs" Right = NzDbl }
	Optional = { Left = "middle_everyday_needs" Right = NzDbl }
	Optional = { Left = "middle_luxury_needs" Right = NzDbl }
	Optional = { Left = "rich_life_needs" Right = NzDbl }
	Optional = { Left = "rich_everyday_needs" Right = NzDbl }
	Optional = { Left = "rich_luxury_needs" Right = NzDbl }
}

   *)
let country_modifiers = symbol_table_init [
    (Literal "poor_life_needs", Number);
    (Literal "middle_life_needs", Number);
    (Literal "rich_life_needs", Number);
    (Literal "poor_everyday_needs", Number);
    (Literal "middle_everyday_needs", Number);
    (Literal "rich_everyday_needs", Number);
    (Literal "poor_luxury_needs", Number);
    (Literal "middle_luxury_needs", Number);
    (Literal "rich_luxury_needs", Number);
    (Literal "war_exhaustion_effect", Number);
    (Literal "administrative_efficiency_modifier", Number);
    (Literal "badboy", Number);
    (Literal "cb_generation_speed_modifier", Number);
    (Literal "core_pop_militancy_modifier", Number);
    (Literal "core_pop_consciousness_modifier", Number);
    (Literal "diplomatic_points_modifier", Number);
    (Literal "education_efficiency_modifier", Number);
    (Literal "factory_cost", Number);
    (Literal "factory_input", Number);
    (Literal "factory_output", Number);
    (Literal "factory_owner_cost", Number);
    (Literal "factory_throughput", Number);
    (Literal "global_assimilation_rate", Number);
    (Literal "global_immigrant_attract", Number);
    (Literal "global_pop_consciousness_modifier", Number);
    (Literal "global_pop_militancy_modifier", Number);
    (Literal "global_population_growth", Number);
    (Literal "import_cost", Number);
    (Literal "influence_modifier", Number);
    (Literal "issue_change_speed", Number);
    (Literal "land_organisation", Number);
    (Literal "land_unit_start_experience", Number);
    (Literal "leadership_modifier", Number);
    (Literal "loan_interest", Number);
    (Literal "max_loan_modifier", Number);
    (Literal "max_military_spending", Number);
    (Literal "max_social_spending", Number);
    (Literal "max_tariff", Number);
    (Literal "max_tax", Number);
    (Literal "min_military_spending", Number);
    (Literal "min_social_spending", Number);
    (Literal "min_tariff", Number);
    (Literal "min_tax", Number);
    (*british and american spelling*)
    (Literal "mobilisation_economy_impact", Number);
    (Literal "mobilisation_impact", Number);
    (Literal "mobilisation_size", Number);
    (Literal "mobilization_economy_impact", Number);
    (Literal "mobilization_impact", Number);
    (Literal "mobilization_size", Number);
    (Literal "naval_organisation", Number);
    (Literal "naval_unit_start_experience", Number);
    (Literal "non_accepted_pop_consciousness_modifier", Number);
    (Literal "non_accepted_pop_militancy_modifier", Number);
    (Literal "org_regain", Number);
    (Literal "political_reform_desire", Number);
    (Literal "prestige", Number);
    (Literal "research_points", Number);
    (Literal "research_points_modifier", Number);
    (Literal "research_points_on_conquer", Number);
    (Literal "ruling_party_support", Number);
    (Literal "social_reform_desire", Number);
    (Literal "suppression_points_modifier", Number);
    (Literal "supply_consumption", Number);
    (Literal "poor_vote", Number);
    (Literal "middle_vote", Number);
    (Literal "rich_vote", Number);
    (Literal "tax_efficiency", Number);
    (Literal "navy_tech_research_bonus", Number);
    (Literal "army_tech_research_bonus", Number);
    (Literal "industry_tech_research_bonus", Number);
    (Literal "culture_tech_research_bonus", Number);
    (Literal "commerce_tech_research_bonus", Number);
    (Literal "leadership", Number);
    (Literal "unit_start_experience", Number);
    (Literal "war_exhaustion", Number);
    (Literal "self_unciv_military_modifier", Number);
    (Literal "max_war_exhaustion", Number);
    (Literal "supply_limit", Number);
    (Literal "naval_defense_modifier", Number);
    (Literal "naval_attack_modifier", Number);
    (Literal "naval_unit_start_experience", Number);
    (Literal "land_defense_modifier", Number);
    (Literal "reinforce_speed", Number);
    (Literal "tariff_efficiency_modifier", Number);
    (Literal "self_unciv_economic_modifier", Number);
    (Literal "mobilisation_impact", Number);
    (Literal "suppression_points_modifier", Number);
    (Literal "education_efficiency_modifier", Number);
    (Literal "max_attrition", Number);
    (Literal "colonial_life_rating", Number);
    (Literal "soldier_to_pop_loss", Number);
    (Literal "colonial_prestige", Number);
    (Literal "colonial_migration", Number);
    (Literal "diplomatic_points", Number);
    (Literal "seperatism", Number);
    (Literal "life_rating", Number);
    (Literal "land_attrition", Number);
    (Literal "naval_attrition", Number);
    (Literal "colonial_migration", Number);
    (Literal "farm_RGO_eff", Number);
    (Literal "farm_rgo_eff", Number);
    (Literal "rgo_output", Number);
    (Literal "RGO_output", Number);
    (Literal "farm_rgo_size", Number);
    (Literal "farm_RGO_size", Number);
    (Literal "mine_rgo_size", Number);
    (Literal "mine_RGO_size", Number);
    (Literal "RGO_throughput", Number);
    (Literal "rgo_throughput", Number);
    (Literal "local_RGO_output", Number);
    (Literal "local_rgo_output", Number);
    (Literal "local_RGO_throughput", Number);
    (Literal "local_rgo_throughput", Number);
    (Literal "mine_rgo_eff", Number);
    (Literal "mine_RGO_eff", Number);
    (Literal "max_attrition", Number);
    (Literal "local_factory_throughput", Number);
    (Literal "population_growth", Number);
    (Literal "immigrant_attract", Number);
    (Literal "literacy_con_impact", Number);
    (Literal "unit_recruitment_time", Number);
    (Literal "rgo_size",  SubTable (symbol_table_init [
        (Type "good", Number);
    ]));
    (Literal "rgo_goods_throughput", SubTable (symbol_table_init [
        (Type "good", Number);
    ]));
    (Literal "rgo_goods_output", SubTable (symbol_table_init [
        (Type "good", Number);
    ]));
    (Literal "factory_goods_throughput", SubTable (symbol_table_init [
        (Type "good", Number);
    ]));
    (Literal "factory_goods_output", SubTable (symbol_table_init [
        (Type "good", Number);
    ]));
    (Literal "rebel_org_gain", SubTable (symbol_table_init [
        (Literal "faction", Type "rebel_type");
        (Literal "value", Number);
    ]));
    (Literal "global_pop_growth", Number);
]






(*let country_modifiers = symbol_table_init [
    (Literal "administrative_efficiency_modifier", Number);
    (Literal "badboy", Number);
    (Literal "cb_generation_speed_modifier", Number);
    (Literal "core_pop_militancy_modifier", Number);
    (Literal "core_pop_consciousness_modifier", Number);
    (Literal "diplomatic_points_modifier", Number);
    (Literal "education_efficiency_modifier", Number);
    (Literal "factory_cost", Number);
    (Literal "factory_input", Number);
    (Literal "factory_output", Number);
    (Literal "factory_owner_cost", Number);
    (Literal "factory_throughput", Number);
    (Literal "global_assimilation_rate", Number);
    (Literal "global_immigrant_attract", Number);
    (Literal "global_pop_consciousness_modifier", Number);
    (Literal "global_pop_militancy_modifier", Number);
    (Literal "global_population_growth", Number); 
    (Literal "import_cost", Number);
    (Literal "influence_modifier", Number);
    (Literal "issue_change_speed", Number);
    (Literal "land_organisation", Number);
    (Literal "land_unit_start_experience", Number);
    (Literal "leadership_modifier", Number);
    (Literal "loan_interest", Number);
    (Literal "max_loan_modifier", Number);
    (Literal "max_military_spending", Number);
    (Literal "max_social_spending", Number);
    (Literal "max_tariff", Number);
    (Literal "max_tax", Number);
    (Literal "min_military_spending", Number);
    (Literal "min_social_spending", Number);
    (Literal "min_tariff", Number);
    (Literal "min_tax", Number);
    (Literal "mobilisation_economy_impact", Number);
    (Literal "mobilization_impact", Number);
    (Literal "mobilisation_size", Number);
    (Literal "naval_organisation", Number);
    (Literal "naval_unit_start_experience", Number);
    (Literal "non_accepted_pop_consciousness_modifier", Number);
    (Literal "non_accepted_pop_militancy_modifier", Number);
    (Literal "org_regain", Number);
    (Literal "political_reform_desire", Number);
    (Literal "prestige", Number);
    (Literal "research_points", Number);
    (Literal "research_points_modifier", Number);
    (Literal "research_points_on_conquer", Number);
    (Literal "rgo_output", Number);
    (Literal "rgo_throughput", Number);
    (Literal "farm_rgo_size", Number);
    (Literal "mine_rgo_size", Number);
    (Literal "ruling_party_support", Number);
    (Literal "social_reform_desire", Number);
    (Literal "suppression_points_modifier", Number);
    (Literal "supply_consumption", Number);
    (Literal "poor_vote", Number);
    (Literal "middle_vote", Number); 
    (Literal "rich_vote", Number);
    (Literal "tax_efficiency", Number);
    (Literal "industry_tech_research_bonus", Number);
    (Literal "culture_tech_research_bonus", Number);
    (Literal "commerce_tech_research_bonus", Number);
    (Literal "leadership", Number);
    (Literal "unit_start_experience", Number);
    (Literal "war_exhaustion", Number);
    (Literal "self_unciv_military_modifier", Number);
    (Literal "RGO_throughput", Number);
    (Literal "max_war_exhaustion", Number);
    (Literal "supply_limit", Number);
] 
*)
let province_modifiers = symbol_table_init [
    (Literal "assimilation_rate", Number);
    (Literal "immigrant_attract", Number);
    (Literal "immigrant_push", Number);
    (Literal "life_rating", Number);
    (Literal "local_artisan_output", Number);
    (Literal "local_artisan_input", Number);
    (Literal "local_factory_input", Number);
    (Literal "local_factory_output", Number);
    (Literal "local_factory_throughput", Number);
    (Literal "local_repair", Number);
    (Literal "local_RGO_output", Number);
    (Literal "local_RGO_throughput", Number);
    (Literal "local_ruling_party_support", Number);
    (Literal "local_ship_build", Number);
    (Literal "pop_consciousness_modifier", Number);
    (Literal "pop_militancy_modifier", Number);
    (Literal "population_growth", Number);
    (Literal "farm_rgo_eff", Number);
    (Literal "mine_rgo_eff", Number);
    (Literal "goods_demand", Number);
    (Literal "poor_income_modifier", Number);
    (Literal "middle_income_modifier", Number);
    (Literal "rich_income_modifier", Number);

    (Literal "poor_life_needs", Number);
    (Literal "middle_life_needs", Number);
    (Literal "rich_life_needs", Number);
];
