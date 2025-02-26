open Symbol_table
open Type_def

let bookmarks = symbol_table_init [
    KEYWORD_SYMBOL "bookmark",PARAM_LIST (symbol_table_init [
        KEYWORD_SYMBOL "name", PARAM_VALUE STRING;
        KEYWORD_SYMBOL "desc", PARAM_VALUE STRING; 
        KEYWORD_SYMBOL "date", PARAM_VALUE KEYWORD;
        KEYWORD_SYMBOL "cameraX", PARAM_VALUE INT;
        KEYWORD_SYMBOL "cameraY", PARAM_VALUE INT;
        ]
    ); 
]


(*
aeroplane_factory = {
	type = factory
	on_completion = factory
	completion_size = 0.2
	max_level = 99
	goods_cost =
	{
		machine_parts = 200
		electric_gear = 600
		steel = 600
		cement = 600
	}
	time = 730
	visibility = yes
	onmap = no

	production_type = aeroplane_factory
	pop_build_factory = yes
	advanced_factory = yes
}
*)
let buildings = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "on_completion", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "completion_size", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "max_level", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "goods_cost", PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_VALUE INT);
        ]
        ));
        (KEYWORD_SYMBOL "time", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "visibility", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "onmap", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "production_type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "pop_build_factory", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "advanced_factory", PARAM_VALUE BOOL);
    ]));
]
(*
dismantle_cb_add = {
	sprite_index = 20
	is_triggered_only = yes
	months = 12
	crisis = no
	
	construction_speed = 1.5

	badboy_factor = 0
	prestige_factor = 10
	peace_cost_factor = 35
	penalty_factor = 3
	always = yes

	break_truce_prestige_factor = 5
	break_truce_infamy_factor = 3
	break_truce_militancy_factor = 1
	truce_months = 108
	
	good_relation_prestige_factor = 0
	good_relation_infamy_factor = 0
	good_relation_militancy_factor = 0
	
	can_use = {
		NOT = { is_our_vassal = THIS }
		NOT = { has_country_modifier = neutrality }
		NOT = { has_country_flag = dismantle_declared }
		has_country_flag = in_great_war
		OR = {
			is_greater_power = yes
			is_secondary_power = yes
			colonial_nation = yes
			any_owned_province = { is_overseas = yes }
		}
		civilized = yes
		is_disarmed = no
		is_vassal = no
		war_with = THIS
		THIS = {
			NOT = { has_country_modifier = neutrality }
			has_country_flag = in_great_war
			is_greater_power = yes
			is_disarmed = no
			is_vassal = no
			mass_politics = 1
		}		
	}
	
	po_disarmament = yes
	po_reparations = yes
	
	war_name = WAR_DISMANTLE_NAME
	
	on_add = {
		FROM = {
			set_country_flag = dismantle_declared
		}
		move_issue_percentage = { 
			from = pro_military 
			to = jingoism
			value = 0.10
		}
	}
	
	on_po_accepted = {
		set_country_flag = dismantling_treaty
	}
}

*)




    (*
cb_types.txt|11 col 3-151| # po_xxx - Peace options. If toggled on, badboy_factor and prestige_factor are multiplied with any associated base changes to these (see defines.txt.)
cb_types.txt|12 col 24-110| # allowed_states - If 'po_demand_states' is on, badboy_factor applies to these provinces. 'THIS' scope is us.
cb_types.txt|16 col 3-92| # po_remove_cores - may be used only with: po_transfer_provinces, po_demand_state, po_annex

cb_types.txt|20 col 2-10| #po_annex
cb_types.txt|21 col 2-17| #po_demand_state
cb_types.txt|22 col 2-18| #po_add_to_sphere
cb_types.txt|23 col 2-16| #po_disarmament
cb_types.txt|24 col 2-16| #po_reparations
cb_types.txt|25 col 2-23| #po_transfer_provinces
cb_types.txt|26 col 2-20| #po_remove_prestige
cb_types.txt|27 col 2-16| #po_make_puppet
cb_types.txt|28 col 2-19| #po_release_puppet
cb_types.txt|29 col 2-15| #po_status_quo
cb_types.txt|30 col 2-31| #po_install_communist_gov_type
cb_types.txt|31 col 2-33| #po_uninstall_communist_gov_type
cb_types.txt|32 col 2-17| #po_remove_cores
cb_types.txt|33 col 2-11| #po_colony
cb_types.txt|168 col 2-22| po_disarmament = yes
cb_types.txt|169 col 2-22| po_reparations = yes
cb_types.txt|238 col 2-22| po_disarmament = yes
cb_types.txt|239 col 2-22| po_reparations = yes
cb_types.txt|300 col 2-22| po_disarmament = yes
cb_types.txt|301 col 2-22| po_reparations = yes
cb_types.txt|347 col 2-22| po_disarmament = yes
cb_types.txt|348 col 2-22| po_reparations = yes
cb_types.txt|349 col 2-26| po_remove_prestige = yes
cb_types.txt|429 col 2-23| po_demand_state = yes
cb_types.txt|516 col 2-29| po_transfer_provinces = yes
cb_types.txt|618 col 2-23| po_demand_state = yes
cb_types.txt|713 col 2-16| po_annex = yes
cb_types.txt|799 col 2-23| po_demand_state = yes
cb_types.txt|906 col 2-23| po_demand_state = yes
cb_types.txt|907 col 2-23| po_remove_cores = yes
cb_types.txt|1010 col 2-23| po_demand_state = yes
cb_types.txt|1050 col 2-16| po_annex = yes
cb_types.txt|1120 col 2-16| po_annex = yes
cb_types.txt|1179 col 2-16| po_annex = yes
cb_types.txt|1251 col 2-24| po_add_to_sphere = yes
cb_types.txt|1322 col 2-29| po_clear_union_sphere = yes
cb_types.txt|1401 col 2-26| po_remove_prestige = yes
cb_types.txt|1471 col 2-25| po_release_puppet = yes
cb_types.txt|1472 col 2-24| po_add_to_sphere = yes
cb_types.txt|1533 col 2-24| po_add_to_sphere = yes
cb_types.txt|1627 col 2-24| po_add_to_sphere = yes
cb_types.txt|1981 col 2-29| po_transfer_provinces = yes
cb_types.txt|2295 col 2-29| po_transfer_provinces = yes
cb_types.txt|2296 col 2-25| po_release_puppet = yes
cb_types.txt|2405 col 2-29| po_transfer_provinces = yes
cb_types.txt|2550 col 2-29| po_transfer_provinces = yes
cb_types.txt|2551 col 2-23| po_remove_cores = yes
cb_types.txt|2705 col 2-29| po_transfer_provinces = yes
cb_types.txt|2785 col 2-29| po_transfer_provinces = yes
cb_types.txt|2849 col 2-25| po_release_puppet = yes
cb_types.txt|2850 col 2-24| po_add_to_sphere = yes
cb_types.txt|2927 col 2-25| po_release_puppet = yes
cb_types.txt|2928 col 2-24| po_add_to_sphere = yes
cb_types.txt|2996 col 2-18| po_gunboat = yes
cb_types.txt|3021 col 2-22| po_disarmament = yes
cb_types.txt|3022 col 2-22| po_reparations = yes
cb_types.txt|3143 col 2-22| po_make_puppet = yes
cb_types.txt|3144 col 2-24| po_add_to_sphere = yes
cb_types.txt|3277 col 2-22| po_make_puppet = yes
cb_types.txt|3424 col 2-26| po_remove_prestige = yes
cb_types.txt|3524 col 2-23| po_demand_state = yes
cb_types.txt|3734 col 2-23| po_demand_state = yes
cb_types.txt|3885 col 2-23| po_demand_state = yes
cb_types.txt|3967 col 2-23| po_demand_state = yes
cb_types.txt|4142 col 2-23| po_demand_state = yes
cb_types.txt|4302 col 2-23| po_demand_state = yes
cb_types.txt|4469 col 2-23| po_demand_state = yes
cb_types.txt|4523 col 2-26| po_remove_prestige = yes
cb_types.txt|4524 col 2-22| po_disarmament = yes
cb_types.txt|4525 col 2-22| po_reparations = yes
cb_types.txt|4734 col 2-16| po_annex = yes
cb_types.txt|4831 col 2-23| po_demand_state = yes
cb_types.txt|4930 col 2-16| po_annex = yes
cb_types.txt|5077 col 2-16| po_annex = yes
cb_types.txt|5202 col 2-16| po_annex = yes
cb_types.txt|5327 col 2-16| po_annex = yes
cb_types.txt|5371 col 2-21| po_status_quo = yes
cb_types.txt|5405 col 2-22| po_reparations = yes
cb_types.txt|5452 col 2-22| po_reparations = yes
cb_types.txt|5453 col 2-21| po_disarmament = no
cb_types.txt|5496 col 2-26| po_remove_prestige = yes
cb_types.txt|5535 col 2-22| po_reparations = yes
cb_types.txt|5609 col 2-22| po_make_puppet = yes
cb_types.txt|5670 col 2-25| po_release_puppet = yes
cb_types.txt|5671 col 2-24| po_add_to_sphere = yes
cb_types.txt|5731 col 2-39| po_uninstall_communist_gov_type = yes
cb_types.txt|5786 col 2-26| po_remove_prestige = yes
cb_types.txt|5840 col 2-26| po_remove_prestige = yes
cb_types.txt|5894 col 2-26| po_remove_prestige = yes
cb_types.txt|5895 col 2-37| po_install_communist_gov_type = yes
cb_types.txt|5964 col 2-26| po_remove_prestige = yes
cb_types.txt|6038 col 2-26| po_remove_prestige = yes
cb_types.txt|6113 col 2-26| po_remove_prestige = yes
cb_types.txt|6114 col 2-37| po_install_communist_gov_type = yes
cb_types.txt|6193 col 2-23| po_demand_state = yes
cb_types.txt|6265 col 2-23| po_demand_state = yes
cb_types.txt|6300 col 2-17| po_colony = yes
cb_types.txt|6378 col 2-24| po_destroy_forts = yes
cb_types.txt|6379 col 2-30| po_destroy_naval_bases = yes
cb_types.txt|6422 col 2-26| po_remove_prestige = yes
cb_types.txt|6461 col 2-23| po_demand_state = yes
cb_types.txt|6523 col 2-16| po_annex = yes
cb_types.txt|6620 col 2-22| po_reparations = yes
cb_types.txt|6709 col 2-22| po_make_puppet = yes
cb_types.txt|6750 col 2-22| po_make_puppet = yes
cb_types.txt|6791 col 2-22| po_make_puppet = yes
cb_types.txt|6915 col 2-23| po_demand_state = yes
cb_types.txt|6996 col 2-22| po_reparations = yes

*)


let cb_types = symbol_table_init [
    (KEYWORD_SYMBOL "peace_order", VALUE_LIST (PARAM_VALUE KEYWORD));
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "sprite_index", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "is_triggered_only", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "months", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "crisis", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "construction_speed", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "badboy_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "prestige_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "peace_cost_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "penalty_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "always", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "break_truce_prestige_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "break_truce_infamy_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "break_truce_militancy_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "truce_months", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "good_relation_prestige_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "good_relation_infamy_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "good_relation_militancy_factor", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "can_use", COUNTRY_CONDITIONS);
        (KEYWORD_SYMBOL "on_add", COUNTRY_EFFECTS);
        (KEYWORD_SYMBOL "allowed_states", COUNTRY_CONDITIONS);
        (KEYWORD_SYMBOL "on_po_accepted", COUNTRY_EFFECTS);
        (KEYWORD_SYMBOL "po_disarmament", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_reparations", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "war_name", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "po_remove_cores", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_transfer_provinces", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_demand_state", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_add_to_sphere", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_remove_prestige", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_make_puppet", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_release_puppet", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_status_quo", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_install_communist_gov_type",PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_uninstall_communist_gov_type",PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_colony", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_destroy_forts", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_destroy_naval_bases", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_clear_union_sphere", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_gunboat", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_demand_states", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_annex", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_make_puppet", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "po_release_puppet", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "great_war_obligatory", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "mutual", PARAM_VALUE BOOL);
    ]
    ))]

let countries = symbol_table_init [
    (TYPE_SYMBOL TAG,PARAM_VALUE STRING);
]
let country_colors = symbol_table_init [
    (TYPE_SYMBOL TAG, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
        (KEYWORD_SYMBOL "color2", VALUE_LIST (PARAM_VALUE INT));
        (KEYWORD_SYMBOL "color3", VALUE_LIST (PARAM_VALUE INT));
    ]));
]
let crime = symbol_table_init [
    (TYPE_SYMBOL KEYWORD,APPEND_SYMBOLS([
        (KEYWORD_SYMBOL "trigger", POP_CONDITIONS);
    ],PROVINCE_MODIFIERS));

]
