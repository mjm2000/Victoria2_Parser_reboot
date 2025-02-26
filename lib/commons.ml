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

let event_modifiers = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_OPTION [COUNTRY_MODIFIERS;PROVINCE_MODIFIERS]);
]

let goods = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL "cost", PARAM_VALUE FLOAT);
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "availiable_from_start", PARAM_VALUE BOOL);
        ]));
    ]));
]
(*
# If you add types, and use those tags, do not change them without changing everywhere they are used. #

## Conservative Parties ##
conservative_group = {
	# Conservatives #
	conservative = {
		color = { 10 10 250 }
		can_reduce_militancy = yes

		add_political_reform = {
			base = 0
			group = {
				modifier = {
					factor = 0.1
					OR = { 
						militancy = 3
						political_movement_strength = 0.3
					}
				}
				#modifier = {
				#	factor = 0.1
				#	OR = { 
				#		militancy = 4
				#		political_movement_strength = 0.4
				#	}
				#}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 5
						political_movement_strength = 0.5
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 6
						political_movement_strength = 0.6
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 7
						political_movement_strength = 0.7
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 8
						political_movement_strength = 0.8
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 9
						political_movement_strength = 0.9
					}
				}
			}
		}
		remove_political_reform = {
			base = 0
		}
		add_social_reform = {
			base = 0
			group = {
				modifier = {
					factor = 0.1
					OR = {
						militancy = 2
						social_movement_strength = 0.2
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 3
						social_movement_strength = 0.3
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 4
						social_movement_strength = 0.4
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 5
						social_movement_strength = 0.5
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 6
						social_movement_strength = 0.6
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 7
						social_movement_strength = 0.7
					}
				}
			}

		}
		remove_social_reform = {
			base = 0
		}
		add_military_reform = {
			base = 0.5
		}
		add_economic_reform = {
			base = -0.5
		}
	}

	# Reactionaries #
	reactionary = {
		color = { 30 30 100 }

		add_political_reform = {
			base = 0
		}
		remove_political_reform = {
			base = 1
		}
		add_social_reform = {
			base = 0
		}
		remove_social_reform = {
			base = 1
		}
		add_military_reform = {
			base = -1
		}
		add_economic_reform = {
			base = -1
		}
	}

	tribalist = {
		color = { 139 69 19 }

		add_political_reform = {
			base = 0
		}
		remove_political_reform = {
			base = 1
		}
		add_social_reform = {
			base = 0
		}
		remove_social_reform = {
			base = 1
		}
		add_military_reform = {
			base = -1
		}
		add_economic_reform = {
			base = -1
		}
	}
}

## Fascist & Ultra-Nationalist Parties ##
fascist_group = {
	# Fascists #
	fascist = {
		uncivilized = no
		color = { 60 60 60 }
		date = 1900.1.1

		add_political_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = fascist }
			}
		}
		remove_political_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = fascist }
			}
		}
		add_social_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = fascist }
			}
		}
		remove_social_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = fascist }
			}
		}
	}
}

## Liberal Parties ##
liberal_group = {
	# Radicals #
	anarcho_liberal = {
		color = { 150 150 10 }
		#I removed the date as Radicalism predates the game timeline, but requires all country files to be updated.
		uncivilized = no

		add_political_reform = {
			base = 1
		}
		remove_political_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = anarcho_liberal }
			}
		}
		add_social_reform = {
			base = 1
		}
		remove_social_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = anarcho_liberal }
			}
		}

		add_military_reform = {
			base = -1
		}

		add_economic_reform = {
			base = -1
		}
	}

	# Liberals #
	liberal = {
		color = { 255 255 0 }

		add_political_reform = {
			base = 1
		}
		remove_political_reform = {
			base = 0
		}
		add_social_reform = {
			base = 0
			group = {
				modifier = {
					factor = 0.1
					OR = { 
						militancy = 4
						social_movement_strength = 0.4
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 5
						social_movement_strength = 0.5
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 6
						social_movement_strength = 0.6
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 7
						social_movement_strength = 0.7
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 8
						social_movement_strength = 0.8
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 9
						social_movement_strength = 0.9
					}
				}
			}
		}
		remove_social_reform = {
			base = 0
		}
		add_military_reform = {
			base = 0.5
		}
		add_economic_reform = {
			base = 1
		}
	}
}

## Socialist & Communist Parties ##
socialist_group = {
	# Syndicalists #
	anarchist = {
		uncivilized = no
		color = { 150 60 60 }
		date = 1872.9.15 #The day the anarchists expelled by the IWA formed their own "Anti-authoritarian International"

		add_political_reform = {
			base = 1
		}
		remove_political_reform = {
			base = 0
		}
		add_social_reform = {
			base = 1
		}
		remove_social_reform = {
			base = 0
		}
	}

	# Communists #
	communist = {
		uncivilized = no
		color = { 150 10 10 }
		date = 1900.1.1 #Close The turn of the century and to the II Congress of the RSDLP during which the first split between marxist and revisionist social democrats was consummated in the August of 1903 (respectively Bolsheviks and Mensheviks)

		add_political_reform = {
			base = 0
			group = { #this was broken
				modifier = {
					factor = 0.3
					OR = {
						militancy = 8
						political_movement_strength = 0.7
					}
				}
				modifier = {
					factor = 0.3
					OR = {
						militancy = 9
						political_movement_strength = 0.8
					}
				}
				modifier = {
					factor = 0.3
					OR = {
						militancy = 9.5
						political_movement_strength = 0.9
					}
				}
			}
		}
		remove_political_reform = {
			base = 1
		}
		add_social_reform = {
			base = 1
			modifier = {
				factor = -1
				NOT = { ruling_party_ideology = communist }
			}
		}
		remove_social_reform = {
			base = 0
		}
	}

	# Socialists #
	socialist = {
		uncivilized = no
		color = { 255 0 0 }
		date = 1864.9.29 #First International

		add_political_reform = {
			base = 0
			group = {
				modifier = {
					factor = 0.1
					OR = {
						militancy = 1
						political_movement_strength = 0.1
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 2
						political_movement_strength = 0.2
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 3
						political_movement_strength = 0.3
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 4
						political_movement_strength = 0.4
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 5
						political_movement_strength = 0.5
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 6
						political_movement_strength = 0.6
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 7
						political_movement_strength = 0.7
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 8
						political_movement_strength = 0.8
					}
				}
				modifier = {
					factor = 0.1
					OR = {
						militancy = 9
						political_movement_strength = 0.9
					}
				}
			}
		}
		remove_political_reform = {
			base = 0
		}
		add_social_reform = {
			base = 1
		}
		remove_social_reform = {
			base = 0
		}
	}
}


 *)

let ideologies = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL "cost", PARAM_VALUE FLOAT);
            (KEYWORD_SYMBOL "date", PARAM_VALUE DATE);
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "can_reduce_militancy", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "add_political_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_VALUE FLOAT);
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],COUNTRY_CONDITIONS));
                ]));
            ]))
        ]));
    ]));
]
