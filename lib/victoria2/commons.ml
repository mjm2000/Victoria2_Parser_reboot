open TypeDef
open SymbolTable


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


let ideologies = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST(symbol_table_init [
            (KEYWORD_SYMBOL "cost", PARAM_VALUE FLOAT);
            (KEYWORD_SYMBOL "date", PARAM_VALUE DATE);
            (KEYWORD_SYMBOL "color", VALUE_LIST (PARAM_VALUE INT));
            (KEYWORD_SYMBOL "can_reduce_militancy", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "remove_political_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));
            (KEYWORD_SYMBOL "add_political_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));
            (KEYWORD_SYMBOL "add_social_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));
            (KEYWORD_SYMBOL "remove_social_reform", PARAM_LIST (symbol_table_init [
                (KEYWORD_SYMBOL "base", PARAM_OPTION ([
                    PARAM_VALUE FLOAT;
                    PARAM_VALUE INT
                ]));
                (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
                        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                            PARAM_VALUE FLOAT;
                            PARAM_VALUE INT
                        ]));
                    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
                ]));
            ]));


            

        ]));
    ]));
]

let issues = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, (APPEND_SYMBOLS ([
                (KEYWORD_SYMBOL "rules",PARAM_LIST (symbol_table_init [
                    (TYPE_SYMBOL KEYWORD, PARAM_VALUE BOOL); 
                ]));
                (KEYWORD_SYMBOL "allow", COUNTRY_CONDITIONS);
                (KEYWORD_SYMBOL "on_execute", PARAM_LIST (symbol_table_init [
                    (KEYWORD_SYMBOL "effect", COUNTRY_EFFECTS);
                ]));

                ],COUNTRY_MODIFIERS);
            ));
        ]));
    ]));
]
let national_focus =  symbol_table_init [
    (TYPE_SYMBOL KEYWORD, (PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD,(APPEND_SYMBOLS ([
            (TYPE_SYMBOL KEYWORD, PARAM_OPTION [PARAM_VALUE INT;PARAM_VALUE FLOAT]);
            (KEYWORD_SYMBOL "limit",PROVINCE_CONDITIONS);
            (KEYWORD_SYMBOL "own_provinces", PARAM_VALUE BOOL);
            (KEYWORD_SYMBOL "has_flashpoint", PARAM_VALUE BOOL);

        ],PROVINCE_MODIFIERS))); 
    ])));
     
]

let nationalvalue = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, COUNTRY_MODIFIERS);
]
let on_actions = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL INT, PARAM_VALUE INT); 
    ]));
]
let pop_types = symbol_table_init [
    (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
        PARAM_VALUE FLOAT;
        PARAM_VALUE INT
    ]));
    (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
        (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
            PARAM_VALUE FLOAT;
            PARAM_VALUE INT
        ]));
    ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));

    (KEYWORD_SYMBOL "group", PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "modifier", APPEND_SYMBOLS ([
            (KEYWORD_SYMBOL "factor", PARAM_OPTION ([
                PARAM_VALUE FLOAT;
                PARAM_VALUE INT
            ]));
        ],PARAM_OPTION [ POP_CONDITIONS; COUNTRY_CONDITIONS; PROVINCE_CONDITIONS]));
    ]));
]

let production_type = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "efficiency", PARAM_LIST(symbol_table_init [
            TYPE_SYMBOL KEYWORD,PARAM_VALUE FLOAT;
        ]));
        (KEYWORD_SYMBOL "owner", PARAM_LIST (symbol_table_init [
            KEYWORD_SYMBOL "poptype", PARAM_VALUE KEYWORD;
            KEYWORD_SYMBOL "effect", PARAM_VALUE KEYWORD;
            KEYWORD_SYMBOL "effect_multiplier", PARAM_VALUE FLOAT;
        ]));
        (KEYWORD_SYMBOL "employees", VALUE_LIST (PARAM_LIST (symbol_table_init [
            KEYWORD_SYMBOL "poptype", PARAM_VALUE KEYWORD;
            KEYWORD_SYMBOL "effect", PARAM_VALUE KEYWORD;
            KEYWORD_SYMBOL "amount", PARAM_VALUE FLOAT;
            KEYWORD_SYMBOL "effect_multiplier", PARAM_VALUE FLOAT;
        ])));
        (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "workforce", PARAM_OPTION [
            PARAM_VALUE FLOAT;
            PARAM_VALUE INT;
        ] );
        (KEYWORD_SYMBOL "value", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "input_goods", PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, NUMBER);
    
        ])); 
        (KEYWORD_SYMBOL "output_goods", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "bonus", PARAM_LIST (symbol_table_init [
            (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
            (KEYWORD_SYMBOL "value", NUMBER);
        ]));
        (KEYWORD_SYMBOL "farm", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "mine", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "is_coastal", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "type", CHOICE_VALUE ["rgo";"artisan";"factory"]);
    ]));
]


let rebel_type = symbol_table_init [
    (KEYWORD_SYMBOL "icon", PARAM_VALUE INT);
    (KEYWORD_SYMBOL "area", PARAM_VALUE KEYWORD);
    (KEYWORD_SYMBOL "break_alliance_on_win", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "government", PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_VALUE KEYWORD);
    ]));
    (KEYWORD_SYMBOL "defection", PARAM_VALUE KEYWORD);
    (KEYWORD_SYMBOL "independence", PARAM_VALUE KEYWORD);
    (KEYWORD_SYMBOL "defect_delay", PARAM_VALUE INT);
    (KEYWORD_SYMBOL "ideology", PARAM_VALUE KEYWORD);
    (KEYWORD_SYMBOL "allow_all_cultures", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "allow_all_religions", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "allow_all_ideologies", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "resilient", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "reinforcing", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "general", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "smart", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "unit_transfer", PARAM_VALUE BOOL);
    (KEYWORD_SYMBOL "occupation_mult", PARAM_VALUE FLOAT);
    (KEYWORD_SYMBOL "will_rise", PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "factor", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "modifier", 
            (APPEND_SYMBOLS([(KEYWORD_SYMBOL "factor", PARAM_VALUE BOOL)] , COUNTRY_CONDITIONS)) 
        );
    ]));
    (KEYWORD_SYMBOL "spawn_chance", PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "factor", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "modifier", 
            (APPEND_SYMBOLS([(KEYWORD_SYMBOL "factor", PARAM_VALUE BOOL)] , POP_CONDITIONS)) 
        );
    ]));
    (KEYWORD_SYMBOL "movement_evaluation", PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "factor", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "modifier", 
            (APPEND_SYMBOLS([(KEYWORD_SYMBOL "factor", PARAM_VALUE BOOL)] , PROVINCE_CONDITIONS)) 
        );
    ]));
    (KEYWORD_SYMBOL "siege_won_trigger", PROVINCE_CONDITIONS);
    (KEYWORD_SYMBOL "siege_won_effect",  PROVINCE_EFFECTS);
    (KEYWORD_SYMBOL "demands_enforced_trigger", COUNTRY_CONDITIONS);
    (KEYWORD_SYMBOL "demands_enforced_effect",  COUNTRY_EFFECTS);
] 

let religion = symbol_table_init [
    TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        KEYWORD_SYMBOL "icon", PARAM_VALUE INT;
        KEYWORD_SYMBOL "color", VALUE_LIST (NUMBER);
        KEYWORD_SYMBOL "pagan", PARAM_VALUE BOOL;
    ]) 
]
let static_modifiers = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, COUNTRY_MODIFIERS);
]

let tech_schools = symbol_table_init [
    (KEYWORD_SYMBOL "schools", PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST(symbol_table_init[
            (KEYWORD_SYMBOL "army_tech_research_bonus" ,NUMBER);
            (KEYWORD_SYMBOL "commerce_tech_research_bonus",NUMBER);
            (KEYWORD_SYMBOL "culture_tech_research_bonus" ,NUMBER);
            (KEYWORD_SYMBOL "industry_tech_research_bonus",NUMBER);
            (KEYWORD_SYMBOL "navy_tech_research_bonus" ,NUMBER);
            (KEYWORD_SYMBOL "unciv_economic_modifier" ,NUMBER);
            (KEYWORD_SYMBOL "unciv_military_modifier" ,NUMBER);
        ]));
    ]));
    (KEYWORD_SYMBOL "folders", PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, VALUE_LIST (PARAM_VALUE KEYWORD));
    ]));
]
let traits = symbol_table_init [
    (KEYWORD_SYMBOL "personality",PARAM_LIST (symbol_table_init [
        (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init[
            (KEYWORD_SYMBOL "attack",NUMBER);
            (KEYWORD_SYMBOL "defence",NUMBER);
            (KEYWORD_SYMBOL "morale",NUMBER);
            (KEYWORD_SYMBOL "organisation",NUMBER);
            (KEYWORD_SYMBOL "reconnaissance",NUMBER);
            (KEYWORD_SYMBOL "speed",NUMBER);
            (KEYWORD_SYMBOL "attrition",NUMBER);
            (KEYWORD_SYMBOL "experience",NUMBER);
            (KEYWORD_SYMBOL "reliability",NUMBER);

        ])) 
    ]))  
]
let triggered_modifiers = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, APPEND_SYMBOLS (
        [
        (KEYWORD_SYMBOL "trigger", COUNTRY_CONDITIONS);
        (KEYWORD_SYMBOL "icon", COUNTRY_MODIFIERS);
        ],COUNTRY_MODIFIERS)
        );
]
