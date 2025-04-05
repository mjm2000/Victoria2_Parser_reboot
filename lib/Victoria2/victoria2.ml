open SymbolTable
open Commons
open Conditions
open Decisions
open Effects
open Events
open Modifiers
open Mtth
open TypeDef
let victoria2_symbol_table =  symbol_table_init ([
    Definition("country_conditions_def"),PARAM_LIST(country_conditions);
    Definition("province_conditions_def"),PARAM_LIST(province_conditions);
    Definition("pop_conditions_def"),PARAM_LIST(pop_conditions);
    Definition("decision_def"),PARAM_LIST(decisions);
    Definition("country_effects_def"),PARAM_LIST(country_effects);
    Definition("province_effects_def"),PARAM_LIST(province_effects);
    Definition("pop_effects_def"),PARAM_LIST(pop_effects);
    Definition("country_modifiers_def"),PARAM_LIST(country_modifiers);
    Definition("province_modifiers_def"),PARAM_LIST(province_modifiers);
    Definition("country_mtth"),PARAM_LIST(country_mtth);
    Definition("province_mtth"),PARAM_LIST(province_mtth);
    Definition("event_def"),PARAM_LIST(events);
    Definition("state_effects_def"),PARAM_LIST(state_effects);
    Definition("state_conditions_def"),PARAM_LIST(state_conditions);
] @ commons)

let victoria2_paths =
    [
        (*"common/static_modifiers.txt","static_modifiers_def";
        "common/buildings.txt","buildings";
        "common/countries.txt","countries_def";
        "common/cb_types.txt","cb_types_def"; 
        "common/cultures.txt","culture_groups_def"; 
        "common/event_modifiers.txt","event_modifiers_def";
        "common/goods.txt","goods_def";
        "common/religion.txt","religions_def";
        "common/technology.txt","technologies_def";
        "common/ideologies.txt","ideologies_def";
        "common/national_focus.txt","national_focus_group_def";
        "common/traits.txt","trait_file_def";
        "common/triggered_modifiers.txt","triggered_modifiers_def";
        "common/production_types.txt","production_type_def";
        "common/on_actions.txt","on_actions_def"; 
        "common/rebel_types.txt","rebel_type_def";
        "common/religion.txt","religions_def";
        "common/bookmarks.txt","bookmarks_def";
        "technologies/*","technology_def";
        *)
        "events","event_def";
        "decisions","decision_def";
    
    ]

