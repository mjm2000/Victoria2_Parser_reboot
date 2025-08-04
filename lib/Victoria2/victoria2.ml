open SymbolTable
open Commons
open Conditions
open Decisions
open Effects
open Events
open Modifiers
open Mtth
open TypeDef
open Map 
open Technology
open Units
open Pops
let victoria2_symbol_table =  symbol_table_init ([
    Definition("country_conditions_def"),SubTable(country_conditions);
    Definition("province_conditions_def"),SubTable(province_conditions);
    Definition("pop_conditions_def"),SubTable(pop_conditions);
    Definition("decision_def"),SubTable(decisions);
    Definition("country_effects_def"),SubTable(country_effects);
    Definition("province_effects_def"),SubTable(province_effects);
    Definition("pop_effects_def"),SubTable(pop_effects);
    Definition("country_modifiers_def"),SubTable(country_modifiers);
    Definition("province_modifiers_def"),SubTable(province_modifiers);
    Definition("country_mtth"),SubTable(country_mtth);
    Definition("province_mtth"),SubTable(province_mtth);
    Definition("event_def"),SubTable(events);
    Definition("state_effects_def"),SubTable(state_effects);
    Definition("state_conditions_def"),SubTable(state_conditions);
    Definition("continents_def"),SubTable(continents);
    Definition("technology_def"),SubTable(technology);
    Definition("unit_def"),SubTable(unit_def);
    Definition("pop_file_def"),CatalogFile("pop_type", ".*/([^/]+?)\\.[^/.]+$",SubTable(pop_file));
    Definition("regions_def"),SubTable(regions);
] @ commons)

let victoria2_paths =
    [

        "common/static_modifiers.txt","static_modifiers_def";
        "common/buildings.txt","buildings";


        "common/cb_types.txt","cb_types_def"; 
        "common/event_modifiers.txt","event_modifiers_def";

        "common/production_types.txt","production_type_def";
        "common/religion.txt","religions_def";
        "common/national_focus.txt","national_focus_group_def";
        "common/traits.txt","trait_file_def";
        "common/triggered_modifiers.txt","triggered_modifiers_def";


        "common/rebel_types.txt","rebel_type_def";
        "common/religion.txt","religions_def";
        "common/bookmarks.txt","bookmarks_def";
    


        "common/technology.txt","technology_group_def";

        "decisions","decision_def";

        "events","event_def";

        "common/countries.txt","countries_def";
        "map/continent.txt","continents_def";
        "poptypes","pop_file_def";
        "common/ideologies.txt","ideologies_def";
        (*
        "common/on_actions.txt","on_actions_def"; 
        *)
        "common/issues.txt","issues_file_def";

        "common/graphicalculturetype.txt","graphical_culture_type_def";

        "common/cultures.txt","culture_groups_def"; 
        "technologies","technology_def";

        "common/technology.txt","technology_group_def";

        "units","unit_def";

        "common/goods.txt","goods_def";

        "map/region.txt","regions_def";
    ]

