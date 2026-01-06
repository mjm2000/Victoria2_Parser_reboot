open SymbolTable
open Commons
open Conditions
open Decisions
open Effects
open Events
open Modifiers
open Mtth
open Map
open TypeDef

let eu4_symbol_table = symbol_table_init ([
    Definition "country_conditions_def" , SubTable country_conditions ;
    Definition "province_conditions_def" , SubTable province_conditions ;
    Definition "decision_def" , SubTable decisions ;
    Definition "country_effects_def" , SubTable country_effects ;
    Definition "province_effects_def" , SubTable province_effects ;
    Definition "country_modifiers_def" , SubTable country_modifiers ;
    Definition "province_modifiers_def" , SubTable province_modifiers ;
    Definition "country_mtth" , SubTable country_mtth ;
    Definition "province_mtth" , SubTable province_mtth ;
    Definition "event_def" , SubTable events ;
    Definition "areas_def" , SubTable areas ;
    Definition "regions_def" , SubTable regions ;
    Definition "superregions_def" , SubTable superregions ;
    Definition "continents_def" , SubTable continents ;
    Definition "provincegroups_def" , SubTable provincegroups ;
    Definition "scope", (TypeOption [Literal "ROOT";Literal"FROM";Literal"PREV";Literal "THIS"]);
    Definition "target", TypeOption [Type "scope";Type "country_tag"] ;
] @ commons)

let eu4_paths =
    [
        "map/continent.txt", "continents_def";
        (*
        "map/area.txt", "areas_def";
        "map/region.txt", "regions_def";
        "map/superregion.txt", "superregions_def";
        "map/provincegroup.txt", "provincegroups_def";
        *)
        "common/countries", "country_def";
        "events", "event_def";
        "decisions", "decision_def";
        "common/institutions", "institutions_def";
        "common/estates", "estate_privileges_def";
        "common/advisortypes", "advisor_types_def";
        "common/tradegoods", "trade_goods_def";
        "common/governments", "government_types_def";
        "common/ideas", "idea_groups_def";
        "common/bookmarks", "bookmarks_def";
        "common/religions", "religions_def";
        "common/cb_types", "cb_types_def";
        "common/buildings", "buildings_def";
        "common/static_modifiers", "static_modifiers_def";
    ]
