open Pop_effects
open Country_effects
open Province_effects
open Exception 
open Trigger_string
let pops_effect_string effect = match effect with
|CONSCIOUSNESS v -> Printf.sprintf "consciousness %i" v 
|MILITANCY v -> Printf.sprintf "militancy %i" v  
|DOMINANT_ISSUE(v,y) -> Printf.sprintf "dominant_issue %s %i" v  y 
|IDEOLOGY (v,y) -> Printf.sprintf "ideology %i %s" v  y 
|LITERACY v -> Printf.sprintf "literacy %i" v  
|MONEY v -> Printf.sprintf "money %i" v 
|MOVE_ISSUE_PERCENTAGE (v,y,z) -> Printf.sprintf "move_issue_percentage %s %s %i" v y z 
|MOVE_POP v -> Printf.sprintf "move_pop %s" v 
|POP_TYPE v -> Printf.sprintf "pop_type %s" v 
|REDUCE_POP i -> Printf.sprintf "reduce_pop %i" i
|POP_EXCEPTION i -> Printf.sprintf "pop_exception:%s" (exception_string i)



let state_effect_string effect = match effect with 
|CHANGE_REGION_NAME  string -> Printf.sprintf "change_region_name %s" string
|FLASHPOINT_TENSION  int -> Printf.sprintf "flashpoint_tension %i" int 
|IS_SLAVE string -> Printf.sprintf "is_slave %s" string



let rec country_effect_string effect = match effect with

|ANY_POP(ls) -> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_pop:{\n%s\t}" mems

|ANY_OWNED (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_owned:{\n%s\t}" mems
|ANY_CORE (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_core:{\n%s\t}" mems
|ALL_CORE (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "all_core:{\n%s\t}" mems
|ANY_GREATER_POWER (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_greater_power:{\n%s\t}" mems
|ANY_NEIGHBOR_COUNTRY (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_neighbor_country:{\n%s\t}" mems
|ANY_OWNED_PROVINCE (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_owned_province:{\n%s\t}" mems

|ANY_SPHERE_MEMBER (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_sphere_member:{\n%s\t}" mems

|ANY_STATE (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_state:{\n%s\t}" mems
|ANY_SUBSTATE (ls)-> 
        let mems = List.fold_left (fun x y -> Printf.sprintf "%s\t%s\n" x (Trigger_string.string_condition y) ) "" ls in
        Printf.sprintf "any_substate:{\n%s\t}" mems

|ACTIVATE_TECHNOLOGY (string) -> Printf.sprintf            "activate_technology: %s "string                                   
|ADD_ACCEPTED_CULTURE (string) -> Printf.sprintf           "add_accepted_culture: %s "string                                   
|REMOVE_ACCEPTED_CULTURE (string) -> Printf.sprintf        "remove_accepted_culture: %s " string                                   
|ADD_COUNTRY_MODIFIER (string , int) -> Printf.sprintf     "add_country_modifier: %s %i" string  int                                    
|ADD_CRISIS_INTEREST (bool) -> Printf.sprintf              "add_crisis_interest: %b " bool                                   
|BADBOY (int) -> Printf.sprintf                            "badboy: %i " int
|BUILD_FACTORY_IN_CAPITAL_STATE (string) -> Printf.sprintf "build_factory_in_capital_state: %s "string                                   
|CAPITAL (string) -> Printf.sprintf                        "capital: %s "string                                   
|CIVILIZED (bool) -> Printf.sprintf                        "civilized: %b " bool                                   
|NATIONALVALUE (string) -> Printf.sprintf                  "nationalvalue: %s "string                                   
|PLURALITY (int) -> Printf.sprintf                         "plurality: %i "int                                   
|PRESTIGE (int) -> Printf.sprintf                          "prestige: %i "int                                   
|PRESTIGE_FACTOR (int) -> Printf.sprintf                   "prestige_factor: %i "int                                   
|PRIMARY_CULTURE (string) -> Printf.sprintf                "primary_culture: %s "string                                   
|RELIGION (string) -> Printf.sprintf                       "religion: %s "string                                   
|RESEARCH_POINTS (int) -> Printf.sprintf                   "research_points: %i "int                                   
|WAR_EXHAUSTION (int) -> Printf.sprintf                    "war_exhaustion: %i "int                                   
|YEARS_OF_RESEARCH (int) -> Printf.sprintf                 "years_of_research: %i " int                                   
|NATIONALIZE (bool) -> Printf.sprintf                      "nationalize: %b " bool                                   
|ECONOMIC_REFORM (string) -> Printf.sprintf                "economic_reform: %s "string                                   
|ELECTION (string) -> Printf.sprintf                       "election: %s "string                                   
|GOVERNMENT (string) -> Printf.sprintf                     "government: %s "string                                   
|MILITARY_REFORM (string) -> Printf.sprintf                "military_reform: %s "string                                   
|POLITICAL_REFORM (string) -> Printf.sprintf               "political_reform: %s "string                                   
|RULING_PARTY_IDEOLOGY (string) -> Printf.sprintf          "ruling_party_ideology: %s "string                                   
|SOCIAL_REFORM (string) -> Printf.sprintf                  "social_reform: %s "string                                   
|UPPER_HOUSE (string , string2) -> Printf.sprintf           "upper_house: %s %s" string  string2                                   
|ADD_CASUS_BELLI (string , string2 , int) -> Printf.sprintf "add_casus_belli: %s %s %i" string  string2  int                                   
|ANNEX_TO (string) -> Printf.sprintf                       "annex_to: %s "string                                   
|CASUS_BELLI (string , string2 , int) -> Printf.sprintf     "casus_belli: %s %s %i" string  string2  int                                   
|CREATE_ALLIANCE (string) -> Printf.sprintf                "create_alliance: %s "string                                   
|CREATE_VASSAL (string) -> Printf.sprintf                  "create_vassal: %s "string                                   
|DIPLOMATIC_INFLUENCE (string , int) -> Printf.sprintf     "diplomatic_influence: %s %i" string  int                                    
|END_MILITARY_ACCESS (string) -> Printf.sprintf            "end_military_access: %s "string                                   
|END_WAR (string) -> Printf.sprintf                        "end_war: %s "string                                   
|INHERIT (string) -> Printf.sprintf                        "inherit: %s "string                                   
|LEAVE_ALLIANCE (string) -> Printf.sprintf                 "leave_alliance: %s "string                                   
|MILITARY_ACCESS (string) -> Printf.sprintf                "military_access: %s "string                                   
|NEUTRALITY (bool) -> Printf.sprintf                       "neutrality: %b " bool                                   
|RELATION (string , int) -> Printf.sprintf                 "relation: %s %i" string  int                                    
|RELEASE (string) -> Printf.sprintf                        "release: %s "string                                   
|RELEASE_VASSAL (string) -> Printf.sprintf                 "release_vassal: %s "string                                   
|WAR (string) -> Printf.sprintf                            "war: %s "string
|WAR_DETAILED (string , string1 , string2 , bool) -> Printf.sprintf "war_detailed: %s %s %s %b" string  string1  string2  bool
|ADD_TAX_RELATIVE_INCOME (int) -> Printf.sprintf           "add_tax_relative_income: %i " int                                   
|RESOURCE (string) -> Printf.sprintf                       "resource: %s " string                                   
|TREASURY (int) -> Printf.sprintf                          "treasury: %i " int                                   
|CHANGE_TAG (string) -> Printf.sprintf                     "change_tag: %s " string                                   
|CLR_COUNTRY_FLAG (string) -> Printf.sprintf               "clr_country_flag: %s " string                                   
|SET_COUNTRY_FLAG (string) -> Printf.sprintf               "set_country_flag: %s " string                                   
|CLR_GLOBAL_FLAG (string) -> Printf.sprintf               "clr_global_flag: %s " string                                   
|SET_GLOBAL_FLAG (string) -> Printf.sprintf               "set_global_flag: %s " string                                   

|TAG(tag,effects)-> 
        let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (country_effect_string y) x ) "" effects) in
        (Printf.sprintf "Tag (effects):%s {\n%s\t}" tag x )
|CAPITAL_SCOPE (triggers)->
    let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
    (Printf.sprintf "Captial Scope: {\n%s\t}"  x )
|COUNTRY_TAG (tag, triggers)->
    let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
    (Printf.sprintf "Tag:%s {\n%s\t}" tag x )

|CULTURAL_UNION triggers-> 
    let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
    Printf.sprintf "cultural_union: {\n%s\t}" x 

|OVERLORD triggers-> 
    let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
    Printf.sprintf "overlord: {\n%s\t}" x
|REGION_NAME (s, triggers)-> 
        let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
        (Printf.sprintf "region_name:%s {\n%s\t}" s x )
|SPHERE_OWNER triggers->
    let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
    Printf.sprintf "sphere_owner: {\n%s\t}"  x 
|WAR_COUNTRIES triggers->
    let x = (List.fold_left  (fun x y->Printf.sprintf "\t\t%s\n%s"  (string_condition y) x ) "" triggers) in
    Printf.sprintf "sphere_owner: {\n%s\t}" x 


let province_effect_string effect = match effect with
|ASSIMILATE (v)-> Printf.sprintf "assimilate: %s" (string_of_bool v)
|ADD_CORE  (v)-> Printf.sprintf "add_core: %s"  v
|CHANGE_CONTROLLER  (v)-> Printf.sprintf "change_controller: %s"  v
|FORT (v)-> Printf.sprintf "fort: %s" (string_of_int v)
|INFRASTRUCTURE (v)-> Printf.sprintf "infrastructure: %s" (string_of_int v)
|LIFE_RATING (v)-> Printf.sprintf "life_rating: %s" (string_of_int v)
|NAVAL_BASE (v)-> Printf.sprintf "naval_base: %s" (string_of_int v)
|REMOVE_CORE  (v)-> Printf.sprintf "remove_core: %s"  v
|REMOVE_PROVINCE_MODIFIER  (v)-> Printf.sprintf "remove_province_modifier: %s"  v
|RGO_SIZE (v)-> Printf.sprintf "rgo_size: %s" (string_of_int v)
|SECEDE_PROVINCE  (v)-> Printf.sprintf "secede_province: %s"  v
|TRADE_GOODS  (v)-> Printf.sprintf "trade_goods: %s"  v
|CLR_PROVINCE_FLAG  (v)-> Printf.sprintf "clr_province_flag: %s"  v
|SET_PROVINCE_FLAG  (v)-> Printf.sprintf "set_province_flag: %s"  v
|ADD_PROVINCE_MODIFIER (v,y) -> Printf.sprintf "add_province_modifier: %s %i" v y 
|SUB_UNIT(v,y) ->  Printf.sprintf "sub_unit: %s %s" v y 
(*
|TAG(tag,effects) -> 
        Printf.sprintf "TAG Effects: %s{%s}" tag (List.fold_left (fun rest item-> String.concat rest [(country_effect_string item);"\n"]   ) "" effects);
*)
|CLR_GLOBAL_FLAG(x) -> Printf.sprintf "clr_global_flag:%s" x
|SET_GLOBAL_FLAG(x) -> Printf.sprintf "set_global_flag:%s" x




