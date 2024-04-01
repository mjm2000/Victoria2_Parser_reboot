open Lexer
open Trigger
let rec string_condition condition =
    let string_condition_list prefix v = 
        let str_list = List.map (fun x -> Printf.sprintf "\t\t%s\n" (string_condition x) ) v 
    in
    String.concat "" (prefix::"{\n"::str_list @["\t}\n"])
    in
    match condition with
| CONDITION_TYPE_ERROR(values,expected) ->
    let rec check_error out values expected =
        match values,expected with 
    |(type_value,str,(x,y))::vrs,type_expected::ers when type_value != type_expected ->
        let v = 
            (Printf.sprintf "%s Error at %i:%i value:%s type:%s|expected:%s\n" out x y str 
            (lexem_to_str type_value) 
            (lexem_to_str type_value))
        in
        check_error v vrs ers
    |_::vrs,_::ers -> check_error out vrs ers
    |_,_ -> out
    in
    check_error "" values expected
| CONDITION_UNEXPECTED_ERROR(type_value,str,(x,y))-> Printf.sprintf "Error: Unexpected word at (%i,%i) %s of type %s" x y str (lexem_to_str type_value)  
| ACCEPTED_CULTURE(v)->Printf.sprintf "accepted_culture:%s" v
| ADMINISTRATION_SPENDING(v)->Printf.sprintf "administration_spending:%s" ( string_of_int v)
| AGREE_WITH_RULING_PARTY(v)->Printf.sprintf "agree_with_ruling_party:%s" ( string_of_float v)
| AI(v)->Printf.sprintf "ai:%s" ( string_of_bool v)
| ALLIANCE_WITH(v)->Printf.sprintf "alliance_with:%s" v
| ALLOW_MULTIPLE_INSTANCES(v)->Printf.sprintf "allow_multiple_instances:%s" ( string_of_bool v)
| AND(v)-> string_condition_list "AND:" v
| OR(v)-> string_condition_list "OR:" v
| NOT(v)-> string_condition_list "NOT:" v
| AVERAGE_CONSCIOUSNESS(v)->Printf.sprintf "average_consciousness:%s" ( string_of_float v)
| AVERAGE_MILITANCY(v)->Printf.sprintf "average_militancy:%s" ( string_of_float v)
| BADBOY(v)->Printf.sprintf "badboy:%s" ( string_of_float v)
| BIG_PRODUCER(v)->Printf.sprintf "big_producer:%s" v
| BLOCKADE(v)->Printf.sprintf "blockade:%s" ( string_of_int v)
| BRIGADES_COMPARE(v)->Printf.sprintf "brigades_compare:%s" ( string_of_float v)
| CAN_BUILD_FACTORY(v)->Printf.sprintf "can_build_factory:%s" ( string_of_bool v)
| CAN_BUILD_FACTORY_IN_CAPITAL_STATE(v)->Printf.sprintf "can_build_factory_in_capital_state:%s" v
| CAN_CREATE_VASSALS(v)->Printf.sprintf "can_create_vassals:%s" ( string_of_bool v)
| CAN_NATIONALIZE(v)->Printf.sprintf "can_nationalize:%s" ( string_of_bool v)
| CAPITAL(v)->Printf.sprintf "capital:%s" v
| CASH_RESERVES(v)->Printf.sprintf "cash_reserves:%s" ( string_of_int v)
| CASUS_BELLI(v)->Printf.sprintf "casus_belli:%s" v
| CHECK_VARIABLE(v1,v2)->Printf.sprintf "check_variable:%s %s"  v1  (string_of_int v2)
| CITIZENSHIP_POLICY(v)->Printf.sprintf "citizenship_policy:%s" v
| CIVILIZATION_PROGRESS(v)->Printf.sprintf "civilization_progress:%s" ( string_of_float v)
| CIVILIZED(v)->Printf.sprintf "civilized:%s" ( string_of_bool v)
| COLONIAL_NATION(v)->Printf.sprintf "colonial_nation:%s" ( string_of_bool v)
| CONSCIOUSNESS(v)->Printf.sprintf "consciousness:%s" ( string_of_int v)
| CONSTRUCTING_CB_PROGRESS(v)->Printf.sprintf "constructing_cb_progress:%s" ( string_of_float v )
| CONSTRUCTING_CB_TYPE(v)->Printf.sprintf "constructing_cb_type:%s" v
| CONTINENT(v)->Printf.sprintf "continent:%s" v
| CONTROLLED_BY(v)->Printf.sprintf "controlled_by:%s" v
| CONTROLLED_BY_REBELS(v)->Printf.sprintf "controlled_by_rebels:%s" ( string_of_bool v)
| CONTROLS(v)->Printf.sprintf "controls:%s" v
| COUNTRY_UNITS_IN_PROVINCE(v)->Printf.sprintf "country_units_in_province:%s" v
| COUNTRY_UNITS_IN_STATE(v)->Printf.sprintf "country_units_in_state:%s" v
| CRIME_FIGHTING(v)->Printf.sprintf "crime_fighting:%s" ( string_of_int v)
| CRIME_HIGHER_THAN_EDUCATION(v)->Printf.sprintf "crime_higher_than_education:%s" ( string_of_bool v)
| CRISIS_EXIST(v)->Printf.sprintf "crisis_exist:%s" ( string_of_bool v)
| CULTURE(v)->Printf.sprintf "culture:%s" v
| CULTURE_HAS_UNION_TAG(v)->Printf.sprintf "culture_has_union_tag:%s" ( string_of_bool v)
| DIPLOMATIC_INFLUENCE(str,v)->(Printf.sprintf "diplomatic_influence:%s,%s"  str  (string_of_int v))
| END_OF_FILE(x,y)-> Printf.sprintf "Error:,unexpected of file %i %i" x y
| ECONOMIC_POLICY(v)->Printf.sprintf "economic_policy:%s" v
| ECONOMIC_REFORM_NAME(v)->Printf.sprintf "economic_reform_name:%s" v
| EDUCATION_SPENDING(v)->Printf.sprintf "education_spending:%s" ( string_of_int v)
| ELECTION(v)->Printf.sprintf "election:%s" ( string_of_bool v)
| EMPTY(v)->Printf.sprintf "empty:%s" ( string_of_bool v)
| EVERYDAY_NEEDS(v)->Printf.sprintf "everyday_needs:%s" ( string_of_int v)
| EXISTS(v)->Printf.sprintf "exists:%s" v
| FIRE_ONLY_ONCE(v)->Printf.sprintf "fire_only_once:%s" ( string_of_bool v)
| FLASHPOINT_TENSION(v)->Printf.sprintf "flashpoint_tension:%s" ( string_of_int v)
| GOVERNMENT(v)->Printf.sprintf "government:%s" v
| GREAT_WARS_ENABLED(v)->Printf.sprintf "great_wars_enabled:%s" ( string_of_bool v)
| HAS_BUILDING(v)->Printf.sprintf "has_building:%s" v
| HAS_COUNTRY_FLAG(v)->Printf.sprintf "has_country_flag:%s" v
| HAS_COUNTRY_MODIFIER(v)->Printf.sprintf "has_country_modifier:%s" v
| HAS_CRIME(v)->Printf.sprintf "has_crime:%s" v
| HAS_CULTURAL_SPHERE(v)->Printf.sprintf "has_cultural_sphere:%s" ( string_of_bool v)
| HAS_CULTURE_CORE(v)->Printf.sprintf "has_culture_core:%s" ( string_of_bool v)
| HAS_EMPTY_ADJACENT_PROVINCE(v)->Printf.sprintf "has_empty_adjacent_province:%s" ( string_of_bool v)
| HAS_EMPTY_ADJACENT_STATE(v)->Printf.sprintf "has_empty_adjacent_state:%s" ( string_of_bool v)
| HAS_FACTORIES(v)->Printf.sprintf "has_factories:%s" ( string_of_bool v)
| HAS_FLASHPOINT(v)->Printf.sprintf "has_flashpoint:%s" ( string_of_bool v)
| HAS_GLOBAL_FLAG(v)->Printf.sprintf "has_global_flag:%s" v
| HAS_LEADER(v)->Printf.sprintf "has_leader:%s" v
| HAS_NATIONAL_MINORITY(v)->Printf.sprintf "has_national_minority:%s" ( string_of_bool v)
| HAS_POP_CULTURE(v)->Printf.sprintf "has_pop_culture:%s" v
| HAS_POP_RELIGION(v)->Printf.sprintf "has_pop_religion:%s" v
| HAS_POP_TYPE(v)->Printf.sprintf "has_pop_type:%s" v
| HAS_PROVINCE_FLAG(v)->Printf.sprintf "has_province_flag:%s" v
| HAS_PROVINCE_MODIFIER(v)->Printf.sprintf "has_province_modifier:%s" v
| HAS_RECENTLY_LOST_WAR(v)->Printf.sprintf "has_recently_lost_war:%s" ( string_of_bool v)
| HAS_RECENT_IMIGRATION(v)->Printf.sprintf "has_recent_imigration:%s" ( string_of_float v)
| HAS_UNCLAIMED_CORES(v)->Printf.sprintf "has_unclaimed_cores:%s" ( string_of_bool v)
| HAVE_CORE_IN(v)->Printf.sprintf "have_core_in:%s" v
| LABEL_VALUE(string,v)->Printf.sprintf "Value:%s:%s"  string  (v)
| LABEL_IDEOLOGY(string,v)->Printf.sprintf "Ideology:%s,%s"  string  (string_of_int v)
| IDEOLOGY(v)->Printf.sprintf "ideology:%s" v
| IMMEDIATE(v)->Printf.sprintf "immediate:%s" v
| INDUSTRIAL_SCORE(v)->Printf.sprintf "industrial_score:%s" ( string_of_int v)
| INVENTION(v)->Printf.sprintf "invention:%s" v
| INVOLVED_IN_CRISIS(v)->Printf.sprintf "involved_in_crisis:%s" ( string_of_bool v)
| IN_DEFAULT(v)->Printf.sprintf "in_default:%s" ( string_of_bool v)
| IN_SPHERE(v)->Printf.sprintf "in_sphere:%s" v
| IS_ACCEPTED_CULTURE(v)->Printf.sprintf "is_accepted_culture:%s" ( string_of_bool v)
| IS_BLOCKADED(v)->Printf.sprintf "is_blockaded:%s" ( string_of_bool v)
| IS_CANAL_ENABLED(v)->Printf.sprintf "is_canal_enabled:%s" v
| IS_CAPITAL(v)->Printf.sprintf "is_capital:%s" ( string_of_bool v)
| IS_CLAIM_CRISIS(v)->Printf.sprintf "is_claim_crisis:%s" ( string_of_bool v)
| IS_COASTAL(v)->Printf.sprintf "is_coastal:%s" ( string_of_bool v)
| IS_COLONIAL(v)->Printf.sprintf "is_colonial:%s" ( string_of_bool v)
| IS_COLONIAL_CRISIS(v)->Printf.sprintf "is_colonial_crisis:%s" ( string_of_bool v)
| IS_CORE(v)->Printf.sprintf "is_core:%s" v
| IS_CULTURAL_UNION_BOOL(v)->Printf.sprintf "is_cultural_union_bool:%s" ( string_of_bool v)
| IS_CULTURAL_UNION_TAG(v)->Printf.sprintf "is_cultural_union_tag:%s" v
| IS_CULTURE_GROUP(v)->Printf.sprintf "is_culture_group:%s" v
| IS_DISARMED(v)->Printf.sprintf "is_disarmed:%s" ( string_of_bool v)
| IS_GREATER_POWER(v)->Printf.sprintf "is_greater_power:%s" ( string_of_bool v)
| IS_IDEOLOGY_ENABLED(v)->Printf.sprintf "is_ideology_enabled:%s" v
| IS_INDEPENDANT(v)->Printf.sprintf "is_independant:%s" ( string_of_bool v)
| IS_LIBERATION_CRISIS(v)->Printf.sprintf "is_liberation_crisis:%s" ( string_of_bool v)
| IS_MOBILISED(v)->Printf.sprintf "is_mobilised:%s" ( string_of_bool v)
| IS_NEXT_REFORM(v)->Printf.sprintf "is_next_reform:%s" v
| IS_OUR_VASSAL(v)->Printf.sprintf "is_our_vassal:%s" v
| IS_OVERSEAS(v)->Printf.sprintf "is_overseas:%s" ( string_of_bool v)
| IS_POSSIBLE_VASSAL(v)->Printf.sprintf "is_possible_vassal:%s" v
| IS_PRIMARY_CULTURE(v)->Printf.sprintf "is_primary_culture:%s" ( string_of_bool v)
| IS_SECONDARY_POWER(v)->Printf.sprintf "is_secondary_power:%s" ( string_of_bool v)
| IS_SPHERE_LEADER_OF(v)->Printf.sprintf "is_sphere_leader_of:%s" v
| IS_STATE_CAPITAL(v)->Printf.sprintf "is_state_capital:%s" ( string_of_bool v)
| IS_STATE_RELIGION(v)->Printf.sprintf "is_state_religion:%s" ( string_of_bool v)
| IS_SUBSTATE(v)->Printf.sprintf "is_substate:%s" ( string_of_bool v)
| IS_TRIGGERED_ONLY(v)->Printf.sprintf "is_triggered_only:%s" ( string_of_bool v)
| IS_VASSAL(v)->Printf.sprintf "is_vassal:%s" ( string_of_bool v)
| LIFE_NEEDS(v)->Printf.sprintf "life_needs:%s" ( string_of_int v)
| LIFE_RATING(v)->Printf.sprintf "life_rating:%s" ( string_of_int v)
| LITERACY(v)->Printf.sprintf "literacy:%s" ( string_of_int v)
| LOST_NATIONAL(v)->Printf.sprintf "lost_national:%s" ( string_of_int v)
| LUXURY_NEEDS(v)->Printf.sprintf "luxury_needs:%s" ( string_of_int v)
| MAJOR(v)->Printf.sprintf "major:%s" v
| MIDDLE_STRATA_EVERYDAY_NEEDS(v)->Printf.sprintf "middle_strata_everyday_needs:%s" ( string_of_float v)
| MIDDLE_STRATA_LIFE_NEEDS(v)->Printf.sprintf "middle_strata_life_needs:%s" ( string_of_float v)
| MIDDLE_STRATA_LUXURY_NEEDS(v)->Printf.sprintf "middle_strata_luxury_needs:%s" ( string_of_float v)
| MIDDLE_TAX(v)->Printf.sprintf "middle_tax:%s" ( string_of_int v)
| MILITANCY(v)->Printf.sprintf "militancy:%s" ( string_of_int v)
| MILITARY_ACCESS(v)->Printf.sprintf "military_access:%s" v
| MILITARY_REFORM_NAME(v)->Printf.sprintf "military_reform_name:%s" v
| MILITARY_SCORE_NAME(v)->Printf.sprintf "military_score_name:%s" v
| MILITARY_SCORE_NUMBER(v)->Printf.sprintf "military_score_number:%s" ( string_of_int v)
| MILITARY_SPENDING(v)->Printf.sprintf "military_spending:%s" ( string_of_int v)
| MINORITIES(v)->Printf.sprintf "minorities:%s" ( string_of_bool v)
| MONEY(v)->Printf.sprintf "money:%s" ( string_of_int v)
| MONTH(v)->Printf.sprintf "month:%s" ( string_of_int v)
| NATIONALVALUE(v)->Printf.sprintf "nationalvalue:%s" v
| NATIONAL_PROVINCES_OCCUPIED(v)->Printf.sprintf "national_provinces_occupied:%s" ( string_of_int v)
| NEIGHBOUR(v)->Printf.sprintf "neighbour:%s" v
| NUMBER_OF_STATES(v)->Printf.sprintf "number_of_states:%s" ( string_of_int v)
| NUM_OF_ALLIES(v)->Printf.sprintf "num_of_allies:%s" ( string_of_int v)
| NUM_OF_CITIES(v)->Printf.sprintf "num_of_cities:%s" ( string_of_int v)
| NUM_OF_PORTS(v)->Printf.sprintf "num_of_ports:%s" ( string_of_int v)
| NUM_OF_REVOLTS(v)->Printf.sprintf "num_of_revolts:%s" ( string_of_int v)
| NUM_OF_SUBSTATES(v)->Printf.sprintf "num_of_substates:%s" ( string_of_int v)
| NUM_OF_VASSALS(v)->Printf.sprintf "num_of_vassals:%s" ( string_of_int v)
| NUM_OF_VASSALS_NO_SUBSTATES(v)->Printf.sprintf "num_of_vassals_no_substates:%s" ( string_of_int v)
| OWNED_BY(v)->Printf.sprintf "owned_by:%s" v
| OWNS(v)->Printf.sprintf "owns:%s" v
| PART_OF_SPHERE(v)->Printf.sprintf "part_of_sphere:%s" ( string_of_bool v)
| POLITICAL_MOVEMENT(v)->Printf.sprintf "political_movement:%s" ( string_of_bool v)
| POLITICAL_MOVEMENT_STRENGTH(v)->Printf.sprintf "political_movement_strength:%s" ( string_of_int v)
| POLITICAL_REFORM_NAME(v)->Printf.sprintf "political_reform_name:%s" v
| POLITICAL_REFORM_WANT_NUMBER(v)->Printf.sprintf "political_reform_want_number:%s" ( string_of_float v)
| POLITICAL_REFORM_WANT_STRING(v)->Printf.sprintf "political_reform_want_string:%s" v
| POOR_STRATA_EVERYDAY_NEEDS(v)->Printf.sprintf "poor_strata_everyday_needs:%s" ( string_of_float v)
| POOR_STRATA_LIFE_NEEDS(v)->Printf.sprintf "poor_strata_life_needs:%s" ( string_of_float v)
| POOR_STRATA_LUXURY_NEEDS(v)->Printf.sprintf "poor_strata_luxury_needs:%s" ( string_of_float v)
| POOR_TAX(v)->Printf.sprintf "poor_tax:%s" ( string_of_int v)
| POP_MAJORITY_CULTURE(v)->Printf.sprintf "pop_majority_culture:%s" v
| POP_MAJORITY_IDEOLOGY(v)->Printf.sprintf "pop_majority_ideology:%s" v
| POP_MAJORITY_ISSUE(v)->Printf.sprintf "pop_majority_issue:%s" v
| POP_MAJORITY_RELIGION(v)->Printf.sprintf "pop_majority_religion:%s" v
| POP_MILITANCY(v)->Printf.sprintf "pop_militancy:%s" ( string_of_int v)
| POP_TYPE(string,v)->Printf.sprintf "%s:pop_type:%s"  string  (string_of_int v)
| PORT(v)->Printf.sprintf "port:%s" ( string_of_bool v)
| PRESTIGE(v)->Printf.sprintf "prestige:%s" ( string_of_int v)
| PRIMARY_CULTURE(v)->Printf.sprintf "primary_culture:%s" v
| PRODUCES(v)->Printf.sprintf "produces:%s" v
| PROVINCE_CONTROL_DAYS(v)->Printf.sprintf "province_control_days:%s" ( string_of_float v)
| PROVINCE_ID(v)->Printf.sprintf "province_id:%s" v
| RANK(v)->Printf.sprintf "rank:%s" ( string_of_int v)
| REBEL_POWER_FRACTION(v)->Printf.sprintf "rebel_power_fraction:%s" ( string_of_int v)
| RECRUITED_PERCENTAGE(v)->Printf.sprintf "recruited_percentage:%s" ( string_of_int v)
| REGION(v)->Printf.sprintf "region:%s" v
| RELATION(string,v)->Printf.sprintf "%s:relation:%s"  string ( string_of_int v)
| RELIGION(v)->Printf.sprintf "religion:%s" v
| RELIGIOUS_POLICY(v)->Printf.sprintf "religious_policy:%s" v
| REVOLT_PERCENTAGE(v)->Printf.sprintf "revolt_percentage:%s" ( string_of_int v)
| RICH_STRATA_EVERYDAY_NEEDS(v)->Printf.sprintf "rich_strata_everyday_needs:%s" ( string_of_float v)
| RICH_STRATA_LIFE_NEEDS(v)->Printf.sprintf "rich_strata_life_needs:%s" ( string_of_float v)
| RICH_STRATA_LUXURY_NEEDS(v)->Printf.sprintf "rich_strata_luxury_needs:%s" ( string_of_float v)
| RICH_TAX(v)->Printf.sprintf "rich_tax:%s" ( string_of_int v)
| RULING_PARTY(v)->Printf.sprintf "ruling_party:%s" v
| RULING_PARTY_IDEOLOGY(v)->Printf.sprintf "ruling_party_ideology:%s" v
| SLAVERY(v)->Printf.sprintf "slavery:%s" v
| SOCIAL_MOVEMENT(v)->Printf.sprintf "social_movement:%s" ( string_of_bool v)
| SOCIAL_MOVEMENT_STRENGTH(v)->Printf.sprintf "social_movement_strength:%s" v
| SOCIAL_REFORM_NAME(v)->Printf.sprintf "social_reform_name:%s" v
| SOCIAL_REFORM_WANT_NUMBER(v)->Printf.sprintf "social_reform_want_number:%s" ( string_of_float v)
| SOCIAL_REFORM_WANT_NAME(v)->Printf.sprintf "social_reform_want_name:%s" v
| SOCIAL_SPENDING(v)->Printf.sprintf "social_spending:%s" ( string_of_int v)
| STATE_ID(v)->Printf.sprintf "state_id:%s" v
| STRATA(v)->Printf.sprintf "strata:%s" v
| STRONGER_ARMY_THAN(v)->Printf.sprintf "stronger_army_than:%s" v
| SUBSTATE_OF(v)->Printf.sprintf "substate_of:%s" v
| TAG(v)->Printf.sprintf "tag:%s" v
| TERRAIN(v)->Printf.sprintf "terrain:%s" v
| TECHNOLOGY(string,v)->Printf.sprintf "%s:technology:%s"  string ( string_of_int v)
| THIS_CULTURE_UNION(v)->Printf.sprintf "this_culture_union:%s" v
| TOTAL_AMOUNT_OF_DIVISIONS(v)->Printf.sprintf "total_amount_of_divisions:%s" ( string_of_int v)
| TOTAL_AMOUNT_OF_SHIPS(v)->Printf.sprintf "total_amount_of_ships:%s" ( string_of_int v)
| TOTAL_DEFENSIVES(v)->Printf.sprintf "total_defensives:%s" ( string_of_int v)
| TOTAL_NUM_OF_PORTS(v)->Printf.sprintf "total_num_of_ports:%s" ( string_of_int v)
| TOTAL_OFFENSIVES(v)->Printf.sprintf "total_offensives:%s" ( string_of_int v)
| TOTAL_OF_OURS_SUNK(v)->Printf.sprintf "total_of_ours_sunk:%s" ( string_of_int v)
| TOTAL_POPS(v)->Printf.sprintf "total_pops:%s" v
| TOTAL_SEA_BATTLES(v)->Printf.sprintf "total_sea_battles:%s" ( string_of_int v)
| TOTAL_SUNK_BY_US(v)->Printf.sprintf "total_sunk_by_us:%s" ( string_of_int v)
| TRADE_GOODS(v)->Printf.sprintf "trade_goods:%s" v
| TRADE_POLICY(v)->Printf.sprintf "trade_policy:%s" v
| TRUCE_WITH(v)->Printf.sprintf "truce_with:%s" v
| TYPE(v)->Printf.sprintf "type:%s" v
| UNEMPLOYMENT(v)->Printf.sprintf "unemployment:%s" ( string_of_int v)
| UNEMPLOYMENT_BY_TYPE(v)->Printf.sprintf "unemployment_by_type:%s" v
| UNITS_IN_PROVINCE(v)->Printf.sprintf "units_in_province:%s" ( string_of_int v)
| UNIT_HAS_LEADER(v)->Printf.sprintf "unit_has_leader:%s" ( string_of_bool v)
| UNIT_IN_BATTLE(v)->Printf.sprintf "unit_in_battle:%s" ( string_of_bool v)
| UPPER_HOUSE(string,v)->Printf.sprintf "upper_house name=%s value=%s"  string ( string_of_float v)
| VASSAL_OF(v)->Printf.sprintf "vassal_of:%s" v
| WAR(v)->Printf.sprintf "war:%s" ( string_of_bool v)
| WAR_EXHAUSTION(v)->Printf.sprintf "war_exhaustion:%s" ( string_of_int v)
| WAR_POLICY(v)->Printf.sprintf "war_policy:%s" v
| WAR_SCORE(v)->Printf.sprintf "war_score:%s" ( string_of_int v)
| WAR_WITH(v)->Printf.sprintf "war_with:%s" v
| WORK_AVAILABLE(v)->Printf.sprintf "work_available:%s" v
| YEAR(v)->Printf.sprintf "year:%s" ( string_of_int v)
| PARTY_ISSUE(string,v)->Printf.sprintf "%s:party_issue:%s"  string ( string_of_int v)
| ANY_POP (ls)->              string_condition_list  "any_pop" ls
| ALL_CORE (ls)->             string_condition_list  "all_core" ls
| ANY_CORE (ls)->             string_condition_list  "any_core" ls
| ANY_GREATER_POWER (ls)->    string_condition_list  "any_greater_power" ls
| ANY_NEIGHBOR_COUNTRY (ls)-> string_condition_list  "any_neighbor_country" ls
| ANY_OWNED_PROVINCE (ls)->   string_condition_list  "any_owned_province" ls
| ANY_SPHERE_MEMBER (ls)->    string_condition_list  "any_sphere_member" ls
| ANY_STATE (ls)->            string_condition_list  "any_state" ls
| ANY_SUBSTATE (ls)->         string_condition_list  "any_substate" ls
| CAPITAL_SCOPE (ls)->        string_condition_list  "capital_scope" ls
| COUNTRY_TAG (ls)->          string_condition_list  "country_tag" ls
| CULTURAL_UNION (ls)->       string_condition_list  "cultural_union" ls
| OVERLORD (ls)->             string_condition_list  "overlord" ls
| REGION_NAME (ls)->          string_condition_list  "region_name" ls
| SPHERE_OWNER (ls)->         string_condition_list  "sphere_owner" ls

let triggers_string values= String.concat "" (List.map (fun a -> Printf.sprintf "\t%s\n" (string_condition a))  values) 
let print_trigger values= List.iter (fun x -> Printf.printf "\t%s\n" (string_condition x)) values;
