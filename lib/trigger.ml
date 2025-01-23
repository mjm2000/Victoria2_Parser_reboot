exception Syntax_error of string
open Exception
open Pre_parser
open Lexer
type condition = 
| CONDITION_EXCEPTION of exception_value
| ACCEPTED_CULTURE of string
| ADMINISTRATION_SPENDING of int
| AGREE_WITH_RULING_PARTY of float
| AI of bool
| ALLIANCE_WITH of string
| ALLOW_MULTIPLE_INSTANCES of bool
| AND of condition list
| AVERAGE_CONSCIOUSNESS of float
| AVERAGE_MILITANCY of float
| BADBOY of float
| BIG_PRODUCER of string
| BLOCKADE of int
| BRIGADES_COMPARE of float
| CAN_BUILD_FACTORY of bool
| CAN_BUILD_FACTORY_IN_CAPITAL_STATE of string
| CAN_CREATE_VASSALS of bool
| CAN_NATIONALIZE of bool
| CAPITAL of string
| CASH_RESERVES of int
| CASUS_BELLI of string
| CHECK_VARIABLE of string * int
| CITIZENSHIP_POLICY of string
| CIVILIZATION_PROGRESS of float
| CIVILIZED of bool
| COLONIAL_NATION of bool
| CONSCIOUSNESS of int
| CONSTRUCTING_CB_PROGRESS of float
| CONSTRUCTING_CB_TYPE of string
| CONTINENT of string
| CONTROLLED_BY of string
| CONTROLLED_BY_REBELS of bool
| CONTROLS of string
| COUNTRY_UNITS_IN_PROVINCE of string
| COUNTRY_UNITS_IN_STATE of string
| CRIME_FIGHTING of int
| CRIME_HIGHER_THAN_EDUCATION of bool
| CRISIS_EXIST of bool
| CULTURE of string
| CULTURE_HAS_UNION_TAG of bool
| DIPLOMATIC_INFLUENCE of param list 
| END_OF_FILE  of int * int 
| ECONOMIC_POLICY of string
| ECONOMIC_REFORM_NAME of string
| EDUCATION_SPENDING of int
| ELECTION of bool
| EMPTY of bool
| EVERYDAY_NEEDS of int
| EXISTS of string
| FIRE_ONLY_ONCE of bool
| FLASHPOINT_TENSION of int
| GOVERNMENT of string
| GREAT_WARS_ENABLED of bool
| HAS_BUILDING of string
| HAS_COUNTRY_FLAG of string
| HAS_COUNTRY_MODIFIER of string
| HAS_CRIME of string
| HAS_CULTURAL_SPHERE of bool
| HAS_CULTURE_CORE of bool
| HAS_EMPTY_ADJACENT_PROVINCE of bool
| HAS_EMPTY_ADJACENT_STATE of bool
| HAS_FACTORIES of bool
| HAS_FLASHPOINT of bool
| HAS_GLOBAL_FLAG of string
| HAS_LEADER of string
| HAS_NATIONAL_MINORITY of bool
| HAS_POP_CULTURE of string
| HAS_POP_RELIGION of string
| HAS_POP_TYPE of string
| HAS_PROVINCE_FLAG of string
| HAS_PROVINCE_MODIFIER of string
| HAS_RECENTLY_LOST_WAR of bool
| HAS_RECENT_IMIGRATION of float
| HAS_UNCLAIMED_CORES of bool
| HAVE_CORE_IN of string
| LABEL_VALUE of string * string
| LABEL_IDEOLOGY of string * int 
| IDEOLOGY of string
| IMMEDIATE of string
| INDUSTRIAL_SCORE of int
| INVENTION of string
| INVOLVED_IN_CRISIS of bool
| IN_DEFAULT of bool
| IN_SPHERE of string
| IS_ACCEPTED_CULTURE of bool
| IS_BLOCKADED of bool
| IS_CANAL_ENABLED of string
| IS_CAPITAL of bool
| IS_CLAIM_CRISIS of bool
| IS_COASTAL of bool
| IS_COLONIAL of bool
| IS_COLONIAL_CRISIS of bool
| IS_CORE of string
| IS_CULTURAL_UNION_BOOL of bool
| IS_CULTURAL_UNION_TAG of string
| IS_CULTURE_GROUP of string
| IS_DISARMED of bool
| IS_GREATER_POWER of bool
| IS_IDEOLOGY_ENABLED of string
| IS_INDEPENDANT of bool
| IS_LIBERATION_CRISIS of bool
| IS_MOBILISED of bool
| IS_NEXT_REFORM of string
| IS_OUR_VASSAL of string
| IS_OVERSEAS of bool
| IS_POSSIBLE_VASSAL of string
| IS_PRIMARY_CULTURE of bool
| IS_SECONDARY_POWER of bool
| IS_SPHERE_LEADER_OF of string
| IS_STATE_CAPITAL of bool
| IS_STATE_RELIGION of bool
| IS_SUBSTATE of bool
| IS_TRIGGERED_ONLY of bool
| IS_VASSAL of bool
| LIFE_NEEDS of int
| LIFE_RATING of int
| LITERACY of int
| LOST_NATIONAL of int
| LUXURY_NEEDS of int
| MAJOR of string
| MIDDLE_STRATA_EVERYDAY_NEEDS of float
| MIDDLE_STRATA_LIFE_NEEDS of float
| MIDDLE_STRATA_LUXURY_NEEDS of float
| MIDDLE_TAX of int
| MILITANCY of int
| MILITARY_ACCESS of string
| MILITARY_REFORM_NAME of string
| MILITARY_SCORE_NAME of string
| MILITARY_SCORE_NUMBER of int
| MILITARY_SPENDING of int
| MINORITIES of bool
| MONEY of int
| MONTH of int
| NATIONALVALUE of string
| NATIONAL_PROVINCES_OCCUPIED of int
| NEIGHBOUR of string
| NOT of condition list
| NUMBER_OF_STATES of int
| NUM_OF_ALLIES of int
| NUM_OF_CITIES of int
| NUM_OF_PORTS of int
| NUM_OF_REVOLTS of int
| NUM_OF_SUBSTATES of int
| NUM_OF_VASSALS of int
| NUM_OF_VASSALS_NO_SUBSTATES of int
| OR of  condition list
| OWNED_BY of string
| OWNS of string
| PART_OF_SPHERE of bool
| POLITICAL_MOVEMENT of bool
| POLITICAL_MOVEMENT_STRENGTH of int
| POLITICAL_REFORM_NAME of string
| POLITICAL_REFORM_WANT_NUMBER of float 
| POLITICAL_REFORM_WANT_STRING of string
| POOR_STRATA_EVERYDAY_NEEDS of float
| POOR_STRATA_LIFE_NEEDS of float
| POOR_STRATA_LUXURY_NEEDS of float
| POOR_TAX of int
| POP_MAJORITY_CULTURE of string
| POP_MAJORITY_IDEOLOGY of string
| POP_MAJORITY_ISSUE of string
| POP_MAJORITY_RELIGION of string
| POP_MILITANCY of int
| POP_TYPE of string * int
| PORT of bool
| PRESTIGE of int
| PRIMARY_CULTURE of string
| PRODUCES of string
| PROVINCE_CONTROL_DAYS of float
| PROVINCE_ID of string
| RANK of int
| REBEL_POWER_FRACTION of int
| RECRUITED_PERCENTAGE of int
| REGION of string
| RELATION of param list 
| RELIGION of string
| RELIGIOUS_POLICY of string
| REVOLT_PERCENTAGE of int
| RICH_STRATA_EVERYDAY_NEEDS of float
| RICH_STRATA_LIFE_NEEDS of float
| RICH_STRATA_LUXURY_NEEDS of float
| RICH_TAX of int
| RULING_PARTY of string
| RULING_PARTY_IDEOLOGY of string
| SLAVERY of string
| SOCIAL_MOVEMENT of bool
| SOCIAL_MOVEMENT_STRENGTH of string
| SOCIAL_REFORM_NAME of string
| SOCIAL_REFORM_WANT_NUMBER of float 
| SOCIAL_REFORM_WANT_NAME of string
| SOCIAL_SPENDING of int
| STATE_ID of string
| STRATA of string
| STRONGER_ARMY_THAN of string
| SUBSTATE_OF of string
| TAG of string
| TERRAIN of string
| TECHNOLOGY of string * int
| THIS_CULTURE_UNION of string
| TOTAL_AMOUNT_OF_DIVISIONS of int
| TOTAL_AMOUNT_OF_SHIPS of int
| TOTAL_DEFENSIVES of int
| TOTAL_NUM_OF_PORTS of int
| TOTAL_OFFENSIVES of int
| TOTAL_OF_OURS_SUNK of int
| TOTAL_POPS of string
| TOTAL_SEA_BATTLES of int
| TOTAL_SUNK_BY_US of int
| TRADE_GOODS of string
| TRADE_POLICY of string
| TRUCE_WITH of string
| TYPE of string
| UNEMPLOYMENT of int
| UNEMPLOYMENT_BY_TYPE of param list
| UNITS_IN_PROVINCE of int
| UNIT_HAS_LEADER of bool
| UNIT_IN_BATTLE of bool
| UPPER_HOUSE of param list 
| VASSAL_OF of string
| WAR of bool
| WAR_EXHAUSTION of int
| WAR_POLICY of string
| WAR_SCORE of int
| WAR_WITH of string
| WORK_AVAILABLE of param list 
| YEAR of int
| PARTY_ISSUE of string * int 
| ANY_POP of  condition list
| ALL_CORE of  condition list
| ANY_CORE of  condition list
| ANY_GREATER_POWER of  condition list
| ANY_NEIGHBOR_COUNTRY of  condition list
| ANY_OWNED_PROVINCE of  condition list
| ANY_SPHERE_MEMBER of  condition list
| ANY_STATE of  condition list
| ANY_SUBSTATE of  condition list
| CAPITAL_SCOPE of  condition list
| COUNTRY_TAG of  condition list
| CULTURAL_UNION of  condition list
| OVERLORD of  condition list
| REGION_NAME of  condition list
| SPHERE_OWNER of  condition list



let bool_of_string str = match str with 
|"yes" -> true 
| "no" -> false
|x-> raise (Syntax_error (Printf.sprintf "%s:not a bool" x) )

let province_conditions lexems = 
let rec condition_r lexems out= match lexems with
|ASSIGNMENT((CONDITION,"AND",_),ASSIGNMENT_LIST(ls))::rest->
    let value,rest = condition_r ls [] in
    let v = AND((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("AND"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = UNEXPECTED_LEXEM(value,wrong_type) in
    let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("OR"),_),ASSIGNMENT_LIST(ls))::rest->
    let value,rest = condition_r rest [] in
    let v = OR((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("OR"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = UNEXPECTED_LEXEM(value,wrong_type) in
    let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("NOT"),_),ASSIGNMENT_LIST(ls))::rest->
    let value,rest = condition_r rest [] in
    let v = NOT(value) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("NOT"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = UNEXPECTED_LEXEM(value,wrong_type) in
    let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("year"),_),LEXEM(INT,value,_))::rest->
    let v = YEAR(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("year"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,wrong_type,INT) in
    let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("month"),_),LEXEM(INT,value,_))::rest->
    let v = MONTH(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("month"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = UNEXPECTED_LEXEM(value,wrong_type) in
    let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("allow_multiple_instances"),_),LEXEM(BOOL,value,_))::rest->
    let v = ALLOW_MULTIPLE_INSTANCES(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("allow_multiple_instances"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("fire_only_once"),_),LEXEM(BOOL,value,_))::rest->
    let v = FIRE_ONLY_ONCE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("fire_only_once"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_triggered_only"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_TRIGGERED_ONLY(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_triggered_only"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("major"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = MAJOR((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("major"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_consciousness"),_),LEXEM(FLOAT,value,_))::rest->
    let v = AVERAGE_CONSCIOUSNESS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_consciousness"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_militancy"),_),LEXEM(FLOAT,value,_))::rest->
    let v = AVERAGE_MILITANCY(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_militancy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_build_factory"),_),LEXEM(BOOL,value,_))::rest->
    let v = CAN_BUILD_FACTORY(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_build_factory"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("continent"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CONTINENT((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("continent"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("controlled_by"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CONTROLLED_BY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("controlled_by"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("controlled_by_rebels"),_),LEXEM(BOOL,value,_))::rest->
    let v = CONTROLLED_BY_REBELS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("controlled_by_rebels"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("country_units_in_province"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = COUNTRY_UNITS_IN_PROVINCE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("country_units_in_province"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("country_units_in_state"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = COUNTRY_UNITS_IN_STATE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("country_units_in_state"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crime_fighting"),_),LEXEM(INT,value,_))::rest->
    let v = CRIME_FIGHTING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crime_fighting"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("education_spending"),_),LEXEM(INT,value,_))::rest->
    let v = EDUCATION_SPENDING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("education_spending"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("empty"),_),LEXEM(BOOL,value,_))::rest->
    let v = EMPTY(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("empty"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("flashpoint_tension"),_),LEXEM(INT,value,_))::rest->
    let v = FLASHPOINT_TENSION(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("flashpoint_tension"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_building"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_BUILDING((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_building"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_crime"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_CRIME((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_crime"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_culture_core"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_CULTURE_CORE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_culture_core"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_empty_adjacent_province"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_EMPTY_ADJACENT_PROVINCE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_empty_adjacent_province"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_empty_adjacent_state"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_EMPTY_ADJACENT_STATE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_empty_adjacent_state"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_factories"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_FACTORIES(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_factories"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_flashpoint"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_FLASHPOINT(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_flashpoint"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_national_minority"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_NATIONAL_MINORITY(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_national_minority"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_pop_type"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_POP_TYPE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_pop_type"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_province_flag"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_PROVINCE_FLAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_province_flag"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_province_modifier"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_PROVINCE_MODIFIER((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_province_modifier"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_recent_imigration"),_),LEXEM(FLOAT,value,_))::rest->
    let v = HAS_RECENT_IMIGRATION(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_recent_imigration"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_accepted_culture"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_ACCEPTED_CULTURE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_accepted_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_blockaded"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_BLOCKADED(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_blockaded"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_capital"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_CAPITAL(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_capital"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_coastal"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_COASTAL(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_coastal"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_colonial"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_COLONIAL(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_colonial"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_core"),_),LEXEM(TAG,value,_))::rest->
    let v = IS_CORE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_core"),_),LEXEM(SCOPE,value,_))::rest->
    let v = IS_CORE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_core"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_ideology_enabled"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_IDEOLOGY_ENABLED((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_ideology_enabled"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_overseas"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_OVERSEAS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_overseas"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_primary_culture"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_PRIMARY_CULTURE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_primary_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_state_capital"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_STATE_CAPITAL(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_state_capital"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_state_religion"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_STATE_RELIGION(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_state_religion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("life_rating"),_),LEXEM(INT,value,_))::rest->
    let v = LIFE_RATING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("life_rating"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("literacy"),_),LEXEM(INT,value,_))::rest->
    let v = LITERACY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("literacy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_spending"),_),LEXEM(INT,value,_))::rest->
    let v = MILITARY_SPENDING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_spending"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("minorities"),_),LEXEM(BOOL,value,_))::rest->
    let v = MINORITIES(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("minorities"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("owned_by"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = OWNED_BY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("owned_by"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_militancy"),_),LEXEM(INT,value,_))::rest->
    let v = POP_MILITANCY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_militancy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("port"),_),LEXEM(BOOL,value,_))::rest->
    let v = PORT(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("port"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("province_control_days"),_),LEXEM(FLOAT,value,_))::rest->
    let v = PROVINCE_CONTROL_DAYS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("province_control_days"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("province_id"),_),LEXEM(INT,value,_))::rest->
    let v = PROVINCE_ID((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("province_id"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("region"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = REGION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("region"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("state_id"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = STATE_ID((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("state_id"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("terrain"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = TERRAIN((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("terrain"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("trade_goods"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = TRADE_GOODS((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("trade_goods"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_pops"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = TOTAL_POPS((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_pops"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = UNEMPLOYMENT(int_of_string (value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment_by_type"),_),ASSIGNMENT_LIST(ls))::rest->
    let param_list = verify_params ls [(KEYWORD,"type",KEYWORD);(KEYWORD,"value",INT)] in
    let v =  UNEMPLOYMENT_BY_TYPE(param_list) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment_by_type"),_),LEXEM(wrong_type,value,pos))::rest->
    let v  = CONDITION_TYPE_ERROR ([wrong_type,value,pos], [KEYWORD;INT]) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("units_in_province"),_),LEXEM(INT,value,_))::rest->
    let v = UNITS_IN_PROVINCE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("units_in_province"),_),LEXEM(wrong_type,value,pos))::rest->
    let error_type = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error_type,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("work_available"),_),ASSIGNMENT_LIST(ls))::rest->
    let param_list = verify_params ls [(KEYWORD,"worker",KEYWORD)] in
    let v = WORK_AVAILABLE(param_list) in
    condition_r rest (v::out)

|ASSIGNMENT((KEYWORD,("work_available"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)

|ASSIGNMENT((type_value,str,(x,y)),_)::rest-> condition_r rest (CONDITION_UNEXPECTED_ERROR(type_value,str,(x,y))::out)
    
(*add more error catching*)
|_::rest ->  out,[]
|[]->out,[]
in

(condition_r lexems [])

let pop_condition lexems =
    let rec condition_r lexems out = match lexems with
|ASSIGNMENT((KEYWORD,("agree_with_ruling_party"),_),LEXEM(FLOAT,value,_))::rest->
    let v = AGREE_WITH_RULING_PARTY(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("agree_with_ruling_party"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("cash_reserves"),_),LEXEM(INT,value,_))::rest->
    let v = CASH_RESERVES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("cash_reserves"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("consciousness"),_),LEXEM(INT,value,_))::rest->
    let v = CONSCIOUSNESS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("consciousness"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("culture"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CULTURE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("everyday_needs"),_),LEXEM(INT,value,_))::rest->
    let v = EVERYDAY_NEEDS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("everyday_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_pop_culture"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_POP_CULTURE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_pop_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_pop_religion"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_POP_RELIGION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_pop_religion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_primary_culture"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_PRIMARY_CULTURE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_primary_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_accepted_culture"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_ACCEPTED_CULTURE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_accepted_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_culture_group"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_CULTURE_GROUP((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_culture_group"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_state_religion"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_STATE_RELIGION(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_state_religion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("life_needs"),_),LEXEM(INT,value,_))::rest->
    let v = LIFE_NEEDS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("life_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("literacy"),_),LEXEM(INT,value,_))::rest->
    let v = LITERACY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("literacy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("luxury_needs"),_),LEXEM(INT,value,_))::rest->
    let v = LUXURY_NEEDS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("luxury_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("militancy"),_),LEXEM(INT,value,_))::rest->
    let v = MILITANCY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("militancy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("money"),_),LEXEM(INT,value,_))::rest->
    let v = MONEY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("money"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_movement"),_),LEXEM(BOOL,value,_))::rest->
    let v = POLITICAL_MOVEMENT(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_movement"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_reform_want"),_),LEXEM(FLOAT,value,_))::rest->
    let v = POLITICAL_REFORM_WANT_NUMBER(float_of_string (value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_reform_want"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_culture"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_CULTURE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_ideology"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_IDEOLOGY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_ideology"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_issue"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_ISSUE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_issue"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_religion"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_RELIGION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_religion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("religion"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = RELIGION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("religion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_movement"),_),LEXEM(BOOL,value,_))::rest->
    let v = SOCIAL_MOVEMENT(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_movement"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_reform_want"),_),LEXEM(FLOAT,value,_))::rest->
    let v = SOCIAL_REFORM_WANT_NUMBER(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_reform_want"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("strata"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = STRATA((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("strata"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("type"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = TYPE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("type"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment"),_),LEXEM(INT,value,_))::rest->
    let v = UNEMPLOYMENT(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,name,_),LEXEM(_,value,_))::rest->
    let v = LABEL_VALUE (name, value) in
    condition_r rest (v::out)
|ASSIGNMENT((lh_type,lh_value,lh_cords),_)::rest-> condition_r rest (CONDITION_UNEXPECTED_ERROR(lh_type,lh_value,lh_cords)::out)
|[]->out,[] 

in
let return,rest= condition_r lexems [] in
(List.rev return),rest


let rec country_conditions lexems :( condition list * (lexem_type * string * (int *int )) list)  =
let rec condition_r lexems (out:condition list) :( condition list * (lexem_type * string * (int *int )) list)= match lexems with

    |ASSIGNMENT((KEYWORD,"any_pop",_),ASSIGNMENT_LIST(ls))::rest->
        let pe,rest= pop_condition rest
        in 
        condition_r rest (ANY_POP(pe)::out)
    |ASSIGNMENT((KEYWORD,"all_core",_),ASSIGNMENT_LIST(ls))::rest->
        let pe,rest= province_conditions rest
        in 
        condition_r rest (ALL_CORE(pe)::out)
    |ASSIGNMENT((KEYWORD,"any_core",_),ASSIGNMENT_LIST(ls))::rest->
        let pe,rest= province_conditions rest
        in 
        condition_r rest (ALL_CORE(pe)::out)
    |ASSIGNMENT((KEYWORD,"any_greater_power",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_GREATER_POWER(ce)::out)
    |ASSIGNMENT((KEYWORD,"any_neighbor_country",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_NEIGHBOR_COUNTRY(ce)::out)
    |ASSIGNMENT((KEYWORD,"any_owned_province",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_OWNED_PROVINCE(ce)::out)
    |ASSIGNMENT((KEYWORD,"any_sphere_member",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_SPHERE_MEMBER(ce)::out)
    |ASSIGNMENT((KEYWORD,"any_state",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_STATE(ce)::out)
    |ASSIGNMENT((KEYWORD,"any_substate",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_SUBSTATE(ce)::out)
    |ASSIGNMENT((KEYWORD,"capital_scope",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (CAPITAL_SCOPE(ce)::out)
    |ASSIGNMENT((KEYWORD,"country_tag",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_STATE(ce)::out)
    |ASSIGNMENT((KEYWORD,"cultural_union",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_STATE(ce)::out)

    |ASSIGNMENT((KEYWORD,"overlord",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_STATE(ce)::out)

    |ASSIGNMENT((KEYWORD,"region_name",_),ASSIGNMENT_LIST(ls))::rest->
let ce,rest = country_conditions rest in
        condition_r rest (ANY_STATE(ce)::out)

    |ASSIGNMENT((KEYWORD,"sphere_owner",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = country_conditions rest in
        condition_r rest (ANY_STATE(ce)::out)

|ASSIGNMENT((CONDITION,("AND"),_),ASSIGNMENT_LIST(ls))::rest->
    let value,rest = condition_r rest [] in
    let v = AND((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("AND"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("OR"),_),ASSIGNMENT_LIST(ls))::rest->
    let value,rest = condition_r rest [] in
    let v = OR((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("OR"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("NOT"),_),ASSIGNMENT_LIST(ls))::rest->
    let value,rest = condition_r rest [] in
    let v = NOT(value) in
    condition_r rest (v::out)
|ASSIGNMENT((CONDITION,("NOT"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("year"),_),LEXEM(INT,value,_))::rest->
    let v = YEAR(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("year"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("month"),_),LEXEM(INT,value,_))::rest->
    let v = MONTH(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("month"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("allow_multiple_instances"),_),LEXEM(BOOL,value,_))::rest->
    let v = ALLOW_MULTIPLE_INSTANCES(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("allow_multiple_instances"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("fire_only_once"),_),LEXEM(BOOL,value,_))::rest->
    let v = FIRE_ONLY_ONCE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("fire_only_once"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_triggered_only"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_TRIGGERED_ONLY(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_triggered_only"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("major"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = MAJOR((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("major"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("immediate"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IMMEDIATE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("immediate"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT(((KEYWORD,("check_variable"),_),ASSIGNMENT_LIST(ls)))::rest ->
    let param_list = verify_params ls [(KEYWORD,"which",KEYWORD);(KEYWORD,"value",INT)] in
    let v = CHECK_VARIABLE(param_list) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("check_variable"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_global_flag"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_GLOBAL_FLAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_global_flag"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_canal_enabled"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_CANAL_ENABLED((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_canal_enabled"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("administration_spending"),_),LEXEM(INT,value,_))::rest->
    let v = ADMINISTRATION_SPENDING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("administration_spending"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ai"),_),LEXEM(BOOL,value,_))::rest->
    let v = AI(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ai"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("alliance_with"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = ALLIANCE_WITH((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("alliance_with"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_consciousness"),_),LEXEM(FLOAT,value,_))::rest->
    let v = AVERAGE_CONSCIOUSNESS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_consciousness"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_militancy"),_),LEXEM(FLOAT,value,_))::rest->
    let v = AVERAGE_MILITANCY(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("average_militancy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("badboy"),_),LEXEM(FLOAT,value,_))::rest->
    let v = BADBOY(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("badboy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("big_producer"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = BIG_PRODUCER((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("big_producer"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("blockade"),_),LEXEM(INT,value,_))::rest->
    let v = BLOCKADE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("blockade"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("brigades_compare"),_),LEXEM(FLOAT,value,_))::rest->
    let v = BRIGADES_COMPARE(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("brigades_compare"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_build_factory_in_capital_state"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CAN_BUILD_FACTORY_IN_CAPITAL_STATE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_build_factory_in_capital_state"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crime_higher_than_education"),_),LEXEM(BOOL,value,_))::rest->
    let v = CRIME_HIGHER_THAN_EDUCATION(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crime_higher_than_education"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_nationalize"),_),LEXEM(BOOL,value,_))::rest->
    let v = CAN_NATIONALIZE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_nationalize"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_create_vassals"),_),LEXEM(BOOL,value,_))::rest->
    let v = CAN_CREATE_VASSALS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("can_create_vassals"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("capital"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CAPITAL((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("capital"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("casus_belli"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CASUS_BELLI((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("casus_belli"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("citizenship_policy"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CITIZENSHIP_POLICY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("citizenship_policy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("civilization_progress"),_),LEXEM(FLOAT,value,_))::rest->
    let v = CIVILIZATION_PROGRESS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("civilization_progress"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("civilized"),_),LEXEM(BOOL,value,_))::rest->
    let v = CIVILIZED(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("civilized"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("colonial_nation"),_),LEXEM(BOOL,value,_))::rest->
    let v = COLONIAL_NATION(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("colonial_nation"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("constructing_cb_progress"),_),LEXEM(FLOAT,value,_))::rest->
    let v = CONSTRUCTING_CB_PROGRESS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("constructing_cb_progress"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("constructing_cb_type"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CONSTRUCTING_CB_TYPE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("constructing_cb_type"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("controls"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = CONTROLS((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("controls"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crime_fighting"),_),LEXEM(INT,value,_))::rest->
    let v = CRIME_FIGHTING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crime_fighting"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crisis_exist"),_),LEXEM(BOOL,value,_))::rest->
    let v = CRISIS_EXIST(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("crisis_exist"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("culture_has_union_tag"),_),LEXEM(BOOL,value,_))::rest->
    let v = CULTURE_HAS_UNION_TAG(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("culture_has_union_tag"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("diplomatic_influence"),_),ASSIGNMENT_LIST(ls))::rest->
    let param_list = verify_params(ls,[(KEYWORD,"who",TAG),(KEYWORD,"who",SCOPE),(KEYWORD,"value",INT)]) in
    let v = DIPLOMATIC_INFLUENCE(param_list) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("diplomatic_influence"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("economic_policy"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = ECONOMIC_POLICY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("economic_policy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("economic_reform_name"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = ECONOMIC_REFORM_NAME((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("economic_reform_name"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("education_spending"),_),LEXEM(INT,value,_))::rest->
    let v = EDUCATION_SPENDING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("education_spending"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("election"),_),LEXEM(BOOL,value,_))::rest->
    let v = ELECTION(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("election"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("exists"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = EXISTS((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("exists"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("government"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = GOVERNMENT((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("government"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("great_wars_enabled"),_),LEXEM(BOOL,value,_))::rest->
    let v = GREAT_WARS_ENABLED(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("great_wars_enabled"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("have_core_in"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAVE_CORE_IN((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("have_core_in"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_country_flag"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_COUNTRY_FLAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_country_flag"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_country_modifier"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_COUNTRY_MODIFIER((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_country_modifier"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_cultural_sphere"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_CULTURAL_SPHERE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_cultural_sphere"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_leader"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = HAS_LEADER((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_leader"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_recently_lost_war"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_RECENTLY_LOST_WAR(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_recently_lost_war"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_unclaimed_cores"),_),LEXEM(BOOL,value,_))::rest->
    let v = HAS_UNCLAIMED_CORES(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("has_unclaimed_cores"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("[ideology]"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ideology"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IDEOLOGY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ideology"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("industrial_score"),_),LEXEM(INT,value,_))::rest->
    let v = INDUSTRIAL_SCORE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("industrial_score"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("in_sphere"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IN_SPHERE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("in_sphere"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("in_default"),_),LEXEM(BOOL,value,_))::rest->
    let v = IN_DEFAULT(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("in_default"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("invention"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = INVENTION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("invention"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("involved_in_crisis"),_),LEXEM(BOOL,value,_))::rest->
    let v = INVOLVED_IN_CRISIS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("involved_in_crisis"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_claim_crisis"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_CLAIM_CRISIS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_claim_crisis"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_colonial_crisis"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_COLONIAL_CRISIS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_colonial_crisis"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_core"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_CORE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_core"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_cultural_union"),_),LEXEM(TAG,value,_))::rest->
    let v = IS_CULTURAL_UNION_TAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_cultural_union"),_),LEXEM(SCOPE,value,_))::rest->
    let v = IS_CULTURAL_UNION_TAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_cultural_union"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_CULTURAL_UNION_BOOL(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_cultural_union"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_culture_group"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_CULTURE_GROUP((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_culture_group"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_disarmed"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_DISARMED(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_disarmed"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_greater_power"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_GREATER_POWER(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_greater_power"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_ideology_enabled"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_IDEOLOGY_ENABLED((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_ideology_enabled"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_independant"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_INDEPENDANT(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_independant"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_liberation_crisis"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_LIBERATION_CRISIS(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_liberation_crisis"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_mobilised"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_MOBILISED(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_mobilised"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_next_reform"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_NEXT_REFORM((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_next_reform"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_our_vassal"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_OUR_VASSAL((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_our_vassal"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_possible_vassal"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_POSSIBLE_VASSAL((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_possible_vassal"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_secondary_power"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_SECONDARY_POWER(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_secondary_power"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_sphere_leader_of"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = IS_SPHERE_LEADER_OF((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_sphere_leader_of"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_vassal"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_VASSAL(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_vassal"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_substate"),_),LEXEM(BOOL,value,_))::rest->
    let v = IS_SUBSTATE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("is_substate"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("literacy"),_),LEXEM(INT,value,_))::rest->
    let v = LITERACY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("literacy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("lost_national"),_),LEXEM(INT,value,_))::rest->
    let v = LOST_NATIONAL(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("lost_national"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_strata_everyday_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = MIDDLE_STRATA_EVERYDAY_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_strata_everyday_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_strata_life_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = MIDDLE_STRATA_LIFE_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_strata_life_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_strata_luxury_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = MIDDLE_STRATA_LUXURY_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_strata_luxury_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_tax"),_),LEXEM(INT,value,_))::rest->
    let v = MIDDLE_TAX(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("middle_tax"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_access"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = MILITARY_ACCESS((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_access"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_reform_name"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = MILITARY_REFORM_NAME((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_reform_name"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_score"),_),LEXEM(INT,value,_))::rest->
    let v = MILITARY_SCORE_NUMBER(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_score"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = MILITARY_SCORE_NAME((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_score"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_spending"),_),LEXEM(INT,value,_))::rest->
    let v = MILITARY_SPENDING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("military_spending"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("money"),_),LEXEM(INT,value,_))::rest->
    let v = MONEY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("money"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("nationalvalue"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = NATIONALVALUE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("nationalvalue"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("national_provinces_occupied"),_),LEXEM(INT,value,_))::rest->
    let v = NATIONAL_PROVINCES_OCCUPIED(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("national_provinces_occupied"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("neighbour"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = NEIGHBOUR((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("neighbour"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_allies"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_ALLIES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_allies"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_cities"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_CITIES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_cities"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_ports"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_PORTS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_ports"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_revolts"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_REVOLTS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_revolts"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("number_of_states"),_),LEXEM(INT,value,_))::rest->
    let v = NUMBER_OF_STATES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("number_of_states"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_substates"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_SUBSTATES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_substates"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_vassals"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_VASSALS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_vassals"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_vassals_no_substates"),_),LEXEM(INT,value,_))::rest->
    let v = NUM_OF_VASSALS_NO_SUBSTATES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("num_of_vassals_no_substates"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("owns"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = OWNS((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("owns"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("part_of_sphere"),_),LEXEM(BOOL,value,_))::rest->
    let v = PART_OF_SPHERE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("part_of_sphere"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)


|ASSIGNMENT((KEYWORD,("political_movement_strength"),_),LEXEM(INT,value,_))::rest->
    let v = POLITICAL_MOVEMENT_STRENGTH(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_movement_strength"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_reform_name"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POLITICAL_REFORM_NAME((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_reform_name"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_reform_want"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POLITICAL_REFORM_WANT_STRING((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("political_reform_want"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_strata_everyday_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = POOR_STRATA_EVERYDAY_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_strata_everyday_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_strata_life_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = POOR_STRATA_LIFE_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_strata_life_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_strata_luxury_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = POOR_STRATA_LUXURY_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_strata_luxury_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_tax"),_),LEXEM(INT,value,_))::rest->
    let v = POOR_TAX(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("poor_tax"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_culture"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_CULTURE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_ideology"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_IDEOLOGY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_ideology"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_religion"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = POP_MAJORITY_RELIGION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_majority_religion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_militancy"),_),LEXEM(INT,value,_))::rest->
    let v = POP_MILITANCY(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("pop_militancy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)

|ASSIGNMENT((KEYWORD,("prestige"),_),LEXEM(INT,value,_))::rest->
    let v = PRESTIGE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("prestige"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("primary_culture"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = PRIMARY_CULTURE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("primary_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("accepted_culture"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = ACCEPTED_CULTURE((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("accepted_culture"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("produces"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = PRODUCES((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("produces"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rank"),_),LEXEM(INT,value,_))::rest->
    let v = RANK(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rank"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rebel_power_fraction"),_),LEXEM(INT,value,_))::rest->
    let v = REBEL_POWER_FRACTION(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rebel_power_fraction"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("recruited_percentage"),_),LEXEM(INT,value,_))::rest->
    let v = RECRUITED_PERCENTAGE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("recruited_percentage"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("relation"),_),ASSIGNMENT_LIST(ls))::rest->
    let param_list = verify_params [(KEYWORD,"who",TAG),(KEYWORD,"who",SCOPE),(KEYWORD,"value",INT)] in
    let v = RELATION(param_list) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("relation"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("religious_policy"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = RELIGIOUS_POLICY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("religious_policy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("revolt_percentage"),_),LEXEM(INT,value,_))::rest->
    let v = REVOLT_PERCENTAGE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("revolt_percentage"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_strata_everyday_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = RICH_STRATA_EVERYDAY_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_strata_everyday_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_strata_life_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = RICH_STRATA_LIFE_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_strata_life_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_strata_luxury_needs"),_),LEXEM(FLOAT,value,_))::rest->
    let v = RICH_STRATA_LUXURY_NEEDS(float_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_strata_luxury_needs"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,FLOAT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_tax"),_),LEXEM(INT,value,_))::rest->
    let v = RICH_TAX(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("rich_tax"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ruling_party"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = RULING_PARTY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ruling_party"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ruling_party_ideology"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = RULING_PARTY_IDEOLOGY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("ruling_party_ideology"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("slavery"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = SLAVERY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("slavery"),_),LEXEM(wrong_type,value,pos))::rest->

    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_movement_strength"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = SOCIAL_MOVEMENT_STRENGTH((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_movement_strength"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_reform_name"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = SOCIAL_REFORM_NAME((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_reform_name"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_reform_want"),_),LEXEM(FLOAT,value,_))::rest->
    let v = SOCIAL_REFORM_WANT_NUMBER(float_of_string (value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_reform_want"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_spending"),_),LEXEM(INT,value,_))::rest->
    let v = SOCIAL_SPENDING(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("social_spending"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("stronger_army_than"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = STRONGER_ARMY_THAN((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("stronger_army_than"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("substate_of"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = SUBSTATE_OF((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("substate_of"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("tag"),_),LEXEM(TAG,value,_))::rest->
    let v = TAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("tag"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)


|ASSIGNMENT((KEYWORD,("this_culture_union"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = THIS_CULTURE_UNION((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("this_culture_union"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_amount_of_divisions"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_AMOUNT_OF_DIVISIONS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_amount_of_divisions"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_amount_of_ships"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_AMOUNT_OF_SHIPS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_amount_of_ships"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_defensives"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_DEFENSIVES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_defensives"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_num_of_ports"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_NUM_OF_PORTS(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_num_of_ports"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_offensives"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_OFFENSIVES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_offensives"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_of_ours_sunk"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_OF_OURS_SUNK(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_of_ours_sunk"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("TAG"),_),LEXEM(TAG,value,_))::rest->
    let v = TAG((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("TAG"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_sea_battles"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_SEA_BATTLES(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_sea_battles"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_sunk_by_us"),_),LEXEM(INT,value,_))::rest->
    let v = TOTAL_SUNK_BY_US(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("total_sunk_by_us"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("trade_policy"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = TRADE_POLICY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("trade_policy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("truce_with"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = TRUCE_WITH((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("truce_with"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment"),_),LEXEM(INT,value,_))::rest->
    let v = UNEMPLOYMENT(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unemployment"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unit_has_leader"),_),LEXEM(BOOL,value,_))::rest->
    let v = UNIT_HAS_LEADER(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unit_has_leader"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unit_in_battle"),_),LEXEM(BOOL,value,_))::rest->
    let v = UNIT_IN_BATTLE(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("unit_in_battle"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("upper_house"),_),ASSIGNMENT_LIST(ls))::rest->
    let param_list = verify_params ls [(KEYWORD,"ideology",KEYWORD),(KEYWORD,"value",FLOAT)] in
    let v = UPPER_HOUSE(param_list)
    in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("upper_house"),_),LEXEM(wrong_type,value,pos) as wrong_expr)::rest->
    let error = EXPECTED_ASSIGN_LIST(wrong_expr) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("vassal_of"),_),LEXEM(TAG,value,_))::rest->
    let v = VASSAL_OF((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("vassal_of"),_),LEXEM(SCOPE,value,_))::rest->
    let v = VASSAL_OF((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("vassal_of"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war"),_),LEXEM(BOOL,value,_))::rest->
    let v = WAR(bool_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,BOOL,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_exhaustion"),_),LEXEM(INT,value,_))::rest->
    let v = WAR_EXHAUSTION(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_exhaustion"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_policy"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = WAR_POLICY((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_policy"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_score"),_),LEXEM(INT,value,_))::rest->
    let v = WAR_SCORE(int_of_string(value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_score"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,INT,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_with"),_),LEXEM(KEYWORD,value,_))::rest->
    let v = WAR_WITH((value)) in
    condition_r rest (v::out)
|ASSIGNMENT((KEYWORD,("war_with"),_),LEXEM(wrong_type,value,pos))::rest->
    let error = TYPE_MISHMASH(value,KEYWORD,wrong_type) in
	let v = (error,pos) in
    condition_r rest (v::out)


|ASSIGNMENT((KEYWORD,ideology,_),LEXEM(_,value,_))::rest->
    let v = LABEL_VALUE(ideology, value) in
    condition_r rest (v::out)

|ASSIGNMENT(type_value,str,(x,y))::rest-> condition_r rest (CONDITION_UNEXPECTED_ERROR(type_value,str,(x,y))::out)
|rest-> condition_r rest (END_OF_FILE::out)

|[]->out,[]

in
condition_r lexems [] 
