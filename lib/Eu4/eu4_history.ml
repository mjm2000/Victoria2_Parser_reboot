open SymbolTable
open TypeDef

let eu4_diplomacy_relation_block =
  SubTable
    (symbol_table_init
       [
         Literal "first", Value Tag;
         Literal "second", Value Tag;
         Literal "start_date", Value Date;
         Literal "end_date", Value Date;
         Literal "trade_league", TypeOption [Value Bool; Integer];
         Literal "subject_type", Value Keyword;
       ])

let eu4_diplomacy_date_clause_block =
  SubTable
    (symbol_table_init
       [
         Literal "emperor", TypeOption [Value Tag; Value String];
         Literal "celestial_emperor", Value Tag;
       ])

let eu4_diplomacy_history_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "alliance", Type "eu4_diplomacy_relation_block_def";
         Literal "vassal", Type "eu4_diplomacy_relation_block_def";
         Literal "union", Type "eu4_diplomacy_relation_block_def";
         Literal "royal_marriage", Type "eu4_diplomacy_relation_block_def";
         Literal "guarantee", Type "eu4_diplomacy_relation_block_def";
         Literal "march", Type "eu4_diplomacy_relation_block_def";
         Literal "dependency", Type "eu4_diplomacy_relation_block_def";
         Literal "protectorate", Type "eu4_diplomacy_relation_block_def";
         (Value Date, Type "eu4_diplomacy_date_clause_block_def");
       ])

let eu4_battle_side_block =
  SubTable
    (symbol_table_init
       [
         Literal "commander", Value String;
         Literal "losses", Number;
         Literal "country", Value Tag;
         (Catalog ("unit_column", Value Keyword), Integer);
       ])

let eu4_battle_block =
  SubTable
    (symbol_table_init
       [
         Literal "name", Value String;
         Literal "location", Integer;
         Literal "attacker", Type "eu4_battle_side_block_def";
         Literal "defender", Type "eu4_battle_side_block_def";
         Literal "result", Value Bool;
       ])

let eu4_war_events_block =
  SubTable
    (symbol_table_init
       [
         Literal "add_attacker", Value Tag;
         Literal "add_defender", Value Tag;
         Literal "rem_attacker", Value Tag;
         Literal "rem_defender", Value Tag;
         Literal "battle", Type "eu4_battle_block_def";
       ])

let eu4_war_goal_block =
  SubTable
    (symbol_table_init
       [
         Literal "type", Value Keyword;
         Literal "casus_belli", Value Keyword;
         Literal "tag", Value Tag;
         Literal "province", Integer;
       ])

let eu4_war_history_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "name", Value String;
         Literal "succession", Value Tag;
         Literal "war_goal", Type "eu4_war_goal_block_def";
         (Value Date, Type "eu4_war_events_block_def");
       ])

let eu4_province_history_file_def =
  Inherit ([ (Value Date, Type "province_effects_def") ], [ "province_effects_def" ])

let eu4_country_history_file_def =
  Inherit ([ (Value Date, Type "country_effects_def") ], [ "country_effects_def" ])

let eu4_advisor_history_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "advisor",
         SubTable
           (symbol_table_init
              [
                Literal "advisor_id", Value PositiveInt;
                Literal "name", Value String;
                Literal "location", Integer;
                Literal "type", Value Keyword;
                Literal "skill", Value PositiveInt;
                Literal "date", Value Date;
                Literal "death_date", Value Date;
                Literal "culture", Value Keyword;
                Literal "religion", Value Keyword;
                Literal "female", Value Bool;
                Literal "discount", Value Bool;
              ]);
       ])

let history_definitions =
  [
    Definition "eu4_diplomacy_relation_block_def", eu4_diplomacy_relation_block;
    Definition "eu4_diplomacy_date_clause_block_def", eu4_diplomacy_date_clause_block;
    Definition "eu4_diplomacy_history_file_def", eu4_diplomacy_history_file_def;
    Definition "eu4_battle_side_block_def", eu4_battle_side_block;
    Definition "eu4_battle_block_def", eu4_battle_block;
    Definition "eu4_war_events_block_def", eu4_war_events_block;
    Definition "eu4_war_goal_block_def", eu4_war_goal_block;
    Definition "eu4_war_history_file_def", eu4_war_history_file_def;
    Definition "eu4_province_history_file_def", eu4_province_history_file_def;
    Definition "eu4_country_history_file_def", eu4_country_history_file_def;
    Definition "eu4_advisor_history_file_def", eu4_advisor_history_file_def;
  ]
