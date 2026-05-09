open SymbolTable
open TypeDef
open Eu4_pulse

let eu4_on_actions_file_def =
  SubTable (symbol_table_init Eu4_on_actions_hooks.on_actions_hook_rows)

let eu4_achievements_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("achievement", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "id", Integer;
                  Literal "possible", Type "country_conditions_def";
                  Literal "visible", Type "country_conditions_def";
                  Literal "happened", Type "country_conditions_def";
                  Literal "provinces_to_highlight", Type "province_conditions_def";
                ]) );
       ])

let eu4_rebel_types_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("rebel_type", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "area", Value Keyword;
                  Literal "government", TypeOption [ Value Keyword; Value String ];
                  Literal "defection", Value Keyword;
                  Literal "independence", Value Keyword;
                  Literal "gfx_type", Value Keyword;
                  Literal "religion", Value Keyword;
                  Literal "unit_transfer", Value Bool;
                  Literal "defect_delay", Integer;
                  Literal "resilient", Value Bool;
                  Literal "reinforcing", Value Bool;
                  Literal "general", Value Bool;
                  Literal "smart", Value Bool;
                  Literal "disband_on_leader_death", Value Bool;
                  Literal "revolutionary", Value Bool;
                  Literal "artillery", Decimal;
                  Literal "infantry", Decimal;
                  Literal "cavalry", Decimal;
                  Literal "morale", Decimal;
                  Literal "dynasty", Value Bool;
                  Literal "will_relocate", Value Bool;
                  Literal "handle_action_negotiate", Value Bool;
                  Literal "handle_action_stability", Value Bool;
                  Literal "handle_action_build_core", Value Bool;
                  Literal "handle_action_send_missionary", Value Bool;
                  Literal "handle_action_change_culture", Value Bool;
                  Literal "siege_won_trigger", Type "province_conditions_def";
                  Literal "siege_won_effect", Type "province_effects_def";
                  Literal "can_negotiate_trigger", Type "country_conditions_def";
                  Literal "can_enforce_trigger", Type "country_conditions_def";
                  Literal "demands_accepted_effect", Type "country_effects_def";
                  Literal "demands_enforced_effect", Type "country_effects_def";
                  Literal "demands_description", Value Keyword;
                  Literal "spawn_chance",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "factor", Number;
                         Literal "modifier",
                         Inherit ([ Literal "factor", Number ], [ "province_conditions_def" ]);
                       ]);
                  Literal "movement_evaluation",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "factor", Number;
                         Literal "modifier",
                         Inherit ([ Literal "factor", Number ], [ "province_conditions_def" ]);
                       ]);
                ]) );
       ])

let eu4_disasters_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("disaster", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "potential", Type "country_conditions_def";
                  Literal "can_start", Type "country_conditions_def";
                  Literal "can_end", Type "country_conditions_def";
                  Literal "can_stop", Type "country_conditions_def";
                  Literal "stability_hit_on_war", Integer;
                  Literal "ended_by_country_breaking_to_rebels", Value Bool;
                  Literal "on_start", Integer;
                  Literal "on_end", Integer;
                  Literal "modifier", Type "country_modifiers_def";
                  Literal "on_start_effect", Type "country_effects_def";
                  Literal "on_progress_effect", Type "country_effects_def";
                  Literal "progress",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "factor", Number;
                         Literal "modifier",
                         Inherit ([ Literal "factor", Number ], [ "country_conditions_def" ]);
                       ]);
                  Literal "down_progress",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "factor", Number;
                         Literal "modifier",
                         Inherit ([ Literal "factor", Number ], [ "country_conditions_def" ]);
                       ]);
                  Literal "on_monthly",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "random_events", eu4_random_events_weighted;
                         Literal "events", ValueList Integer;
                       ]);
                ]) );
       ])

let eu4_scripted_triggers_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("scripted_trigger", Value Keyword)
         , Inherit ([], [ "country_conditions_def"; "province_conditions_def" ]) );
       ])

let eu4_scripted_effects_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("scripted_effect", Value Keyword)
         , Inherit ([], [ "country_effects_def"; "province_effects_def" ]) );
       ])

let eu4_scripted_functions_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("scripted_function", Value Keyword)
         , Inherit
             ( [
                 Literal "condition",
                 SubTable
                   (symbol_table_init
                      [
                        Literal "tooltip", TypeOption [ Value String; Identifier ];
                        Literal "potential", Type "country_conditions_def";
                        Literal "allow", Type "country_conditions_def";
                      ]);
               ]
             , [ "country_conditions_def"; "country_effects_def" ] ) );
       ])

let eu4_opinion_modifiers_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("opinion_modifier", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "opinion", Integer;
                  Literal "yearly_decay", Decimal;
                  Literal "min", Integer;
                  Literal "max", Integer;
                  Literal "max_vassal", Integer;
                  Literal "max_in_other_direction", Integer;
                  Literal "years", Integer;
                  Literal "months", Integer;
                ]) );
       ])

let eu4_province_triggered_modifiers_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("ptm", Value Keyword)
         , Inherit
             ( [
                 Literal "potential", Type "province_conditions_def";
                 Literal "trigger", Type "province_conditions_def";
                 Literal "on_activation", Type "province_effects_def";
                 Literal "on_deactivation", Type "province_effects_def";
                 Literal "picture", Value String;
               ]
             , [ "province_modifiers_def" ] ) );
       ])

let eu4_timed_modifiers_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("timed_modifier", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  ( Catalog ("modifier_component", Value Keyword)
                  , SubTable
                      (symbol_table_init
                         [
                           Literal "value", Decimal;
                           Literal "yearly_decay", Decimal;
                         ]) );
                ]) );
       ])

let eu4_technology_txt_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "groups",
         SubTable
           (symbol_table_init
              [
                ( Catalog ("tech_group", Value Keyword)
                , SubTable
                    (symbol_table_init
                       [
                         Literal "start_level", Value PositiveInt;
                         Literal "start_cost_modifier", Decimal;
                         Literal "power", Decimal;
                         Literal "cav_to_inf_ratio", Decimal;
                         Literal "trade_company", Value Bool;
                         Literal "valid_for_nation_designer", Value Bool;
                         Literal "is_primitive", Value Bool;
                         Literal "nation_designer_trigger", Type "country_conditions_def";
                         Literal "nation_designer_unit_type", Value Keyword;
                         Literal "nation_designer_cost",
                         SubTable
                           (symbol_table_init
                              [
                                Literal "trigger", Type "country_conditions_def";
                                Literal "value", Integer;
                              ]);
                       ]) );
              ]);
         Literal "tables",
         SubTable
           (symbol_table_init [ (Catalog ("table_row", Value Keyword), Value String) ]);
       ])

let eu4_technologies_dir_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("technology_id", Value Keyword)
         , Inherit
             ( [
                 Literal "year", Value PositiveInt;
                 Literal "monarch_power", Value Keyword;
                 Literal "ahead_of_time", Type "country_modifiers_def";
                 Literal "merchants", Value Bool;
                 Literal "allow_client_states", Value Bool;
                 Literal "may_build_flagships", Value Bool;
                 Literal "form_coalition", Value Bool;
                 Literal "blockade", Value Bool;
                 Literal "may_support_rebels", Value Bool;
                 Literal "trade_company", Value Bool;
                 Literal "trade_range", Integer;
                 Literal "naval_maintenance", Decimal;
                 Literal "reduced_naval_attrition", Value Bool;
                 Literal "supply_limit", Decimal;
                 Literal "military_tactics", Decimal;
                 Literal "maneuver_value", Decimal;
                 Literal "sprite_level", Decimal;
                 Literal "may_drill", Value Bool;
                 Literal "combat_width", Decimal;
                 Literal "naval_engagement_width", Integer;
                 Literal "allowed_idea_groups", Integer;
               ]
               @ [
                   ( Catalog ("enable_line", Value Keyword)
                   , TypeOption [ Value Bool; Value Keyword ] );
                 ]
             , [ "country_modifiers_def" ] ) );
       ])

let eu4_units_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("unit_name", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "unit_type", Value Keyword;
                  Literal "type", Value Keyword;
                  Literal "trigger", Type "country_conditions_def";
                  Literal "maneuver", Integer;
                  Literal "offensive_morale", Integer;
                  Literal "defensive_morale", Integer;
                  Literal "offensive_fire", Integer;
                  Literal "defensive_fire", Integer;
                  Literal "offensive_shock", Integer;
                  Literal "defensive_shock", Integer;
                  Literal "hull_size", Decimal;
                  Literal "base_cannons", Decimal;
                  Literal "sail_speed", Decimal;
                  Literal "blockade", Decimal;
                  Literal "trade_power", Decimal;
                  Literal "sprite_level", Integer;
                  Literal "manpower", Decimal;
                  Literal "sailors", Decimal;
                ]) );
       ])

let eu4_naval_doctrines_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("doctrine", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "can_select", Type "country_conditions_def";
                  Literal "cost", Decimal;
                  Literal "country_modifier", Type "country_modifiers_def";
                  Literal "effect", Type "country_effects_def";
                  Literal "removed_effect", Type "country_effects_def";
                  Literal "button_gfx", Integer;
                ]) );
       ])

let eu4_flagship_modifications_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("modification", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "base_modification", Value Bool;
                  Literal "trigger", Type "country_conditions_def";
                  Literal "modifier", Type "country_modifiers_def";
                  Literal "ai_trade_score",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "factor", Decimal;
                         Literal "modifier",
                         Inherit ([ Literal "factor", Decimal ], [ "country_conditions_def" ]);
                       ]);
                  Literal "ai_war_score",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "factor", Decimal;
                         Literal "modifier",
                         Inherit ([ Literal "factor", Decimal ], [ "country_conditions_def" ]);
                       ]);
                ]) );
       ])

let eu4_professionalism_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("professionalism_entry", Value Keyword)
         , Inherit
             ( [
                 Literal "hidden", Value Bool;
                 Literal "army_professionalism", Decimal;
                 Literal "trigger", Type "country_conditions_def";
                 Literal "marker_sprite", Value String;
                 Literal "unit_sprite_start", Value String;
                 Literal "may_build_supply_depot", Value Bool;
                 Literal "may_refill_garrison", Value Bool;
                 Literal "may_return_manpower_on_disband", Value Bool;
               ]
             , [ "country_modifiers_def" ] ) );
       ])

let eu4_government_mechanics_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("mechanic", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "alert_icon_gfx", Value Keyword;
                  Literal "alert_icon_index", Integer;
                  Literal "available", Type "country_conditions_def";
                  Literal "interactions",
                  SubTable
                    (symbol_table_init
                       [
                         ( Catalog ("interaction", Value Keyword)
                         , SubTable
                             (symbol_table_init
                                [
                                  Literal "icon", TypeOption [ Value Keyword; Value String ];
                                  Literal "trigger", Type "country_conditions_def";
                                  Literal "effect", Type "country_effects_def";
                                  Literal "ai_will_do",
                                  SubTable
                                    (symbol_table_init
                                       [
                                         Literal "factor", Number;
                                         Literal "modifier",
                                         Inherit
                                           ([ Literal "factor", Number ], [ "country_conditions_def" ]);
                                       ]);
                                ]) );
                       ]);
                ]) );
       ])

let eu4_music_songs_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "song",
         SubTable
           (symbol_table_init
              [
                Literal "name", Value String;
                Literal "chance",
                SubTable
                  (symbol_table_init
                     [
                       Literal "modifier",
                       Inherit ([ Literal "factor", Number ], [ "country_conditions_def" ]);
                     ]);
              ]);
       ])

let eu4_trade_node_outgoing_block =
  SubTable
    (symbol_table_init
       [
         Literal "name", TypeOption [ Value Keyword; Value String ];
         Literal "path", ValueList Integer;
         Literal "control", ValueList Decimal;
       ])

let eu4_trade_nodes_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("trade_node", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "location", Integer;
                  Literal "color", ValueList Number;
                  Literal "inland", Value Bool;
                  Literal "outgoing", Type "eu4_trade_node_outgoing_block_def";
                  Literal "members", ValueList Integer;
                ]) );
       ])

let eu4_trade_companies_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("trade_company", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "color", ValueList Number;
                  Literal "provinces", ValueList Integer;
                  Literal "names",
                  SubTable
                    (symbol_table_init
                       [
                         Literal "name", TypeOption [ Value String; Identifier ];
                       ]);
                ]) );
       ])

let eu4_centers_of_trade_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("cot", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "level", Integer;
                  Literal "type", Value Keyword;
                  Literal "development", Integer;
                  Literal "cost", Integer;
                  Literal "province_modifiers", Type "province_modifiers_def";
                ]) );
       ])

let eu4_prices_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("good_price", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "base_price", Decimal;
                  Literal "goldtype", Value Bool;
                ]) );
       ])

let eu4_country_colors_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("tag", Value Tag)
         , SubTable
             (symbol_table_init
                [
                  Literal "color1", ValueList Number;
                  Literal "color2", ValueList Number;
                  Literal "color3", ValueList Number;
                ]) );
       ])

let eu4_province_names_file_def =
  SubTable
    (symbol_table_init
       [
         ( Integer
         , TypeOption [ Value String; ValueList (Value String) ] );
       ])

let eu4_customizable_localization_file_def =
  SubTable Customizable_localization.customizeable_localization

let eu4_color_palette_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "num_symbols", Integer;
         Literal "color", ValueList Number;
       ])

let eu4_historical_lucky_file_def =
  SubTable
    (symbol_table_init
       [
         (Catalog ("lucky_tag", Value Tag), Type "country_conditions_def");
       ])

let eu4_map_lakes_file_def =
  SubTable
    (symbol_table_init
       [
         Literal "lake",
         SubTable
           (symbol_table_init
              [
                Literal "triangle_strip", ValueList Integer;
                Literal "height", Integer;
              ]);
       ])

let eu4_units_display_file_def =
  SubTable
    (symbol_table_init
       [
         ( Catalog ("display_category", Value Keyword)
         , SubTable
             (symbol_table_init
                [
                  Literal "factor", Number;
                  Literal "modifier",
                  Inherit ([ Literal "factor", Number ], [ "country_conditions_def" ]);
                ]) );
       ])

let catalog_definitions =
  [
    Definition "eu4_units_display_file_def", eu4_units_display_file_def;
    Definition "eu4_trade_node_outgoing_block_def", eu4_trade_node_outgoing_block;
    Definition "eu4_on_actions_file_def", eu4_on_actions_file_def;
    Definition "eu4_achievements_file_def", eu4_achievements_file_def;
    Definition "eu4_rebel_types_file_def", eu4_rebel_types_file_def;
    Definition "eu4_disasters_file_def", eu4_disasters_file_def;
    Definition "eu4_scripted_triggers_file_def", eu4_scripted_triggers_file_def;
    Definition "eu4_scripted_effects_file_def", eu4_scripted_effects_file_def;
    Definition "eu4_scripted_functions_file_def", eu4_scripted_functions_file_def;
    Definition "eu4_opinion_modifiers_file_def", eu4_opinion_modifiers_file_def;
    Definition "eu4_province_triggered_modifiers_file_def"
    , eu4_province_triggered_modifiers_file_def;
    Definition "eu4_timed_modifiers_file_def", eu4_timed_modifiers_file_def;
    Definition "eu4_technology_txt_file_def", eu4_technology_txt_file_def;
    Definition "eu4_technologies_dir_file_def", eu4_technologies_dir_file_def;
    Definition "eu4_units_file_def", eu4_units_file_def;
    Definition "eu4_naval_doctrines_file_def", eu4_naval_doctrines_file_def;
    Definition "eu4_flagship_modifications_file_def", eu4_flagship_modifications_file_def;
    Definition "eu4_professionalism_file_def", eu4_professionalism_file_def;
    Definition "eu4_government_mechanics_file_def", eu4_government_mechanics_file_def;
    Definition "eu4_music_songs_file_def", eu4_music_songs_file_def;
    Definition "eu4_trade_nodes_file_def", eu4_trade_nodes_file_def;
    Definition "eu4_trade_companies_file_def", eu4_trade_companies_file_def;
    Definition "eu4_centers_of_trade_file_def", eu4_centers_of_trade_file_def;
    Definition "eu4_prices_file_def", eu4_prices_file_def;
    Definition "eu4_country_colors_file_def", eu4_country_colors_file_def;
    Definition "eu4_province_names_file_def", eu4_province_names_file_def;
    Definition "eu4_customizable_localization_file_def", eu4_customizable_localization_file_def;
    Definition "eu4_color_palette_file_def", eu4_color_palette_file_def;
    Definition "eu4_historical_lucky_file_def", eu4_historical_lucky_file_def;
    Definition "eu4_map_lakes_file_def", eu4_map_lakes_file_def;
  ]
