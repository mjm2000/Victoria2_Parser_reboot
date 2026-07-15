open SymbolTable
open Commons
open Conditions
open Decisions
open Effects
open Events
open Modifiers
open Mtth
open Map
open Map_random
open Missions
open TypeDef

(* Descriptor matches launcher .mod / descriptor.mod (see Mod structure on the EU4 wiki). *)

let eu4_mod_file =
  symbol_table_init [
    Literal "name", Catalog ("modname", Value String);
    Literal "path", Catalog ("modpath", Link);
    Literal "supported_version", Value String;
    Literal "picture", Value String;
    Literal "remote_file_id", Value String;
    Literal "version", Value String;
    Literal "tags", ValueList (Value String);
    Literal "dependencies", ValueList (Value String);
    Literal "replace_path", Catalog ("replace_path", Value String);
  ]

let eu4_symbol_table =
  symbol_table_init
    ( [
        Definition "mod_file_def", SubTable eu4_mod_file;
        Definition "country_conditions_def", SubTable country_conditions;
        Definition "province_conditions_def", SubTable province_conditions;
        Definition "decision_def", SubTable decisions;
        Definition "country_effects_def", SubTable country_effects;
        Definition "province_effects_def", SubTable province_effects;
        Definition "country_modifiers_def", SubTable country_modifiers;
        Definition "province_modifiers_def", SubTable province_modifiers;
        Definition "country_mtth", SubTable country_mtth;
        Definition "province_mtth", SubTable province_mtth;
        Definition "event_def", SubTable events;
        Definition "areas_def", SubTable areas;
        Definition "regions_def", SubTable regions;
        Definition "superregions_def", SubTable superregions;
        Definition "continents_def", SubTable continents;
        Definition "provincegroups_def", SubTable provincegroups;
        Definition "eu4_default_map_def", SubTable default_map;
        Definition "eu4_climate_file_def", SubTable climate_file;
        Definition "eu4_trade_winds_file_def", SubTable trade_winds_file;
        Definition "eu4_seasons_file_def", SubTable seasons_file;
        Definition "eu4_terrain_file_def", SubTable terrain_file;
        Definition "eu4_ambient_object_file_def", SubTable ambient_object_file;
        Definition "eu4_random_rnw_scenarios_file_def", SubTable rnw_scenarios_file;
        Definition "eu4_random_name_pool_file_def", SubTable random_name_pool_file;
        Definition "eu4_random_tile_file_def", rnw_tile_file;
        Definition "eu4_mission_task_body_def", eu4_mission_task_body;
        Definition "eu4_mission_province_task_body_def", eu4_mission_province_task_body;
        Definition "mission_group_block_def", SubTable mission_group_inner;
        Definition "eu4_mission_file_def", SubTable eu4_mission_file;
        Definition "scope", TypeOption [ Literal "ROOT"; Literal "FROM"; Literal "PREV"; Literal "THIS" ];
        Definition "target", TypeOption [ Type "scope"; Type "country_tag" ];
      ]
    @ Eu4_pulse.pulse_definitions
    @ Eu4_history.history_definitions
    @ Eu4_fallback.fallback_definitions
    @ Eu4_catalog.catalog_definitions
    @ commons )

(* Game paths grouped like Audax.Validator [Extension/Data/Eu4]: section order
   follows [Core.pdox] Includes; paths under each heading match the FileGroups in
   the corresponding *.pdox (History/, Map.pdox, Governments.pdox, …). *)

let eu4_paths =
  List.rev
    [

      "common/country_tags", "countries_def";
      "common/religions", "religions_def";
      (* --- History (Audax History/*.pdox) --- *)
      "history/advisors", "eu4_advisor_history_file_def";
      "history/countries", "eu4_country_history_file_def";
      "history/diplomacy", "eu4_diplomacy_history_file_def";
      "history/wars", "eu4_war_history_file_def";
      (* --- Achievements --- *)
      "common/achievements.txt", "eu4_achievements_file_def";
      (* --- AdvisorTypes --- *)
      "common/advisortypes", "advisor_types_def";
      (* --- AI --- *)
      "common/ai_army", "eu4_country_script_file_def";
      "common/ai_attitudes", "eu4_country_script_file_def";
      "common/ai_personalities", "eu4_country_script_file_def";
      (* --- Bookmarks --- *)
      "common/bookmarks", "bookmarks_def";
      (* --- Buildings --- *)
      "common/buildings", "buildings_def";
      "common/great_projects", "eu4_mixed_scope_script_file_def";
      (* --- Colonial regions --- *)
      "common/colonial_regions", "eu4_mixed_scope_script_file_def";
      "common/powerprojection", "eu4_country_script_file_def";
      (* --- Country --- *)
      "common/countries", "country_def";
      "common/country_colors", "eu4_country_colors_file_def";
      "common/custom_country_colors", "eu4_color_palette_file_def";
      "common/dynasty_colors", "eu4_color_palette_file_def";
      (* --- Cultures --- *)
      "common/cultures", "cultures_def";
      (* --- Diplomacy definitions --- *)
      "common/diplomatic_actions", "eu4_mixed_scope_script_file_def";
      "common/new_diplomatic_actions", "eu4_mixed_scope_script_file_def";
      "common/subject_types", "eu4_mixed_scope_script_file_def";
      "common/subject_type_upgrades", "eu4_mixed_scope_script_file_def";
      (* --- Disasters --- *)
      "common/disasters", "eu4_disasters_file_def";
      (* --- Factions --- *)
      "common/factions", "eu4_country_script_file_def";
      (* --- Governments / estates --- *)
      "common/client_states", "eu4_mixed_scope_script_file_def";
      "common/custom_ideas", "eu4_mixed_scope_script_file_def";
      "common/decrees", "eu4_mixed_scope_script_file_def";
      "common/estate_agendas", "eu4_mixed_scope_script_file_def";
      "common/estate_crown_land", "eu4_mixed_scope_script_file_def";
      "common/estates", "eu4_mixed_scope_script_file_def";
      "common/estates_preload", "eu4_mixed_scope_script_file_def";
      "common/fervor", "eu4_mixed_scope_script_file_def";
      "common/golden_bulls", "eu4_mixed_scope_script_file_def";
      "common/government_mechanics", "eu4_government_mechanics_file_def";
      "common/government_names", "eu4_country_script_file_def";
      "common/government_ranks", "eu4_country_script_file_def";
      "common/government_reforms", "eu4_mixed_scope_script_file_def";
      "common/governments", "government_types_def";
      "common/hegemons", "eu4_mixed_scope_script_file_def";
      "common/institutions", "institutions_def";
      "common/isolationism", "eu4_mixed_scope_script_file_def";
      "common/leader_personalities", "eu4_mixed_scope_script_file_def";
      "common/parliament_bribes", "eu4_mixed_scope_script_file_def";
      "common/parliament_issues", "eu4_mixed_scope_script_file_def";
      "common/policies", "eu4_mixed_scope_script_file_def";
      "common/revolution", "eu4_mixed_scope_script_file_def";
      "common/ruler_personalities", "eu4_mixed_scope_script_file_def";
      "common/state_edicts", "eu4_mixed_scope_script_file_def";
      (* --- Imperial --- *)
      "common/imperial_incidents", "eu4_mixed_scope_script_file_def";
      "common/imperial_reforms", "eu4_mixed_scope_script_file_def";
      (* --- Customizable localization --- *)
      "customizable_localization", "eu4_customizable_localization_file_def";
      (* --- Map --- *)
      "map/default.map", "eu4_default_map_def";
      "map/area.txt", "areas_def";
      "map/climate.txt", "eu4_climate_file_def";
      "map/provincegroup.txt", "provincegroups_def";
      "map/region.txt", "regions_def";
      "map/superregion.txt", "superregions_def";
      "map/ambient_object.txt", "eu4_ambient_object_file_def";
      "map/lakes", "eu4_map_lakes_file_def";
      "map/positions.txt", "eu4_mixed_scope_script_file_def";
      "map/random/RNWScenarios.txt", "eu4_random_rnw_scenarios_file_def";
      "map/random/RandomLakeNames.txt", "eu4_random_name_pool_file_def";
      "map/random/RandomLandNames.txt", "eu4_random_name_pool_file_def";
      "map/random/RandomSeaNames.txt", "eu4_random_name_pool_file_def";
      (*"map/random/tiles", "eu4_random_tile_file_def"; *)
      "map/seasons.txt", "eu4_seasons_file_def";
      "map/terrain.txt", "eu4_terrain_file_def";
      "map/trade_winds.txt", "eu4_trade_winds_file_def";
      "common/natives", "eu4_mixed_scope_script_file_def";
      "common/province_names", "eu4_province_names_file_def";
      (* --- Misc --- *)
      "common/ages", "eu4_mixed_scope_script_file_def";
      "common/alerts.txt", "eu4_mixed_scope_script_file_def";
      "common/ancestor_personalities", "eu4_mixed_scope_script_file_def";
      "common/custom_gui", "eu4_mixed_scope_script_file_def";
      "common/federation_advancements", "eu4_mixed_scope_script_file_def";
      "common/graphicalculturetype.txt", "graphical_culture_type_def";
      "common/historial_lucky.txt", "eu4_historical_lucky_file_def";
      "common/incidents", "eu4_mixed_scope_script_file_def";
      "common/insults", "eu4_mixed_scope_script_file_def";
      "music/songs.txt", "eu4_music_songs_file_def";
      (*"tutorial", "eu4_mixed_scope_script_file_def";*)
      (* --- On actions --- *)
      "common/on_actions", "eu4_on_actions_file_def";
      (* --- Rebels --- *)
      "common/rebel_types", "eu4_rebel_types_file_def";
      (* --- Religion extras --- *)
      "common/church_aspects", "eu4_mixed_scope_script_file_def";
      "common/defender_of_faith", "eu4_mixed_scope_script_file_def";
      "common/fetishist_cults", "eu4_mixed_scope_script_file_def";
      "common/holy_orders", "eu4_mixed_scope_script_file_def";
      "common/personal_deities", "eu4_mixed_scope_script_file_def";
      "common/religious_conversions", "eu4_mixed_scope_script_file_def";
      "common/religious_reforms", "eu4_mixed_scope_script_file_def";
      (* --- Revolt triggers --- *)
      "common/revolt_triggers", "eu4_country_script_file_def";
      (* --- Tech --- *)
      "common/technologies", "eu4_technologies_dir_file_def";
      "common/technology.txt", "eu4_technology_txt_file_def";
      (* --- Trading --- *)
      "common/centers_of_trade", "eu4_centers_of_trade_file_def";
      "common/prices", "eu4_prices_file_def";
      "common/region_colors", "eu4_color_palette_file_def";
      "common/tradecompany_investments", "eu4_mixed_scope_script_file_def";
      "common/tradegoods", "trade_goods_def";
      "common/tradenodes", "eu4_trade_nodes_file_def";
      "common/trading_policies", "eu4_country_script_file_def";
      (* --- Military --- *)
      "common/flagship_modifications", "eu4_flagship_modifications_file_def";
      "common/mercenary_companies", "eu4_mixed_scope_script_file_def";
      "common/naval_doctrines", "eu4_naval_doctrines_file_def";
      "common/professionalism", "eu4_professionalism_file_def";
      "common/units", "eu4_units_file_def";
      "common/units_display", "eu4_units_display_file_def";
      (* --- Wars / treaties --- *)
      "common/peace_treaties", "eu4_mixed_scope_script_file_def";
      "common/wargoal_types", "eu4_mixed_scope_script_file_def";
      (* --- Scripted --- *)
      "common/scripted_effects", "eu4_scripted_effects_file_def";
      "common/scripted_functions", "eu4_scripted_functions_file_def";
      "common/scripted_triggers", "eu4_scripted_triggers_file_def";
      "decisions", "decision_def";
      "events", "event_def";
      "map/continent.txt", "continents_def";
      "history/provinces", "eu4_province_history_file_def";
      "common/estate_privileges", "estate_privileges_def";
      "common/event_modifiers", "event_modifiers_def";
      "common/opinion_modifiers", "eu4_opinion_modifiers_file_def";
      "common/province_triggered_modifiers", "eu4_province_triggered_modifiers_file_def";
      "common/static_modifiers", "static_modifiers_def";
      "common/timed_modifiers", "eu4_timed_modifiers_file_def";
      "common/triggered_modifiers", "triggered_modifiers_def";
      "common/cb_types", "cb_types_def";
      "common/ideas", "idea_groups_def";
      "missions", "eu4_mission_file_def";
      "common/trade_companies", "eu4_trade_companies_file_def";
    ]
