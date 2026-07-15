open SymbolTable
open TypeDef

(* Mission series and tasks mirror in-game structure (testdata/missions). Nested
   series use the same block shape as the root series. Recursion is only through
   [Type "mission_group_block_def"] resolved in the outer symbol table (not [let rec]). *)

let eu4_mission_task_body =
  Inherit
    ( [
        Literal "icon", TypeOption [ Value Keyword; Value String ];
        Literal "position", TypeOption [ Value PositiveInt; Integer; WholeNumber ];
        Literal "completed_by", TypeOption [ Value Date; Value Bool ];
        Literal "mandatory", Value Bool;
        Literal "label", TypeOption [ Value Keyword; Value String ];
        Literal "search_strings", ValueList (Value String);
        Literal "provinces_to_highlight", Type "province_conditions_def";
        Literal "trigger", Type "country_conditions_def";
        Literal "required_missions", ValueList (Value Keyword);
        Literal "effect", Type "country_effects_def";
        Literal "immediate", Type "country_effects_def";
        Literal "abort_effect", Type "country_effects_def";
        Literal "ai_weight",
        SubTable
          (symbol_table_init
             [
               Literal "factor", Number;
               Literal "modifier",
               Inherit ([ Literal "factor", Number ], [ "country_conditions_def" ]);
             ]);
        Literal "ai_priority",
        SubTable (symbol_table_init [ Literal "factor", Decimal ]);
      ]
    , [] )

let eu4_mission_province_task_body =
  Inherit
    ( [
        Literal "allow", Type "province_conditions_def";
        Literal "abort", Type "province_conditions_def";
        Literal "success", Type "province_conditions_def";
        Literal "effect", Type "province_effects_def";
        Literal "immediate", Type "province_effects_def";
        Literal "abort_effect", Type "province_effects_def";
        Literal "chance",
        SubTable
          (symbol_table_init
             [
               Literal "factor", Number;
               Literal "modifier",
               Inherit ([ Literal "factor", Number ], [ "province_conditions_def" ]);
             ]);
      ]
    , [] )

let mission_group_inner =
  symbol_table_init
    [
      Literal "slot", TypeOption [ Value PositiveInt; WholeNumber; Integer ];
      Literal "generic", Value Bool;
      Literal "ai", Value Bool;
      Literal "potential_on_load", Type "country_conditions_def";
      Literal "potential", Type "country_conditions_def";
      Literal "has_country_shield", Value Bool;
      Literal "prevent_potential_on_load_event", Value Bool;
      ( Catalog ("mission_node", Value Keyword)
      , TypeOption
          [
            Type "mission_group_block_def";
            Type "eu4_mission_task_body_def";
            Type "eu4_mission_province_task_body_def";
          ] );
    ]


let eu4_mission_file =
  symbol_table_init
    [ (Catalog ("mission_series", Value Keyword), Type "mission_group_block_def") ]
