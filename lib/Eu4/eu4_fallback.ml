open TypeDef

(* Narrower than the old generic_script merge: scope-appropriate clauses only. *)

let eu4_country_script_file_def =
  Inherit ([], [ "country_conditions_def"; "country_effects_def"; "country_modifiers_def" ])

let eu4_mixed_scope_script_file_def =
  Inherit
    ( []
    , [
        "country_conditions_def";
        "country_effects_def";
        "country_modifiers_def";
        "province_conditions_def";
        "province_effects_def";
        "province_modifiers_def";
      ] )

let fallback_definitions =
  [
    Definition "eu4_country_script_file_def", eu4_country_script_file_def;
    Definition "eu4_mixed_scope_script_file_def", eu4_mixed_scope_script_file_def;
  ]
