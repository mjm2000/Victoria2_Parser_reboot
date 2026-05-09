open SymbolTable
open TypeDef

(* Weighted random_event lists (EU4 on_actions, disasters, pulses). *)

let eu4_random_events_weighted =
  SubTable (symbol_table_init [ (Value PositiveInt, Integer) ])

(* Audax CountryPulse / ProvincePulse: optional events + inherits *Command *)

let eu4_country_pulse_def =
  Inherit
    ( [
        Literal "random_events", eu4_random_events_weighted;
        Literal "events", ValueList Integer;
      ]
    , [ "country_effects_def" ] )

let eu4_province_pulse_def =
  Inherit
    ( [
        Literal "random_events", eu4_random_events_weighted;
        Literal "events", ValueList Integer;
      ]
    , [ "province_effects_def" ] )

let pulse_definitions =
  [
    Definition "eu4_country_pulse_def", eu4_country_pulse_def;
    Definition "eu4_province_pulse_def", eu4_province_pulse_def;
  ]
