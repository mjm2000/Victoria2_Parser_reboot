open SymbolTable
open TypeDef

(* map/random: RNW scenario definitions, name pools, and per-tile color maps.
   Shapes taken from vanilla EU4 (testdata map/random). *)



let rnw_rgb_list = ValueList WholeNumber

let rnw_strait_block =
  symbol_table_init [
    Literal "from", rnw_rgb_list;
    Literal "to", rnw_rgb_list;
    Literal "through", rnw_rgb_list;
  ]

let rnw_name_list_block =
  symbol_table_init [
    (Nothing, TypeOption [Value Keyword; Value String; Identifier]);
  ]

let rnw_scenario_block =
  SubTable (symbol_table_init (
    [ Literal "temperate", Value Bool
    ; Literal "arid", Value Bool
    ; Literal "arctic", Value Bool
    ; Literal "tropical", Value Bool
    ; Literal "min_native_size", WholeNumber
    ; Literal "max_native_size", WholeNumber 
    ; Literal "min_native_hostility", WholeNumber 
    ; Literal "max_native_hostility", WholeNumber 
    ; Literal "min_native_ferocity", WholeNumber 
    ; Literal "max_native_ferocity", WholeNumber 
    ; Literal "religion", Type "religion"
    ; Literal "unique", Value Bool
    ; Literal "chance",  Value PositiveFloat  
    ; Literal "technology_group", Value Keyword
    ; Literal "government", TypeOption [Value Keyword; Value String]
    ; Literal "graphical_culture", Value Keyword
    ; Literal "force_apart", Value Bool
    ; Literal "force_together", Value Bool
    ; Literal "minor_tags", Value Bool
    ; Literal "min_provinces", WholeNumber 
    ; Literal "min_countries", WholeNumber 
    ; Literal "max_countries", WholeNumber 
    ; Literal "min_country_size", WholeNumber 
    ; Literal "max_country_size", WholeNumber 
    ; Literal "culture_group", Value Keyword
    ; Literal "culture", Value Keyword
    ; Literal "names", SubTable rnw_name_list_block
    ]
    )
    )

let rnw_scenarios_file =
  symbol_table_init [
    (Catalog ("rnw_scenario", Value Keyword), rnw_scenario_block);
  ]

let random_name_pool_file =
  symbol_table_init [
    Literal "random_names", SubTable rnw_name_list_block;
  ]

let rnw_tile_file =
  SubTable (symbol_table_init(
    [ Literal "sea_province", rnw_rgb_list
    ; Literal "wasteland_province", rnw_rgb_list
    ; Literal "lake_province", rnw_rgb_list
    ; Literal "river_estuary_modifier", rnw_rgb_list
    ; Literal "level_1_center_of_trade", rnw_rgb_list
    ; Literal "devils_gate_modifier", rnw_rgb_list
    ; Literal "strait", (SubTable (symbol_table_init [
        Literal "from", ValueList WholeNumber;
        Literal "to", ValueList WholeNumber;
        Literal "through", ValueList WholeNumber;
      ] )
    )
    ; Literal "region", ValueList WholeNumber 
    ; Literal "regions",  WholeNumber
    ; Literal "num_sea_provinces", WholeNumber 
    ; Literal "num_land_provinces", WholeNumber 
    ; Literal "size", ValueList WholeNumber
    ; Literal "continent", Value Bool
    ; Literal "weight", WholeNumber 
    ; Literal "empty",ValueList WholeNumber 
    ; Literal "do_not_rotate", Value Bool
    ; Literal "fantasy", Value Bool
    ; Literal "province_names", SubTable (symbol_table_init [
        (Value String, ValueList WholeNumber);
      ])
    ]))
