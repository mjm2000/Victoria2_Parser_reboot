open SymbolTable
open TypeDef

(* Terrain categories use sprite / color metadata mixed with modifier-like keys (Audax Map.pdox). *)

let terrain_inner_merge =
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

let areas =
  symbol_table_init
    [ (Catalog ("area", Value Keyword), ValueList (Catalog ("land_provid", WholeNumber))) ]

let regions =
  symbol_table_init
    [ (Catalog ("region", Value Keyword), SubTable (symbol_table_init [
          (Literal "areas", SubTable (symbol_table_init [
                (Value Keyword, ValueList (Value Keyword));
              ]));
        ]))
    ]

let superregions =
  symbol_table_init
    [ (Catalog ("superregion", Value Keyword), ValueList (Type "region")) ]

let continents =
  symbol_table_init
    [ (Catalog ("continent", Value Keyword), ValueList (Catalog ("land_provid", WholeNumber))) ]

let provincegroups =
  symbol_table_init
    [ (Catalog ("provincegroup", Value Keyword), ValueList (Type "land_provid")) ]

(* --- Audax Map.pdox / MapCommon.pdox (EU4 map) --- *)

let default_map =
  symbol_table_init [
    Literal "width", TypeOption [Value PositiveInt; WholeNumber; Integer];
    Literal "height", TypeOption [Value PositiveInt; WholeNumber; Integer];
    Literal "max_provinces", TypeOption [Value PositiveInt; WholeNumber; Integer];
    Literal "sea_starts", ValueList WholeNumber;
    Literal "only_used_for_random", ValueList WholeNumber;
    Literal "lakes", ValueList WholeNumber;
    Literal "force_coastal", ValueList WholeNumber;
    Literal "definitions", Value String;
    Literal "provinces", Value String;
    Literal "positions", Value String;
    Literal "terrain", Value String;
    Literal "rivers", Value String;
    Literal "terrain_definition", Value String;
    Literal "heightmap", Value String;
    Literal "tree_definition", Value String;
    Literal "continent", Value String;
    Literal "adjacencies", Value String;
    Literal "climate", Value String;
    Literal "region", Value String;
    Literal "superregion", Value String;
    Literal "area", Value String;
    Literal "provincegroup", Value String;
    Literal "ambient_object", Value String;
    Literal "seasons", Value String;
    Literal "trade_winds", Value String;
    Literal "canal_definition", SubTable (symbol_table_init [
        Literal "name", Value String;
        Literal "x", TypeOption [Value PositiveInt; WholeNumber; Integer];
        Literal "y", TypeOption [Value PositiveInt; WholeNumber; Integer];
      ]);
    Literal "tree", ValueList WholeNumber;
  ]

(* ClimateFile in Audax Map.pdox *)
let climate_file =
  let province_block = ValueList WholeNumber in
  symbol_table_init [
    Literal "impassable", province_block;
    Literal "arid", province_block;
    Literal "arctic", province_block;
    Literal "tropical", province_block;
    Literal "mild_winter", province_block;
    Literal "normal_winter", province_block;
    Literal "severe_winter", province_block;
    Literal "mild_monsoon", province_block;
    Literal "normal_monsoon", province_block;
    Literal "severe_monsoon", province_block;
    Literal "equator_y_on_province_image", TypeOption [WholeNumber; Integer; Value PositiveInt];
  ]

let trade_winds_file =
  symbol_table_init [
    (Catalog ("trade_wind_province", WholeNumber), TypeOption [
        WholeNumber; Integer; Decimal; Number; Value PositiveInt; Value NegativeInt;
      ]);
  ]

let season_block =
  symbol_table_init [
    Literal "start_date", TypeOption [Value Date; Value String];
    Literal "end_date", TypeOption [Value Date; Value String];
    Literal "hsv_north", ValueList Number;
    Literal "colorbalance_north", ValueList Number;
    Literal "hsv_center", ValueList Number;
    Literal "colorbalance_center", ValueList Number;
    Literal "hsv_south", ValueList Number;
    Literal "colorbalance_south", ValueList Number;
  ]

let seasons_file =
  symbol_table_init [
    Literal "winter", SubTable season_block;
    Literal "spring", SubTable season_block;
    Literal "summer", SubTable season_block;
    Literal "autumn", SubTable season_block;
  ]

let tree_terrain_entry =
  symbol_table_init [
    Literal "terrain", Value Keyword;
    Literal "color", ValueList WholeNumber;
  ]

(* TerrainFile: categories / terrain / tree (Audax Map.pdox) *)
let terrain_file =
  symbol_table_init [
    Literal "categories", SubTable (symbol_table_init [
        (Catalog ("terrain_category_name", Value Keyword), terrain_inner_merge);
      ]);
    Literal "terrain", SubTable (symbol_table_init [
        (Catalog ("terrain_map_entry", Value Keyword), terrain_inner_merge);
      ]);
    Literal "tree", SubTable (symbol_table_init [
        (Catalog ("tree_kind", Value Keyword), SubTable tree_terrain_entry);
      ]);
  ]

(* AmbientObject path in Core.pdox: map\ambient_object.txt type\object\... *)
let ambient_object_body =
  symbol_table_init [
    Literal "name", TypeOption [Value String; Value Keyword];
    Literal "hidden_on_start", Value Bool;
    Literal "position", ValueList Number;
    Literal "rotation", ValueList Number;
  ]

let ambient_object_type_block =
  symbol_table_init [
    Literal "type", TypeOption [Value String; Value Keyword];
    Literal "use_animation", Value Bool;
    Literal "scale", Decimal;
    Literal "time_duration", Decimal;
    Literal "object", SubTable ambient_object_body;
  ]

let ambient_object_file =
  symbol_table_init [
    Literal "type", SubTable ambient_object_type_block;
  ]
