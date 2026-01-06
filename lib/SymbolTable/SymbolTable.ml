
let symbol_table_init symbols =
    let symbol_list_size = List.length symbols in
    (* Pre-size with 2x multiplier for better load factor (~50% load) *)
    let symbol_table = Hashtbl.create (symbol_list_size * 2) in
    let rec add_symbols symbols = match symbols with 
        |(TypeDef.Catalog(catagory,symtype),rhs)::tail-> 
            Hashtbl.add symbol_table symtype (TypeDef.CatalogLeft(catagory,rhs));
            add_symbols tail
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols; 
    symbol_table

let append_table symbol_table symbols = 
    let rec add_symbols symbols = match symbols with 
        |(lh_value, _)::tail when Hashtbl.mem symbol_table lh_value -> 
            add_symbols tail;
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols;; 
    

let combine_table_pair table_take table_give = 
    Hashtbl.iter (fun key value -> Hashtbl.replace table_take key value) table_give ;;

let combine_table_list tables = 
  (* Estimate size: sum of all table sizes, with 2x multiplier for load factor *)
  let estimated_size = List.fold_left (fun acc t -> acc + Hashtbl.length t) 0 tables in
  let table = Hashtbl.create (max 500 (estimated_size * 2)) in
  List.iter (fun t -> combine_table_pair table t) tables;
  table







let member symbol_table symbol = 
    Hashtbl.mem symbol_table symbol;;

(* Optimized lookup that combines membership check and retrieval *)
(* Returns Some value if found, None otherwise - avoids double lookup *)
let find_opt symbol_table symbol = 
    Hashtbl.find_opt symbol_table symbol;;

(* Fast lookup with membership check - returns (found, value_opt) *)
(* Useful when you need both membership and value *)
let find_with_mem symbol_table symbol = 
    match Hashtbl.find_opt symbol_table symbol with
    | Some v -> (true, Some v)
    | None -> (false, None);;
