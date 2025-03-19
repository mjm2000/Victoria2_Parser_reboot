
let symbol_table_init symbols =
    let symbol_list_size = List.length symbols in
    let symbol_table = Hashtbl.create symbol_list_size in
    let rec add_symbols symbols = match symbols with 
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols; 
    symbol_table

let append_table symbol_table symbols = 
    let rec add_symbols symbols = match symbols with 
        |(lh_value, _)::_ when Hashtbl.mem symbol_table lh_value -> 
                ()
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols;; 
    

let combine_table_pair table_take table_give = 
    Hashtbl.iter (fun key value -> Hashtbl.replace table_take key value) table_give ;;

let combine_table_list tables = 
  let table = Hashtbl.create 500 in
  List.iter (fun t -> combine_table_pair table t) tables;
  table







let member symbol_table symbol = 
    Hashtbl.mem symbol_table symbol;;
