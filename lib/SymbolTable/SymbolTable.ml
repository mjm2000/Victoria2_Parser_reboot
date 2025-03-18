

let symbol_table_init symbols =
    let symbol_list_size = List.length symbols in
    let symbol_table = CCHashtbl.create symbol_list_size in
    let rec add_symbols symbols = match symbols with 
        (*|(lh_value, rh_value)::tail when CCHashtbl.mem symbol_table lh_value -> 
            raise Invalid_Argument ("Symbol " ^ lh_value ^ " already exists") *)
        |(lh_value, rh_value)::tail ->
            CCHashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols; 
    symbol_table

let append_table symbol_table symbols = 
    let rec add_symbols symbols = match symbols with 
        |(lh_value, _)::_ when CCHashtbl.mem symbol_table lh_value -> 
                ()
        |(lh_value, rh_value)::tail ->
            CCHashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols;; 
    

let combine_table_pair table_take table_give = 
    CCHashtbl.iter (fun key value -> Hashtbl.replace table_take key value) table_give ;;

let combine_table_list tables = 
  let table = CCHashtbl.create 500 in
  List.iter (fun t -> combine_table_pair table t) tables;
  table

let lookup symbol_table symbol = 
    match CCHashtbl.find_opt symbol_table symbol with
    | Some value -> value
    | None -> raise (Invalid_argument ("Symbol  not found"))


let member symbol_table symbol = 
    CCHashtbl.mem symbol_table symbol;;
