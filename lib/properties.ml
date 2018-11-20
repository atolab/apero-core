open Acommon
open Key_value

module Property = KeyValueF.Make (String) (String)

module Properties = struct
  include Property.Map

  let get = find_opt

  let rec of_list = function
    | [] -> empty
    | (k,v)::l -> add k v @@ of_list l

  let to_list = bindings

  let to_string (p:Property.Value.t t) =
    to_list p |> List.map (fun (k,v)-> k^"="^v) |> String.concat ","
  
  let contains_key prop ps = match get prop ps with | Some _ -> true | None -> false 

  let contains_property k v ps = 
    match get k ps with
    | Some v' -> v = v'
    | None -> false  

  let contains_conflicting_property k v ps = 
    match get k ps with
    | Some v' -> v <> v'
    | None -> false

  let is_subset ps ps' = not @@ exists (fun k v -> not @@ contains_property k v ps') ps

  let not_conflicting ps ps' = not @@ exists (fun k v -> contains_conflicting_property k v ps') ps

  let decode_property_value decoder prop ps = 
    let open Option.Infix in 
    get prop ps >>= fun v -> 
    try 
      Some (decoder v)
    with 
    | _ -> None

  let encode_property_value encoder v = 
    try 
      Some (encoder v)
    with 
    | _ -> None

end

type properties = Property.Value.t Properties.t
