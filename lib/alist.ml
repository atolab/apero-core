include List

let drop = Common.drop

let take = Common.take

let zip = combine

let unzip = split

let  to_string ?(sep=", ") xs f =
  let s = match xs with
      | h::tl -> List.fold_left (fun a v -> Printf.sprintf "%s%s%s" a sep @@ f v) (f h) tl
      | [] -> ""
  in Printf.sprintf "(%s)" s          
