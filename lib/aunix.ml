let inet_addrs () = 
  let rec recurse inchan l f = 
    try 
      String.(List.(
        let words = input_line inchan |> trim |> split_on_char ' ' in
        match words with 
        | _::flags::"mtu"::_ -> flags |> split_on_char '<' |> tl |> hd |>
                                split_on_char '>' |> hd |> split_on_char ',' |>
                                recurse inchan l
        | "inet"::addr::_ -> recurse inchan ((Unix.inet_addr_of_string addr, f)::l) f
        | _ -> recurse inchan l f
      ))
    with _ -> l
  in
  recurse (Unix.open_process_in "ifconfig") [] []

let inet_addrs_up_nolo = 
  Acommon.Infix.(String.(List.(
    inet_addrs 
    %> filter (fun (_, f) -> 
            (exists (equal "UP") f, exists (equal "LOOPBACK") f) |> function 
            | true, false -> true
            | _ -> false)
    %> map (fun (addr, _) -> addr)
  )))
