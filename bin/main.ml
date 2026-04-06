module StringMap = Map.Make(String)

type values =
    | VWord of string
    | VString of string

let to_string tokens = 
    String.concat " " (List.map (function
        | VWord w -> w
        | VString s -> "\"" ^ s ^ "\""
    ) tokens)

let commands = StringMap.of_list
    [ ("echo", print_endline)
    ]

let rec process_line line tokens =
    match line with
    | [] -> tokens
    | h::t -> 
        match h with
        | 'a' .. 'z'
        | 'A' .. 'Z' -> process_word (String.make 1 h) t tokens
        | '"' | '\'' -> process_string h "" t tokens
        | _ -> process_line t tokens
and process_word word line tokens =
    match line with
    | [] -> tokens
    | h::t ->
        match h with
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> process_word (word ^ (String.make 1 h)) t tokens
        | _ -> process_line t (tokens @ [VWord word])
and process_string quote str line tokens =
    match line with
    | [] -> tokens
    | h::t ->
        if h == '\\'
        then match line with
            | [] -> tokens
            | h::t -> process_string quote (str ^ (String.make 1 h)) t tokens
        else if h == quote
        then process_line t (tokens @ [VString str])
        else process_string quote (str ^ (String.make 1 h)) t tokens

let process_file filename =
    let ic = open_in filename in
    let (tokens : values list) = [] in
    try
        while true do
            tokens <- (input_line ic
            |> String.to_seq
            |> List.of_seq
            |> process_line tokens)
        done;
        print_endline (to_string tokens)
    with
    | End_of_file ->
        close_in ic
    | e ->
        close_in_noerr ic;
        raise e

let () = process_file Sys.argv.(1)

