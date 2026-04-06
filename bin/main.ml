module StringMap = Map.Make(String)

type values =
    | Word of string
    | String of string

let commands = StringMap.of_list
    [ ("echo", print_endline)
    ]

let rec process_line line tokens =
    match line with
    | [] -> ()
    | h::t -> 
        match h with
        | 'a' .. 'z'
        | 'A' .. 'Z' -> process_word (String.make 1 h) t
        | '"' | '\'' -> process_string h "" t
        | _ -> process_line t
and process_word word line tokens =
    match line with
    | [] -> ()
    | h::t ->
        match h with
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> process_word (word ^ (String.make 1 h)) t tokens
        | _ ->
            print_endline word;
            process_line t tokens
and process_string quote string line tokens =
    match line with
    | [] -> ()
    | h::t ->
        match h with
        | '\' ->
            match line with
            | [] -> ()
            | h::t -> process_string quote (string ^ (String.make 1 h)) t tokens
        | quote ->
            print_endline string;
            process_line t tokens
        | _ ->
            process_string quote (string ^ (String.make 1 h)) t tokens

let process_file filename =
    let ic = open_in filename in
    let (tokens : values list) = [] in
    try
        while true do
            input_line ic
            |> String.to_seq
            |> List.of_seq
            |> process_line
        done
    with
    | End_of_file ->
        close_in ic
    | e ->
        close_in_noerr ic;
        raise e

let () = process_file Sys.argv.(1)

