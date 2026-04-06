module StringMap = Map.Make(String)

type expr =
    | EValue of value
    | EVar of string
    | EOp of op
    | ECmd of cmd
and value =
    | VWord of string
    | VString of string
    | VNum of float
and op =
    | OAdd of expr * expr
    | OSub of expr * expr
    | OMul of expr * expr
    | ODiv of expr * expr
    | OMod of expr * expr
    | OEq of expr * expr
    | ONeq of expr * expr
    | OLt of expr * expr
    | OGt of expr * expr
    | OLeq of expr * expr
    | OGeq of expr * expr
    | OAnd of expr * expr
    | OOr of expr * expr
    | ONot of expr
and cmd =
    | CEcho of expr
    | CLet of string * expr
    | CIf of expr * (expr list) * (expr list)
    | CFor of expr * expr * expr * (expr list)
    | CForEach of string * expr * (expr list)
    | CWhile of expr * (expr list)
    | CEnd

let to_string tokens = 
    String.concat " " (List.map (function
        | VWord w -> "(VWord " ^ w ^ ")"
        | VString s -> "(VString \"" ^ s ^ "\")"
        | VNum n -> "(VNum " ^ string_of_float n ^ ")"
    ) tokens)

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
    let tokens = ref [] in
    (try
        while true do
            tokens := (input_line ic
            |> String.to_seq
            |> List.of_seq
            |> fun line -> process_line line !tokens)
        done
    with
    | End_of_file ->
        close_in ic
    | e ->
        close_in_noerr ic;
        raise e);
    print_endline (to_string !tokens)

let () = process_file Sys.argv.(1)

