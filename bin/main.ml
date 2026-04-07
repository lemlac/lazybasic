module StringMap = Map.Make(String)

let keywords =
    [ "echo"
    ; "let"
    ; "if"
    ; "else"
    ; "for"
    ; "from"
    ; "to"
    ; "foreach"
    ; "in"
    ; "while"
    ; "begin"
    ; "end" ]

let is_keyword word = List.mem (String.lowercase_ascii word) keywords

type expr =
    | ExprValue of value
    | ExprVar   of string
    | ExprOp    of op
    | ExprCmd   of cmd
and value =
    | ValWord   of string
    | ValString of string
    | ValNum    of float
and op =
    | OpAdd    of expr * expr
    | OpSub    of expr * expr
    | OpMul    of expr * expr
    | OpDiv    of expr * expr
    | OpMod    of expr * expr
    | OpEq     of expr * expr
    | OpNeq    of expr * expr
    | OpLt     of expr * expr
    | OpGt     of expr * expr
    | OpLeq    of expr * expr
    | OpGeq    of expr * expr
    | OpAnd    of expr * expr
    | OpOr     of expr * expr
    | OpNot    of expr
    | OpConcat of expr * expr
and cmd =
    | CmdEcho    of expr
    | CmdLet     of string * expr
    | CmdIf      of expr * (expr list) * (expr list)
    | CmdFor     of string * expr * expr * (expr list)
    | CmdForEach of string * expr * (expr list)
    | CmdWhile   of expr * (expr list)
    | CmdBegin
    | CmdEnd
    | CmdUnknown of string

let rec tokens_to_string tokens = 
    match tokens with
    | [] -> ""
    | h::t -> (expr_to_string h) ^ " " ^ (tokens_to_string t)
and expr_to_string expr =
    match expr with
    | ExprValue v   -> "(ExprValue " ^ (val_to_string v) ^ ")"
    | ExprVar   var -> "(ExprVar " ^ var ^ ")"
    | ExprOp    op  -> "(ExprOp " ^ (op_to_string op) ^ ")"
    | ExprCmd   cmd -> "(ExprCmd " ^ (cmd_to_string cmd) ^ ")"
and val_to_string v =
    match v with
    | ValWord   w -> "(ValWord '" ^ w ^ "')"
    | ValString s -> "(ValString \"" ^ s ^ "\")"
    | ValNum    n -> "(ValNum " ^ (string_of_float n) ^ ")"
and op_to_string op =
    match op with
    | OpAdd    (e1, e2) -> "(OpAdd "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpSub    (e1, e2) -> "(OpSub "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpMul    (e1, e2) -> "(OpMul "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpDiv    (e1, e2) -> "(OpDiv "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpMod    (e1, e2) -> "(OpMod "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpEq     (e1, e2) -> "(OpEq "     ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpNeq    (e1, e2) -> "(OpNeq "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpLt     (e1, e2) -> "(OpLt "     ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpGt     (e1, e2) -> "(OpGt "     ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpLeq    (e1, e2) -> "(OpLeq "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpGeq    (e1, e2) -> "(OpGeq "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpAnd    (e1, e2) -> "(OpAnd "    ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpOr     (e1, e2) -> "(OpOr "     ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | OpNot    ( e    ) -> "(OpNot "    ^ (expr_to_string e ) ^ ")"
    | OpConcat (e1, e2) -> "(OpConcat " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
and cmd_to_string cmd =
    match cmd with
    | CmdEcho    (e) -> "(CmdEcho " ^ (expr_to_string e) ^ ")"
    | CmdLet     (v, e) -> "(CmdLet " ^ v ^ " " ^ (expr_to_string e) ^ ")"
    | CmdIf      (cond, then_branch, else_branch) -> "(CmdIf " ^ (expr_to_string cond) ^ " " ^ (tokens_to_string then_branch) ^ " " ^ (tokens_to_string else_branch) ^ ")"
    | CmdFor     (var, start, stop, body) -> "(CmdFor " ^ var ^ " " ^ (expr_to_string start) ^ " " ^ (expr_to_string stop) ^ " " ^ (tokens_to_string body) ^ ")"
    | CmdForEach (var, list, body) -> "(CmdForEach " ^ var ^ " " ^ (expr_to_string list) ^ " " ^ (tokens_to_string body) ^ ")"
    | CmdWhile   (cond, body) -> "(CmdWhile " ^ (expr_to_string cond) ^ " " ^ (tokens_to_string body) ^ ")"
    | CmdBegin       -> "CmdBegin"
    | CmdEnd         -> "CmdEnd"
    | CmdUnknown cmd -> "(CmdUnknown '" ^ cmd ^ "')"

let rec parse_line line tokens =
    match line with
    | [] -> tokens
    | h::t -> 
        match h with
        | 'a' .. 'z' | '_'
        | 'A' .. 'Z' -> parse_word (String.make 1 h) t tokens
        | '0' .. '9' -> parse_number (String.make 1 h) t tokens
        | '"' | '\'' -> parse_string h "" t tokens
        | _ -> parse_line t tokens
and parse_word word line tokens =
    let emit line = parse_line line (tokens @ [ExprValue (ValWord word)]) in
    let next line h = parse_word (word ^ (String.make 1 h)) line tokens in
    match line with
    | [] -> emit []
    | h::t ->
        match h with
        | 'a' .. 'z' | '_'
        | 'A' .. 'Z'
        | '0' .. '9' -> next t h
        | _ -> emit t
and parse_cmd (word : string) line tokens =
    let emit line = parse_line line (tokens @ [ExprCmd (match word with
        (* to be implemented later *)
        | _ -> CmdUnknown word
    )]) in
    let next c line = parse_cmd (word ^ (String.make 1 c)) line tokens in
    match line with
    | [] -> emit []
    | h::t ->
        match h with
        | 'a' .. 'z'
        | 'A' .. 'Z' -> next h t
        | _ -> emit t
and parse_string quote str line tokens =
    let emit line = parse_line line (tokens @ [ExprValue (ValString str)]) in
    let next c line = parse_string quote (str ^ (String.make 1 c)) line tokens in
    match line with
    | [] -> emit []
    | h::t ->
        if h == '\\'
        then match t with
            | [] -> emit t
            | h::t -> next (match h with
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | c -> c) t
        else if h == quote
        then emit t
        else next h t
and parse_number num line tokens =
    let emit line = parse_line line (tokens @ [ExprValue (ValNum (float_of_string num))]) in
    let next c line = parse_number (num ^ (String.make 1 c)) line tokens in
    match line with
    | [] -> emit []
    | h::t ->
        match h with
        | '0' .. '9' -> next h t
        | '.' ->
            if String.contains num '.'
            then emit line
            else next '.' t
        | _ -> emit t

let parse_file filename =
    let ic = open_in filename in
    let tokens = ref [] in
    (try
        while true do
            tokens := (input_line ic
            |> String.to_seq
            |> List.of_seq
            |> fun line -> parse_line line !tokens)
        done
    with
    | End_of_file ->
        close_in ic
    | e ->
        close_in_noerr ic;
        raise e);
    print_endline (tokens_to_string !tokens)

let () = parse_file Sys.argv.(1)

