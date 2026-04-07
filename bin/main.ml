module StringMap = Map.Make(String)

(***********************
    Helper Functions
 ***********************)

let get_absolute_path (path : string) : string =
  if Filename.is_relative path then
    Filename.concat (Sys.getcwd ()) path
  else
    path

let match_brkt b =
    match b with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | _ -> failwith "Invalid bracket"

let is_symbol c =
    match c with
    | '+' | '-' | '*' | '/' | '%'
    | '=' | '!' | '<' | '>' | '&'
    | '|' | '#' | '$' | '.' | '?'
    | '~' | '^' | ':'
    | '\\' -> true
    | _ -> false

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

(****************
    Tokenizer
 ****************)

 type loc = { filename: string option; line: int; col: int }

type token = 
    | TWord  of string
    | TOp    of string
    | TStr   of string
    | TNum   of float
    | TCmnt  of string
    | TLBrkt of char
    | TRBrkt of char
    | TEndl
    | TDelim

type token_loc = token * loc

let token_to_string token =
    match token with
    | TWord word  -> "WORD \"" ^ word ^ "\""
    | TOp op      -> "OP \"" ^ op ^ "\""
    | TStr str    -> "STR \"" ^ str ^ "\""
    | TNum num    -> "NUM " ^ (string_of_float num)
    | TCmnt cmnt  -> "CMNT \"" ^ cmnt ^ "\""
    | TLBrkt brkt -> "LBRKT '" ^ (String.make 1 brkt) ^ "'"
    | TRBrkt brkt -> "RBRKT '" ^ (String.make 1 brkt) ^ "'"
    | TEndl       -> "ENDL"
    | TDelim      -> "DELIM"

let loc_to_string loc =
    (match loc.filename with
    | Some filename -> filename
    | None -> "<unknown>") ^ ":" ^ (string_of_int loc.line) ^ ":" ^ (string_of_int loc.col)

let rec token_loc_list_to_string tokens = 
    match tokens with
    | [] -> ""
    | (token, loc)::t -> (token_to_string token) ^ " at " ^ (loc_to_string loc) ^ "\n" ^ (token_loc_list_to_string t)

let next_col loc = { loc with col = loc.col + 1 }
let next_line loc = { loc with line = loc.line + 1; col = 0 }

let rec tokenize_line line loc tokens =
    match line with
    | [] -> tokens @ [(TEndl, loc)]
    | h::t ->
        match h with
        | 'a' .. 'z' | '_'
        | 'A' .. 'Z' -> tokenize_word (String.make 1 h) t (next_col loc) tokens
        | '0' .. '9' -> tokenize_number (String.make 1 h) t (next_col loc) tokens
        | '"' | '\'' -> tokenize_string h "" t (next_col loc) tokens
        | ';' -> tokenize_line t (next_col loc) (tokens @ [(TEndl, loc)])
        | ',' -> tokenize_line t (next_col loc) (tokens @ [(TDelim, loc)])
        | '(' | '[' | '{' -> tokenize_line t (next_col loc) (tokens @ [(TLBrkt h, loc)])
        | ')' | ']' | '}' -> tokenize_line t (next_col loc) (tokens @ [(TRBrkt h, loc)])
        | x when is_symbol x -> tokenize_op (String.make 1 x) t (next_col loc) tokens
        | _ -> tokenize_line t (next_col loc) tokens
and tokenize_word word line loc tokens =
    let emit line = tokenize_line line (next_col loc) (tokens @ [(TWord word, loc)]) in
    let next c line = tokenize_word (word ^ (String.make 1 c)) line (next_col loc) tokens in
    match line with
    | [] -> emit []
    | h::t ->
        match h with
        | 'a' .. 'z' | '_'
        | 'A' .. 'Z'
        | '0' .. '9' -> next h t
        | _ -> emit t
and tokenize_op op line loc tokens =
    let emit line = (match op with
        | "//" -> tokenize_cmnt "//" line (next_col loc) tokens
        | _ -> tokenize_line line (next_col loc) (tokens @ [(TOp op, loc)]))in
    let next c line = tokenize_op (op ^ (String.make 1 c)) line (next_col loc) tokens in
    match line with
    | [] -> emit []
    | h::t ->
        if is_symbol h
        then next h t
        else emit t
and tokenize_cmnt cmnt line loc tokens =
    let emit line = tokenize_line line (next_col loc) (tokens @ [(TCmnt cmnt, loc)]) in
    let next c line = tokenize_cmnt (cmnt ^ (String.make 1 c)) line (next_col loc) tokens in
    match line with
    | [] -> emit []
    | h::t ->
        if h == '\n'    
        then emit t
        else next h t
and tokenize_string quote str line loc tokens =
    let emit line = tokenize_line line (next_col loc) (tokens @ [(TStr str, loc)]) in
    let next c line = tokenize_string quote (str ^ (String.make 1 c)) line (next_col loc) tokens in
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
and tokenize_number num line loc tokens =
    let emit line = tokenize_line line (next_col loc) (tokens @ [(TNum (float_of_string num), loc)]) in
    let next c line = tokenize_number (num ^ (String.make 1 c)) line (next_col loc) tokens in
    match line with
    | [] -> emit []
    | h::t ->
        match h with
        | '0' .. '9' -> next h t
        | '.' ->
            if String.contains num '.'
            then emit line
            else next '.' t
        | _ -> emit line

let rec tokenize_file_loop ic loc tokens =
    (try
        tokenize_file_loop ic (next_line loc) (input_line ic
        |> String.to_seq
        |> List.of_seq
        |> fun line -> tokenize_line line loc tokens)
    with
    | End_of_file ->
        close_in ic;
        tokens
    | e ->
        close_in_noerr ic;
        raise e)

let tokenize_file filename =
    let ic = open_in filename in
    tokenize_file_loop ic
    { filename = Some (get_absolute_path filename)
    ; line = 1
    ; col = 0
    } []

(*************
    Parser
 *************)

type parse_state =
    { tokens: token_loc list
    ; pos: int }
type parse_error =
    { loc: loc option
    ; message: string }

let current_token state = List.nth_opt state.tokens state.pos
let advance state = { state with pos = state.pos + 1 }
let peek state = List.nth_opt state.tokens (state.pos + 1)

let error state message =
    match current_token state with
    | Some (_, loc) -> { loc = Some loc; message }
    | None -> { loc = None; message }

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

let rec exprs_to_string exprs = 
    match exprs with
    | [] -> ""
    | h::t -> (expr_to_string h) ^ " " ^ (exprs_to_string t)
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
    | CmdIf      (cond, then_branch, else_branch) -> "(CmdIf " ^ (expr_to_string cond) ^ " " ^ (exprs_to_string then_branch) ^ " " ^ (exprs_to_string else_branch) ^ ")"
    | CmdFor     (var, start, stop, body) -> "(CmdFor " ^ var ^ " " ^ (expr_to_string start) ^ " " ^ (expr_to_string stop) ^ " " ^ (exprs_to_string body) ^ ")"
    | CmdForEach (var, list, body) -> "(CmdForEach " ^ var ^ " " ^ (expr_to_string list) ^ " " ^ (exprs_to_string body) ^ ")"
    | CmdWhile   (cond, body) -> "(CmdWhile " ^ (expr_to_string cond) ^ " " ^ (exprs_to_string body) ^ ")"
    | CmdBegin       -> "CmdBegin"
    | CmdEnd         -> "CmdEnd"
    | CmdUnknown cmd -> "(CmdUnknown '" ^ cmd ^ "')"

(*
(* TODO: Implment parsing *)
let rec parse_tokens state =
    match current_token state with
    | Some (TWord "echo", _) -> parse_echo state
    | Some (TWord "let", _) -> parse_let state
    | Some (TWord "if", _) -> parse_if state
    | Some (TWord "for", _) -> parse_for state
    | Some (TWord "foreach", _) -> parse_foreach state
    | Some (TWord "while", _) -> parse_while state
    | Some (TWord "begin", _) -> parse_begin state
    | Some (TWord "end", _) -> parse_end state
    | Some (TWord cmd, _) -> let state = advance state in (ExprCmd (CmdUnknown cmd), state)
    | Some _ -> let state = advance state in error state "Unexpected token"
    | None -> error { loc = None; message = "Unexpected end of input" }
let start_parse_tokens tokens =
    let state = { tokens; pos = 0 } in
    match parse_tokens state with
    | (expr, state) ->
        if current_token state == None
        then Ok expr
        else Error (error state "Unexpected token after end of command")
*)

(***********
    Main
 ***********)

let () =
    tokenize_file Sys.argv.(1)
    |> token_loc_list_to_string
    |> print_endline
