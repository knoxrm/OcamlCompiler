
open Base

type t = { input : string; position : int; ch : char option }

let init input =
  if String.is_empty input then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }

let move parser =
  let next_pos = parser.position + 1 in
  if next_pos >= String.length parser.input then
    { parser with position = next_pos; ch = None }
  else
    {
      parser with
      position = next_pos;
      ch = Some (String.get parser.input next_pos);
    }

let rec skip_whitespace parser =
  match parser.ch with
  | Some (' ' | '\t' | '\r' | '\n') -> skip_whitespace (move parser)
  | _ -> parser

let is_ident ch = Char.is_alpha ch || Char.is_digit ch || Char.equal ch '_'
let is_int ch = Char.is_digit ch

let rec check parser condition =
  match parser.ch with
  | Some ch when condition ch ->
      let parser = move parser in
      check parser condition
  | _ -> parser

let next_token parser =
  let parser = skip_whitespace parser in
  let open Token in
  match parser.ch with
  | None -> (parser, None)
  | Some ch ->
      let parser, token =
        match ch with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
            let start_pos = parser.position in
            let parser = check parser is_ident in
            let end_pos = parser.position - start_pos in
            let ident = String.sub parser.input ~pos:start_pos ~len:end_pos in
            let token =
              match ident with
              | "int" | "return" -> KEYWORD ident
              | _ -> IDENT ident
            in
            (parser, token)
        | '0' .. '9' ->
            let start_pos = parser.position in
            let parser = check parser is_int in
            let end_pos = parser.position - start_pos in
            let num_str = String.sub parser.input ~pos:start_pos ~len:end_pos in
            let num = Int.of_string num_str in
            let token = INT_LITERAL num in
            (parser, token)
        | ('+' | '-' | '*' | '/' | '=') as op ->
            let operator = String.make 1 op in
            (move parser, OPERATOR operator)
        | ('(' | ')' | '{' | '}' | ';') as delim ->
            let delimiter = String.make 1 delim in
            (move parser, DELIMITER delimiter)
        | '\000' -> (parser, EOF)
        | _ -> failwith "Unexpected character"
      in
      (parser, Some token)
