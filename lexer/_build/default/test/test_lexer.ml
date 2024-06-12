open Base
open Lexer

let () =
  let parser = init "int x = 42" in
  let rec tokenize parser =
    match next_token parser with
    | parser, Some token ->
        Printf.printf "Token: %s\n"
          (match token with
          | Token.KEYWORD k -> "KEYWORD " ^ k
          | Token.IDENT id -> "IDENT " ^ id
          | Token.INT_LITERAL num -> "INT_LITERAL " ^ Int.to_string num
          | Token.OPERATOR op -> "OPERATOR " ^ op
          | Token.DELIMITER d -> "DELIMITER " ^ d
          | Token.EOF -> "EOF");
        tokenize parser
    | parser, None -> ()
  in
  tokenize parser
