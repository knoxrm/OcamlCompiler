(*open Base*)
open Stdlib
open Lexlib.Lexer
open Lexlib.Token

let rec lex_all lexer tokens =
  let lexer, token = next_token lexer in 
  match token with
  | None -> List.rev (EOF :: tokens)
  | Some token -> lex_all lexer (token :: tokens)

let lexer = init "int main() { return 42; }"
let tokens = lex_all lexer []

 (*Print tokens for demonstration *)
let print_list = function
  | IDENT lex -> Format.printf ("IDENT(%s)\n") lex
  | KEYWORD lex -> Format.printf "KEYWORD(%s)\n" lex
  | INT_LITERAL value -> Format.printf "INT_LITERAL(%d)\n" value
  | BOOL_LITERAL boolean -> Format.printf "INT_LITERAL(%s)\n" (if boolean then "true" else "false")
  | OPERATOR lex -> Format.printf "OPERATOR(%s)\n" lex
  | DELIMITER lex -> Format.printf "DELIMITER(%s)\n" lex
  | EOF -> Format.printf "EOF\n"
let () = 
  List.iter print_list tokens
  (*List.iter tokens ~f:(print_list)*)
