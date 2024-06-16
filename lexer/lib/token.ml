
type t =
  | IDENT of string
  | KEYWORD of string
  | INT_LITERAL of int
  | OPERATOR of string
  | DELIMITER of string
  | EOF
[@@deriving show]
