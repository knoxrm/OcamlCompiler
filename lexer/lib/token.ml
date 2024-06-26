
type t =
  | IDENT of string
  | KEYWORD of string
  | INT_LITERAL of int
  | BOOL_LITERAL of bool
  | OPERATOR of string
  | DELIMITER of string
  | EOF
[@@deriving show]


let is_token_equal t1 t2 = 
    match t1, t2 with 
    | IDENT s1, IDENT s2 -> s1 = s2
    | KEYWORD k1, KEYWORD k2 -> k1 = k2
    | INT_LITERAL i1, INT_LITERAL i2 -> i1 = i2
    | BOOL_LITERAL b1, BOOL_LITERAL b2 -> b1 = b2
    | OPERATOR o1, OPERATOR o2 -> o1 = o2
    | DELIMITER d1, DELIMITER d2 -> d1 = d2
    | EOF, EOF -> true
    | _ -> false
