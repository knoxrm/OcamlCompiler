open Base
open Ast
open Token
open Lexer

type t = 
    { 
        lexer: Lexer.t;
        curToken: Token.t option;
        peekToken: Token.t option
    }
let advance parser = 
    match parser.peekToken with 
    | Some token -> 
        let lexer, next = Lexer.next_token parser.lexer in 
        {  
          lexer
        ; curToken = Some token
        ; peekToken = next
        }
    | None -> { parser with curToken = None }
;;

let init_parser lexer = 
    let parser = { lexer; curToken = None; peekToken = None } in 
    let parser = advance parser in 
    let parser = advance parser in
    parser
;;

(* let is_token_equal t1 t2 =  *)
(*     match t1, t2 with  *)
(*     | IDENT s1, IDENT s2 -> s1 = s2 *)
(*     | INT_LITERAL i1, INT_LITERAL i2 -> i1 = i2 *)
(*     | DELIMITER d1, DELIMITER d2 -> d1 = d2 *)
(*     | KEYWORD k1, KEYWORD k2 -> k1 = k2 *)
(*     | BOOL_LITERAL b1, BOOL_LITERAL b2 -> b1 = b2 *)
(*     | EOF, EOF -> true *)
(*     | _ -> false *)


let expect_token parser expected = 
    match parser.curToken with 
    | Some token when is_token_equal token expected -> advance parser 
    | _ -> failwith "Unexpected Token"


let match_token parser =
  match parser.curToken with
  | Some token -> token
  | None -> failwith "Unexpected end of input"

(* let rec parse parser =  *)
(*     let rec parse' parser stmts =  *)
(*         match parser.curToken with  *)
(*             | Some _ ->  *)
(*                 (match parse_statement parser with *)
(*                     | Ok (parser, stmt) -> parse' ( parser) (stmt :: stmts) *)
(*                     | Error msg -> err parser msg stmts *)
(*                 ) *)
(*             | None -> Ok (parser, List.rev stmt)  *)
(*     in  *)
(*     let _, stmts = parse' parser [] in *)
(*     Ok (Ast.Program { Stmts }) *)

let rec parse_expr parser = 
    parse_infix parser 0 

and parse_infix parser min_prec = 
    let rec loop parser left = 
        match parser.curToken with
        | Some (OPERATOR op) when (op_precedence op) >= min_prec -> 
            let parser = advance parser in
            let parser, right = parse_main_expr parser in
            let left = Infix {left; operator = op; right} in
            loop parser left
        | _ -> parser, left
    in 
    let parser, left = parse_main_expr parser in 
    loop parser left

and parse_main_expr parser = 
    match parser.curToken with 
    | Some (IDENT name) -> 
        let parser = advance parser in
        parser, Identifier {identifier = name}
    | Some (INT_LITERAL value) -> 
        let parser = advance parser in
        parser, Integer value
    | Some (DELIMITER "(") -> 
        let parser = advance parser in
        let parser, expr = parse_expr parser in 
        let parser = expect_token parser (DELIMITER ")") in
        parser, expr
    | _ -> failwith "Expected primary expression"

and op_precedence = function
    | "+" | "-" -> 1
    | "*" | "/" -> 2
    | _ -> 0
;;
let rec parse_statement parser = 
    match parser.curToken with
    | Some (KEYWORD "return") -> parse_return parser 
    | _ -> parse_expr_stmt parser

and parse_array_literal parser =
    let rec parse_elements parser elements =
    match parser.curToken with
    | Some (DELIMITER "]") ->
        let parser = advance parser in
        List.rev elements, parser
    | _ ->
        let parser, expr = parse_expr parser in
        let parser = parse_comma parser in
        parse_elements parser (expr :: elements)
    in
    let elements, parser = parse_elements parser [] in
    Array elements, parser

and parse_comma parser = 
    match parser.curToken with
    | Some (DELIMITER ",") -> advance parser
    | _ -> parser

and parse_if_expression parser =
  let parser = expect_token parser (DELIMITER "(") in
  let parser, condition = parse_expr parser in
  let parser = expect_token parser (DELIMITER ")") in
  let consequence, parser = parse_block parser in
  let alternative, parser =
    match parser.curToken with
    | Some (KEYWORD "else") ->
      let parser = advance parser in
      let alternative, parser = parse_block parser in
      Some alternative, parser
    | _ -> None, parser
  in
  If { condition; consequence; alternative }, parser



and parse_return parser = 
    let parser = expect_token parser (KEYWORD "return") in 
    let value = parse_expr parser in
    let parser = expect_token parser (DELIMITER ";") in 
    parser, value

and parse_expr_stmt parser = 
    let parser, expr = parse_expr parser in
    let parser = expect_token parser (DELIMITER ";") in 
    parser, Stmt (ExprStmt expr)

and parse_block parser = 
    let parser = expect_token parser (DELIMITER "{") in 
    parse_statements parser []

and parse_statements parser stmts = 
    match parser.curToken with
    | Some (DELIMITER "}") -> 
        let parser = advance parser in 
        { block = List.rev stmts }, parser
    | _ -> 
        let parser, stmt = parse_statement parser in 
        parse_statements parser (stmt::stmts)

let parse_function_literal parser = 
    let parser = expect_token parser (DELIMITER "9") in
    let parameters = [] in
    let parser = expect_token parser (DELIMITER ")") in
    let parser, body = parse_block parser in
    parser, Expr (FunctionLiteral { parameters; body })

let parse_program parser =
  parse_statements parser []

let parse tokens =
  let lexer = Lexer.init_lexer tokens in
  let parser = init_parser lexer in
  parse_program parser
