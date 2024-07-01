open Base
open Stdlib
open Ast
open Token
open Lexer

type t = 
    { 
        lexer: Lexer.t;
        curToken: Token.t option;
        peekToken: Token.t option
    }
(* for debugging purposes *)
let show_token token = 
    match token with
    | IDENT s -> Format.asprintf "IDENT(%s)" s
    | KEYWORD s -> Format.asprintf "KEYWORD(%s)" s
    | INT_LITERAL n -> Format.asprintf "INT_LITERAL(%d)" n
    | BOOL_LITERAL b -> Format.asprintf "BOOL_LITERAL(%b)" b
    | OPERATOR s -> Format.asprintf "OPERATOR(%s)" s
    | DELIMITER s -> Format.asprintf "DELIMITER(%s)" s
    | EOF -> Format.asprintf "EOF"

let advance parser = 
    match parser.peekToken with 
    | Some token -> 
        let lexer, next = Lexer.next_token parser.lexer in 
        {  
          lexer
        ; curToken = Some token
        ; peekToken = next
        }
    | None -> 
        { parser with curToken = None }
;;



let init_parser lexer = 
    let lexer, first_token = Lexer.next_token lexer in 
    let lexer, second_token = Lexer.next_token lexer in
    let parser = { lexer; curToken = first_token; peekToken = second_token } in 
    parser
;;

let expect_token parser expected = 
    match parser.curToken with 
    | Some token when is_token_equal token expected -> advance parser 
    | _ -> failwith "Unexpected Token"


let match_token parser =
  match parser.curToken with
  | Some token -> token
  | None -> failwith "Unexpected end of input"

let rec parse_expr parser = 
    match parser.curToken with
    | None -> 
        parser, Identifier {identifier = "EOF"} 
    | Some _ -> 
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
    | Some (INT_LITERAL value) -> 
        let parser = advance parser in
        parser, Integer value
    | Some (IDENT name) -> 
        let parser = advance parser in
        parser, Identifier {identifier = name}
    | Some (DELIMITER "(") -> 
        let parser = advance parser in
        let parser, expr = parse_expr parser in 
        let parser = expect_token parser (DELIMITER ")") in
        parser, expr
    | Some t -> 
        Format.printf "Unexpected token in parse_main_expr: %s@." (show_token t);
        parser, Identifier {identifier = "EOF"} 
    | None -> 
        parser, Identifier {identifier = "EOF"} 

and op_precedence = function
    | "+" | "-" -> 1
    | "*" | "/" -> 2
    | _ -> 0

let rec parse_program parser =
  let rec parse_all parser stmts =
    match parser.curToken with
    | None -> 
        parser, List.rev stmts
    | Some _ ->
        let parser, stmt = parse_statement parser in
        parse_all parser (stmt :: stmts)
  in
  let parser, stmts = parse_all parser [] in
  Format.printf "Finished parsing, found %d statements@." (List.length stmts);
  parser, stmts

and parse_statements parser stmts = 
    match parser.curToken with
    | Some (DELIMITER "}") -> 
        let parser = advance parser in 
        parser, List.rev stmts
    | _ -> 
        let parser, stmt = parse_statement parser in 
        parse_statements parser (stmt::stmts)

and parse_statement parser = 
    match parser.curToken with
    | Some (KEYWORD "return") -> 
        parse_return parser 
    | Some (KEYWORD "if") ->
        let parser, expr = parse_if_expression parser in
        parser, ExprStmt expr
    | Some (KEYWORD ("int" | "bool" | "void")) -> 
        parse_function_literal parser 
    | _ -> 
        parse_expr_stmt parser

and parse_expr_stmt parser = 
    let parser, expr = parse_expr parser in
    let parser = 
        match parser.curToken with 
        | Some (DELIMITER ";") -> 
            advance parser
        | _ -> 
            Format.printf "No semicolon found@.";
            parser
    in
    parser, ExprStmt expr

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
    let parser = expect_token parser (KEYWORD "if") in
    let parser = expect_token parser (DELIMITER "(") in
    let parser, condition = parse_expr parser in
    let parser = expect_token parser (DELIMITER ")") in
    let parser, consequence = parse_block parser in
    let parser, alternative =
    match parser.curToken with
    | Some (KEYWORD "else") ->
        let parser = advance parser in
        let parser, alt_block = parse_block parser in
        parser, Some alt_block
    | _ -> parser, None
    in 
        parser, If { condition; consequence; alternative }

and parse_return parser = 
    let parser = expect_token parser (KEYWORD "return") in 
    let parser, value = parse_expr parser in
    let parser = expect_token parser (DELIMITER ";") in 
    parser, Return value


and parse_block parser = 
    let parser = expect_token parser (DELIMITER "{") in 
    let parser, stmts = parse_statements parser [] in 
    parser, {block = stmts}


and parse_function_literal parser = 
    let parser, return_type = 
        match parser.curToken with 
        | Some (KEYWORD t) when List.mem t ["int"; "void"; "bool"] -> 
            let p = advance parser in 
            p, t
        | _ -> failwith "Expected return type (int, bool, or void"
    in
    let parser, name = 
        match parser.curToken with 
        | Some (IDENT n) -> advance parser, n
        | _ -> failwith "Expected function name" 
    in
    let parser = expect_token parser (DELIMITER "(") in
    let parameters = [] in
    let parser = expect_token parser (DELIMITER ")") in
    let parser, body = parse_block parser in
    parser, ExprStmt (FunctionLiteral { parameters; body; return_type = Some return_type; name = Some name })


let parse tokens =
  let lexer = init tokens in
  let parser = init_parser lexer in
  parse_program parser
