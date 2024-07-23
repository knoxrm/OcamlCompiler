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
    parser, { stmts= stmts }

and expect_token parser expected = 
    match parser.curToken with 
    | Some token when is_token_equal token expected -> advance parser 
    | Some t -> 
        (* Format.printf "Failed token: %s@." (show_token t); *)
        Format.printf "Expected %s, but got: %s@." (show_token expected) (show_token t);
        failwith "Unexpected Token"
    | _ ->
        failwith "Unexpected Token"

and match_token parser =
  match parser.curToken with
  | Some token -> token
  | None -> failwith "Unexpected end of input"

and parse_expr parser = 
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
        | Some (DELIMITER "[") -> 
            let parser = advance parser in
            let parser, index = parse_expr parser in
            let parser = expect_token parser (DELIMITER "]") in 
            let left = Index {left; right = index} in
            loop parser left
        | Some (DELIMITER "(") ->
            let parser, args = parse_arguments parser in
            let left = Call {fn = left; args} in
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
    | Some (BOOL_LITERAL value) -> 
        let parser = advance parser in
        parser, Boolean value
    | Some (IDENT name) -> 
        let parser = advance parser in
        parser, Identifier {identifier = name}
    | Some (DELIMITER "(") -> 
        let parser = advance parser in
        let parser, expr = parse_expr parser in 
        let parser = expect_token parser (DELIMITER ")") in
        parser, expr
    | Some (DELIMITER "[") -> 
        parse_array_literal parser
    | Some t -> 
        Format.printf "Unexpected token in parse_main_expr: %s@." (show_token t);
        (* exit 0; *)
        parser, Identifier {identifier = "EOF"}
    | None -> 
        parser, Identifier {identifier = "EOF"} 

and op_precedence = function
    | "=" -> 1
    | "||" -> 2
    | "&&" -> 3
    | "==" | "!=" -> 4
    | "<" | ">" | "<=" | ">=" -> 5
    | "+" | "-" -> 6
    | "*" | "/" | "%" -> 7
    | "^" -> 8
    | _ -> 0 


and parse_statements parser stmts = 
    match parser.curToken with
    | Some (DELIMITER "}") -> 
        let parser = advance parser in 
        parser, List.rev stmts
    | None ->
        failwith "Unexpected end of input while parsing block"
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
        parse_type_declaration parser 
    | Some (KEYWORD ("for" | "while" | "do")) -> parse_loops parser
    | Some (DELIMITER "{") ->
        let parser, block = parse_block parser in
        parser, BlockStmt block
    | _ -> 
        parse_expr_stmt parser

and parse_type_declaration parser = 
    let parser, keyword_type = 
        match parser.curToken with
        | Some (KEYWORD t) when List.mem t ["int" ; "bool" ; "void"] -> 
            advance parser, t
        | _ -> failwith "Unexpected type"
    in 
    let parser, name = 
        match parser.curToken with
        | Some (IDENT n) -> advance parser, n
        | _ as e -> failwith "Unexpected identifier %s" e
    in
    match parser.curToken with
    | Some (DELIMITER "(") -> parse_function_literal parser keyword_type name
    | Some (DELIMITER "[") ->
        let parser = advance parser in
        let parser, size = parse_expr parser in
        let parser = expect_token parser (DELIMITER "]") in
        parse_array_declaration parser keyword_type name size
    | Some (OPERATOR "=") | Some (DELIMITER ";") -> 
        parse_var_declaration parser keyword_type name
    | Some t -> 
        Format.printf "Unexpected token in type declaration: %s@." (show_token t);
        failwith (Format.sprintf "Expected '(', '[', '=', or ';', got %s" (show_token t))
    | None -> 
        failwith "Unexpected end of input in type declaration"

and parse_array_declaration parser var_type name size =
    match parser.curToken with
    | Some (OPERATOR "=") ->
        let parser = advance parser in
        let parser, value = parse_expr parser in
        let parser = expect_token parser (DELIMITER ";") in
        parser, Var { var_type = var_type ^ "[]"; name; init = Some (Array [size; value]) }
    | Some (DELIMITER ";") ->
        let parser = advance parser in
        parser, Var { var_type = var_type ^ "[]"; name; init = Some (Array [size]) }
    | Some t -> 
        Format.printf "Unexpected token in array declaration: %s@." (show_token t);
        failwith (Format.sprintf "Expected '=' or ';', got %s" (show_token t))
    | None -> 
        failwith "Unexpected end of input in array declaration"

and parse_function_literal parser return_type name = 
    let parser = expect_token parser (DELIMITER "(") in
    let parser, parameters = parse_parameters parser in
    let parser = expect_token parser (DELIMITER ")") in
    let parser, body = parse_block parser in
    parser, ExprStmt (FunctionLiteral { parameters; body; return_type = Some return_type; name = Some name })


and parse_var_declaration parser var_type name = 
    match parser.curToken with
    | Some (OPERATOR "=") ->
        let parser = advance parser in
        let parser, value = parse_expr parser in
        let parser = expect_token parser (DELIMITER ";") in
        parser, Var { var_type; name; init=Some value }
    | Some (DELIMITER ";") ->
        let parser = advance parser in
        parser, Var { var_type; name; init=None }
    | Some t -> 
        Format.printf "Unexpected token in var declaration: %s@." (show_token t);
        failwith (Format.sprintf "Expected '=', '[', or ';', got %s" (show_token t))
    | None -> 
        failwith "Unexpected end of input in var declaration"
    (* | _ -> failwith "Expected '=' or ';'" *)

and parse_expr_stmt parser = 
    let parser, expr = parse_expr parser in
    match expr with
    | If _ -> parser, ExprStmt expr
    | _ ->
        let parser = 
            match parser.curToken with 
            | Some (DELIMITER ";") -> 
                advance parser
            | Some (DELIMITER "}") -> 
                parser
            | _ -> 
                parser
        in
        parser, ExprStmt expr

and parse_array_literal parser =
    let rec parse_elements parser elements =
        match parser.curToken with
        | Some (DELIMITER "]") ->
            let parser = advance parser in
            parser, List.rev elements
        | _ ->
            let parser, expr = parse_expr parser in
            let parser = 
                match parser.curToken with
                | Some (DELIMITER ",") -> advance parser
                | _ -> parser 
            in
                (* parse_comma parser in *)
            parse_elements parser (expr :: elements)
    in
    let parser, elements = parse_elements parser [] in
    parser, Array elements

and parse_if_expression parser =
    let parser = expect_token parser (KEYWORD "if") in
    let parser = expect_token parser (DELIMITER "(") in
    let parser, condition = parse_expr parser in
    let parser = expect_token parser (DELIMITER ")") in
    let parser, consequence = 
        match parser.curToken with
        | Some (DELIMITER "{") -> parse_block parser
        | _ -> 
            let parser, stmt = parse_statement parser in
            parser, {block = [stmt]} 
    in
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
    let parser = 
        match parser.curToken with
        | Some (DELIMITER ";") -> advance parser
        | _ -> 
            Format.printf "Warning: Missing semicolon after return statement\n";
            parser
    in
    parser, Return value

and parse_block parser = 
    let parser = expect_token parser (DELIMITER "{") in 
    let parser, stmts = parse_statements parser [] in 
    parser, {block = stmts}

and parse_parameters parser =
    let rec parse_param_list parser params =
        match parser.curToken with
        | Some (DELIMITER ")") -> parser, List.rev params
        | Some (KEYWORD t) when List.mem t ["int"; "bool"; "void"] ->
            let parser = advance parser in 
            let parser = 
                match parser.curToken with
                | Some (DELIMITER "[") ->
                    let parser = advance parser in
                    expect_token parser (DELIMITER "]")
                | _ -> parser
            in
            let parser, name = 
                match parser.curToken with
                | Some (IDENT n) -> advance parser, n
                | _ -> failwith "Expected parameter name"
            in
            let param = { identifier = name } in
            let parser = 
                match parser.curToken with
                | Some (DELIMITER ",") -> advance parser
                | _ -> parser
            in
            parse_param_list parser (param :: params)
        | _ -> failwith "Expected type keyword or closing parenthesis"
    in
    parse_param_list parser [] 
       
and parse_arguments parser = 
    let parser = expect_token parser (DELIMITER "(") in
    let rec parse_args parser acc =
        match parser.curToken with
        | Some (DELIMITER ")") ->
            let parser = advance parser in
            parser, List.rev acc
        | _ ->
            let parser, arg = parse_expr parser in
            let parser = 
                match parser.curToken with
                | Some (DELIMITER ",") -> advance parser
                | _ -> parser
            in
            parse_args parser (arg :: acc)
    in
    parse_args parser []
    
and parse_loops parser = 
    let parse_condition parser = 
        let parser = expect_token parser (DELIMITER "(") in
        let parser, condition =  parse_expr parser in
        let parser = expect_token parser (DELIMITER ")") in
        parser, condition
    in
    match parser.curToken with
    | Some (KEYWORD "for") ->
        let parser = expect_token parser (KEYWORD "for") in
        let parser = expect_token parser (DELIMITER "(") in
        let parser, init = 
            if parser.curToken = Some (DELIMITER ";") then
                advance parser, None
            else
                let parser, init_stmt = parse_statement parser in
                match init_stmt with
                | ExprStmt (Infix { left = Identifier _; operator = "="; _ }) ->
                    (* This is likely a variable declaration, so we need to expect a semicolon *)
                    let parser = expect_token parser (DELIMITER ";") in
                    parser, Some init_stmt
                | _ ->
                    (* For other types of statements, assume parse_type_declaration handled the semicolon *)
                    parser, Some init_stmt
        in
        let parser, condition = 
            if parser.curToken = Some (DELIMITER ";") then
                advance parser, None
            else
                let parser, cond_expr = parse_expr parser in
                let parser = expect_token parser (DELIMITER ";") in
                parser, Some cond_expr
        in
        let parser, update = 
            if parser.curToken = Some (DELIMITER ")") then
                parser, None
            else
                let parser, update_stmt = parse_statement parser in
                parser, Some update_stmt
        in
        let parser = expect_token parser (DELIMITER ")") in
        let parser, body = parse_block parser in
            parser, ExprStmt (ForLoop { init; condition; update; body })

    | Some (KEYWORD "while") -> 
        let parser = expect_token parser (KEYWORD "while") in
        let parser, condition = parse_condition parser in
        let parser, body = parse_block parser in
        parser, ExprStmt (WhileLoop {condition; body})
    | Some (KEYWORD "do") -> 
        let parser = expect_token parser (KEYWORD "do") in
        let parser, body = parse_block parser in
        let parser = expect_token parser (KEYWORD "while") in
        let parser, condition = parse_condition parser in
        let parser = expect_token parser (DELIMITER ";") in
        parser, ExprStmt (DoWhileLoop {body; condition})
    | _ -> failwith "Expected loop keyword (for, while, or do)"

let parse tokens =
  let lexer = init tokens in
  let parser = init_parser lexer in
  parse_program parser
