open Lexlib.Lexer
open Lexlib.Parser
open Lexlib.Token
open Lexlib.Ast
open Lexlib.Evaluator
(* open Llvm *)

let rec print_ast {stmts} =
    match stmts with
    | [] -> 
        Format.printf "Empty AST@."
    | _ ->
        Format.printf "AST contains %d statements:@." (List.length stmts);
        List.iteri (fun i stmt -> 
            Format.printf "Statement %d:@." (i + 1);
            print_stmt 1 stmt
        ) stmts


and stringify token =
  match token with
  | IDENT s -> Format.asprintf "IDENT(%s)" s
  | KEYWORD s -> Format.asprintf "KEYWORD(%s)" s
  | INT_LITERAL n -> Format.asprintf "INT_LITERAL(%d)" n
  | BOOL_LITERAL b -> Format.asprintf "BOOL_LITERAL(%b)" b
  | OPERATOR s -> Format.asprintf "OPERATOR(%s)" s
  | DELIMITER s -> Format.asprintf "DELIMITER(%s)" s
  | EOF -> "EOF"


and print_expr level expr =
    let ind = indent level in
    match expr with
    | Integer n -> 
        Format.printf "%sInteger: %d\n" ind n
    | Identifier { identifier } -> 
        Format.printf "%sIdentifier: %s\n" ind identifier
    | Boolean b -> 
        Format.printf "%sBoolean: %b\n" ind b
    | String s -> 
        Format.printf "%sString: %s\n" ind s
    | Prefix { operator; right } ->
        Format.printf "%sPrefix:\n" ind;
        Format.printf "%s\tOperator: %s\n" ind operator;
        Format.printf "%s\tRight:\n" ind;
        print_expr (level + 2) right
    | Infix { left; operator; right } ->
        Format.printf "%sInfix:\n" ind;
        Format.printf "%s\tOperator: %s\n" ind operator;
        Format.printf "%s\tLeft:\n" ind;
        print_expr (level + 2) left;
        Format.printf "%s\tRight:\n" ind;
        print_expr (level + 2) right
    | If { condition; consequence; alternative } ->
        Format.printf "%sIf:\n" ind;
        Format.printf "%s\tCondition:\n" ind;
        print_expr (level + 2) condition;
        Format.printf "%s\tConsequence:\n" ind;
        print_block (level + 2) consequence;
        (match alternative with
        | Some alt -> 
            Format.printf "%s\tAlternative:\n" ind;
            print_block (level + 2) alt
        | None -> ())
    | FunctionLiteral { parameters; body; return_type; name } ->
        Format.printf "%sFunction Literal:\n" ind;
        (match name with
        | Some n -> Format.printf "%s\tName: %s\n" ind n
        | None -> ());
        (match return_type with
        | Some t -> Format.printf "%s\tReturn type: %s\n" ind t
        | None -> ());
        Format.printf "%s\tParameters:\n" ind;
        List.iter (fun p -> Format.printf "%s\t\t%s\n" ind p.identifier) parameters;
        Format.printf "%s\tBody:\n" ind;
        print_block (level + 2) body
    | Call { fn; args } ->
        Format.printf "%sFunction Call:\n" ind;
        Format.printf "%s\tFunction:\n" ind;
        print_expr (level + 2) fn;
        Format.printf "%s\tArguments:\n" ind;
        List.iter (print_expr (level + 2)) args
    | Array exprs ->
        Format.printf "%sArray:\n" ind;
        List.iter (print_expr (level + 1)) exprs
    | Index { left; right } ->
        Format.printf "%sIndex:\n" ind;
        Format.printf "%s\tLeft:\n" ind;
        print_expr (level + 2) left;
        Format.printf "%s\tRight:\n" ind;
        print_expr (level + 2) right
    | ForLoop { init; condition; update; body } ->
        Format.printf "%sFor Loop:\n" ind;
        (match init with
         | Some init_stmt -> 
             Format.printf "%s\tInitialization:\n" ind;
             print_stmt (level + 1) init_stmt
         | None -> Format.printf "%s\tNo initialization\n" ind);
        (match condition with
         | Some cond_expr -> 
             Format.printf "%s\tCondition:\n" ind;
             print_expr (level + 1) cond_expr
         | None -> Format.printf "%s\tNo condition (infinite loop)\n" ind);
        (match update with
         | Some update_stmt -> 
             Format.printf "%s\tUpdate:\n" ind;
             print_stmt (level + 1) update_stmt
         | None -> Format.printf "%s\tNo update\n" ind);
        Format.printf "%s\tBody:\n" ind;
        print_block (level + 1) body
    | WhileLoop { condition; body } ->
        Format.printf "%sWhile Loop:\n" ind;
        Format.printf "%s\tCondition:\n" ind;
        print_expr (level + 1) condition;
        Format.printf "%s\tBody:\n" ind;
        print_block (level + 1) body
    | DoWhileLoop { body; condition } ->
        Format.printf "%sDo-While Loop:\n" ind;
        Format.printf "%s\tBody:\n" ind;
        print_block (level + 1) body;
        Format.printf "%s\tCondition:\n" ind;
        print_expr (level + 1) condition

and print_block level block =
    let ind = indent level in
    Format.printf "%sBlock:\n" ind;
    List.iter (print_stmt (level + 1)) block.block

and print_stmt level stmt =
  let ind = indent level in
  match stmt with
  | ExprStmt expr ->
      Format.printf "%sExpression Statement:\n" ind;
      print_expr (level + 1) expr
  | Return expr ->
      Format.printf "%sReturn Statement:\n" ind;
      print_expr (level + 1) expr
  | BlockStmt block ->
      Format.printf "%sBlock Statement:\n" ind;
      print_block (level + 1) block
  | Var { var_type; name; init } ->
      Format.printf "%sVariable Declaration:@." ind;
      Format.printf "%s  Type: %s@." ind var_type;
      Format.printf "%s  Name: %s@." ind name;
      (match init with
      | Some expr -> 
          Format.printf "%s  Initializer:@." ind;
          print_expr (level + 2) expr
      | None -> 
          Format.printf "%s  No initializer@." ind)


and indent level = String.make level '\t'

let parse_and_print input =
  Format.printf "Input:\n%s@." input;
  let lexer = init input in
  Format.printf "Initiating Lexer: ";
  let rec print_tokens lexer =
    let lexer, token = next_token lexer in
    match token with
    | None -> Format.printf "Token: EOF@."
    | Some t -> 
        Format.printf "Token: %s@." (stringify t);
        print_tokens lexer
  in
  print_tokens lexer;
  Format.printf "\n@.";
  let parser = init_parser lexer in
  try
    let _, ast = parse_program parser in
    Format.printf "@.Parsed AST:@.";
    print_ast ast;
    
    (* Generate and print LLVM IR *)
    let ir = generate_ir ast in
    Format.printf "@.Generated LLVM IR:@.%s@." ir
  with
  | Failure msg ->
      Format.printf "Generation failed: %s@." msg

(* Your test cases *)
let () =
  let test_cases = [
    "int main() { return 42; }";
    "int main() { int x = 10; int y = 20; return x + y; }";
    (* Add more test cases here *)
  ] in
  List.iter parse_and_print test_cases
