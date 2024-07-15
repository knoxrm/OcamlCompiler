(* let () = Format.printf "Basic Lexer" *)
open Lexlib.Lexer
open Lexlib.Parser
open Lexlib.Token
open Lexlib.Ast

let rec print_ast stmts =
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
    print_ast ast
  with
    Failure msg ->
      Format.printf "Parsing failed: %s@." msg


let () =
    (* Test cases *)
    let test_cases = [
        "5 + 3;";
        "return 42;";
        "x = 10; y = 20; x + y;";
        "if (x < 10) { return x; } else { return y; }";
        "int main() { return 0; }";
        "int add(int a, int b) { return a + b; }";
        "int[] numbers = [1, 2, 3, 4, 5];";
        "numbers[2] = 10;";
        "int third = numbers[2];";
        "void printArray(int[] arr) { return; }";
            (* Function calls *)
        "foo(1, 2, 3);";
        "bar(x, y + z);";
        "baz(qux(), 5);";

        (* Function calls in expressions *)
        "int result = add(5, multiply(3, 4));";

        (* Function calls as statements *)
        "printArray(numbers);";
        "process(getData(), 10);";
        (* For loops *)
        "for (int i = 0; i < 10; i = i + 1) { print(i); }";
        "for (;;) { if (x > 10) break; }";  (* Infinite loop *)
        "for (int i = 0; i < 5;) { print(i); i = i + 1; }";  (* No update in for statement *)
        "for (; x < 100; x = x + 1) { process(x); }";  (* No initialization *)

        (* While loops *)
        "while (x < 10) { x = x + 1; }";
        "while (true) { if (condition()) return false; }";  (* Infinite loop with break *)

        (* Do-while loops *)
        "do { print(x); x = x + 1; } while (x < 10);";
        "do { process(); } while (false);";  (* Always executes once *)

        (* Nested loops *)
        "for (int i = 0; i < 3; i = i + 1) { for (int j = 0; j < 3; j = j + 1) { print(i, j); } }";
        "int i = 0; while (i < 3) { for (int j = 0; j < 3; j = j + 1) { print(i * 3 + j); } i = i + 1; }";

        (* (* (* Loops with complex conditions *) *) *)
        (* "for (int i = 0; i < 10 && !done(); i = i + 1) { process(i); }"; *)
        (* "while (x < 10 || y > 20) { updateXY(); }"; *)

        (* (* (* Loops with break and continue *) *) *)
        (* "for (int i = 0; i < 10; i = i + 1) { if (i == 5) continue; if (i == 8) break; print(i); }"; *)
        (* "while (true) { if (condition1()) continue; if (condition2()) break; process(); }"; *)

        (* Loops with function calls *)
        "for (int i = getStart(); i < getEnd(); i = i + getStep()) { process(i); }";
        "while (hasNext()) { processNext(); }";

        (* Loops with array operations *)
        "for (int i = 0; i < arrayLength(arr); i = i + 1) { arr[i] = i * 2; }";
        "while (i < arrayLength(arr)) { sum = sum + arr[i]; i = i + 1; }";
    ] in

    List.iter (fun case ->
        Format.printf "\n--- Test Case ---\n";
        parse_and_print case;
        Format.printf "--- End Test Case ---\n\n"
    ) test_cases
