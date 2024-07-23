open Lexlib.Lexer
open Lexlib.Parser
open Lexlib.Token
open Lexlib.Evaluator

let stringify token =
  match token with
  | IDENT s -> Format.asprintf "IDENT(%s)" s
  | KEYWORD s -> Format.asprintf "KEYWORD(%s)" s
  | INT_LITERAL n -> Format.asprintf "INT_LITERAL(%d)" n
  | BOOL_LITERAL b -> Format.asprintf "BOOL_LITERAL(%b)" b
  | OPERATOR s -> Format.asprintf "OPERATOR(%s)" s
  | DELIMITER s -> Format.asprintf "DELIMITER(%s)" s
  | EOF -> "EOF"



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
    (* Format.printf "@.Parsed AST:@."; *)
    (* print_ast ast; *)
    
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
    "int main() { int x = 10; if (x > 5) { return 1; } else { return 0; } }";
    "
    int add(int a, int b) {
      return a + b;
    }

    int main() {
      return add(3, 4);
    }";
    "
    int main() {
      int[5] arr;
      arr[0] = 10;
      arr[1] = 20;
      arr[2] = 30;
      arr[3] = 40;
      arr[4] = 50;
      return arr[2] + arr[4];
    }
  "
    (* Add more test cases here *)
  ] in
  List.iter parse_and_print test_cases
