open Lexlib.Lexer
open Lexlib.Parser
open Lexlib.Evaluator

let skip_whitespace str =
  str
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> String.concat "\n"
(* Helper function to generate IR from input string *)
let generate_ir_from_string input =
  let lexer = init input in
  let parser = init_parser lexer in
  try
    let _, ast = parse_program parser in
    generate_ir ast
  with
  | Failure msg -> failwith ("IR generation failed: " ^ msg)

(* Test function for IR generation *)
let test_ir_generation input expected_ir () =
  let generated_ir = generate_ir_from_string input in
    let expected = skip_whitespace expected_ir in
    let generated = skip_whitespace generated_ir in
  Alcotest.(check string) "same IR" expected generated

(* Test cases *)
(* Test cases *)
let ir_generation_tests = [
  "Simple function", `Quick, test_ir_generation 
    "int main() { return 42; }"
    "; ModuleID = 'my_module'
    source_filename = \"my_module\"

    define i32 @main() {
    entry:
    ret i32 42
    }
    ";

    "Function with variables", `Quick, test_ir_generation
        "int main() { int x = 10; int y = 20; return x + y; }"
    "; ModuleID = 'my_module'
     source_filename = \"my_module\"

     define i32 @main() {
     entry:
       %x = alloca i32, align 4
       store i32 10, i32* %x, align 4
       %y = alloca i32, align 4
       store i32 20, i32* %y, align 4
       %addtmp = add i32* %x, %y
       ret i32* %addtmp
    }";

    "Function with if statement", `Quick, test_ir_generation
        "int main() { int x = 10; if (x > 5) { return 1; } else { return 0; } }"
        "; ModuleID = 'my_module'
    source_filename = \"my_module\"

    define i32 @main() {
    entry:
    %x = alloca i32
    store i32 10, i32* %x
    %1 = load i32, i32* %x
    %2 = icmp sgt i32 %1, 5
    br i1 %2, label %then, label %else

    then:
    ret i32 1

    else:
    ret i32 0
    }
    ";
]

(* Run the tests *)
let () =
  Alcotest.run "IR Generation Tests" [
    "IR generation", ir_generation_tests;
  ]

