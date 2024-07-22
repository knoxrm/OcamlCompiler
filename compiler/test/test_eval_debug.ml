open Lexlib.Ast
open Lexlib.Evaluator
open Lexlib.Environment
open Llvm

let () =
  (* Create a simple AST *)
  let ast = Program { stmts = [ExprStmt (Integer 42)] } in

  (* Generate IR *)
  let env = generate_ir ast in

  (* Print the IR *)
  print_ir env;

  (* Print additional debug information *)
  let module_ = get_module env in
  Printf.printf "\nModule name: %s\n" (Llvm.module_identifier module_);
  Printf.printf "Number of functions: %d\n" (Array.length (Llvm.functions module_));
  
  (* Print details of each function *)
  Array.iter (fun f ->
    Printf.printf "Function name: %s\n" (Llvm.value_name f);
    Printf.printf "Number of basic blocks: %d\n" (Array.length (Llvm.basic_blocks f));
    Array.iter (fun bb ->
      Printf.printf "Basic block:\n%s\n" (Llvm.string_of_llvalue bb)
    ) (Llvm.basic_blocks f)
  ) (Llvm.functions module_)
