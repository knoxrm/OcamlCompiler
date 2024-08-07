open Ast
open Llvm

let symbol_table = Hashtbl.create 100


let create_context_and_module () =
  let context = global_context () in
  let the_module = create_module context "my_module" in
  let builder = builder context in
  (context, the_module, builder)

let rec string_of_expr = function
  | Integer n -> Format.sprintf "Integer(%d)" n
  | Identifier { identifier } -> Format.sprintf "Identifier(%s)" identifier
  | Infix { left; operator; right } -> 
      Format.sprintf "Infix(%s, %s, %s)" (string_of_expr left) operator (string_of_expr right)
  | FunctionLiteral { name; parameters; return_type; _ } ->
      Format.sprintf "FunctionLiteral(%s, [%s], %s)" 
        (Option.value name ~default:"anonymous")
        (String.concat "; " (List.map (fun p -> p.identifier) parameters))
        (Option.value return_type ~default:"unspecified")
  | _ -> "Unknown expression"

let rec codegen_expr context the_module builder expr = 
  match expr with 
  | Integer n ->
      const_int (i32_type context) n
  
  | Boolean b ->
      const_int (i1_type context) (if b then 1 else 0)
  
  | Identifier { identifier } ->
      (match Hashtbl.find_opt symbol_table identifier with
       | Some llvalue -> llvalue
       | None -> raise (Failure ("Undefined variable: " ^ identifier)))
  
  | Infix { left; operator; right } ->
      let lhs = codegen_expr context the_module builder left in
      let rhs = codegen_expr context the_module builder right in
      (match operator with
       | "+" -> build_add lhs rhs "addtmp" builder
       | "-" -> build_sub lhs rhs "subtmp" builder
       | "*" -> build_mul lhs rhs "multmp" builder
       | "/" -> build_sdiv lhs rhs "divtmp" builder
       | "==" -> build_icmp Icmp.Eq lhs rhs "eqtmp" builder
       | "!=" -> build_icmp Icmp.Ne lhs rhs "neqtmp" builder
       | "<" -> build_icmp Icmp.Slt lhs rhs "slttmp" builder
       | "<=" -> build_icmp Icmp.Sle lhs rhs "sletmp" builder
       | ">" -> build_icmp Icmp.Sgt lhs rhs "sgttmp" builder
       | ">=" -> build_icmp Icmp.Sge lhs rhs "sgetmp" builder
       | _ -> raise (Failure ("Unknown operator: " ^ operator)))
  
  | If { condition; consequence; alternative } ->
      (* let cond = codegen_expr context the_module builder condition in *)
      (* let zero = const_int (i1_type context) 0 in *)
      (* let cond_val = build_icmp Icmp.Ne cond zero "ifcond" builder in *)

      (* let start_bb = insertion_block builder in *)
      (* let the_function = block_parent start_bb in *)

      (* let then_bb = append_block context "then" the_function in *)
      (* position_at_end then_bb builder; *)
      (* let then_val = codegen_block context the_module builder consequence in *)
      (* let new_then_bb = insertion_block builder in *)

      (* let else_bb = append_block context "else" the_function in *)
      (* position_at_end else_bb builder; *)
      (* let else_val = match alternative with *)
      (*   | Some alt -> codegen_block context the_module builder alt *)
      (*   | None -> const_int (i32_type context) 0 in *)
      (* let new_else_bb = insertion_block builder in *)

      (* let merge_bb = append_block context "ifcont" the_function in *)
      (* position_at_end merge_bb builder; *)
      (* let phi = build_phi [(then_val, new_then_bb); (else_val, new_else_bb)] "iftmp" builder in *)

      (* position_at_end start_bb builder; *)
      (* ignore (build_cond_br cond_val then_bb else_bb builder); *)

      (* position_at_end new_then_bb builder; *)
      (* ignore (build_br merge_bb builder); *)

      (* position_at_end new_else_bb builder; *)
      (* ignore (build_br merge_bb builder); *)

      (* position_at_end merge_bb builder; *)
      (* phi *)
    let cond = codegen_expr context the_module builder condition in
    let cond_val = match condition with
      | Identifier _ -> 
          (* If it's a variable, we need to load its value *)
          build_load cond "condtmp" builder
      | _ -> cond
    in
    let cmp = build_icmp Icmp.Sgt cond_val (const_int (i32_type context) 5) "cmptmp" builder in
    
    let the_function = block_parent (insertion_block builder) in
    let then_bb = append_block context "then" the_function in
    let else_bb = append_block context "else" the_function in
    
    ignore (build_cond_br cmp then_bb else_bb builder);
    
    position_at_end then_bb builder;
    ignore (codegen_block context the_module builder consequence);
    
    position_at_end else_bb builder;
    (match alternative with
     | Some alt -> ignore (codegen_block context the_module builder alt)
     | None -> ());
    
    (* Both branches should end with a return, so we don't need a merge block *)
    const_int (i32_type context) 0 (* Dummy value, will not be used *)

  | FunctionLiteral { parameters; body; return_type; name } ->
      Format.printf "  Function literal: %s\n" (Option.value name ~default:"anonymous");
      let func_name = Option.value name ~default:"anonymous" in
      let return_ty = match return_type with
        | Some "int" -> i32_type context
        | Some "void" -> void_type context
        | Some t -> raise (Failure ("Unsupported return type: " ^ t))
        | None -> i32_type context  (* default to int if not specified *)
      in
      let param_types = Array.make (List.length parameters) (i32_type context) in
      let func_type = function_type return_ty param_types in
      let func = declare_function func_name func_type the_module in
      
      let bb = append_block context "entry" func in
      position_at_end bb builder;
      
      (* Create a new scope for function parameters *)
      let old_symbol_table = Hashtbl.copy symbol_table in
      Hashtbl.clear symbol_table;
      
      (* List.iteri (fun i param -> *)
      (*     let param_value =  *)
      (*         match param_begin func with *)
      (*         | Before first_param ->  *)
      (*             let rec nth_param n current = *)
      (*             if n = 0 then Some current *)
      (*             else  *)
      (*                 match instr_succ current with *)
      (*                 | Before next -> nth_param (n-1) next *)
      (*                 | At_end _ -> None *)
      (*             in *)
      (*             nth_param i first_param *)
      (*         | At_end _ -> None *)
      (*     in *)
      (*     match param_value with *)
      (*     | Some value -> *)
      (*         set_value_name param.identifier value; *)
      (*         let alloca = build_alloca (type_of value) param.identifier builder in *)
      (*         ignore (build_store value alloca builder); *)
      (*         Hashtbl.add symbol_table param.identifier alloca *)
      (*     | None -> raise (Failure ("Parameter not found: " ^ param.identifier)) *)
      (*   ) parameters;  *)
      (* Generate code for function body *)
      Format.printf "Number of parameters: %d\n" (List.length parameters);
      List.iteri (fun i param ->
        Format.printf "Processing parameter %d: %s\n" i param.identifier;
        let param_value = Llvm.param func i in
        Format.printf "Parameter value: %s\n" (string_of_llvalue param_value);
        set_value_name param.identifier param_value;
        let alloca = build_alloca (type_of param_value) param.identifier builder in
        ignore (build_store param_value alloca builder);
        Hashtbl.add symbol_table param.identifier alloca;
        Format.printf "Added parameter %s to symbol table\n" param.identifier;
      ) parameters;

      Format.printf "  Generating code for function body\n";
      let _ = codegen_block context the_module builder body in
      
      (* Restore the old symbol table *)
      Hashtbl.clear symbol_table;
      Hashtbl.iter (Hashtbl.add symbol_table) old_symbol_table;
      
      (* Ensure the function always returns *)
      if not (block_terminator (insertion_block builder) |> Option.is_some) then
        ignore (build_ret (const_int (i32_type context) 0) builder);
      
      func
  | Call { fn; args } ->
      (* raise (Failure "Function calls not yet implemented") *)
      Format.printf "  Function call\n";
      let callee =
        match fn with
        | Identifier { identifier } -> 
            (match lookup_function identifier the_module with
            | Some f -> f
            | None -> raise (Failure ("Unknown function: " ^ identifier)))
        | _ -> codegen_expr context the_module builder fn
      in
      let args = List.map (codegen_expr context the_module builder) args in
      build_call callee (Array.of_list args) "calltmp" builder
  | Array exprs ->
      Printf.printf "  Array literal\n";
      let elements = List.map (codegen_expr context the_module builder) exprs in
      let ty = type_of (List.hd elements) in
      let arr_type = array_type ty (List.length elements) in
      let arr_alloca = build_alloca arr_type "array" builder in
      List.iteri (fun i el ->
        let idx = const_int (i32_type context) i in
        let gep = build_gep arr_alloca [| const_int (i32_type context) 0; idx |] "array_index" builder in
        ignore (build_store el gep builder)
      ) elements;
      arr_alloca

  | Index { left; right } ->
      Printf.printf "  Array indexing\n";
      let arr = codegen_expr context the_module builder left in
      let idx = codegen_expr context the_module builder right in
      let gep = build_gep arr [| const_int (i32_type context) 0; idx |] "array_index" builder in
      build_load gep "array_load" builder
  | ForLoop _ ->
      raise (Failure "For loops not yet implemented")
  | WhileLoop _ ->
      raise (Failure "While loops not yet implemented")
  | DoWhileLoop _ ->
      raise (Failure "Do-while loops not yet implemented")

  | _ as e -> 
      Format.printf "  Unsupported expression type: %s\n" (string_of_expr e);
      raise (Failure ("Unsupported expression type: " ^ string_of_expr e))

and codegen_stmt context the_module builder stmt =
  match stmt with
  | Return expr ->
      let ret_val = codegen_expr context the_module builder expr in
      ignore (build_ret ret_val builder);
      ret_val
  | ExprStmt expr ->
      codegen_expr context the_module builder expr
  | Var { var_type; name; init } ->
      let alloca = match var_type with
        | "int" -> build_alloca (i32_type context) name builder
        | "bool" -> build_alloca (i1_type context) name builder
        | _ -> raise (Failure ("Unsupported type: " ^ var_type))
      in
      Hashtbl.add symbol_table name alloca;
      (match init with
       | Some expr ->
           let init_val = codegen_expr context the_module builder expr in
           ignore (build_store init_val alloca builder);
           init_val
       | None -> alloca)
  | BlockStmt block ->
      codegen_block context the_module builder block
  (* ... other statement types ... *)


and codegen_block context the_module builder { block } =
  List.fold_left (fun _ stmt -> codegen_stmt context the_module builder stmt) 
    (const_int (i32_type context) 0) block

let codegen_program context the_module builder program =
  Format.printf "Starting codegen for program\n";
  let codegen_top_level_function = function
  | ExprStmt (FunctionLiteral func) ->
      ignore (codegen_expr context the_module builder (FunctionLiteral func));
  | _ ->
        raise (Failure "Expected a single function (main) at the top level")
  in
  List.iter codegen_top_level_function program.stmts;
  
  (* Ensure main function exists *)
  (match lookup_function "main" the_module with
  | Some _ -> ()
  | None -> raise (Failure "No main function defined"));
  
  the_module

let generate_ir program =
  Format.printf "Starting IR generation\n";
  let context, the_module, builder = create_context_and_module () in
  let result = codegen_program context the_module builder program in
  let ir = string_of_llmodule result in
  Format.printf "Finished IR generation\n";
  ir
  (* Format.printf "Starting IR generation\n"; *)
  (* let context, the_module, builder = create_context_and_module () in *)
  (* let _ = codegen_program context the_module builder program in *)
  (* let result = string_of_llmodule the_module in *)
  (* Format.printf "Finished IR generation\n"; *)
  (* result *)

