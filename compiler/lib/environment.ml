open Llvm

type t = 
{ ctx: llcontext
; builder: llbuilder
; module_ : llmodule
; values: (string, llvalue) Hashtbl.t
}

let create () = 
    let context = global_context () in
    let module_ = create_module context "c_compiler" in 
    let builder = builder context in 
    { context
    ; builder
    ; module_
    ; values = Hashtbl.create 10
    }

let get_context env = env.context
let get_module env = env.module_
let get_builder env = env.builder


let add_var env name value = 
    Hashtbl.add env.values name

let print_ir env = 
    print_string (string_of_llmodule env.module_)
