type node = 
    | Program of program
    | Expr of expr
    | Stmt of stmt

and expr = 
    | Identifier of ident
    | Integer of int
    | Boolean of bool
    | String of string
    | Prefix of 
        { operator: string;
          right: expr
        }
    | Infix of 
        { left: expr
        ; operator: string
        ; right: expr
        }
    | If of 
        { condition: expr
        ; consequence: block
        ; alternative: block option
        }
    | FunctionLiteral of 
        { parameters: ident list
        ; body: block 
        ; return_type: string option
        ; name: string option
        }
    | Call of 
        { fn: expr 
        ; args: expr list
        }
    | Array of expr list
    | Index of 
        { left: expr
        ; right: expr
        }
    | ForLoop of 
        { init: stmt option
        ; condition: expr option
        ; update: stmt option
        ; body: block
        }
    | WhileLoop of 
        { condition: expr
        ; body: block
        }
    | DoWhileLoop of 
        { body: block
        ; condition: expr;
        }

and stmt = 
    | Return of expr
    | ExprStmt of expr
    | BlockStmt of block
    | Var of 
        { var_type: string
        ; name: string
        ; init: expr option
        }

and ident = { identifier : string } 
and block = { block : stmt list } 
and program = { stmts : stmt list } 

let token_literal = function
    | Program _ -> "program"
    | Expr _ -> "expression"
    | Stmt _ -> "statement"
;;
