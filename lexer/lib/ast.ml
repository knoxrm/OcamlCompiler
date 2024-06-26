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

and stmt = 
    | Return of expr
    | ExprStmt of expr
    | BlockStmt of block

and ident = { identifier : string } 
and block = { block : stmt list } 
and program = { stmts : stmt list } 

let token_literal = function
    | Program _ -> "program"
    | Expr _ -> "expression"
    | Stmt _ -> "statement"
;;
