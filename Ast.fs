module Ast

type Expr =
    | Num of float
    | Binary of Expr * char * Expr
    | Unary of char * Expr
    | Call of string * Expr list
    | ConstantRef of string