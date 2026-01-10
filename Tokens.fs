module Tokens

type Token =
    | Number of float
    | Operator of char * int * bool
    | UnaryOperator of char * int
    | Function of string
    | Constant of string
    | LeftParen
    | RightParen
    | Comma