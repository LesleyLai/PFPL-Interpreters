type typ =
  | TNum
  | TStr

type var = string
type num = int

type expr =
  | Var of var
  | Num of num
  | Str of string
  | Plus of expr * expr
  | Times of expr * expr
  | Cat of expr * expr
  | Len of expr
  | Let of expr * var * expr
