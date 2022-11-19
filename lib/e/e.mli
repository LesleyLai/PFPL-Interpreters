include module type of Types

val pp_typ: Format.formatter -> typ -> unit
val pp_expr: Format.formatter -> expr -> unit

val expr_typ: expr -> typ option
val is_val: expr -> bool
val step: expr -> expr
