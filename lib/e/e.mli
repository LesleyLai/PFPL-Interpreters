include module type of Types

val pp_typ: Format.formatter -> typ -> unit

val expr_typ: expr -> typ option
