type exp =
  | Int of int
  | Float of float
  | Minus_unary of exp
  | Int_fun of exp
  | Float_fun of exp
  | Plus_int of exp*exp
  | Plus_float of exp*exp
  | Minus_int of exp*exp
  | Minus_float of exp*exp
  | Times_int of exp*exp
  | Times_float of exp*exp
  | Div of exp*exp
  | Mod of exp*exp
  | Fact of exp

exception Invalid_syntax
exception Invalid_type

val analyse_syntaxique : Analyseur_lexical.lexem list -> exp
