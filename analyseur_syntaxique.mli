type exp =
  | Int of int
  | Float of float
  | Parenth of exp
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

val analyse_syntaxique : Analyseur_lexical.lexem list -> exp
