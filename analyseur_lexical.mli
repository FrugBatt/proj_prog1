type lexem =
  | L_Parenth
  | R_Parenth
  | Plus_int
  | Plus_float
  | Minus_int
  | Minus_float
  | Times_int
  | Times_float
  | Div_int
  | Div_float
  | Mod
  | Int_fun
  | Float_fun
  | Int of int
  | Float of float
  | Fact
  | Pow

exception Invalid_lexem

val analyse_lexicale : string -> lexem list
