(* Type lexem qui permet de faire l'analyse lexicale *)
type lexem =
  | L_Parenth
  | R_Parenth
  | Plus_int
  | Plus_float
  | Minus_int
  | Minus_float
  | Times_int
  | Times_float
  | Div
  | Mod
  | Int_fun
  | Float_fun
  | Int of int
  | Float of float
  | Fact
  | Pow

(* Exception qui permet de reconnaitre les erreurs dans l'analyse lexicale *)
exception Invalid_lexem

(* Vérifie si un caractère est une lettre *)
let is_letter c = 'a' <= c && c <= 'z'
(* Vérifie si un caractère est un chiffre *)
let is_digit c = '0' <= c && c <= '9'

(* Vérifie si un string possède le format d'un nombre entier *)
let is_number s =
  let f b c = b && (is_digit c) in
    String.fold_left f true s 

(* Vérifie si un string possède le format d'un nombre flottant *)
let is_float s =
  let splitted = String.split_on_char '.' s in match splitted with
    | [s1;s2] -> (is_number s1) && (is_number s2)
    | _ -> false

(* Traduction d'un string en lexem *)
let lexem_of_string = function
  | "(" -> Some L_Parenth
  | ")" -> Some R_Parenth
  | "+" -> Some Plus_int
  | "+." -> Some Plus_float
  | "-" -> Some Minus_int
  | "-." -> Some Minus_float
  | "*" -> Some Times_int
  | "*." -> Some Times_float
  | "/" -> Some Div
  | "%" -> Some Mod
  | "int" -> Some Int_fun
  | "float" -> Some Float_fun
  | "!" -> Some Fact
  | "^" -> Some Pow
  | s ->
    if is_number s then Some(Int(int_of_string s))
    else if is_float s then Some(Float(float_of_string s))
    else None

(* Analyse lexicale principale *)
let analyse_lexicale exp_s =
  let n = String.length exp_s in
  let rec aux buf i buf_reco =
    let old_buf = String.sub exp_s buf (i-buf) in
      if i = n then begin (* Cas de base *)
        if buf_reco then [Option.get (lexem_of_string old_buf)]
        else raise Invalid_lexem
      end else
      let new_buf = String.sub exp_s buf (i-buf+1) in
      let new_lexem = lexem_of_string new_buf in
      let new_reco = Option.is_some new_lexem in
      if new_reco then aux buf (i+1) true (* On essaye de prendre le plus grand lexem reconnu *)
      else if buf_reco then  (* Le lexem précédent était maximal, on l'ajoute *)
        let new_bool = Option.is_some (lexem_of_string (String.sub exp_s i 1)) in
          (Option.get (lexem_of_string old_buf)) :: (aux i (i+1) new_bool)
      else aux buf (i+1) false
  in
  let first_buf = String.sub exp_s 0 1 in
  let is_reco = Option.is_some (lexem_of_string first_buf) in
  aux 0 1 is_reco
