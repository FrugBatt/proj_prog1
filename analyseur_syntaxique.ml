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

type syntax_t =
  | SExp of exp
  | SL_Parenth
  | SR_Parenth
  | SPlus_unary
  | SMinus_unary
  | SPlus_int
  | SPlus_float
  | SMinus_int
  | SMinus_float
  | STimes_int
  | STimes_float
  | SDiv
  | SMod
  | SInt_fun
  | SFloat_fun
  | SFact

let max_prio = 3
let t_priority = function
  | SExp _ -> -1
  | SL_Parenth -> -1
  | SR_Parenth -> -1
  | SPlus_unary -> 3
  | SMinus_unary -> 3
  | SPlus_int -> 1
  | SPlus_float -> 1
  | SMinus_int -> 1
  | SMinus_float -> 1
  | STimes_int -> 2
  | STimes_float -> 2
  | SDiv -> 2
  | SMod -> 2
  | SInt_fun -> 3
  | SFloat_fun -> 3
  | SFact -> 3

let t_arity = function
  | SExp _ -> 0
  | SL_Parenth -> 0
  | SR_Parenth -> 0
  | SPlus_unary -> 1
  | SMinus_unary -> 1
  | SPlus_int -> 2
  | SPlus_float -> 2
  | SMinus_int -> 2
  | SMinus_float -> 2
  | STimes_int -> 2
  | STimes_float -> 2
  | SDiv -> 2
  | SMod -> 2
  | SInt_fun -> 1
  | SFloat_fun -> 1
  | SFact -> -1

let process_t ope e1 e2 = match ope with
  | SExp e -> SExp e
  | SL_Parenth -> failwith "Unprocessable"
  | SR_Parenth -> failwith "Unprocessable"
  | SPlus_unary -> SExp (e1)
  | SMinus_unary -> SExp (Minus_unary e1)
  | SPlus_int -> SExp (Plus_int (e1,e2))
  | SPlus_float -> SExp (Plus_float (e1,e2))
  | SMinus_int -> SExp (Minus_int (e1,e2))
  | SMinus_float -> SExp (Minus_float (e1,e2))
  | STimes_int -> SExp (Times_int (e1,e2))
  | STimes_float -> SExp (Times_float (e1,e2))
  | SDiv -> SExp (Div (e1,e2))
  | SMod -> SExp (Mod (e1,e2))
  | SInt_fun -> SExp (Int_fun e1)
  | SFloat_fun -> SExp (Float_fun e1)
  | SFact -> SExp (Fact e1)

let is_exp = function
  | SExp _ -> true
  | _ -> false
let get_exp = function
  | SExp e -> e
  | _ -> raise Invalid_syntax

let reconstruct_stack st =
  let l = ref [] in
  while not (Stack.is_empty st) do
    l := (Stack.pop st) :: !l
  done;
  !l

let extract_parenth st =
  let l = ref [] in
  while Stack.top st <> SL_Parenth do
    l := (Stack.pop st) :: !l
  done;
  ignore (Stack.pop st);
  !l

let t_of_lexem = function
  | Analyseur_lexical.L_Parenth -> SL_Parenth
  | Analyseur_lexical.R_Parenth -> SR_Parenth
  | Analyseur_lexical.Plus_int -> SPlus_int
  | Analyseur_lexical.Plus_float -> SPlus_float
  | Analyseur_lexical.Minus_int -> SMinus_int
  | Analyseur_lexical.Minus_float -> SMinus_float
  | Analyseur_lexical.Times_int -> STimes_int
  | Analyseur_lexical.Times_float -> STimes_float
  | Analyseur_lexical.Div -> SDiv
  | Analyseur_lexical.Mod -> SMod
  | Analyseur_lexical.Int_fun -> SInt_fun
  | Analyseur_lexical.Float_fun -> SFloat_fun
  | Analyseur_lexical.Int x -> SExp (Int x)
  | Analyseur_lexical.Float x -> SExp (Float x)
  | Analyseur_lexical.Fact -> SFact

let rec analyse_syntaxique_t l =
  let st = Stack.create () in
  let rec aux priority l = match l with
      | [] ->
        if priority = 1 then if Stack.length st = 1 then get_exp (Stack.pop st) else raise Invalid_syntax
        else let new_l = reconstruct_stack st in aux (priority-1) new_l
      | h::t ->
        if is_exp h && not (Stack.is_empty st) then begin
          let top = Stack.top st and exp = get_exp h in
            if t_arity top = 1 && t_priority top = priority then begin
              ignore (Stack.pop st);
              Stack.push (process_t top exp exp) st
            end else if t_arity top = 2 && t_priority top = priority then begin
              ignore (Stack.pop st);
              let exp2 = get_exp (Stack.pop st) in
                Stack.push (process_t top exp2 exp) st
            end else Stack.push h st;
            aux priority t
        end else if t_arity h = (-1) && t_priority h = priority then begin
          if Stack.is_empty st then raise Invalid_syntax
          else let top = Stack.pop st in
            if is_exp top then Stack.push (process_t h (get_exp top) (get_exp top)) st
            else raise Invalid_syntax;
            aux priority t
        end else if h = SR_Parenth then
          let parenth = extract_parenth st in aux priority ((SExp (analyse_syntaxique_t parenth))::t);
        else begin
          if h = SPlus_int && (Stack.is_empty st || not (is_exp (Stack.top st))) then Stack.push SPlus_unary st
          else if h = SMinus_int && (Stack.is_empty st || not (is_exp (Stack.top st))) then (print_endline "pushed"; Stack.push SMinus_unary st)
          else Stack.push h st;
          aux priority t
        end
  in aux max_prio l



type exp_t =
  | TInt
  | TFloat

let rec type_of_exp = function
  | Int _ -> TInt
  | Float _ -> TFloat
  | Minus_unary e -> type_of_exp e
  | Int_fun _ -> TInt
  | Float_fun _ -> TFloat
  | Plus_int _ -> TInt
  | Plus_float _ -> TFloat
  | Minus_int _ -> TInt
  | Minus_float _ -> TFloat
  | Times_int _ -> TInt
  | Times_float _ -> TFloat
  | Div _ -> TInt
  | Mod _ -> TInt
  | Fact _ -> TInt

let rec check_type = function
  | Int _ -> true
  | Float _ -> true
  | Minus_unary e -> check_type e
  | Int_fun e -> (type_of_exp e = TFloat) && check_type e
  | Float_fun e -> (type_of_exp e = TInt) && check_type e
  | Plus_int (e1,e2) -> (type_of_exp e1 = TInt) && (type_of_exp e2 = TInt) && check_type e1 && check_type e2
  | Plus_float (e1,e2) -> (type_of_exp e1 = TFloat) && (type_of_exp e2 = TFloat) && check_type e1 && check_type e2
  | Minus_int (e1,e2) -> (type_of_exp e1 = TInt) && (type_of_exp e2 = TInt) && check_type e1 && check_type e2
  | Minus_float (e1,e2) -> (type_of_exp e1 = TFloat) && (type_of_exp e2 = TFloat) && check_type e1 && check_type e2
  | Times_int (e1,e2) -> (type_of_exp e1 = TInt) && (type_of_exp e2 = TInt) && check_type e1 && check_type e2
  | Times_float (e1,e2) -> (type_of_exp e1 = TFloat) && (type_of_exp e2 = TFloat) && check_type e1 && check_type e2
  | Div (e1,e2) -> (type_of_exp e1 = TInt) && (type_of_exp e2 = TInt) && check_type e1 && check_type e2
  | Mod (e1,e2) -> (type_of_exp e1 = TInt) && (type_of_exp e2 = TInt) && check_type e1 && check_type e2
  | Fact e -> (type_of_exp e = TInt) && check_type e

let analyse_syntaxique lexems =
  let ast = analyse_syntaxique_t (List.map t_of_lexem lexems) in
  if check_type ast then ast
  else raise Invalid_type
