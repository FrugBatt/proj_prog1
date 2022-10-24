open Analyseur_syntaxique
open X86_64

type as_exp =
  | AInt of int
  | AFloat of string*float
  | AMinus_unary of as_exp
  | AInt_fun of as_exp
  | AFloat_fun of as_exp
  | APlus_int of as_exp*as_exp
  | APlus_float of as_exp*as_exp
  | AMinus_int of as_exp*as_exp
  | AMinus_float of as_exp*as_exp
  | ATimes_int of as_exp*as_exp
  | ATimes_float of as_exp*as_exp
  | ADiv of as_exp*as_exp
  | AMod of as_exp*as_exp

type exp_type =
  | TInt
  | TFloat

let rec type_of_exp = function
  | AInt _ -> TInt
  | AFloat _ -> TFloat
  | AMinus_unary e -> type_of_exp e
  | AInt_fun _ -> TInt
  | AFloat_fun _ -> TFloat
  | APlus_int _ -> TInt
  | APlus_float _ -> TFloat
  | AMinus_int _ -> TInt
  | AMinus_float _ -> TFloat
  | ATimes_int _ -> TInt
  | ATimes_float _ -> TFloat
  | ADiv _ -> TInt
  | AMod _ -> TInt

let as_exp_of_exp e =
  let i = ref (-1) in
  let rec aux = function
    | Int x -> AInt x
    | Float x -> incr i; AFloat (".LC"^(string_of_int !i), x)
    | Minus_unary e1 -> AMinus_unary (aux e1)
    | Int_fun e1 -> AInt_fun (aux e1)
    | Float_fun e1 -> AFloat_fun (aux e1)
    | Plus_int (e1,e2) -> APlus_int (aux e1,aux e2)
    | Plus_float (e1,e2) -> APlus_float (aux e1, aux e2)
    | Minus_int (e1,e2) -> AMinus_int (aux e1, aux e2)
    | Minus_float (e1,e2) -> AMinus_float (aux e1, aux e2)
    | Times_int (e1,e2) -> ATimes_int (aux e1, aux e2)
    | Times_float (e1,e2) -> ATimes_float (aux e1, aux e2)
    | Div (e1,e2) -> ADiv (aux e1, aux e2)
    | Mod (e1,e2) -> AMod (aux e1, aux e2)
  in aux e

let rec as_exp_data = function
  | AInt x -> nop
  | AFloat (l,x) -> label l ++ double x
  | AMinus_unary e -> as_exp_data e
  | AInt_fun e -> as_exp_data e
  | AFloat_fun e -> as_exp_data e
  | APlus_int (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | APlus_float (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | AMinus_int (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | AMinus_float (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | ATimes_int (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | ATimes_float (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | ADiv (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  | AMod (e1,e2) -> as_exp_data e1 ++ as_exp_data e2
  
let print_int_fun =
  label "print_int" ++
    movq (reg rdi) (reg rsi) ++
    movq (ilab "S_int") (reg rdi) ++
    movq (imm 0) (reg rax) ++
    call "printf" ++
    ret

let print_float_fun =
  label "print_float" ++
    movq (ilab "S_float") (reg rdi) ++
    movq (imm 1) (reg rax) ++
    call "printf" ++
    ret

let push_int op = subq (imm 8) (reg rbp) ++ movq op (ind rbp)
let pop_int r = movq (ind rbp) (reg r) ++ addq (imm 8) (reg rbp)
let push_float op = subq (imm 8) (reg rbp) ++ movsd op (reg xmm0) ++ movsd (reg xmm0) (ind rbp)
let pop_float regx = movsd (ind rbp) (reg regx) ++ addq (imm 8) (reg rbp)

let extract_stack = pop_int r13 ++ pop_int r14
let extract_stack_float = pop_float xmm0 ++ pop_float xmm1

let rec generate_main = function
  | AInt x -> push_int (imm x)
  | AFloat (l,_) -> push_float (lab l)
  | APlus_int (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ addq (reg r13) (reg r14) ++ push_int (reg r14)
  | AMinus_int (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ subq (reg r13) (reg r14) ++ push_int (reg r14)
  | ATimes_int (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ imulq (reg r13) (reg r14) ++ push_int (reg r14)
  | ADiv (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ movq (reg r14) (reg rax) ++ movq (imm 0) (reg rdx) ++ idivq (reg r13) ++ push_int (reg rax)
  | AMod (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ movq (reg r14) (reg rax) ++ movq (imm 0) (reg rdx) ++ idivq (reg r13) ++ push_int (reg rdx)
  | APlus_float (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack_float ++ addsd (reg xmm0) (reg xmm1) ++ push_float (reg xmm1)
  | AMinus_float (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack_float ++ subsd (reg xmm0) (reg xmm1) ++ push_float (reg xmm1)
  | AInt_fun e -> (generate_main e) ++ pop_float xmm0 ++ cvttsd2si (reg xmm0) (reg r14) ++ push_int (reg r14)
  | AFloat_fun e -> (generate_main e) ++ pop_int r14 ++ cvtsi2sdq (reg r14) (reg xmm0) ++ push_float (reg xmm0)
  | AMinus_unary (e) ->
      if type_of_exp e = TInt then generate_main (ATimes_int (AInt (-1), e))
      else (generate_main e) ++ pop_float xmm0 ++ mulsd (lab ".NEG") (reg xmm0) ++ push_float (reg xmm0)
  | _ -> failwith "not implemented"

let generate_assembly e =
  let header = globl "main" in
  let t = type_of_exp e in
  let main = label "main" ++ movq (reg rsp) (reg rbp) ++ (generate_main e) in
  let data = label "S_int" ++ string "%d\n" ++ label "S_float" ++ string "%f\n" ++ label ".NEG" ++ double (-1.0) ++ as_exp_data e in
  if t = TInt then {text = header ++ main ++ pop_int rdi ++ call "print_int" ++ ret ++ print_int_fun; data = data}
  else {text = header ++ main ++ pop_float xmm0 ++ call "print_float" ++ ret ++ print_float_fun; data = data}

let write_assembly file e =
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  print_program fmt (generate_assembly (as_exp_of_exp e)); close_out oc
