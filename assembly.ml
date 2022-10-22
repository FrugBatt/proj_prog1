open Analyseur_syntaxique
open X86_64

let print_int_fun =
  label "print_int" ++
    movq (reg rdi) (reg rsi) ++
    movq (ilab "S_int") (reg rdi) ++
    movq (imm 0) (reg rax) ++
    call "printf" ++
    ret

let extract_stack = popq r13 ++ popq r14

let rec generate_main = function
  | Int x -> pushq (imm x)
  | Float x -> failwith "not implemented"
  | Plus_int (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ addq (reg r13) (reg r14) ++ pushq (reg r14)
  | Minus_int (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ subq (reg r13) (reg r14) ++ pushq (reg r14)
  | Times_int (e1,e2) -> (generate_main e1) ++ (generate_main e2) ++ extract_stack ++ imulq (reg r13) (reg r14) ++ pushq (reg r14)
  | _ -> failwith "not implemented"

let generate_assembly e =
  let header = globl "main" in
  let main = label "main" ++ (generate_main e) ++ popq rdi ++ call "print_int" ++ ret in
  let data = label "S_int" ++ string "%d" in
  {text = header ++ main ++ print_int_fun; data = data}

let write_assembly file e =
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  print_program fmt (generate_assembly e); close_out oc
