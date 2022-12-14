open Analyseur_lexical

let usage = "Usage : ./aritha expression.exp"

(* Compile une expression et l'écrit dans un fichier *)
let compile_exp exp_s file_s =
  let lexems = Analyseur_lexical.analyse_lexicale (List.fold_left (^) "" (String.split_on_char ' ' exp_s)) in
  let ast = Analyseur_syntaxique.analyse_syntaxique lexems in
  Assembly.write_assembly file_s ast

(* Fonction main du programme qui lis le fichier et le compile *)
let _ =
  if Array.length Sys.argv <= 1 then print_endline usage
  else let exp_file = Sys.argv.(1) in
    let ic = open_in exp_file in
    try
      let line = input_line ic in
      let file_s = (String.sub exp_file 0 (String.index exp_file '.'))^".s" in
      compile_exp line file_s;
      flush stdout;
      close_in ic
    with
      | Analyseur_lexical.Invalid_lexem -> Printf.printf "Invalid lexem\n"
      | Analyseur_syntaxique.Invalid_syntax -> Printf.printf "Invalid syntax\n"
      | Analyseur_syntaxique.Invalid_type -> Printf.printf "Invalid type\n"
      | e -> close_in_noerr ic
