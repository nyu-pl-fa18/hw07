(** {0} Entry point of MiniML interpreter *)

open Util
open Ast
open Eval


let usage_message =
  "MiniML\n" ^
  "\nUsage:\n  " ^ Sys.argv.(0) ^ 
  " <input file> [options]\n"

let verbose = ref false
    
let cmd_options_spec =
  [("-v", Arg.Set verbose, " Display debug messages")]
    
let () =
  try
    let input_file = ref None in
    let set_file s =
      input_file := Some s
    in
    let _ = Arg.parse cmd_options_spec set_file usage_message in
    let prog = Parser.parse_from_file (Opt.get !input_file) in
    let _ =
      find_free_var prog |>
      Opt.map (fun (x, pos) -> fail pos ("Unbound value " ^ x))
    in
    let _ =
      if !verbose then begin
        print_endline "Evaluating:";
        print_term stdout prog;
        print_string "\nResult: ";
      end
    in
    let v = eval prog in
    print_endline (string_of_value v)     
  with Failure str -> print_endline str
