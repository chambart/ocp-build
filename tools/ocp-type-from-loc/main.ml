open Format

(* default values *)
let pp = ref false
let retype = ref false
let filename = ref ""

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-pp] [-t]"

let speclist = [
  ("-pp", Arg.String   (fun s -> pp := true; filename := s), ": Print Typedtree");
  ("-t", Arg.String (fun s -> retype := true; filename := s), ": Re-type");
  ]

let help () = Printf.printf "%s\n" usage

let () =
  (* Read the arguments *)
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;

  if !pp then
    Printer.print std_formatter !filename;

  (* Tests ! *)
  let lnum = 2 in
  let start =
    {Lexing.pos_cnum = 1;
     Lexing.pos_bol = 0;
     Lexing.pos_lnum = lnum;
     Lexing.pos_fname = !filename} in
  let end_ =
    {Lexing.pos_cnum = 80;
     Lexing.pos_bol = 0;
     Lexing.pos_lnum = lnum;
     Lexing.pos_fname = !filename} in
  let testloc =
    {Location.loc_start = start ;
     Location.loc_end = end_;
     Location.loc_ghost = false} in
  if !retype then
    Retype.print_ty_from_cmt testloc !filename

