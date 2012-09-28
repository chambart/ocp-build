(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open FixMain

include Debug.Tag(struct let tag = "ocp-fix-errors" end)

let main () =
  Printf.eprintf "ocp-fix-errors started\n%!";
  let error_file = Sys.argv.(1) in
  let line_pos = int_of_string Sys.argv.(2) in
  let lines = FileLines.of_file error_file in
  fix_next_error lines line_pos

let _ =
  try
    main ()
  with e ->
    debug "Fatal error: Exception %s\n%!" (Printexc.to_string e);
    exit 2

