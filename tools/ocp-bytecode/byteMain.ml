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

(* "Expunge" a toplevel by removing compiler modules from the global List.map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Sys
open Misc
open ByteArgs
open ByteActions

let loaded_prims = ref false
let arg_usage =
  Printf.sprintf "%s FILENAME [options] [-o FILENAME2] : hack a bytecode program" Sys.argv.(0)

let arg_list = Arg.align [

  "-dump", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.dump();
  ), " : dump information on files on stdout";

  "-disass", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.disass ();
  ), " : dissassemble files on stdout";

  "-expunge", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.set_action ByteActions.expunge;
    must_save := true),
 " MODULES : action is to expunge modules";

  "-N", Arg.Clear must_save, " : don't save file after modifications";

  "-remove-prims", Arg.String (fun filename ->
    ByteActions.flush_action ();
    ByteActions.remove_primitives filename;
    must_save := true
  ), " PRIM_FILENAME : remove primitives from PRIM_FILENAME";

  "-filter-unused-prims", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.filter_unused_primitives ();
    must_save := true
  ), " : filter unused primitives";

  "-set-prims", Arg.String (fun filename ->
    ByteActions.flush_action ();
    ByteActions.set_primitives filename;
    must_save := true
  ), " PRIM_FILENAME : set primitives from PRIM_FILENAME";

  "-save-prims", Arg.String (fun filename ->
    ByteActions.flush_action ();
    ByteActions.save_primitives filename
  ), " PRIM_FILENAME : save primitives in PRIM_FILENAME";

  "-print-prims", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.print_primitives ()
  ), " : print primitives";

  "-make-static", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.make_static ();
    must_save := true
  ), " : action is to remove dynamic dependencies from program";

  "-list-sections", Arg.Unit (fun () ->
    ByteActions.flush_action ();
    ByteActions.list_sections ()
    ), " : action is to list sections of program";

  "-o", Arg.String (fun name ->
    let (filename, t) = ByteActions.get_file () in
    must_save := true;
    current_file := Some (name, t)
  ),
  " <target> : specify target file";

  "-help-sections", Arg.Unit (fun _ ->
    Printf.fprintf stdout "%s%!"
      (String.concat "\n"
	 [
	   "Bytecode sections are:";
	   "  RNTM: the executable code of the runtime";
	   "  CODE: the bytecode";
	   "  DLPT: path to dynamic libraries";
	   "  DLLS: dynamic libraries";
	   "  PRIM: primitive table (output_value: )";
	   "  DATA: global table (output_value: )";
	   "  SYMB: symbol table (output_value: )";
	   "  CRCS: checksums (output_value: )";
	   "  DBUG: debug info (output_value: )";
	   "";
	 ]);
    exit 0
  ), " : print help on bytecode sections";
]

let arg_anon filename =
  match !current_file with
      None ->
        current_file := Some (filename, ByteFile.load filename);
        current_action := None
    | Some _ ->
      match !current_action with
          None ->
            Printf.fprintf stderr "Error: don't know what to do with argument %S\n%!"
              filename;
            Arg.usage arg_list arg_usage
        | Some (set, _) ->
          set := !set @ [filename]

let main () =
  Arg.parse arg_list arg_anon arg_usage;
  ByteActions.flush_action ();
  if !must_save then
    match !current_file with
        None ->
          Printf.fprintf stderr "Error: no file specified\n%!";
          Arg.usage arg_list arg_usage
      | Some (filename, t) ->
        Printf.fprintf stderr "Saving modified file to %s\n%!" filename;
        ByteFile.save filename t

let _ =
  try
    main ()
  with ByteFile.Error (filename, error) ->
    Printf.fprintf stderr "File %s: error %s\n%!" filename
      (ByteFile.string_of_error error);
    exit 2
