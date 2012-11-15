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

(* open BuildRules *)

open BuildEngineTypes

let dependency_loaders = Hashtbl.create 17
let add_dependency_loader name loader =
  Hashtbl.add dependency_loaders name loader
let find_dependency_loader name = Hashtbl.find dependency_loaders name



let new_dir_id b =
  b.build_next_dir_id <- b.build_next_dir_id + 1;
  b.build_next_dir_id

let new_file_id b =
  b.build_next_file_id <- b.build_next_file_id + 1;
  b.build_next_file_id

let new_rule_id b =
  b.build_next_rule_id <- b.build_next_rule_id + 1;
  b.build_next_rule_id


let new_process_id b =
  b.build_next_process_id <- b.build_next_process_id + 1;
  b.build_next_process_id



let file_filename file = File.to_string file.file_file
(*  Printf.eprintf "File dir = %d\n" file.file_dir.dir_id; *)
(*  Filename.concat file.file_dir.dir_fullname file.file_basename *)


