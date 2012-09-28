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

open StringSubst

let env_subst = empty_subst ()

let add_to_subst env v vv =
(*  Printf.eprintf "BuildSubst.add %S -> %S\n%!" v vv; *)
  add_to_subst env v vv

let add_to_subst var var_value =
  add_to_subst env_subst (Printf.sprintf "%%{%s}%%" var) var_value

let _ =
  Array.iter (fun s ->
    let var, var_value = OcpString.cut_at s '=' in
    add_to_subst var var_value;
  ) (Unix.environment ())

let putenv var var_value =
  Unix.putenv var var_value;
  add_to_subst var var_value

let subst s =
  let ss = snd (iter_subst env_subst s) in
(*  Printf.eprintf "BuildSubst.subst %S -> %S\n%!" s ss; *)
  ss


