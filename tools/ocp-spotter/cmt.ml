(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)
open Utils

open Cmt_format

let source_path file =
  Option.map file.cmt_sourcefile ~f:(fun f -> file.cmt_builddir ^/ f)

(* xxx.{ml,cmo,cmx,spot} => xxx.cmt
   xxx.{mli,cmi,spit}    => xxx.cmti *)
let of_path path =
  let dirname, filename =
    try
      let slash = String.rindex path '/' in
      Some (String.sub path 0 slash),
      String.sub path (slash + 1) (String.length path - slash - 1)
    with
    | Not_found -> None, path
  in
  let filename =
    match Filename.split_extension filename with
    | body, (".cmi" | ".mli" | ".cmti" | ".spit") -> body ^ ".cmti"
    | body, _ -> body ^ ".cmt"
  in
  match dirname with
  | None -> filename
  | Some d -> d ^/ filename

(* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)
let is_opt cmt =
  (* We cannot guess this simply by the compiler name "ocamlc" or "ocamlopt",
     since someone can create a modified compiler like gcaml *)
  List.exists (fun x -> match Filename.split_extension x with
    | (_, ".cmx") -> true
    | _ -> false) (Array.to_list cmt.cmt_args)

let recover_env env =

  let module Envaux = struct (* copied from debugger/envaux.ml *)
    open Misc
    open Types
    open Env

    type error =
        Module_not_found of Path.t

    exception Error of error

    let env_cache =
      (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

    let reset_cache () =
      Hashtbl.clear env_cache;
      Env.reset_cache()

    let extract_sig env mty =
      match Mtype.scrape env mty with
        Mty_signature sg -> sg
      | _ -> fatal_error "Envaux.extract_sig"

    let rec env_from_summary sum subst =
    try
      Hashtbl.find env_cache (sum, subst)
    with Not_found ->
      let env =
        match sum with
          Env_empty ->
            Env.empty
        | Env_value(s, id, desc) ->
            Env.add_value id (Subst.value_description subst desc) (env_from_summary s subst)
        | Env_type(s, id, desc) ->
            Env.add_type id (Subst.type_declaration subst desc) (env_from_summary s subst)
        | Env_exception(s, id, desc) ->
            Env.add_exception id (Subst.exception_declaration subst desc) (env_from_summary s subst)
        | Env_module(s, id, desc) ->
            Env.add_module id (Subst.modtype subst desc) (env_from_summary s subst)
        | Env_modtype(s, id, desc) ->
            Env.add_modtype id (Subst.modtype_declaration subst desc) (env_from_summary s subst)
        | Env_class(s, id, desc) ->
            Env.add_class id (Subst.class_declaration subst desc) (env_from_summary s subst)
        | Env_cltype (s, id, desc) ->
            Env.add_cltype id (Subst.cltype_declaration subst desc) (env_from_summary s subst)
        | Env_open(s, path) ->
            let env = env_from_summary s subst in
            let path' = Subst.module_path subst path in
            let mty =
              try
                Env.find_module path' env
              with Not_found ->
                raise (Error (Module_not_found path'))
            in
            Env.open_signature path' (extract_sig env mty) env
      in
        Hashtbl.add env_cache (sum, subst) env;
        env
  end in
  Envaux.reset_cache ();
  Envaux.env_from_summary (Env.summary env) Subst.identity
