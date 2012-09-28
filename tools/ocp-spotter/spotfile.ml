(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Tools                             *)
(*                                                                        *)
(*                             OCamlPro                                   *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  See accompanying files for the terms under      *)
(*  which this file is distributed. In doubt, contact us at               *)
(*  contact@ocamlpro.com (http://www.ocamlpro.com/)                       *)
(**************************************************************************)

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

open Format
open Utils

(* Keep the original modules *)
module Ident0 = Ident

open Spot
open Spoteval

module Load : sig
  exception Old_cmt of string (* cmt *) * string (* source *)
  val load : load_paths:string list -> string -> Unit.t
  val load_module : ?spit:bool -> cwd:string -> load_paths:string list -> string -> Unit.t
end = struct

  let check_time_stamp ~cmt source =
    (* CR jfuruse: aaa.mll creates cmt with aaa.ml as source, but
       aaa.ml often does not exist.
    *)
    let stat_cmt = Unix.stat cmt in
    try
      let stat_source = Unix.stat source in
        (* Needs = : for packed modules, .cmt and the source .cmo are written
           almost at the same moment. *)
      stat_cmt.Unix.st_mtime >= stat_source.Unix.st_mtime
    with
    | Unix.Unix_error(_, "stat", _) ->
        (* CR jfuruse: Camlp4.cmt created from Camlp4.cmx but Camlp4.cmx
           is not installed!!! In such a case, we cannot check the time
           stamp check... (still we can try against Camlp4.cmi installed)
        *)
        eprintf "Warning: source %s does not exist. Time stamp check was skipped.@." source;
        true

  let find_alternative_source ~cmt source =
      (* if [source] is not found, we try finding files with the same basename
         in
         - the directory of [cmt]
         - the directory of [cmt] points to (if [cmt] is symlink)
       *)
    let source_base = Filename.basename source in
    let source_dirs =
        Filename.dirname cmt ::
        begin
          let stat_cmt = Unix.lstat cmt in
          if stat_cmt.Unix.st_kind = Unix.S_LNK then
            [ Filename.dirname (Unix.readlink cmt) ]
          else []
        end
      in
      List.find Sys.file_exists
        (List.map (fun d -> d ^/ source_base) source_dirs)

  let load_cmt_file file = snd (Cmt_format.read file)

  let load_directly path : Unit.t =
    Debug.format "cmt loading from %s@." path;
    match load_cmt_file path with
    | Some cmt ->
        Spot.Unit.of_file (Spot.File.of_cmt path cmt)
    | None -> failwithf "load_directly failed: %s" path

  exception Old_cmt of string (* cmt *) * string (* source *)

  (* CR jfuruse: exception *)
  (* CRv2 jfuruse: add and check cache time stamp *)
  let load_directly_with_cache : string -> Unit.t =
    let cache = Hashtbl.create 17 in
    fun path ->
      try
        Hashtbl.find cache path
      with
      | Not_found ->
          try
            let file = load_directly path in
            if not (check_time_stamp ~cmt:path file.Unit.path) then
              if Spotconfig.strict_time_stamp then
                raise (Old_cmt (path, file.Unit.path))
              else
                eprintf "Warning: source %s is newer than the cmt@." file.Unit.path;
            Hashtbl.replace cache path file;
            file
          with
          | Not_found ->
              failwithf "failed to find cmt file %s" path



  let find_in_path load_paths body ext =
    let body_ext = body ^ ext in
    let find_in_path load_paths name =
      Debug.format "@[<2>searching %s in@ pwd=%s@ paths=[@[%a@]]@]@."
        name
        (Sys.getcwd ())
        (Format.list "; " (fun ppf x -> fprintf ppf "%S" x))
        load_paths;
      try Misc.find_in_path load_paths name with Not_found ->
        Misc.find_in_path_uncap load_paths name
    in
    try find_in_path load_paths body_ext with Not_found ->
    (* We do not give up yet.
       .cmt file is not found,
       but we still find a .cmi which is sym-linked to the original directory with .cmt
    *)
    let cminame = body ^ ".cmi" in
      try
      let cmipath = find_in_path load_paths cminame in
      let stat = Unix.lstat cmipath in
      if stat.Unix.st_kind = Unix.S_LNK then begin
        let cmipath = Filename.dirname cmipath ^/ Unix.readlink cmipath in
        let cmtpath = Filename.chop_extension cmipath ^ ext in
        if Sys.file_exists cmtpath then begin
          Debug.format "Found an alternative %s: %s@." ext cmtpath;
            cmtpath
          end else failwithf "cmt file not found: %s, neither in %s" body_ext cmtpath
        end else raise Not_found
      with
      | (Failure _ as e) -> raise e
      | _ -> failwithf "cmt file not found: %s" body_ext

  let load ~load_paths cmtname : Unit.t =
    let body, ext = Filename.split_extension cmtname in
    let path = try
                 BuildFind.find_cmt_in_obuild_exn cmtname
      with Not_found ->
      find_in_path load_paths body ext in
    load_directly_with_cache path

  (* ocamlbuild tweak *)
  (* for  /.../a/b/c/x.ml
     if   /.../a/_build exists
     then look for
          /.../a/_build/b/c/x.cm*
  *)
  (* seek ocamlbuild _build destination directory *)
  let ocamlbuild_path_tweak cmtname =
    if Filename.is_relative cmtname then None
    else
      let basename = Filename.basename cmtname in
      let dirname = Filename.dirname cmtname in
      let rec loop postfix dir =
        let dir_build = dir ^/ "_build" in
        if Unix.is_dir dir_build then dir_build ^/ postfix
        else
          if dir = "/" then raise Exit
          else loop (Filename.basename dir ^/ postfix) (Filename.dirname dir)
      in
      try
        let cmtname = loop "" dirname ^/ basename in
        Debug.format "Trying ocamlbuild destination %s@." cmtname;
        Some cmtname
      with Exit -> None

  (* .ocamlspot file tweak *)
  let dot_ocamlspot_tweak cmtname =
    if Filename.is_relative cmtname then None
    else
      Option.bind (Dotfile.find_and_load (Filename.dirname cmtname))
        (fun (found_dir, dotfile) ->
          Option.map dotfile.Dotfile.build_dir ~f:(fun build_dir ->
            let length_found_dir = String.length found_dir in
            let found_dir' =
              String.sub cmtname 0 length_found_dir
            in
            let rel_cmtname =
              String.sub cmtname
                (length_found_dir + 1)
                (String.length cmtname - length_found_dir - 1)
            in
            assert (found_dir = found_dir');
            let dir =
              if Filename.is_relative build_dir then found_dir ^/ build_dir
              else build_dir
            in
            let cmtname = dir ^/ rel_cmtname in
            Debug.format "Trying .ocamlspot destination %s@." cmtname;
            cmtname
          ))

  let load ~load_paths cmtname : Unit.t =
    try load ~load_paths cmtname with
    | e ->
        let load_alternative f =
          match f cmtname with
          | None -> None
          | Some cmtname ->
              try Some (load ~load_paths cmtname) with _ -> None
        in
        match load_alternative dot_ocamlspot_tweak with
        | Some v -> v
        | None ->
            match load_alternative ocamlbuild_path_tweak with
            | Some v -> v
            | None -> raise e

  let with_cwd cwd f =
    let d = Sys.getcwd () in
    protect ~f:(fun () -> Sys.chdir cwd; f ()) ()
      ~finally: (fun _ -> Sys.chdir d)

  (* CR jfuruse: searching algorithm must be reconsidered *)
  let load_module ?(spit=false) ~cwd ~load_paths name =
    let cmtname = name ^ if spit then ".cmti" else ".cmt" in
    try
      with_cwd cwd (fun () -> load ~load_paths cmtname)
    with
    | Failure s ->
        let spitname = name ^ if spit then ".cmt" else ".cmti" in
        Format.printf "%s load failed. Try to load %s@."
          cmtname spitname;
        try
          load ~load_paths spitname
        with
        | Failure s' -> failwithf "%s\n%s" s s'
end

include Load

let initial_env file =
  { Env.path = file.Unit.path;
    cwd = file.Unit.builddir;
    load_paths = file.Unit.loadpath;
    binding = Binding.predef }

let invalid_env file =
  { Env.path = file.Unit.path;
    cwd = file.Unit.builddir;
    load_paths = file.Unit.loadpath;
    binding = Binding.invalid }

type result =
    | File_itself
    | Found_at of Region.t
    | Predefined

let find_path_in_flat file path : PIdent.t * result =
  let env =
    let env = invalid_env file in
    let str = Eval.structure env !!(file.Unit.flat) in
    Binding.set env.Env.binding str; (* dirty hack *)
    env
  in
  let find_loc pid =
    match  pid.PIdent.path with
    | "" -> Predefined
    | path ->
        (* CR jfuruse: loading twice... *)
        Debug.format "Finding %a@." PIdent.format pid;
        let file = Load.load ~load_paths:[] (Cmt.of_path path) in
        match pid.PIdent.ident with
        | None -> File_itself (* the whole file *)
        | Some id ->
            Found_at begin try
              Hashtbl.find !!(file.Unit.id_def_regions) id
            with
            | Not_found ->
                eprintf "Error: find location of id %a failed@."
                  PIdent.format pid;
                raise Not_found
            end
  in

  let eval_and_find path =
    (* we need evaluate the path *)
    let v = !!(Eval.find_path env path) in
    Debug.format "Value=%a@." Value.Format.t v;
    match v with
    | Value.Ident id -> id, find_loc id
    | Value.Parameter id -> id, find_loc id
    | Value.Structure (id, _, _)  -> id, find_loc id
    | Value.Closure (id, _, _, _, _) -> id, find_loc id
    | Value.Error (Failure _ as e) -> raise e
    | Value.Error (Load.Old_cmt _ as exn) -> raise exn
    | Value.Error exn -> raise exn
  in
  eval_and_find path

let str_of_global_ident ~cwd ~load_paths id =
  assert (Ident.global id);
  let file = Load.load_module ~spit:Spotconfig.print_interface ~cwd ~load_paths (Ident0.name id) in
  file.Unit.path,
  Eval.structure (initial_env file) file.Unit.top

let _ = Eval.str_of_global_ident := str_of_global_ident

let eval_packed env file =
  let f = Load.load ~load_paths:[""] (Cmt.of_path (env.Env.cwd ^/ file)) in
  Value.Structure ({ PIdent.path = f.Unit.path; ident = None },
                  Eval.structure (initial_env f) f.Unit.top,
                  None (* packed has no .mli *))

let _ = Eval.packed := eval_packed

(*
  let dump_elem = function
    | Source_path (Some s) -> eprintf "Source_path: %s@." s
    | Source_path None -> eprintf "Source_path: None@."
    | Cwd s -> eprintf "Cwd: %s@." s
    | Load_paths ds ->
        eprintf "Load_paths: @[%a@]@."
          (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) ds
    | Argv argv ->
        eprintf "Argv: @[%a@]@."
          (Format.list "; " (fun ppf s -> fprintf ppf "%S" s))
            (Array.to_list argv)
    | Top None -> eprintf "Top None@."
    | Top (Some str) ->
        eprintf "@[<2>Top@ %a@]@."
          format_structure str
    | Annots _ -> eprintf "Annots [...]@."

  let dump_elems elems = List.iter dump_elem elems
*)
