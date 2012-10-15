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

(* TODO:

   Currently, there is no verification that all the dependencies
   appearing in pp_requires also appear in requires. Actually,
   pp_requires could be automatically added.

   We could simplify this by:

   ppflags = [ "" ]

   requires =

   syntax = [ "pa_dyntype" ]

   A syntax MUST either depend on another syntax (requires = [ "toto" ])
   where toto is a syntax, OR set the "pp_master" option. If we depend
   on a package, and this package depends on another syntax, it does
   not matter.



*)

open OcpLang
open OcpSystem


open BuildMisc

open BuildEngineTypes
open BuildEngineGlobals
open BuildEngineContext
open BuildEngineRules
open BuildEngineRules


open BuildOCPVariable
open BuildOCPTree
open BuildOCPTypes

open BuildTypes
open BuildGlobals
open BuildConfig

open BuildOCamlTypes
open BuildOCamlVariables
open BuildOCamlMisc


let execution_dependencies pk kind =
  let pk_name = pk.lib_name in
  try
    match pk.lib_type with
    | ProgramPackage ->
      let exe_ext = if kind = "byte" then byte_exe else asm_exe in
      [find_dst_file pk.lib_dst_dir (pk_name ^ exe_ext)]
    | LibraryPackage ->
      let ext = if kind = "byte" then "cma" else "cmxa" in
      [find_dst_file pk.lib_dst_dir (pk_name ^ "." ^ ext)]
    | ObjectsPackage ->
      if kind = "byte" then
        pk.lib_cmo_objects
      else
        assert false (* TODO *)
  with NoSuchFileInDir (filename, dirname) as e ->
    Printf.fprintf stderr "BuildOCamlRules.find_dst_file: could not find %S in %S\n%!"
      filename dirname;
    raise e

(* This can only work if the package already appears in requires.
For camlp4 and camlp5, we should:
- at the package creation, add all camlp4/camlp5 packages to 'requires',
with the nolink option.

 *)
let add_pp_require lib s =
  let pk_name, kind = OcpString.cut_at s ':' in
  let _exe_ext =
    match kind with
      "asm" -> asm_exe
    | "byte" -> byte_exe
    | "" ->
      Printf.fprintf stderr "Error: package %s\n%!" lib.lib_name;
      Printf.fprintf stderr "Error: you must specify either kind 'asm' or 'byte' for package '%s'\n%!" pk_name;
      exit 2
    | _ ->
      Printf.fprintf stderr "Error: package %s\n%!" lib.lib_name;
      Printf.fprintf stderr "Error: pp_requires option contains unknown kind [%s] for package '%s'\n%!" kind pk_name;
      exit 2
  in
  let pk = try
             StringMap.find pk_name !packages_by_name
    with Not_found ->
      Printf.fprintf stderr "Error: package %s\n%!" lib.lib_name;
      Printf.fprintf stderr "Error: unknown package '%s'\n%!" pk_name;
      exit 2
  in
  let declared = ref false in
  List.iter (fun dep ->
    if dep.dep_project == pk then begin
      if not dep.dep_syntax then begin
        Printf.fprintf stderr "Warning: package %S\n%!" lib.lib_name;
        Printf.fprintf stderr "Warning: pp dependency %S not declared as syntax\n%!" pk_name
      end;

      declared := true
    end
  ) lib.lib_requires;
  if not !declared then begin
    Printf.fprintf stderr "Warning: package %s\n%!" lib.lib_name;
    Printf.fprintf stderr "Warning: pp dependency %S not declared\n%!" pk_name
  end;
  execution_dependencies pk kind

(* TODO: for syntax extensions:
1/ check the "syntax" attribute.
2/ find the corresponding package. Verify that it was provided in the
   "requires" or "syntaxes".
3/ if it is a program, add a dependency to the bytecode version of it,
   set "pp" to be [ "%{program_DST_DIR}%/program.byte" ] + ppflags
4/ if it is a set of libraries, check the libraries to find for which
   tool they are a plugin (one of them should contain an attribute
   "plugin_for"). Then, build the corresponding command and add it
   to "pp".
*)

let add_pp_requires r pp =
  List.iter (fun file -> add_rule_source r file) pp.pp_requires

let get_pp lib options =
(*  Printf.eprintf "get_pp %S\n%!" lib.lib_name; *)
  let syntax = match strings_option options syntax_option with
    |  [] ->
      if bool_option_true options copy_syntaxes then
        List.map (fun dep -> dep.dep_project)
          (List.filter (fun dep -> dep.dep_syntax) lib.lib_requires)
      else
        []
    | syntax ->
      List.map (fun s ->
        try
          StringMap.find s !packages_by_name
        with Not_found -> assert false) syntax
  in

  if syntax = [] then
    let pp_requires = strings_option options pp_requires_option in
    let pp_option = strings_option options pp_option in
    let pp_requires =
      List.map (add_pp_require lib) pp_requires
    in
    {
      pp_flags = [];
      pp_option = pp_option;
      pp_requires = List.flatten pp_requires;
    }

  else
(* Discover the syntaxes that are needed *)
(* Add the dependencies of these syntaxes *)
    let pp_components = ref [] in
    List.iter (fun dep ->
      if dep.dep_syntax && List.memq dep.dep_project syntax then
        pp_components := dep :: (List.rev
                                   (List.filter (fun dep ->
                                     dep.dep_link
                                    ) dep.dep_project.lib_requires))
        @ !pp_components;
    ) lib.lib_requires;
    let pp_components = List.rev !pp_components in
    Printf.eprintf "pp_components for %s:\n%!" lib.lib_name;
    List.iter (fun dep ->
      Printf.eprintf "\t%s\n%!" dep.dep_project.lib_name
    ) pp_components;
(* Find the plugin program to use *)
    let preprocessor = ref [] in
    let plugins = ref [] in
    let plugins_map = ref StringMap.empty in
    List.iter (fun dep ->
      let p = dep.dep_project in
      if not (StringMap.mem p.lib_name !plugins_map) then begin
        match p.lib_type with
        | ProgramPackage ->
          if not (List.memq p !preprocessor) then
            preprocessor := p :: !preprocessor
        | LibraryPackage | ObjectsPackage ->
          if not (StringMap.mem p.lib_name !plugins_map) then begin
            plugins_map := StringMap.add p.lib_name p !plugins_map;
            plugins := p :: !plugins
          end
      end
    ) pp_components;
    Printf.eprintf "syntax for %s:\n%!" lib.lib_name;
    List.iter (fun p ->
      Printf.eprintf "\tpp: %s\n%!" p.lib_name
    ) !preprocessor;
    List.iter (fun p ->
      Printf.eprintf "\tplugin: %s\n%!" p.lib_name
    ) !plugins;

    let pp_flags = ref [] in
    let pp_option = ref [] in
    let pp_requires = ref [] in
    begin
      match !preprocessor with
        [ p ] ->

          if bool_option_true p.lib_options generated_option then
            pp_option := [ p.lib_name ]
          else begin
            pp_requires := [ find_dst_file lib.lib_dst_dir (p.lib_name ^ ".byte") ];
            pp_option := [
              Printf.sprintf "%%{%s_DST_DIR}%%/%s.byte" p.lib_name p.lib_name
            ];
          end;
          (* remove libraries that are already included in the preprocessor *)
          List.iter (fun dep ->
            if dep.dep_link then
              plugins_map := StringMap.remove dep.dep_project.lib_name !plugins_map
          ) p.lib_requires;

          List.iter (fun p ->
            if StringMap.mem p.lib_name !plugins_map && p.lib_sources <> [] then begin
              pp_requires := (execution_dependencies p "byte") @ !pp_requires;
              pp_flags := !pp_flags @
                [ S "-I"; BD p.lib_dst_dir ] @
                (match p.lib_type with
                | ProgramPackage -> assert false
                | ObjectsPackage ->
                  List.map (fun s -> BF s) p.lib_cmo_objects
                | LibraryPackage ->
                  [ S (p.lib_name ^ ".cma") ])
            end
          ) (List.rev !plugins)

      | _ ->
        (* either no preprocessor was provided, or too many of them ! *)
        assert false
    end;
    {
      pp_flags = !pp_flags;
      pp_option = !pp_option;
      pp_requires = !pp_requires;
    }
