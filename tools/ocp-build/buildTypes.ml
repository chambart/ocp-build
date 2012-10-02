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

open OcpLang
open BuildEngineTypes
open BuildOCPTree
open BuildOCPTypes

module StringsMap = Map.Make(struct
  type t = string list
  let compare = compare
end)

type mklib_kind =
    MKLIB_Unix
  | MKLIB_Msvc

type module_origin =
    ML | MLI | MLandMLI

and package_info = {
  lib_context : BuildEngineTypes.build_context;
  lib_id : int;
  lib_name : string;
  mutable lib_dirname : File.t;
  mutable lib_provides : string;
  mutable lib_type : package_type;
  mutable lib_tag : string;

  lib_filename : string;

  lib_node : LinearToposort.node;
  mutable lib_missing_deps : int;

  mutable lib_requires : package_info package_dependency list;
  mutable lib_added : bool;
  mutable lib_options : BuildOCPVariable.options;
  mutable lib_installed : bool;


  lib_loc : string * int * string;
  lib_src_dir : build_directory;
  lib_dst_dir : build_directory;
  lib_mut_dir : build_directory;
  lib_modules : (module_origin * string) StringMap.t ref;
  mutable lib_internal_modules :
    (build_directory *
    ((module_origin * string) StringMap.t ref)) StringsMap.t;
  mutable lib_byte_targets : build_file list;
  mutable lib_cmo_objects : build_file list;
  mutable lib_bytecomp_deps : build_file list;
  mutable lib_bytelink_deps : build_file list;
  mutable lib_asm_targets : build_file list;
  mutable lib_asm_cmx_objects : build_file list; (* .cmx *)
  mutable lib_asm_cmxo_objects : build_file list; (* .o *)
  mutable lib_asmcomp_deps : build_file list;
  mutable lib_asmlink_deps : build_file list;
  mutable lib_clink_deps : build_file list;
  mutable lib_dep_deps : build_file IntMap.t;
  mutable lib_includes : string list option;
  mutable lib_sources : (string * BuildOCPVariable.options) list;
}
