

open BuildOCPVariable


(* is this file an implementation source ? *)
let ml_file_option = new_bool_option "ml" false

(* is this file an interface source ? *)
let mli_file_option = new_bool_option "mli" false

(* flags to use when compiling C code *)
let cflags_option = new_strings_option "cflags" ([] : string list)

(* same as cflags *)
let ccopt_option = new_strings_option "ccopt" ([] : string list)

(* should we remove implicit dependencies towards Pervasives *)
let nopervasives = BuildOCPVariable.new_bool_option "nopervasives" false

(* dependencies inferred by ocamldep that should be removed (cyclic) *)
let nodeps_option = BuildOCPVariable.new_strings_option "nodeps" []

(* cmx dependencies inferred by ocamldep that should be removed (cyclic) *)
let nocmxdeps_option = BuildOCPVariable.new_strings_option "noimpldeps" []


let bytelink_option = new_strings_option "bytelink" ([] : string list)
let bytecomp_option = new_strings_option "bytecomp" ([] : string list)
let asmcomp_option = new_strings_option "asmcomp" ([] : string list)
let asmlink_option = new_strings_option "asmlink" ([] : string list)
let dep_option = new_strings_option "dep" ([] : string list)


let rule_sources_option = new_strings_option "rule_sources" []

(* dependencies before preprocessing *)
let pp_requires_option = new_strings_option "pp_requires" []

(* which preprocessor command to use *)
let pp_option = new_strings_option "pp" []

(* should files be sorted before linking ? *)
let sort_files_option = new_bool_option "sort" false

(* used to implement the pack syntax *)
let pack_option = new_strings_option "pack" ([] : string list)
let packed_option = new_strings_option "packed" ([] : string list)

(* should we compile in bytecode *)
let byte_option = new_bool_option "has_byte" true
(* should we compile in native code *)
let asm_option = new_bool_option "has_asm" true

(* do not use the provided interface file *)
let no_mli_option = new_bool_option "no_mli" false


(* for dependencies *)

(* not implemented *)
let syntax_option = new_strings_option "syntax" ([] : string list)
let plugin_of_option = new_strings_option "plugin_of" ([] : string list)
let ppflags_option = new_strings_option "ppflags" ([] : string list)
let pplink_option = new_strings_option "pplink" ([] : string list)

let package_option = new_strings_option "package" ([] : string list)

(*
let install_interface_option = new_bool_option "install_cmi" true
*)
let dirname_option = new_strings_option "dirname" ([] : string list)
let subdir_option = new_strings_option "subdir" ([] : string list)
let cclib_option = new_strings_option "cclib" ([] : string list)

