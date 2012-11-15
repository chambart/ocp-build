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

type options = option_value StringMap.t

and option_value =
    OptionBool of bool
  | OptionList of string list

let new_options = StringMap.empty

let default_options = new_options

type 'a source_option = {
  option_name : string;
  mutable option_default : 'a;
}

let options = ref []

let new_bool_option name default =
  { option_name = name; option_default = default }

let new_initial_bool_option name default =
  let o = new_bool_option name default in
  options := (fun vars -> StringMap.add name (OptionBool o.option_default) vars)
      :: !options;
  o

let new_strings_option name (default : string list) =
  { option_name = name; option_default = default }

let new_initial_strings_option name default =
  let o = new_strings_option name default in
  options := (fun vars ->
(*    Printf.fprintf stderr "Setting as default %s = %S\n%!" name
      (String.concat ";" o.option_default); *)
    StringMap.add name (OptionList o.option_default) vars)
      :: !options;
  o

let options_find option options =
(*  try *)
    StringMap.find option.option_name options
(*  with Not_found as e ->
    match options.options_inherit with
	None -> raise e
      | Some options ->
	StringMap.find option.option_name options.options_vars
*)

let bool_option_true options bool_option =
  try
    match options_find bool_option options with
	OptionBool bool -> bool
      | _ ->
	Printf.eprintf "Warning: bad type for bool option %s. Returning default value %b\n%!" bool_option.option_name bool_option.option_default;
	bool_option.option_default
  with Not_found -> bool_option.option_default

let set_strings_option strings_option default =
  strings_option.option_default <- default

let strings_option options strings_option =
  try
    match options_find strings_option options with
	OptionList list -> list
      | _ ->
	Printf.eprintf "Warning: bad type for string list option %s. Returning default value [%s]\n%!" strings_option.option_name
	  (String.concat ";" strings_option.option_default);
	strings_option.option_default
  with Not_found -> strings_option.option_default

let direct_strings_option options strings_option =
  try
    match StringMap.find strings_option.option_name options with
	OptionList list -> list
      | _ ->
	Printf.eprintf "Warning: bad type for string list option %s. Returning default value [%s]\n%!" strings_option.option_name
	  (String.concat ";" strings_option.option_default);
	strings_option.option_default
  with Not_found -> strings_option.option_default

let string_option options string_option =  String.concat " " (strings_option options string_option)

let get_strings_option strings_option =
  String.concat " " strings_option.option_default




let enabled_option = new_bool_option "enabled" true
let generated_option = new_bool_option "generated" false
let requires_keep_order_option = new_bool_option "requires_keep_order" false
