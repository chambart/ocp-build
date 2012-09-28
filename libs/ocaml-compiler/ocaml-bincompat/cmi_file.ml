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

open Cmi_format


let cmi_magic_number_001 = "Caml1999I001" (* csl 1.06 - csl 1.07 *)
let cmi_magic_number_002 = "Caml1999I002" (* csl 1.10 - csl 1.15 - 1.00 - 1.05 *)
let cmi_magic_number_003 = "Caml1999I003" (* 1.06 - 1.07 *)
let cmi_magic_number_004 = "Caml1999I004" (* 2.00 - 2.04 *)
let cmi_magic_number_005 = "Caml1999I005" (* 2.99 *)
let cmi_magic_number_006 = "Caml1999I006" (* 3.00 *)
let cmi_magic_number_007 = "Caml1999I007" (* 3.01 *)
let cmi_magic_number_008 = "Caml1999I008" (* 3.02 - 3.05 *)
let cmi_magic_number_009 = "Caml1999I009" (* 3.06 - 3.07 *)
let cmi_magic_number_010 = "Caml1999I010" (* 3.08 - 3.10.2 *)
let cmi_magic_number_011 = "Caml1999I011" (* 3.11.0 - 3.11.2 *)
let cmi_magic_number_012 = "Caml1999I012" (* 3.12.0 - 3.12.1 *)



let versions = [
  V4000_types.cmi_magic_number, ("4.00", V4000_input.input_cmi);
  V3120_types.cmi_magic_number, ("3.12", V3120_input.input_cmi);
  V3112_types.cmi_magic_number, ("3.11", V3112_input.input_cmi);
]

module Reader = Bincompat.MakeReader(struct

  type t = Cmi_format.cmi_infos

  let versions = versions
  let magic_number = Config.cmi_magic_number
  let error_bad_magic filename =
      raise (Cmi_format.Error(Cmi_format.Not_an_interface filename))
  let error_corrupted_file filename version =
    let msg = Printf.sprintf "%s(%s)" filename version in
    raise(Cmi_format.Error(Cmi_format.Corrupted_interface msg))

end)

let input = Reader.input
let read = Reader.read

(*
let magic_length = String.length Config.cmi_magic_number

let input_cmi filename ic magic_number =
  let version, inputer =
    try
      List.assoc magic_number versions
    with Not_found ->
  in
  try
    inputer ic
  with e ->

let read_cmi filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create magic_length in
    really_input ic buffer 0 magic_length;
    let cmi = input_cmi filename ic buffer in
    close_in ic;
    cmi
  with e ->
    close_in ic;
    raise
      (match e with Error _ -> e | _ ->
        Cmi_format.Error(Cmi_format.Corrupted_interface(filename)))
*)

let read_module modname filename =
  let cmi = read_cmi filename in
  if cmi.cmi_name <> modname then
    raise(Env.Error(Env.Illegal_renaming(cmi.cmi_name, filename)));
  cmi
