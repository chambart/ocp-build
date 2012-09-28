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

module INTERFACE = Bincompat.MakeReader(struct
  type t = string * Parsetree.signature
  let versions =
  [

    V4000_types.ast_intf_magic_number, ("4.00", V4000_input.input_ast_intf);
    V3120_types.ast_intf_magic_number, ("3.12", V3120_input.input_ast_intf);
  ]
  let magic_number = Config.ast_intf_magic_number
  let error_bad_magic filename =
    failwith (Printf.sprintf "Bad AST intf magic %s" filename)
  let error_corrupted_file filename version =
    failwith (Printf.sprintf "Corrupted AST intf %s for version %s" filename version)

end)

(*
  let ast_magic_number_len = String.length Config.ast_intf_magic_number
  let ast_magic_prefix = "Caml1999N"

  let input_interface ic =
    let magic =
      String.create ast_magic_number_len in
    let pp =
      try
        let version, input =
          really_input ic magic 0 ast_magic_number_len;
          List.assoc magic versions
      in
      Some input

        raise Outdated_version
      | _ -> None
    in
    match pp with
      | Some input -> Some (input  ic)
      | None -> None

  let read_implementation ic = None

end
*)


module IMPLEMENTATION = Bincompat.MakeReader(struct
  type t = string * Parsetree.structure
  let versions =
  [
    V4000_types.ast_impl_magic_number, ("4.00", V4000_input.input_ast_impl);
    V3120_types.ast_impl_magic_number, ("3.12", V3120_input.input_ast_impl);
  ]

  let magic_number = Config.ast_impl_magic_number
  let error_bad_magic filename =
    failwith (Printf.sprintf "Bad AST impl magic %s" filename)
  let error_corrupted_file filename version =
    failwith (Printf.sprintf "Corrupted AST impl %s for version %s" filename version)

(*
  let ast_magic_number_len = String.length
  let ast_magic_prefix = "Caml1999M"

  let read_implementation ic =
    let magic = String.create ast_magic_number_len in
    let pp =
    try
      let version, input =
          really_input ic magic 0 ast_magic_number_len; (* the min of the two *)
          List.assoc (String.sub magic 0 ast_magic_number_len) versions
      in
      Some input
    with
      | Not_found when
          String.sub magic 0 (String.length ast_magic_prefix) =
                    ast_magic_prefix ->
        raise Outdated_version
      | _ -> None
    in
    match pp with
      | Some input -> Some (input  ic)
      | None -> None
*)

end)

let input_interface = INTERFACE.input
let input_implementation = IMPLEMENTATION.input









