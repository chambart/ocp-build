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

(*
  - replace the type in the interface when mismatch with the implementation (Error)

We should be able to deal with that:
Command stderr:
File "./tools/ocp-build/src/buildConfig.ml", line 1, characters 0-1:
Error: The implementation ./tools/ocp-build/src/buildConfig.ml
       does not match the interface ./_obuild/ocp-build-lib/buildConfig.cmi:
       The field `config_file_basename' is required but not provided
       The field `global_config_file' is required but not provided
       The field `global_config_dir' is required but not provided
       The field `ocpdir' is required but not provided


*)

open Asttypes
open Parsetree
open ErrorLocation
include Debug.Tag(struct let tag = "fixInterface" end)


let type_declarations_do_not_match =  "Type declarations do not match:"
let values_do_not_match =  "Values do not match:"

let string_iter_lines f s =
  let len = String.length s in
  let rec iter pos =
    if pos < len then
    match
      try
        let endline = String.index_from s pos '\n' in
        begin
          let endline =
            if endline = pos then endline else
              if s.[endline-1] = '\r' then endline-1 else endline
          in
          f (String.sub s pos (endline - pos))
        end;
        Some endline
      with Not_found ->
        f (String.sub s pos (len-pos));
        None
    with
        None -> ()
      | Some endline -> iter (endline+1)
  in
  iter 0

module ForValue = struct

  let is_not_included_in =  "is not included in"

  let fix loc before_begin_pos message (* error_line next_lines *) =
(*
  let fix loc error_line next_lines =
    let indented_lines = FixUtils.find_indented [error_line] next_lines in
    let message = String.concat " " indented_lines in
    debug "mismatch: [%s]\n%!" message;
    let before_begin_pos = OcpString.find values_do_not_match message in
*)
    let before_end_pos = before_begin_pos + String.length values_do_not_match in
    let middle_begin_pos = OcpString.find_from is_not_included_in message before_end_pos in
    let middle_end_pos = middle_begin_pos + String.length is_not_included_in in
    let should_be = String.sub message before_end_pos (middle_begin_pos - before_end_pos) in
    let should_be = OcpString.unspace should_be in
    debug "should_be = [%s]\n%!" should_be;
    let instead_of = String.sub message middle_end_pos (String.length message - middle_end_pos) in
    debug "instead_of = [%s]\n%!" instead_of;
    let b = Buffer.create 100 in
    begin try
            string_iter_lines (fun s ->
              debug "Testring [%s]\n%!" s;
              if s = "" || OcpString.starts_with s "        " then begin
                debug "Adding [%s]\n%!" s;
                Buffer.add_string b s; Buffer.add_char b '\n';
              end else raise Exit
            ) instead_of
      with Exit -> ()
    end;
    let instead_of = Buffer.contents b in
    debug "instead_of cleaned = [%s]\n%!" instead_of;

    let intf_file = File.add_suffix loc.loc_file.file_file "i" in (* ml ^ i = mli *)
    let intf_file = ErrorLocation.find_file (File.to_string intf_file) in
    let interface = Parse.interface (Lexing.from_string intf_file.file_content) in

    let instead_of_ps = Parse.interface (Lexing.from_string instead_of) in
    let _should_be_ps = Parse.interface (Lexing.from_string should_be) in

    let ident = match instead_of_ps with
        [ { psig_desc = Psig_value (id, _) } ] -> id.txt
      | _ -> assert false
    in

    let found = ref [] in

    let rec sig_items sg =
      match sg with
          [] -> ()
        | item :: sg ->
          sig_item item;
          sig_items sg

    and sig_item item =
      match item.psig_desc with
        | Psig_value (id, _) when id.txt = ident ->
          found := item.psig_loc :: !found
        | Psig_value (id, _) -> ()
        | Psig_recmodule  list ->
          List.iter (fun (_, mty) -> module_type mty) list
        | Psig_module (  _, mty ) -> module_type mty
        | Psig_modtype (  _, Pmodtype_manifest mty ) -> module_type mty
        | Psig_modtype _
        | Psig_class_type  _
        | Psig_class  _
        | Psig_include  _
        | Psig_open  _
        | Psig_exception _
        | Psig_type  _
          -> ()

    and module_type mty =
      match mty.pmty_desc with
        | Pmty_signature  sg -> sig_items sg
        | Pmty_functor ( _ , mty1 ,  mty2 ) -> module_type mty1; module_type mty2
        | Pmty_ident _ -> ()
        | Pmty_typeof  _ -> ()
        | Pmty_with ( mty ,  _ ) -> module_type mty
    in
    sig_items interface;
    begin
      match !found with
          [ lloc ] ->
            debug "FOUND ONE OCCURRNECE !\n%!";
            let loc_start = lloc.Location.loc_start in
            let loc_end = lloc.Location.loc_end in
            assert (loc_start.Lexing.pos_fname = loc_end.Lexing.pos_fname);
            let begin_pos = loc_start.Lexing.pos_cnum in
            let end_pos = loc_end.Lexing.pos_cnum in
            let s = String.sub intf_file.file_content begin_pos
              (end_pos - begin_pos) in
            debug "OCC = [%s]\n%!" s;

            [intf_file, begin_pos, end_pos, should_be],
            Printf.sprintf "Fixed value %s in %s"
              ident (File.basename intf_file.file_file)
        | [] -> failwith "matching declaration not found"
        | _ -> failwith "multiple matching declarations found"
    end

end

module ForType = struct

  let is_not_included_in =  "is not included in"

  let fix loc before_begin_pos message (* error_line next_lines *) =
(*
    let indented_lines = FixUtils.find_indented [error_line] next_lines in
    let message = String.concat " " indented_lines in
    debug "mismatch: [%s]\n%!" message;
    let before_begin_pos = OcpString.find type_declarations_do_not_match message in
*)
    let before_end_pos = before_begin_pos + String.length type_declarations_do_not_match in
    let middle_begin_pos = OcpString.find_from is_not_included_in message before_end_pos in
    let middle_end_pos = middle_begin_pos + String.length is_not_included_in in
    let should_be = String.sub message before_end_pos (middle_begin_pos - before_end_pos) in
    let should_be = OcpString.unspace should_be in
    debug "should_be = [%s]\n%!" should_be;
    let instead_of = String.sub message middle_end_pos (String.length message - middle_end_pos) in
    debug "instead_of = [%s]\n%!" instead_of;
    let b = Buffer.create 100 in
    begin try
            string_iter_lines (fun s ->
              debug "Testring [%s]\n%!" s;
              if s = "" || OcpString.starts_with s "        " then begin
                debug "Adding [%s]\n%!" s;
                Buffer.add_string b s; Buffer.add_char b '\n';
              end else raise Exit
            ) instead_of
      with Exit -> ()
    end;
    let instead_of = Buffer.contents b in
    debug "instead_of cleaned = [%s]\n%!" instead_of;

    let intf_file = File.add_suffix loc.loc_file.file_file "i" in (* ml ^ i = mli *)
    let intf_file = ErrorLocation.find_file (File.to_string intf_file) in
    let interface = Parse.interface (Lexing.from_string intf_file.file_content) in

    let instead_of_ps = Parse.interface (Lexing.from_string instead_of) in
    let _should_be_ps = Parse.interface (Lexing.from_string should_be) in

    let ident = match instead_of_ps with
        [ { psig_desc = Psig_type [id, _] } ] -> id.Location.txt
      | _ -> assert false
    in

    let found = ref [] in

    let rec sig_items sg =
      match sg with
          [] -> ()
        | item :: sg ->
          sig_item item;
          sig_items sg

    and sig_item item =
      match item.psig_desc with
        | Psig_type [ id, _ ]
            when ident = id.Location.txt ->
              found := item.psig_loc :: !found
        | Psig_value _ -> ()
        | Psig_recmodule  list ->
          List.iter (fun (_, mty) -> module_type mty) list
        | Psig_module ( _ ,  mty ) -> module_type mty
        | Psig_modtype ( _ ,  Pmodtype_manifest mty ) -> module_type mty
        | Psig_modtype _
        | Psig_class_type  _
        | Psig_class  _
        | Psig_include  _
        | Psig_open  _
        | Psig_exception _
        | Psig_type _  -> ()

    and module_type mty =
      match mty.pmty_desc with
        | Pmty_signature  sg -> sig_items sg
        | Pmty_functor ( _ , mty1 ,  mty2 ) -> module_type mty1; module_type mty2
        | Pmty_ident _ -> ()
        | Pmty_typeof  _ -> ()
        | Pmty_with ( mty ,  _ ) -> module_type mty
    in
    sig_items interface;
    begin
      match !found with
          [ lloc ] ->
            debug "FOUND ONE OCCURRNECE !\n%!";
            let loc_start = lloc.Location.loc_start in
            let loc_end = lloc.Location.loc_end in
            assert (loc_start.Lexing.pos_fname = loc_end.Lexing.pos_fname);
            let begin_pos = loc_start.Lexing.pos_cnum in
            let end_pos = loc_end.Lexing.pos_cnum in
            let s = String.sub intf_file.file_content begin_pos
              (end_pos - begin_pos) in
            debug "OCC = [%s]\n%!" s;

            [intf_file, begin_pos, end_pos, should_be],
            Printf.sprintf "Fixed type %s in %s"
              ident (File.basename intf_file.file_file)
        | [] -> failwith "matching declaration not found"
        | _ -> failwith "multiple matching declarations found"
    end
end

let fix loc error_line next_lines =
  let indented_lines = FixUtils.find_indented [error_line] next_lines in
  let message = String.concat "\n" indented_lines in
  debug "mismatch: [%s]\n%!" message;
  match try
          Some (OcpString.find type_declarations_do_not_match message)
    with Not_found -> None
  with
      Some before_begin_pos -> ForType.fix loc before_begin_pos message
    | None ->
      match try
              Some (OcpString.find values_do_not_match message)
        with Not_found -> None
      with
          Some before_begin_pos -> ForValue.fix loc before_begin_pos message
        | None ->
            failwith "interface missmatch is only supported for types and values"
