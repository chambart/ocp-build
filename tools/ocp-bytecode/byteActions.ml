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

(* "Expunge" a toplevel by removing compiler modules from the global List.map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Sys
open Misc
open ByteArgs
open ByteMisc

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

let flush_action () =
  match !current_action with
      None -> ()
    | Some (set, action) ->
      current_action := None;
      action !set

let set_action action =
  flush_action ();
  current_action := Some (ref [], action)

let get_file () =
  match !current_file with
      None ->
        Printf.fprintf stderr "Error: first argument must be the name of the file to load\n%!";
        exit 2
    | Some (filename, t) -> (filename, t)

(* from bytecomp/symtable *)
let filter_global_map p (n,s) =
  let newtbl = ref Tbl.empty in
  Tbl.iter
    (fun id num -> if p id then newtbl := Tbl.add id num !newtbl)
    s;
  (n, !newtbl)

let expunge modules =
  let to_keep = ref StringSet.empty in

  let expunge_map tbl =
    filter_global_map
      (fun id -> StringSet.mem (Ident.name id) !to_keep)
      tbl
  in
  let expunge_crcs tbl =
    List.filter (fun (unit, crc) -> StringSet.mem unit !to_keep) tbl
  in
  Array.iter
    (fun exn -> to_keep := StringSet.add exn !to_keep)
    Runtimedef.builtin_exceptions;
  List.iter (fun name ->
    to_keep := StringSet.add (String.capitalize name) !to_keep
  ) modules;

  let (filename, t) = get_file () in

  let (nsymbols, symbols) = expunge_map (t.ByteFile.nsymbols, t.ByteFile.symbols) in
  let imports = expunge_crcs t.ByteFile.imports in

  t.ByteFile.imports <- imports;
  t.ByteFile.nsymbols <- nsymbols;
  t.ByteFile.symbols <- symbols

let load_primitives input_name =
  let prims = ref [] in
  let ic = open_in input_name in
  try
    while true do
      let line = input_line ic in
      prims := line :: !prims
    done;
    assert false
  with End_of_file ->
    close_in ic;
    Array.of_list (List.rev !prims)

(*

let print_primitives () =
  StringSet.iter (fun s ->
    Printf.fprintf stderr "%s\n" s
  ) !used_prims;
  Printf.fprintf stderr "%!"

let primitive_table = ref [||]

let keep_primitives input_name =
  let t = ByteFile.load input_name in
  let filter_prims prims =
    Array.map (fun prim ->
      if StringSet.mem prim !used_prims then
        prim
      else begin
        Printf.fprintf stderr "Removing primitive %s\n" prim;
        "caml_sys_exit"
      end
    ) prims
  in
  let new_t = { t with ByteFile.primitives = filter_prims t.ByteFile.primitives } in
  ByteFile.save !target_arg new_t;
  exit 0

let load_used_primitives filename =
  let t = ByteFile.load filename in
  let module IterUsedPrimitives = ByteCode.IterPrimitives(struct
    let unit (_,prim) = use_prim t.ByteFile.primitives.(prim) end) in
  ByteCode.iter (fun _  opcode -> IterUsedPrimitives.unit opcode) t;
  ()

let load_primitives_table filename =
  let t = ByteFile.load filename in
  Array.iter use_prim t.ByteFile.primitives

*)


let remove_primitives prim_filename =
  let (filename,t) = get_file () in
  let primitives = load_primitives prim_filename in
  let removed_prims = ref StringSet.empty in
  let remove_prim s =
    if not (StringSet.mem s !removed_prims) then
      removed_prims := StringSet.add s !removed_prims
  in
  Array.iter remove_prim primitives;

  let filter_prims prims =
    Array.map (fun prim ->
      if not ( StringSet.mem prim !removed_prims ) then
        prim
      else begin
        Printf.fprintf stderr "Removing primitive %s\n" prim;
        "caml_sys_exit"
      end
    ) prims
  in
  t.ByteFile.primitives <- filter_prims t.ByteFile.primitives


let filter_unused_primitives () =
  let (filename,t) = get_file () in
  let used_prims = ref StringSet.empty in
  let use_prim s =
    if not (StringSet.mem s !used_prims) then
      used_prims := StringSet.add s !used_prims
  in

  let module IterUsedPrimitives = ByteCode.IterPrimitives(struct
    let unit (_,prim) = use_prim t.ByteFile.primitives.(prim) end) in
  ByteCode.iter (fun _  opcode -> IterUsedPrimitives.unit opcode) t;

  let filter_prims prims =
    Array.map (fun prim ->
      if StringSet.mem prim !used_prims then
        prim
      else begin
        Printf.fprintf stderr "Removing primitive %s\n" prim;
        "caml_sys_exit"
      end
    ) prims
  in
  t.ByteFile.primitives <- filter_prims t.ByteFile.primitives



let set_primitive_table primitive_table =
  let (filename,t) = get_file () in
  let prim_map = ref StringMap.empty in
  Array.iteri (fun i prim ->
    prim_map := StringMap.add prim i !prim_map
  ) primitive_table;

  let module IterPrimitives = ByteCode.IterPrimitives(struct
    let unit (pos,prim) =
      let prim = t.ByteFile.primitives.(prim) in
      let new_pos = StringMap.find prim !prim_map in
      LittleEndian.str_uint t.ByteFile.code pos new_pos
  end) in
  ByteCode.iter (fun _  opcode -> IterPrimitives.unit opcode) t;
  t.ByteFile.primitives <- primitive_table

(*

let strip_sections input_name =
  Printf.fprintf stderr "Warning: beware that this will only work if the CODE section is not\n";
  Printf.fprintf stderr "  moved by removing other sections\n";
  let raw = ByteFile.RAW.load input_name in
  let rec filter_sections list =
    match list with
        [] -> []
      | (name, content) :: tail
          when List.mem name !anon_args ->
        Printf.fprintf stderr "Removing section %s of size %d\n%!" name (String.length content);
            filter_sections tail
      | head :: tail -> head :: (filter_sections tail)
  in
  let new_raw = { raw with ByteFile.RAW.sections = filter_sections raw.ByteFile.RAW.sections } in
  Printf.fprintf stderr "Saving to file %s\n%!" !target_arg;
  ByteFile.RAW.save !target_arg new_raw;
  exit 0
*)

let set_primitives prim_filename =
  set_primitive_table (load_primitives prim_filename)

let save_primitives output_filename =
  let (filename, t) = get_file () in
  let oc = open_out output_filename in
  Array.iter (fun prim -> Printf.fprintf oc "%s\n" prim) t.ByteFile.primitives;
  close_out oc

let print_primitives () =
  let (filename, t) = get_file () in
  Array.iter (fun prim -> Printf.printf "%s\n" prim) t.ByteFile.primitives;
  Printf.printf "%!"

let make_static () =
  let (filename, t) = get_file () in
  t.ByteFile.dll_path <- [];
  t.ByteFile.dll_names <- [];
  current_file := Some (filename, t)

let list_sections () =
  let (filename, t) = get_file () in
  let raw = t.ByteFile.raw in
  Printf.printf "File %s\n" filename;
  let header_len = String.length raw.ByteFile.RAW.header in
  Printf.printf "Header: %d bytes\n" header_len;
  let rec iter pos sections =
    match sections with
        [] -> ()
      | (name, content) :: tail ->
        let len = String.length content in
        Printf.printf "Section %s at pos %d len %d\n"
          name pos len;
        iter (pos+len) tail
  in
  iter header_len raw.ByteFile.RAW.sections;
  Printf.printf "%!";
  ()

let dump () =
  let (filename, t) = get_file () in
  Printf.printf "File %s\n" filename;
  let s = ByteFile.string_of_file t in
  Printf.printf "%s\n%!" s;
  ()

module IntSet = Set.Make (struct type t = int let compare = compare end)

let disass () =
  let (filename, t) = get_file () in
  Printf.printf "File %s\n" filename;

  let labels = ref IntSet.empty in
  let module GetLabels = ByteCode.IterDisp(struct
    let unit l = labels := IntSet.add l !labels
  end) in
  ByteCode.iter (fun pos opcode -> GetLabels.unit opcode) t;


  let label_names = ref IntMap.empty in
  let counter = ref 0 in
  IntSet.iter (fun l ->
    incr counter;
    label_names := IntMap.add l !counter !label_names) !labels;

  let globals = ref IntMap.empty in
  Tbl.iter (fun name pos ->
    globals := IntMap.add pos (Ident.name name) !globals
  ) t.ByteFile.symbols;

  let module Printer = ByteCode.Printer(struct
    module Disp = struct
      let to_string t = Printf.sprintf "L%d" (IntMap.find t !label_names)
    end

    module Global = struct
      let to_string t = try
                          IntMap.find t !globals
        with Not_found -> string_of_int t
    end

    module Primitive = struct
      let to_string (_, prim) =
        t.ByteFile.primitives.(prim)

    end
  end) in

  let label_name l =
    try
      Printf.sprintf "L%d" (IntMap.find l !label_names)
    with Not_found ->
      ""
  in

  ByteCode.iter (fun pos opcode ->
    Printf.printf "%s\t%d\t%s\n"
      (label_name pos) pos (Printer.string_of_opcode opcode)
  ) t;
  Printf.printf "%!";
