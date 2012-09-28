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

(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)


(*
Sections are:
  RNTM: the executable code of the runtime
  CODE: the bytecode
  DLPT: path to dynamic libraries
  DLLS: dynamic libraries
  PRIM: primitive table (output_value: )
  DATA: global table (output_value: )
  SYMB: symbol table (output_value: )
  CRCS: checksums (output_value: )
  DBUG: debug info (output_value: )
*)

open ByteMisc

type error =
    FileTooShort
  | BadMagic of string  (* what was read *)

exception Error of
    string  (* filename *)
  * error   (* the error *)

let exec_magic_number = "Caml1999X008"

module RAW : sig

  type t = {
    header : string;
    sections : (string * string) list;
    magic : string;
  }
  val load : string -> t
  val save : string -> t -> unit
  val is_bytecode_executable : string -> bool

end = struct

type t = {
  header : string;
  sections : (string * string) list;
  magic : string;
}

let is_bytecode_executable filename =
  let ic = open_in_bin filename in
  try
    let length = in_channel_length ic in
    let magic = exec_magic_number in
    let len_magic = String.length magic in
    if len_magic + 4 > length then raise Not_found;
    let buffer = String.create len_magic in
    let pos_magic = length - len_magic in
    seek_in ic pos_magic;
    really_input ic buffer 0 len_magic;
    close_in ic;
    buffer = magic
  with e ->
    close_in ic;
    Printf.fprintf stderr "is_bytecode_executable %s: false (%s)\n%!" filename
      (Printexc.to_string e);
    false

let load filename =
  let ic = open_in_bin filename in
  try
  let length = in_channel_length ic in
  let magic = exec_magic_number in
  let len_magic = String.length magic in
  if len_magic + 4 > length then
    raise (Error (filename, FileTooShort));
  let buffer = String.create len_magic in
  let pos_magic = length - len_magic in
  seek_in ic pos_magic;
  really_input ic buffer 0 len_magic;
  if buffer <> magic then
    raise (Error (filename, BadMagic buffer));
  let pos_nsections = pos_magic - 4 in
  seek_in ic pos_nsections;
  let nsections = input_binary_int ic in
  let pos_table = pos_nsections - 8 * nsections in
  if pos_table < 0 then
    raise (Error (filename, FileTooShort));
  seek_in ic pos_table;
  let sections = ref [] in
  let total_size = ref 0 in
  for i = 1 to nsections do
    let name = String.create 4 in
    really_input ic name 0 4;
    let len = input_binary_int ic in
    sections := (name, len) :: !sections;
    total_size := !total_size + len;
  done;
  let len_trailer = 8 * nsections + 4 + len_magic in
  let trailer = String.create len_trailer in
  seek_in ic pos_table;
  really_input ic trailer 0 len_trailer;
  let rec read_sections end_pos table sections =
    match table with
        [] -> sections
      | (name, len) :: tail ->
        let begin_pos = end_pos - len in
        let content = String.create len in
        seek_in ic begin_pos;
        really_input ic content 0 len;
        read_sections begin_pos tail ( (name, content) :: sections )
  in
  let sections = read_sections pos_table !sections [] in
  let pos_sections = pos_table - !total_size in
  let header = String.create pos_sections in
  if pos_sections > 0 then begin
      seek_in ic 0;
      really_input ic header 0 pos_sections;
  end;
  close_in ic;
  {
    header = header;
    sections = sections;
    magic = magic;
  }
  with e -> close_in ic ; raise e

let save filename t =
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
      filename in
  output_string oc t.header;
  List.iter (fun (name, content) ->
    output_string oc content;
  ) t.sections;
  List.iter (fun (name, content) ->
    output_string oc name;
    output_binary_int oc (String.length content);
  ) t.sections;
  output_binary_int oc (List.length t.sections);
  output_string oc t.magic;
  close_out oc

end

type t = {
  header : string;
  mutable primitives : string array;
  mutable data : Obj.t array;
  mutable nsymbols : int;
  mutable symbols : (Ident.t, int) Tbl.t;
  mutable debug : (int * Instruct.debug_event list) list;
  mutable code : string;
  mutable dll_path : string list;
  mutable dll_names : string list;
  mutable imports : (string * Digest.t) list;
  mutable runtime : string option;
  mutable other_sections : (string * string) list;

  magic : string;

  raw : RAW.t; (* Cannot be modified *)
}

(* copied from bytecomp/bytesections.ml *)
let read_stringlist p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  split 0 0

let read_primitive_table p = Array.of_list(read_stringlist p)

let load filename =
  let raw = RAW.load filename in
  let t = {
    runtime = None;
    raw = raw;
    header = raw.RAW.header;
    primitives = [||];
    data = [||];
    nsymbols = 0;
    symbols = Tbl.empty;
    debug = [];
    dll_path = [];
    dll_names = [];
    imports = [];
    code = "";
    other_sections = [];
    magic = raw.RAW.magic;
  } in
  List.iter (fun (name, content) ->
    match name with
        "PRIM" ->
          t.primitives <- read_primitive_table content
      | "DATA" ->
        t.data <- Marshal.from_string content 0
      | "SYMB" ->
        let (nsymbols, symbols) = Marshal.from_string content 0 in
        t.symbols <- symbols;
        t.nsymbols <- nsymbols
      | "DBUG" ->
        let num_eventlists = BigEndian.get_uint content 0 in
        let rec iter num_eventlists s pos =
          if num_eventlists > 0 then
            let orig = BigEndian.get_uint s pos in
            let size = Marshal.total_size s (pos+4) in
            let evl = (Marshal.from_string s (pos+4) : Instruct.debug_event list) in
            t.debug <- (orig, evl) :: t.debug;
            iter (num_eventlists-1) s (pos + 4 + size)
        in
        iter num_eventlists content 4
      | "CODE" ->
        t.code <- content;
      | "DLPT" ->
        t.dll_path <- read_stringlist content
      | "DLLS" ->
        t.dll_names <- read_stringlist content
      | "CRCS" ->
        t.imports <- Marshal.from_string content 0
      | "RNTM" ->
        t.runtime <- Some (String.sub content 0 (String.length content - 1))
      | _ -> t.other_sections <- (name, content) :: t.other_sections
  ) raw.RAW.sections;
  t

(* copied from bytecomp/symtable.ml *)
let data_primitive_names prim =
  let b = Buffer.create 512 in
  for i = 0 to Array.length prim - 1 do
    Buffer.add_string b prim.(i); Buffer.add_char b '\000'
  done;
  Buffer.contents b

let string_of_stringlist list =
  let b = Buffer.create 512 in
  List.iter (fun s ->
    Buffer.add_string b s; Buffer.add_char b '\000') list;
  Buffer.contents b

let string_of_debug debug =
  let b = Buffer.create 512 in
  BigEndian.buf_uint b (List.length debug);
  List.iter (fun (orig, evl) ->
    BigEndian.buf_uint b orig;
    Buffer.add_string b (Marshal.to_string evl [])
  ) debug;
  Buffer.contents b

let save filename t =
  let sections =
    ("PRIM", data_primitive_names t.primitives) ::
      ("DATA", Marshal.to_string t.data []) ::
      ("SYMB", Marshal.to_string (t.nsymbols, t.symbols) []) ::
      ("CRCS", Marshal.to_string t.imports []) ::
      t.other_sections
  in
  let sections =
    if t.dll_path = [] then sections else
      ("DLPT", string_of_stringlist t.dll_path) :: sections
  in
  let sections =
    if t.dll_names = [] then sections else
      ("DLLS", string_of_stringlist t.dll_names) :: sections
  in
  let sections =
    if t.debug = [] then sections else
      ("DBUG", string_of_debug t.debug) :: sections
  in
  let sections =
    ("CODE", t.code) :: sections
  in
(* RNTM must always be the first section, as expected by stdlib/headernt.c *)
  let sections =
    match t.runtime with
        None -> sections
      | Some runtime ->
        ("RNTM", Printf.sprintf "%s\n" runtime) :: sections
  in
  let raw = {
    RAW.header = t.header;
    RAW.sections = sections;
    RAW.magic = t.magic;
  } in
  RAW.save filename raw

let string_of_file t =
  let b = Strings.create () in
  (match t.runtime with None -> () | Some runtime ->
    Strings.printf b "Runtime: %s\n" runtime);
  Strings.add_immutable_string b "Imported units:\n";
  List.iter (fun (s, digest) ->
    Strings.printf b "\t%s\t%s\n" (Digest.to_hex digest) s
  ) t.imports;
  Strings.add_immutable_string b "DLL path:\n";
  List.iter (fun s -> Strings.printf b "\t%s\n" s) t.dll_path;
  Strings.add_immutable_string b "Used DLLs:\n";
  List.iter (fun s -> Strings.printf b "\t%s\n" s) t.dll_names;
  Strings.add_immutable_string b "Primitives used:\n";
  Array.iter (fun s -> Strings.printf b "\t%s\n" s) t.primitives;
  Strings.printf b "Global table size: %d\n" (Array.length t.data);
  Strings.printf b "Debugging information: %d positions\n" (List.length t.debug);
  Strings.printf b "Symbols: %d\n" t.nsymbols;
  Strings.contents b

let string_of_error error =
  match error with
      FileTooShort -> "File too short"
    | BadMagic magic -> Printf.sprintf "BadMagic [%s]" (String.escaped magic)

let is_bytecode_executable filename = RAW.is_bytecode_executable filename
