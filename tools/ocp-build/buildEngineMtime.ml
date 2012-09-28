(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)


type t =
    Inode of int * int * float
  | Digest of Digest.t

let use_digests = ref false

let zero = Inode (0,0, 0.)

let to_string mtime =
  match mtime with
      Inode (dev, ino, mtime) -> Printf.sprintf "%d.%d.%.0f" dev ino mtime
    | Digest sha1 -> Digest.to_hex sha1

let compute filename =
  if !use_digests then
    Digest (Digest.file filename)
  else
    let st = Unix.lstat filename in
    Inode (st.Unix.st_dev, st.Unix.st_ino, st.Unix.st_mtime)

let use_digests bool = use_digests := bool

