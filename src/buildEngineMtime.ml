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

