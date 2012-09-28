(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* module for .ocamlspot file 

   build_dir: dirname

      Work around for ocamlbuild. If .ocamlspot is placed in a directory $DIR,
      then spot files of source files under $DIR ex. $DIR/subdir/source.ml
      is searched in $DIR/dirname/subdir/.
*)

type t = {
  build_dir : string option;
}

val find_and_load : string -> (string * t) option
  (** [find_and_load abspath] searches .ocamlspot file 
      and returns its location (directory) and its contents.
      The search starts from directory [abspath] and if not found
      it checks the parent directory recursively till the root. *)
