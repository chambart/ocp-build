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

(* TODO:
The typical pattern is:
File "tools/ocp-fix-errors/fixReplace.ml", line 19, characters 0-4:
Error: Unbound value toto

If the replacement for "toto" is unknown, the user is prompted to
enter a replacement.  The replacement is recorded, and the next time
the same error is detected, the replacement will be done automatically.

There might be two solutions to solve this problem. The first one is
to propose a replacement, the second one to open the module containing
this value.

*)

open FixTypes

let fix loc error_line next_lines =
  failwith "Not yet implemented"

