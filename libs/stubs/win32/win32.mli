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

type os_type = WINDOWS | CYGWIN | UNIX

val os_type : os_type


external waitpids : int -> int array -> int * Unix.process_status
  = "win32_waitpids_ml"

val safe_waitpid : int -> int

val command : string array -> int
val simulate_exec : string array -> 'a

val waitpid : Unix.wait_flag list -> int -> int * Unix.process_status

type open_flag =
    O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL
  | O_NOCTTY
  | O_DSYNC
  | O_SYNC
  | O_RSYNC
  | O_SHARE_DELETE
  | O_TEXT
  | O_BINARY

external openfile : string -> open_flag list -> Unix.file_perm -> Unix.file_descr
           = "win32_open_ml"

