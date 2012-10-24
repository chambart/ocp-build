
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

let os_type =
  match String.lowercase Sys.os_type with
      "win32" -> WINDOWS
    | "cygwin" -> CYGWIN
    | "unix" -> UNIX
    | _ -> assert false

external waitpids : int -> int array -> int * Unix.process_status
  = "win32_waitpids_ml"


(* a wrapper around both Unix and Win32 calls *)
external waitpid : Unix.wait_flag list -> int -> int * Unix.process_status
                 = "win32_waitpid_ml"


let rec waitpid1 pid =
   let (_, status) = waitpid [] pid in
   match status with
     | Unix.WEXITED n -> n
     | Unix.WSIGNALED n -> -n
     | Unix.WSTOPPED n -> -1000-n

let rec safe_waitpid pid =
   try
     waitpid1 pid
   with Unix.Unix_error (Unix.EINTR, _, _) -> safe_waitpid pid

let command argv =
(*    Printf.fprintf stderr "exec %s\n%!" filename; *)
    let pid = try
      Unix.create_process argv.(0) argv
         Unix.stdin Unix.stdout Unix.stderr
      with e ->
	      Printf.fprintf stderr "Error \"%s\" executing %s\n%!"
                (Printexc.to_string e) Sys.argv.(0);
              exit 2
    in
    let status = safe_waitpid pid in
(*    Printf.fprintf stderr "waitpid returned %d\n%!" status; *)
    status

let simulate_exec argv =
   let status = command argv in
   exit status

