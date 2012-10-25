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

open OcpLang
(* open OcpSystem *)

(* open SafeCaml *)

let print_loc filename pos =
  let line = ref 1 in
  let last_line_pos = ref 0 in
  begin try
	  let s = File.string_of_file filename in
	  for i = 0 to pos - 1 do
	    if s.[i] = '\n' then begin
	      incr line;
	      last_line_pos := i
	    end
	  done
    with _ -> ()
  end;
  let pos = pos - !last_line_pos in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n%!"
    filename !line pos pos

exception ParseError

let token_list_of_filename filename lexer =
  let s = File.string_of_file filename in
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  try
    Stream.iter (fun token ->
      let token_pos = Stream.count str1 in
      list := (token, token_pos) :: !list) str2;
    List.rev !list
  with
      Stream.Error error ->
	print_loc filename (Stream.count str1);
	Printf.eprintf "Error: %s\n%!" error;
	raise ParseError


let os_type = Win32.os_type

let waitpids = Win32.waitpids

module Unix2 : sig
  open Unix

  type error_handler = exn -> unit

  val create_process : error_handler ->
    string -> string array -> file_descr -> file_descr -> file_descr -> int

(*
  val create_process_env : error_handler ->
    string -> string array -> string array -> file_descr -> file_descr ->
    file_descr -> int
*)

end = struct
  open Unix
  type error_handler = exn -> unit


let string_rev s =
  let len = String.length s in
  let ss = String.create len in
  let lenMinus1 = len-1 in
  for i = 0 to len-1 do
    ss.[i] <- s.[lenMinus1-i];
  done;
  ss

(* This is supposed to be implemented as Filename.quote ? Verify ! *)
let win_buf_escape_argument b arg =
  let len = String.length arg in
  Buffer.add_char b '"';
  let rec iter escaped b s pos =
    if pos > 0 then
    let pos = pos-1 in
    let c = s.[pos] in
    match c, escaped with
    | '"', _
    | '\\', true ->
          Buffer.add_char b c;
          Buffer.add_char b '\\'; (* after, because we reverse afterwards ! *)
          iter true b s pos
    | _ ->
          Buffer.add_char b c;
          iter false b s pos
  in
  iter true b arg len;
  Buffer.add_char b '"'

let win_cmdline_of_args args =
  let b = Buffer.create 100 in
  let rec iter b need_space args =
    match args with
      [] -> ()
     | arg :: tail ->
       if need_space then Buffer.add_char b ' ';
       win_buf_escape_argument b arg;
       iter b true tail
  in
  iter b false args;
  Buffer.contents b

let rec safe_dup fd =
  let new_fd = dup fd in
  if Obj.magic new_fd >= 3 then (* TODO: windows incompatibility *)
    new_fd
  else begin
    let res = safe_dup fd in
    close new_fd;
    res
  end

let safe_close fd =
  try close fd with Unix_error(_,_,_) -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let newnewstdin = safe_dup new_stdin in
  let newnewstdout = safe_dup new_stdout in
  let newnewstderr = safe_dup new_stderr in
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr;
  dup2 newnewstdin stdin; close newnewstdin;
  dup2 newnewstdout stdout; close newnewstdout;
  dup2 newnewstderr stderr; close newnewstderr

let create_process error_handler cmd args new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvp cmd args
      with e ->
        error_handler e;
        exit 127
      end
  | id -> id


(*
let create_process_env error_handler cmd args env new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvpe cmd args env
      with e ->
        error_handler e;
        exit 127
      end
  | id -> id
*)

let in_path = Hashtbl.create 113
let home_dir = try Sys.getenv "HOME" with _ -> " "
let path = try OcpString.split (Sys.getenv "PATH")
  (if home_dir.[0] = '/' then ':' else ';') (* on Cygwin, use : instead of ; *)
 with _ -> ["."]

let test f =
  Printf.eprintf "Testing %s (%b,%b)\n%!" f
    (Sys.file_exists f)
    (try ignore (Unix.stat f); true with _ -> false)

let win_find_in_path cmd =
  try Hashtbl.find in_path cmd with Not_found ->
    let rec iter path cmd =
      match path with
        [] -> Printf.fprintf Pervasives.stderr "File %s not found in PATH\n%!" cmd;
              exit 2
       | dir :: tail ->
          let test1 = Filename.concat dir cmd in
(*          test test1; *)
          if Sys.file_exists test1 then test1 else
          let test2 = Filename.concat dir (cmd ^ ".exe") in
(*          test test2; *)
          if Sys.file_exists test2 then test2 else
          iter tail cmd
    in
    let cmd_found = iter path cmd in
    Hashtbl.add in_path cmd cmd_found;
    cmd_found

let create_process error_handler cmd args fd1 fd2 fd3 =
  if os_type = Win32.WINDOWS then
    let cmd = if Filename.is_implicit cmd then win_find_in_path cmd
      else cmd in
    Unix.create_process cmd args fd1 fd2 fd3
  else
    create_process error_handler cmd args fd1 fd2 fd3
end

let open_for_pipe filename =
  let oc = open_out_bin filename in
  Unix.descr_of_out_channel oc

let create_process list stdout stderr =
  match list with
      [] -> assert false
    | cmd :: args ->
      let stdout_fd = match stdout with
	  None -> Unix.stdout
	| Some filename -> open_for_pipe filename
      in
      let stderr_fd = match stderr with
	  None -> Unix.stderr
	| Some filename -> open_for_pipe filename
      in
      let error_handler e =
        Printf.eprintf "Error while executing subprocess\n";
        Printf.eprintf "  exception %s\n%!" (Printexc.to_string e);
      in

      let pid = Unix2.create_process error_handler cmd (Array.of_list (cmd :: args))
	Unix.stdin stdout_fd stderr_fd in
      (match stdout with None -> () | Some _ -> Unix.close stdout_fd);
      (match stderr with None -> () | Some _ -> Unix.close stderr_fd);
      pid

let rec wait_command pid =
  try
    let rec iter pid =
      let (_, status) = Win32.waitpid [] pid in
      match status with
	  Unix.WEXITED n -> n
	| _ -> iter pid
    in
    iter pid
  with e ->
    Printf.eprintf "Exception %s in waitpid\n%!" (Printexc.to_string e);
    exit 2


let new_counter_int0 () =
  let counter = ref 0 in
  fun () ->
  let id = !counter in
  incr counter;
  id


let _ =
  Printexc.register_printer (fun exn ->
    match exn with
	Unix.Unix_error (error, s1, s2) ->
          Some (Printf.sprintf "Unix_error(%s, %s, %s)"
		  (Unix.error_message error) s1 s2)
      | _ -> None)
;;

let rec safe_mkdir filename =
  try
    let st = Unix.stat filename in
    match st.Unix.st_kind with
	Unix.S_DIR ->
	  ()
      | _ ->
	Printf.fprintf stderr
	  "Error in BuildGlobals.safe_mkdir: %s is not a directory. Exiting.\n"
	  filename;
	exit 2
  with _ ->
    let dirname = Filename.dirname filename in
    safe_mkdir dirname;
    let basename = Filename.basename filename in
    match basename with
    | "." | ".." -> ()
    | _ ->
      Unix.mkdir filename 0o755

let rename f1 f2 =           
  let target_of_rename = fa2 in
  if BuildMisc.os_type = Win32.WINDOWS &&
    Sys.file_exists fa2 then begin
      try (* on Windows, Sys.rename will fail if target
             exists.  This breaks atomicity of rename, so
             using -njobs > 1 might fail on Windows. There
             is an atomic rename available on Windows too,
             but only versions > XP (we should use it in the
             future). *)
        Sys.remove fa2;
      with e -> ()
    end;
  Sys.rename fa1 fa2
