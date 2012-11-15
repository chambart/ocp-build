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

module Unix = struct
  include Unix
  include OcpUnix
end

type t = {
  p_name   : string;        (* Command name *)
  p_args   : string list;   (* Command args *)
  p_pid    : int;           (* Process PID *)
  p_time   : float;         (* Process start time *)
  p_stdout : string option; (* stdout dump file *)
  p_stderr : string option; (* stderr dump file *)
  p_info   : string option; (* dump info file *)
}

let open_flags =  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]

let create ?info ?stdout ?stderr ?env cmd args =
  let stdout_fd = match stdout with
    | None   -> Unix.stdout
    | Some f -> Unix.openfile f open_flags 0o644 in
  let stderr_fd = match stderr with
    | None   -> Unix.stderr
    | Some f -> Unix.openfile f open_flags 0o644 in
  let env = match env with
    | None   -> Unix.environment ()
    | Some e -> e in
  let time = Unix.gettimeofday () in
  let pid =
    Unix.create_process_env
      cmd
      (Array.of_list (cmd :: args))
      env
      Unix.stdin stdout_fd stderr_fd in
  (match stdout with None -> () | Some _ -> Unix.close stdout_fd);
  (match stderr with None -> () | Some _ -> Unix.close stderr_fd);
  {
    p_name   = cmd;
    p_args   = args;
    p_pid    = pid;
    p_time   = time;
    p_stdout = stdout;
    p_stderr = stderr;
    p_info   = info;
  }

type result = {
  r_proc     : t;           (* Process *)
  r_code     : int;         (* Process exit code *)
  r_duration : float;       (* Process duration *)
  r_stdout   : string list; (* Content of stdout dump file *)
  r_stderr   : string list; (* Content of stderr dump file *)
}

let wait p =
  try
    let rec iter () =
      let _, status = Unix.waitpid [] p.p_pid in
      match status with
        | Unix.WEXITED code ->
          let duration = Unix.gettimeofday () -. p.p_time in
          let stdout =
            Option.default [] (Option.map File.lines_of_file p.p_stdout) in
          let stderr =
            Option.default [] (Option.map File.lines_of_file p.p_stderr) in
          {
            r_proc     = p;
            r_code     = code;
            r_duration = duration;
            r_stdout   = stdout;
            r_stderr   = stderr;
          }
        | _ -> iter () in
    iter ()
  with e ->
    Printf.printf "Exception %s in waitpid\n%!" (Printexc.to_string e);
    exit 2

let run ?env ~name cmd args =
  let stdout = Printf.sprintf "%s.out" name in
  let stderr = Printf.sprintf "%s.err" name in
  let info   = Printf.sprintf "%s.info" name in

  let env = match env with Some e -> e | None -> Unix.environment () in

  (* Write info file *)
  let chan = open_out info in
  File.output_line chan (String.concat " " (cmd :: args));
  File.output_line chan (Unix.getcwd ());
  File.output_line chan (String.concat "\n" (Array.to_list env));
  close_out chan;

  let p = create ~env ~info ~stdout ~stderr cmd args in
  wait p

let is_success r = r.r_code = 0

let is_failure r = r.r_code <> 0

let clean_files r =
  Option.iter Unix.unlink r.r_proc.p_stdout;
  Option.iter Unix.unlink r.r_proc.p_stderr;
  Option.iter Unix.unlink r.r_proc.p_info
