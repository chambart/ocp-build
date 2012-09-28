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

open ByteMisc

module Uint  = struct
  type t = int
  let get s pos = LittleEndian.get_uint s pos, pos + 4
end

module Sint  = struct
  type t = int
  let get s pos = LittleEndian.get_sint s pos, pos + 4
end

module Disp  = struct
  type t = int
  let get s pos = pos / 4 + LittleEndian.get_sint s pos, pos + 4
end

let string_of_intarray t =
  String.concat ", " (Array.to_list (Array.map string_of_int (t)))

module Closurerec  = struct
  type t =
      int (* nfuncs *)
      * int (* nvars *)
      * int array

  let get s pos =
    let nfuncs = LittleEndian.get_uint s pos in
    let nvars = LittleEndian.get_uint s (pos+4) in
    let orig = pos + 8 in
    let funcs = Array.init nfuncs (fun i ->
      orig/4 + LittleEndian.get_sint s (orig + i * 4)
    ) in
    (nfuncs, nvars, funcs), orig + 4 * nfuncs

  let to_string (nfuncs, nvars, funcs) =
    Printf.sprintf "{nfuncs=%d, nvars=%d, funcs=[%s]}"
      nfuncs nvars (string_of_intarray funcs)

end

module Global = struct
  type t = int
  let get s pos = LittleEndian.get_uint s pos, pos + 4
end

module Switch = struct
  type t = int array * int array

  let get s pos =
    let n = LittleEndian.get_uint s pos in
    let orig = pos + 4 in
    let pc = orig / 4 in
    let nconsts = n land 0xFFFF in
    let t1 = Array.init nconsts (fun i ->
      pc + LittleEndian.get_sint s (orig + i * 4)
    ) in
    let n_nonconsts = n lsr 16 in
    let orig = orig + nconsts * 4 in
    let t2 = Array.init n_nonconsts (fun i ->
      pc + LittleEndian.get_sint s (orig + i * 4)
    ) in
    let orig = orig + n_nonconsts * 4 in
    (t1, t2), orig

  let to_string (t1, t2) =
    Printf.sprintf "{ consts = [%s]; non_consts = [%s] }"
      (string_of_intarray t1)
      (string_of_intarray t2)
end

module Primitive = struct
  type t = int * int
  let get s pos = (pos, LittleEndian.get_uint s pos), pos + 4
end

module Pubmet  = struct
  type t = int
  let get s pos =
    let tag = LittleEndian.get_sint s pos in
    let _cache = LittleEndian.get_uint s (pos+4) in
    tag, pos+8
end

type opcode =
   | ACC0
   | ACC1
   | ACC2
   | ACC3
   | ACC4
   | ACC5
   | ACC6
   | ACC7
   | ACC of Uint.t
   | PUSH
   | PUSHACC0
   | PUSHACC1
   | PUSHACC2
   | PUSHACC3
   | PUSHACC4
   | PUSHACC5
   | PUSHACC6
   | PUSHACC7
   | PUSHACC of Uint.t
   | POP of Uint.t
   | ASSIGN of Uint.t
   | ENVACC1
   | ENVACC2
   | ENVACC3
   | ENVACC4
   | ENVACC of Uint.t
   | PUSHENVACC1
   | PUSHENVACC2
   | PUSHENVACC3
   | PUSHENVACC4
   | PUSHENVACC of Uint.t
   | PUSH_RETADDR of Disp.t
   | APPLY of Uint.t
   | APPLY1
   | APPLY2
   | APPLY3
   | APPTERM of Uint.t * Uint.t
   | APPTERM1 of Uint.t
   | APPTERM2 of Uint.t
   | APPTERM3 of Uint.t
   | RETURN of Uint.t
   | RESTART
   | GRAB of Uint.t
   | CLOSURE of Uint.t * Disp.t
   | CLOSUREREC of Closurerec.t
   | OFFSETCLOSUREM2
   | OFFSETCLOSURE0
   | OFFSETCLOSURE2
   | OFFSETCLOSURE of Sint.t  (* was Uint *)
   | PUSHOFFSETCLOSUREM2
   | PUSHOFFSETCLOSURE0
   | PUSHOFFSETCLOSURE2
   | PUSHOFFSETCLOSURE of Sint.t (* was Nothing *)
   | GETGLOBAL of Global.t
   | PUSHGETGLOBAL of Global.t
   | GETGLOBALFIELD of Global.t * Uint.t
   | PUSHGETGLOBALFIELD of Global.t * Uint.t
   | SETGLOBAL of Global.t
   | ATOM0
   | ATOM of Uint.t
   | PUSHATOM0
   | PUSHATOM of Uint.t
   | MAKEBLOCK of Uint.t * Uint.t
   | MAKEBLOCK1 of Uint.t
   | MAKEBLOCK2 of Uint.t
   | MAKEBLOCK3 of Uint.t
   | MAKEFLOATBLOCK of Uint.t
   | GETFIELD0
   | GETFIELD1
   | GETFIELD2
   | GETFIELD3
   | GETFIELD of Uint.t
   | GETFLOATFIELD of Uint.t
   | SETFIELD0
   | SETFIELD1
   | SETFIELD2
   | SETFIELD3
   | SETFIELD of Uint.t
   | SETFLOATFIELD of Uint.t
   | VECTLENGTH
   | GETVECTITEM
   | SETVECTITEM
   | GETSTRINGCHAR
   | SETSTRINGCHAR
   | BRANCH of Disp.t
   | BRANCHIF of Disp.t
   | BRANCHIFNOT of Disp.t
   | SWITCH of Switch.t
   | BOOLNOT
   | PUSHTRAP of Disp.t
   | POPTRAP
   | RAISE
   | CHECK_SIGNALS
   | C_CALL1 of Primitive.t
   | C_CALL2 of Primitive.t
   | C_CALL3 of Primitive.t
   | C_CALL4 of Primitive.t
   | C_CALL5 of Primitive.t
   | C_CALLN of Uint.t * Primitive.t
   | CONST0
   | CONST1
   | CONST2
   | CONST3
   | CONSTINT of Sint.t
   | PUSHCONST0
   | PUSHCONST1
   | PUSHCONST2
   | PUSHCONST3
   | PUSHCONSTINT of Sint.t
   | NEGINT
   | ADDINT
   | SUBINT
   | MULINT
   | DIVINT
   | MODINT
   | ANDINT
   | ORINT
   | XORINT
   | LSLINT
   | LSRINT
   | ASRINT
   | EQ
   | NEQ
   | LTINT
   | LEINT
   | GTINT
   | GEINT
   | OFFSETINT of Sint.t
   | OFFSETREF of Sint.t
   | ISINT
   | GETMETHOD
   | GETDYNMET
   | GETPUBMET of Pubmet.t
   | BEQ of Sint.t * Disp.t
   | BNEQ of Sint.t * Disp.t
   | BLTINT of Sint.t * Disp.t
   | BLEINT of Sint.t * Disp.t
   | BGTINT of Sint.t * Disp.t
   | BGEINT of Sint.t * Disp.t
   | ULTINT
   | UGEINT
   | BULTINT of Uint.t * Disp.t
   | BUGEINT of Uint.t * Disp.t
   | STOP
   | EVENT
   | BREAK


let  opcodes = [|
  (fun s pos -> ACC0, pos);
  (fun s pos -> ACC1, pos);
  (fun s pos -> ACC2, pos);
  (fun s pos -> ACC3, pos);
  (fun s pos -> ACC4, pos);
  (fun s pos -> ACC5, pos);
  (fun s pos -> ACC6, pos);
  (fun s pos -> ACC7, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    ACC n, pos);
  (fun s pos -> PUSH, pos);
  (fun s pos -> PUSHACC0, pos);
  (fun s pos -> PUSHACC1, pos);
  (fun s pos -> PUSHACC2, pos);
  (fun s pos -> PUSHACC3, pos);
  (fun s pos -> PUSHACC4, pos);
  (fun s pos -> PUSHACC5, pos);
  (fun s pos -> PUSHACC6, pos);
  (fun s pos -> PUSHACC7, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    PUSHACC n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    POP n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    ASSIGN n, pos);
  (fun s pos -> ENVACC1, pos);
  (fun s pos -> ENVACC2, pos);
  (fun s pos -> ENVACC3, pos);
  (fun s pos -> ENVACC4, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    ENVACC n, pos);
  (fun s pos -> PUSHENVACC1, pos);
  (fun s pos -> PUSHENVACC2, pos);
  (fun s pos -> PUSHENVACC3, pos);
  (fun s pos -> PUSHENVACC4, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    PUSHENVACC n, pos);
  (fun s pos ->
    let (n,pos) = Disp.get s pos in
    PUSH_RETADDR n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    APPLY n, pos);
  (fun s pos -> APPLY1, pos);
  (fun s pos -> APPLY2, pos);
  (fun s pos -> APPLY3, pos);
  (fun s pos ->
    let (n1,pos) = Uint.get s pos in
    let (n2,pos) = Uint.get s pos in
    APPTERM (n1, n2), pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    APPTERM1 n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    APPTERM2 n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    APPTERM3 n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    RETURN n, pos);
  (fun s pos -> RESTART, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    GRAB n, pos);
  (fun s pos ->
    let (n1,pos) = Uint.get s pos in
    let (n2,pos) = Disp.get s pos in
    CLOSURE (n1,n2), pos);
  (fun s pos ->
    let (n,pos) = Closurerec.get s pos in
    CLOSUREREC n, pos);
  (fun s pos -> OFFSETCLOSUREM2, pos);
  (fun s pos -> OFFSETCLOSURE0, pos);
  (fun s pos -> OFFSETCLOSURE2, pos);
  (fun s pos ->
    let (n,pos) = Sint.get s pos in
    OFFSETCLOSURE n, pos);
  (fun s pos -> PUSHOFFSETCLOSUREM2, pos);
  (fun s pos -> PUSHOFFSETCLOSURE0, pos);
  (fun s pos -> PUSHOFFSETCLOSURE2, pos);
  (fun s pos ->
    let (n,pos) = Sint.get s pos in
    PUSHOFFSETCLOSURE n, pos);
  (fun s pos ->
    let (n,pos) = Global.get s pos in
    GETGLOBAL n, pos);
  (fun s pos ->
    let (n,pos) = Global.get s pos in
    PUSHGETGLOBAL n, pos);
  (fun s pos ->
    let (n1,pos) = Global.get s pos in
    let (n2,pos) = Uint.get s pos in
    GETGLOBALFIELD (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Global.get s pos in
    let (n2,pos) = Uint.get s pos in
    PUSHGETGLOBALFIELD (n1,n2), pos);
  (fun s pos ->
    let (n,pos) = Global.get s pos in
    SETGLOBAL n, pos);
  (fun s pos -> ATOM0, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    ATOM n, pos);
  (fun s pos -> PUSHATOM0, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    PUSHATOM n, pos);
  (fun s pos ->
    let (n1,pos) = Uint.get s pos in
    let (n2,pos) = Uint.get s pos in
    MAKEBLOCK (n1,n2), pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    MAKEBLOCK1 n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    MAKEBLOCK2 n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    MAKEBLOCK3 n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    MAKEFLOATBLOCK n, pos);
  (fun s pos -> GETFIELD0, pos);
  (fun s pos -> GETFIELD1, pos);
  (fun s pos -> GETFIELD2, pos);
  (fun s pos -> GETFIELD3, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    GETFIELD n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    GETFLOATFIELD n, pos);
  (fun s pos -> SETFIELD0, pos);
  (fun s pos -> SETFIELD1, pos);
  (fun s pos -> SETFIELD2, pos);
  (fun s pos -> SETFIELD3, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    SETFIELD n, pos);
  (fun s pos ->
    let (n,pos) = Uint.get s pos in
    SETFLOATFIELD n, pos);
  (fun s pos -> VECTLENGTH, pos);
  (fun s pos -> GETVECTITEM, pos);
  (fun s pos -> SETVECTITEM, pos);
  (fun s pos -> GETSTRINGCHAR, pos);
  (fun s pos -> SETSTRINGCHAR, pos);
  (fun s pos ->
    let (n,pos) = Disp.get s pos in
    BRANCH n, pos);
  (fun s pos ->
    let (n,pos) = Disp.get s pos in
    BRANCHIF n, pos);
  (fun s pos ->
    let (n,pos) = Disp.get s pos in
    BRANCHIFNOT n, pos);
  (fun s pos ->
    let (n,pos) = Switch.get s pos in
    SWITCH n, pos);
  (fun s pos -> BOOLNOT, pos);
  (fun s pos ->
    let (n,pos) = Disp.get s pos in
    PUSHTRAP n, pos);
  (fun s pos -> POPTRAP, pos);
  (fun s pos -> RAISE, pos);
  (fun s pos -> CHECK_SIGNALS, pos);
  (fun s pos ->
    let (n,pos) = Primitive.get s pos in
    C_CALL1 n, pos);
  (fun s pos ->
    let (n,pos) = Primitive.get s pos in
    C_CALL2 n, pos);
  (fun s pos ->
    let (n,pos) = Primitive.get s pos in
    C_CALL3 n, pos);
  (fun s pos ->
    let (n,pos) = Primitive.get s pos in
    C_CALL4 n, pos);
  (fun s pos ->
    let (n,pos) = Primitive.get s pos in
    C_CALL5 n, pos);
  (fun s pos ->
    let (n1,pos) = Uint.get s pos in
    let (n2,pos) = Primitive.get s pos in
    C_CALLN (n1,n2), pos);
  (fun s pos -> CONST0, pos);
  (fun s pos -> CONST1, pos);
  (fun s pos -> CONST2, pos);
  (fun s pos -> CONST3, pos);
  (fun s pos ->
    let (n,pos) = Sint.get s pos in
    CONSTINT n, pos);
  (fun s pos -> PUSHCONST0, pos);
  (fun s pos -> PUSHCONST1, pos);
  (fun s pos -> PUSHCONST2, pos);
  (fun s pos -> PUSHCONST3, pos);
  (fun s pos ->
    let (n,pos) = Sint.get s pos in
    PUSHCONSTINT n, pos);
  (fun s pos -> NEGINT, pos);
  (fun s pos -> ADDINT, pos);
  (fun s pos -> SUBINT, pos);
  (fun s pos -> MULINT, pos);
  (fun s pos -> DIVINT, pos);
  (fun s pos -> MODINT, pos);
  (fun s pos -> ANDINT, pos);
  (fun s pos -> ORINT, pos);
  (fun s pos -> XORINT, pos);
  (fun s pos -> LSLINT, pos);
  (fun s pos -> LSRINT, pos);
  (fun s pos -> ASRINT, pos);
  (fun s pos -> EQ, pos);
  (fun s pos -> NEQ, pos);
  (fun s pos -> LTINT, pos);
  (fun s pos -> LEINT, pos);
  (fun s pos -> GTINT, pos);
  (fun s pos -> GEINT, pos);
  (fun s pos ->
    let (n,pos) = Sint.get s pos in
    OFFSETINT n, pos);
  (fun s pos ->
    let (n,pos) = Sint.get s pos in
    OFFSETREF n, pos);
  (fun s pos -> ISINT, pos);
  (fun s pos -> GETMETHOD, pos);
  (fun s pos ->
    let (n1,pos) = Sint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BEQ (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Sint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BNEQ (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Sint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BLTINT (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Sint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BLEINT (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Sint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BGTINT (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Sint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BGEINT (n1,n2), pos);
  (fun s pos -> ULTINT, pos);
  (fun s pos -> UGEINT, pos);
  (fun s pos ->
    let (n1,pos) = Uint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BULTINT (n1,n2), pos);
  (fun s pos ->
    let (n1,pos) = Uint.get s pos in
    let (n2,pos) = Disp.get s pos in
    BUGEINT (n1,n2), pos);
  (fun s pos ->
    let (n,pos) = Pubmet.get s pos in
    GETPUBMET n, pos);
  (fun s pos -> GETDYNMET, pos);
  (fun s pos -> STOP, pos);
  (fun s pos -> EVENT, pos);
  (fun s pos -> BREAK, pos);
              |]


let iter f t =
  let rec iter f code begin_pos =
    if begin_pos < String.length code then
    let op  = LittleEndian.get_uint code begin_pos in
    let pos = begin_pos + 4 in
    let opcode, end_pos = opcodes.(op) code pos in
    f (begin_pos/4) opcode;
    iter f code end_pos
  in
  iter f t.ByteFile.code 0


module Printer( PrinterArg :
  sig
    module Disp : sig val to_string : int -> string end
    module Global : sig val to_string : int -> string end
    module Primitive : sig val to_string : int*int -> string end
  end
) = struct

  open PrinterArg

  let string_of_disparray t =
  String.concat ", " (Array.to_list (Array.map Disp.to_string (t)))

  module Closurerec  = struct

    let to_string (nfuncs, nvars, funcs) =
      Printf.sprintf "{nfuncs=%d, nvars=%d, funcs=[%s]}"
        nfuncs nvars (string_of_disparray funcs)
  end

  module Switch = struct

    let to_string (t1, t2) =
      Printf.sprintf "{ consts = [%s]; non_consts = [%s] }"
        (string_of_disparray t1)
        (string_of_disparray t2)
  end

  let string_of_opcode op =
    match op with
   | ACC0 -> "ACC0"
   | ACC1 -> "ACC1"
   | ACC2 -> "ACC2"
   | ACC3 -> "ACC3"
   | ACC4 -> "ACC4"
   | ACC5 -> "ACC5"
   | ACC6 -> "ACC6"
   | ACC7 -> "ACC7"
   | ACC (uint_t) -> Printf.sprintf "ACC (%d)" uint_t
   | PUSH -> "PUSH"
   | PUSHACC0 -> "PUSHACC0"
   | PUSHACC1 -> "PUSHACC1"
   | PUSHACC2 -> "PUSHACC2"
   | PUSHACC3 -> "PUSHACC3"
   | PUSHACC4 -> "PUSHACC4"
   | PUSHACC5 -> "PUSHACC5"
   | PUSHACC6 -> "PUSHACC6"
   | PUSHACC7 -> "PUSHACC7"
   | PUSHACC (uint_t) -> Printf.sprintf "PUSHACC (%d)" uint_t
   | POP (uint_t) -> Printf.sprintf "POP (%d)" uint_t
   | ASSIGN (uint_t) -> Printf.sprintf "ASSIGN (%d)" uint_t
   | ENVACC1 -> "ENVACC1"
   | ENVACC2 -> "ENVACC2"
   | ENVACC3 -> "ENVACC3"
   | ENVACC4 -> "ENVACC4"
   | ENVACC (uint_t) -> Printf.sprintf "ENVACC (%d)" uint_t
   | PUSHENVACC1 -> "PUSHENVACC1"
   | PUSHENVACC2 -> "PUSHENVACC2"
   | PUSHENVACC3 -> "PUSHENVACC3"
   | PUSHENVACC4 -> "PUSHENVACC4"
   | PUSHENVACC (uint_t) -> Printf.sprintf "PUSHENVACC (%d)" uint_t
   | PUSH_RETADDR (disp_t) -> Printf.sprintf "PUSH_RETADDR (%s)" (Disp.to_string disp_t)
   | APPLY (uint_t) -> Printf.sprintf "APPLY (%d)" uint_t
   | APPLY1 -> "APPLY1"
   | APPLY2 -> "APPLY2"
   | APPLY3 -> "APPLY3"
   | APPTERM (uint1, uint2) -> Printf.sprintf "APPTERM (%d, %d)" uint1 uint2
   | APPTERM1 (uint_t) -> Printf.sprintf "APPTERM1 (%d)" uint_t
   | APPTERM2 (uint_t) -> Printf.sprintf "APPTERM2 (%d)" uint_t
   | APPTERM3 (uint_t) -> Printf.sprintf "APPTERM3 (%d)" uint_t
   | RETURN (uint_t) -> Printf.sprintf "RETURN (%d)" uint_t
   | RESTART -> "RESTART"
   | GRAB (uint_t) -> Printf.sprintf "GRAB (%d)" uint_t
   | CLOSURE (uint_t, disp_t) -> Printf.sprintf "CLOSURE (%d,%s)" uint_t (Disp.to_string disp_t)
   | CLOSUREREC (closurerec_t) -> Printf.sprintf "CLOSUREREC (%s)"
     (Closurerec.to_string closurerec_t)
   | OFFSETCLOSUREM2 -> "OFFSETCLOSUREM2"
   | OFFSETCLOSURE0 -> "OFFSETCLOSURE0"
   | OFFSETCLOSURE2 -> "OFFSETCLOSURE2"
   | OFFSETCLOSURE (sint_t) -> Printf.sprintf "OFFSETCLOSURE (%d)" sint_t
   | PUSHOFFSETCLOSUREM2 -> "PUSHOFFSETCLOSUREM2"
   | PUSHOFFSETCLOSURE0 -> "PUSHOFFSETCLOSURE0"
   | PUSHOFFSETCLOSURE2 -> "PUSHOFFSETCLOSURE2"
   | PUSHOFFSETCLOSURE (sint_t) -> Printf.sprintf "PUSHOFFSETCLOSURE (%d)" sint_t
   | GETGLOBAL (global_t) -> Printf.sprintf "GETGLOBAL (%s)" (Global.to_string global_t)
   | PUSHGETGLOBAL (global_t) -> Printf.sprintf "PUSHGETGLOBAL (%s)" (Global.to_string global_t)
   | GETGLOBALFIELD (global_t, uint_t) -> Printf.sprintf "GETGLOBALFIELD (%s,%d)" (Global.to_string global_t) uint_t
   | PUSHGETGLOBALFIELD (global_t, uint_t) -> Printf.sprintf "PUSHGETGLOBALFIELD (%s,%d)" (Global.to_string global_t) uint_t
   | SETGLOBAL (global_t) -> Printf.sprintf "SETGLOBAL (%s)" (Global.to_string global_t)
   | ATOM0 -> "ATOM0"
   | ATOM (uint_t) -> Printf.sprintf "ATOM (%d)" uint_t
   | PUSHATOM0 -> "PUSHATOM0"
   | PUSHATOM (uint_t) -> Printf.sprintf "PUSHATOM (%d)" uint_t
   | MAKEBLOCK (uint_t1, uint_t2) -> Printf.sprintf "MAKEBLOCK (%d,%d)" uint_t1 uint_t2
   | MAKEBLOCK1 (uint_t) -> Printf.sprintf "MAKEBLOCK1 (%d)" uint_t
   | MAKEBLOCK2 (uint_t) -> Printf.sprintf "MAKEBLOCK2 (%d)" uint_t
   | MAKEBLOCK3 (uint_t) -> Printf.sprintf "MAKEBLOCK3 (%d)" uint_t
   | MAKEFLOATBLOCK (uint_t) -> Printf.sprintf "MAKEFLOATBLOCK (%d)" uint_t
   | GETFIELD0 -> "GETFIELD0"
   | GETFIELD1 -> "GETFIELD1"
   | GETFIELD2 -> "GETFIELD2"
   | GETFIELD3 -> "GETFIELD3"
   | GETFIELD (uint_t) -> Printf.sprintf "GETFIELD (%d)" uint_t
   | GETFLOATFIELD (uint_t) -> Printf.sprintf "GETFLOATFIELD (%d)" uint_t
   | SETFIELD0 -> "SETFIELD0"
   | SETFIELD1 -> "SETFIELD1"
   | SETFIELD2 -> "SETFIELD2"
   | SETFIELD3 -> "SETFIELD3"
   | SETFIELD (uint_t) -> Printf.sprintf "SETFIELD (%d)" uint_t
   | SETFLOATFIELD (uint_t) -> Printf.sprintf "SETFLOATFIELD (%d)" uint_t
   | VECTLENGTH -> "VECTLENGTH"
   | GETVECTITEM -> "GETVECTITEM"
   | SETVECTITEM -> "SETVECTITEM"
   | GETSTRINGCHAR -> "GETSTRINGCHAR"
   | SETSTRINGCHAR -> "SETSTRINGCHAR"
   | BRANCH (disp_t) -> Printf.sprintf "BRANCH (%s)" (Disp.to_string disp_t)
   | BRANCHIF (disp_t) -> Printf.sprintf "BRANCHIF (%s)" (Disp.to_string disp_t)
   | BRANCHIFNOT (disp_t) -> Printf.sprintf "BRANCHIFNOT (%s)" (Disp.to_string disp_t)
   | SWITCH (switch_t) -> Printf.sprintf "SWITCH (%s)" (Switch.to_string switch_t)
   | BOOLNOT -> "BOOLNOT"
   | PUSHTRAP (disp_t) -> Printf.sprintf "PUSHTRAP (%s)" (Disp.to_string disp_t)
   | POPTRAP -> "POPTRAP"
   | RAISE -> "RAISE"
   | CHECK_SIGNALS -> "CHECK_SIGNALS"
   | C_CALL1 (primitive_t) -> Printf.sprintf "C_CALL1 (%s)" (Primitive.to_string primitive_t)
   | C_CALL2 (primitive_t) -> Printf.sprintf "C_CALL2 (%s)" (Primitive.to_string primitive_t)
   | C_CALL3 (primitive_t) -> Printf.sprintf "C_CALL3 (%s)" (Primitive.to_string primitive_t)
   | C_CALL4 (primitive_t) -> Printf.sprintf "C_CALL4 (%s)" (Primitive.to_string primitive_t)
   | C_CALL5 (primitive_t) -> Printf.sprintf "C_CALL5 (%s)" (Primitive.to_string primitive_t)
   | C_CALLN (uint_t, primitive_t) -> Printf.sprintf "C_CALLN (%d,%s)" uint_t (Primitive.to_string primitive_t)
   | CONST0 -> "CONST0"
   | CONST1 -> "CONST1"
   | CONST2 -> "CONST2"
   | CONST3 -> "CONST3"
   | CONSTINT (sint_t) -> Printf.sprintf "CONSTINT (%d)" sint_t
   | PUSHCONST0 -> "PUSHCONST0"
   | PUSHCONST1 -> "PUSHCONST1"
   | PUSHCONST2 -> "PUSHCONST2"
   | PUSHCONST3 -> "PUSHCONST3"
   | PUSHCONSTINT (sint_t) -> Printf.sprintf "PUSHCONSTINT (%d)" sint_t
   | NEGINT -> "NEGINT"
   | ADDINT -> "ADDINT"
   | SUBINT -> "SUBINT"
   | MULINT -> "MULINT"
   | DIVINT -> "DIVINT"
   | MODINT -> "MODINT"
   | ANDINT -> "ANDINT"
   | ORINT -> "ORINT"
   | XORINT -> "XORINT"
   | LSLINT -> "LSLINT"
   | LSRINT -> "LSRINT"
   | ASRINT -> "ASRINT"
   | EQ -> "EQ"
   | NEQ -> "NEQ"
   | LTINT -> "LTINT"
   | LEINT -> "LEINT"
   | GTINT -> "GTINT"
   | GEINT -> "GEINT"
   | OFFSETINT (sint_t) -> Printf.sprintf "OFFSETINT (%d)" sint_t
   | OFFSETREF (sint_t) -> Printf.sprintf "OFFSETREF (%d)" sint_t
   | ISINT -> "ISINT"
   | GETMETHOD -> "GETMETHOD"
   | GETDYNMET -> "GETDYNMET"
   | GETPUBMET (pubmet_t) -> Printf.sprintf "GETPUBMET (%d)" pubmet_t
   | BEQ (sint_t, disp_t) -> Printf.sprintf "BEQ (%d,%s)" sint_t (Disp.to_string disp_t)
   | BNEQ (sint_t, disp_t) -> Printf.sprintf "BNEQ (%d,%s)" sint_t (Disp.to_string disp_t)
   | BLTINT (sint_t, disp_t) -> Printf.sprintf "BLTINT (%d,%s)" sint_t (Disp.to_string disp_t)
   | BLEINT (sint_t, disp_t) -> Printf.sprintf "BLEINT (%d,%s)" sint_t (Disp.to_string disp_t)
   | BGTINT (sint_t, disp_t) -> Printf.sprintf "BGTINT (%d,%s)" sint_t (Disp.to_string disp_t)
   | BGEINT (sint_t, disp_t) -> Printf.sprintf "BGEINT (%d,%s)" sint_t (Disp.to_string disp_t)
   | ULTINT -> "ULTINT"
   | UGEINT -> "UGEINT"
   | BULTINT (uint_t, disp_t) -> Printf.sprintf "BULTINT (%d,%s)" uint_t (Disp.to_string disp_t)
   | BUGEINT (uint_t, disp_t) -> Printf.sprintf "BUGEINT (%d,%s)" uint_t (Disp.to_string disp_t)
   | STOP -> "STOP"
   | EVENT -> "EVENT"
   | BREAK -> "BREAK"

end

module RAW = Printer(struct
  module Disp = struct
    let to_string t = string_of_int t
  end

  module Global = struct
    let to_string t = string_of_int t
  end

  module Primitive = struct
    let to_string (_, t) = string_of_int t
  end
end)


module Iterator( IteratorArg :
  sig
    module Disp : sig val unit : int -> unit end
    module Global : sig val unit : int -> unit end
    module Primitive : sig val unit : int*int -> unit end
  end
) = struct

  open IteratorArg

   module Closurerec = struct
      let unit (_, _, funcs) = Array.iter Disp.unit funcs
    end
    module Switch = struct
      let unit (t1,t2) =
        Array.iter Disp.unit t1;
        Array.iter Disp.unit t2
    end


  let unit op =
    match op with
   | ACC0 -> ()
   | ACC1 -> ()
   | ACC2 -> ()
   | ACC3 -> ()
   | ACC4 -> ()
   | ACC5 -> ()
   | ACC6 -> ()
   | ACC7 -> ()
   | ACC (uint_t) -> ()
   | PUSH -> ()
   | PUSHACC0 -> ()
   | PUSHACC1 -> ()
   | PUSHACC2 -> ()
   | PUSHACC3 -> ()
   | PUSHACC4 -> ()
   | PUSHACC5 -> ()
   | PUSHACC6 -> ()
   | PUSHACC7 -> ()
   | PUSHACC (uint_t) -> ()
   | POP (uint_t) -> ()
   | ASSIGN (uint_t) -> ()
   | ENVACC1 -> ()
   | ENVACC2 -> ()
   | ENVACC3 -> ()
   | ENVACC4 -> ()
   | ENVACC (uint_t) -> ()
   | PUSHENVACC1 -> ()
   | PUSHENVACC2 -> ()
   | PUSHENVACC3 -> ()
   | PUSHENVACC4 -> ()
   | PUSHENVACC (uint_t) -> ()
   | PUSH_RETADDR (disp_t) -> Disp.unit disp_t
   | APPLY (uint_t) -> ()
   | APPLY1 -> ()
   | APPLY2 -> ()
   | APPLY3 -> ()
   | APPTERM (uint1, uint2) -> ()
   | APPTERM1 (uint_t) -> ()
   | APPTERM2 (uint_t) -> ()
   | APPTERM3 (uint_t) -> ()
   | RETURN (uint_t) -> ()
   | RESTART -> ()
   | GRAB (uint_t) -> ()
   | CLOSURE (uint_t, disp_t) -> Disp.unit disp_t
   | CLOSUREREC (closurerec_t) -> Closurerec.unit closurerec_t
   | OFFSETCLOSUREM2 -> ()
   | OFFSETCLOSURE0 -> ()
   | OFFSETCLOSURE2 -> ()
   | OFFSETCLOSURE (sint_t) -> ()
   | PUSHOFFSETCLOSUREM2 -> ()
   | PUSHOFFSETCLOSURE0 -> ()
   | PUSHOFFSETCLOSURE2 -> ()
   | PUSHOFFSETCLOSURE (sint_t) -> ()
   | GETGLOBAL (global_t) -> Global.unit global_t
   | PUSHGETGLOBAL (global_t) -> Global.unit global_t
   | GETGLOBALFIELD (global_t, uint_t) -> Global.unit global_t
   | PUSHGETGLOBALFIELD (global_t, uint_t) -> Global.unit global_t
   | SETGLOBAL (global_t) -> Global.unit global_t
   | ATOM0 -> ()
   | ATOM (uint_t) -> ()
   | PUSHATOM0 -> ()
   | PUSHATOM (uint_t) -> ()
   | MAKEBLOCK (uint_t1, uint_t2) -> ()
   | MAKEBLOCK1 (uint_t) -> ()
   | MAKEBLOCK2 (uint_t) -> ()
   | MAKEBLOCK3 (uint_t) -> ()
   | MAKEFLOATBLOCK (uint_t) -> ()
   | GETFIELD0 -> ()
   | GETFIELD1 -> ()
   | GETFIELD2 -> ()
   | GETFIELD3 -> ()
   | GETFIELD (uint_t) -> ()
   | GETFLOATFIELD (uint_t) -> ()
   | SETFIELD0 -> ()
   | SETFIELD1 -> ()
   | SETFIELD2 -> ()
   | SETFIELD3 -> ()
   | SETFIELD (uint_t) -> ()
   | SETFLOATFIELD (uint_t) -> ()
   | VECTLENGTH -> ()
   | GETVECTITEM -> ()
   | SETVECTITEM -> ()
   | GETSTRINGCHAR -> ()
   | SETSTRINGCHAR -> ()
   | BRANCH (disp_t) -> Disp.unit disp_t
   | BRANCHIF (disp_t) -> Disp.unit disp_t
   | BRANCHIFNOT (disp_t) -> Disp.unit disp_t
   | SWITCH (switch_t) -> Switch.unit switch_t
   | BOOLNOT -> ()
   | PUSHTRAP (disp_t) -> Disp.unit disp_t
   | POPTRAP -> ()
   | RAISE -> ()
   | CHECK_SIGNALS -> ()
   | C_CALL1 (primitive_t) -> Primitive.unit primitive_t
   | C_CALL2 (primitive_t) -> Primitive.unit primitive_t
   | C_CALL3 (primitive_t) -> Primitive.unit primitive_t
   | C_CALL4 (primitive_t) -> Primitive.unit primitive_t
   | C_CALL5 (primitive_t) -> Primitive.unit primitive_t
   | C_CALLN (uint_t, primitive_t) -> Primitive.unit primitive_t
   | CONST0 -> ()
   | CONST1 -> ()
   | CONST2 -> ()
   | CONST3 -> ()
   | CONSTINT (sint_t) -> ()
   | PUSHCONST0 -> ()
   | PUSHCONST1 -> ()
   | PUSHCONST2 -> ()
   | PUSHCONST3 -> ()
   | PUSHCONSTINT (sint_t) -> ()
   | NEGINT -> ()
   | ADDINT -> ()
   | SUBINT -> ()
   | MULINT -> ()
   | DIVINT -> ()
   | MODINT -> ()
   | ANDINT -> ()
   | ORINT -> ()
   | XORINT -> ()
   | LSLINT -> ()
   | LSRINT -> ()
   | ASRINT -> ()
   | EQ -> ()
   | NEQ -> ()
   | LTINT -> ()
   | LEINT -> ()
   | GTINT -> ()
   | GEINT -> ()
   | OFFSETINT (sint_t) -> ()
   | OFFSETREF (sint_t) -> ()
   | ISINT -> ()
   | GETMETHOD -> ()
   | GETDYNMET -> ()
   | GETPUBMET (pubmet_t) -> ()
   | BEQ (sint_t, disp_t) -> Disp.unit disp_t
   | BNEQ (sint_t, disp_t) -> Disp.unit disp_t
   | BLTINT (sint_t, disp_t) -> Disp.unit disp_t
   | BLEINT (sint_t, disp_t) -> Disp.unit disp_t
   | BGTINT (sint_t, disp_t) -> Disp.unit disp_t
   | BGEINT (sint_t, disp_t) -> Disp.unit disp_t
   | ULTINT -> ()
   | UGEINT -> ()
   | BULTINT (uint_t, disp_t) -> Disp.unit disp_t
   | BUGEINT (uint_t, disp_t) -> Disp.unit disp_t
   | STOP -> ()
   | EVENT -> ()
   | BREAK -> ()

end

module UnitNothing = struct let unit _ = () end

module IterPrimitives(M: sig
  val unit : Primitive.t -> unit
end) = Iterator(struct
  module Disp = UnitNothing
  module Global = UnitNothing
  module Primitive = M
end)

module IterGlobals(M: sig
  val unit : Global.t -> unit
end) = Iterator(struct
  module Disp = UnitNothing
  module Global = M
  module Primitive = UnitNothing
end)

module IterDisp(M: sig
  val unit : int -> unit
end) = Iterator(struct
  module Disp = M
  module Global = UnitNothing
  module Primitive = UnitNothing
end)
