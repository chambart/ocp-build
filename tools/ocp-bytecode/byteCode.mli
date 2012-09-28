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

module Uint : sig type t = int end

module Sint  : sig type t = int end

module Disp  : sig type t = int end

module Closurerec  : sig
  type t =
      int (* nfuncs *)
      * int (* nvars *)
      * int array
end

module Global : sig type t = int end

module Switch : sig
  type t = int array * int array
end

module Primitive : sig
  type t = int * int  (* position * primitive_number *)
end

module Pubmet  : sig type t = int end

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

val iter : (int -> (* position / 4 *)
            opcode -> (* opcode at that position *)
            unit) -> ByteFile.t -> unit

module RAW : sig
  val string_of_opcode : opcode -> string
end

module Printer :
  functor
           (PrinterArg : sig
                           module Disp :
                             sig val to_string : Disp.t -> string end
                           module Global :
                             sig val to_string : Global.t -> string end
                           module Primitive :
                             sig val to_string : Primitive.t -> string end
                         end) ->
           sig val string_of_opcode : opcode -> string end

module Iterator : functor
           (IteratorArg : sig
                            module Disp : sig val unit : Disp.t -> unit end
                            module Global : sig val unit : Global.t -> unit end
                            module Primitive : sig val unit : Primitive.t -> unit end
                          end) ->
           sig val unit : opcode -> unit end

module IterDisp :
  functor (M : sig val unit : Disp.t -> unit end) ->
           sig
             val unit : opcode -> unit
           end
module IterGlobals :
  functor (M : sig val unit : Global.t -> unit end) ->
           sig
             val unit : opcode -> unit
           end
module IterPrimitives :
  functor (M : sig val unit : Primitive.t -> unit end) ->
           sig
             val unit : opcode -> unit
           end
