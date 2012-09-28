(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

type t

(*
val read : (string -> t)
*)
let read string = assert false

(*
val write : (string -> (t -> unit))
*)
let write string t = assert false


    let cmo_magic_number_001 = "Caml1999O001" (* csl 1.06 - csl 1.07 *)
    let cmo_magic_number_002 = "Caml1999O002" (* csl 1.10 - csl 1.15 *)
    let cmo_magic_number_003 = "Caml1999O003" (* 1.00 - 1.05 *)
    let cmo_magic_number_004 = "Caml1999O004" (* 1.06 - 2.99 *)
    let cmo_magic_number_005 = "Caml1999O005" (* 3.00 - 3.03-alpha *)
    let cmo_magic_number_006 = "Caml1999O006" (* 3.04 - 3.07-beta1, 3.07beta2, 3.07-pl2 *)
    let cmo_magic_number_007 = "Caml1999O007" (* 3.08 - 3.12.1 *)
(*    let cmo_magic_number_008 = "Caml1999O008" (* 3.11.1-rc0 - 3.12 *) *)

