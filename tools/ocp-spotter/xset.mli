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

(* Set with binary custom search *)

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  (* They use Obj.magic, so unsafe! *)
  val unsafe_binary : t -> (t * elt * t) option
  val unsafe_middle : t -> elt option
  val unsafe_find : elt -> t -> elt option
end

module Make(Ord : OrderedType) : S 
  with type elt = Ord.t
