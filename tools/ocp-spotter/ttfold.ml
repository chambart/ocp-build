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

open Asttypes
open Types
open Typedtree

class fold =
  object ((self : 'self))
    method ref : 'a1. ('self -> 'a1 -> 'self) -> 'a1 ref -> 'self =
      fun fa r -> fa self !r
    method option : 'a1. ('self -> 'a1 -> 'self) -> 'a1 option -> 'self =
      fun fa -> function
        | None -> self
        | Some v -> fa self v
    method list : 'a1. ('self -> 'a1 -> 'self) -> 'a1 list -> 'self =
      fun fa l -> List.fold_left fa self l
    method pattern : pattern -> 'self =
      fun __value ->
        let self = self#pattern_desc __value.pat_desc in
        let self =
          self#list
            (fun self (__x1, __x2) -> let self = self#pat_extra __x1 in self)
            __value.pat_extra
        in self
    method pat_extra : pat_extra -> 'self =
      fun __value ->
        match __value with
        | Tpat_constraint __x1 -> let self = self#core_type __x1 in self
        | Tpat_type (__x1, __x2) -> self
        | Tpat_unpack -> self
    method pattern_desc : pattern_desc -> 'self =
      fun __value ->
        match __value with
        | Tpat_any -> self
        | Tpat_var (__x1, __x2) -> self
        | Tpat_alias (__x1, __x2, __x3) ->
            let self = self#pattern __x1 in self
        | Tpat_constant __x1 -> self
        | Tpat_tuple __x1 ->
            let self = self#list (fun self -> self#pattern) __x1 in self
        | Tpat_construct (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#list (fun self -> self#pattern) __x4 in self
        | Tpat_variant (__x1, __x2, __x3) ->
            let self = self#option (fun self -> self#pattern) __x2 in
            let self = self#ref (fun self _ -> self) __x3 in self
        | Tpat_record (__x1, __x2) ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3, __x4) ->
                   let self = self#pattern __x4 in self)
                __x1
            in self
        | Tpat_array __x1 ->
            let self = self#list (fun self -> self#pattern) __x1 in self
        | Tpat_or (__x1, __x2, __x3) ->
            let self = self#pattern __x1 in
            let self = self#pattern __x2 in
            let self = self#option (fun self _ -> self) __x3 in self
        | Tpat_lazy __x1 -> let self = self#pattern __x1 in self
    method expression : expression -> 'self =
      fun __value ->
        let self = self#expression_desc __value.exp_desc in
        let self =
          self#list
            (fun self (__x1, __x2) -> let self = self#exp_extra __x1 in self)
            __value.exp_extra
        in self
    method exp_extra : exp_extra -> 'self =
      fun __value ->
        match __value with
        | Texp_constraint (__x1, __x2) ->
            let self = self#option (fun self -> self#core_type) __x1 in
            let self = self#option (fun self -> self#core_type) __x2 in self
        | Texp_open (__x1, __x2, __x3) -> self
        | Texp_poly __x1 ->
            let self = self#option (fun self -> self#core_type) __x1 in self
        | Texp_newtype __x1 -> self
    method expression_desc : expression_desc -> 'self =
      fun __value ->
        match __value with
        | Texp_ident (__x1, __x2, __x3) -> self
        | Texp_constant __x1 -> self
        | Texp_let (__x1, __x2, __x3) ->
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#pattern __x1 in
                   let self = self#expression __x2 in self)
                __x2 in
            let self = self#expression __x3 in self
        | Texp_function (__x1, __x2, __x3) ->
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#pattern __x1 in
                   let self = self#expression __x2 in self)
                __x2
            in self
        | Texp_apply (__x1, __x2) ->
            let self = self#expression __x1 in
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#option (fun self -> self#expression) __x2
                   in self)
                __x2
            in self
        | Texp_match (__x1, __x2, __x3) ->
            let self = self#expression __x1 in
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#pattern __x1 in
                   let self = self#expression __x2 in self)
                __x2
            in self
        | Texp_try (__x1, __x2) ->
            let self = self#expression __x1 in
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#pattern __x1 in
                   let self = self#expression __x2 in self)
                __x2
            in self
        | Texp_tuple __x1 ->
            let self = self#list (fun self -> self#expression) __x1 in self
        | Texp_construct (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#list (fun self -> self#expression) __x4 in self
        | Texp_variant (__x1, __x2) ->
            let self = self#option (fun self -> self#expression) __x2 in self
        | Texp_record (__x1, __x2) ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3, __x4) ->
                   let self = self#expression __x4 in self)
                __x1 in
            let self = self#option (fun self -> self#expression) __x2 in self
        | Texp_field (__x1, __x2, __x3, __x4) ->
            let self = self#expression __x1 in self
        | Texp_setfield (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#expression __x1 in
            let self = self#expression __x5 in self
        | Texp_array __x1 ->
            let self = self#list (fun self -> self#expression) __x1 in self
        | Texp_ifthenelse (__x1, __x2, __x3) ->
            let self = self#expression __x1 in
            let self = self#expression __x2 in
            let self = self#option (fun self -> self#expression) __x3 in self
        | Texp_sequence (__x1, __x2) ->
            let self = self#expression __x1 in
            let self = self#expression __x2 in self
        | Texp_while (__x1, __x2) ->
            let self = self#expression __x1 in
            let self = self#expression __x2 in self
        | Texp_for (__x1, __x2, __x3, __x4, __x5, __x6) ->
            let self = self#expression __x3 in
            let self = self#expression __x4 in
            let self = self#expression __x6 in self
        | Texp_when (__x1, __x2) ->
            let self = self#expression __x1 in
            let self = self#expression __x2 in self
        | Texp_send (__x1, __x2, __x3) ->
            let self = self#expression __x1 in
            let self = self#meth __x2 in
            let self = self#option (fun self -> self#expression) __x3 in self
        | Texp_new (__x1, __x2, __x3) -> self
        | Texp_instvar (__x1, __x2, __x3) -> self
        | Texp_setinstvar (__x1, __x2, __x3, __x4) ->
            let self = self#expression __x4 in self
        | Texp_override (__x1, __x2) ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#expression __x3 in self)
                __x2
            in self
        | Texp_letmodule (__x1, __x2, __x3, __x4) ->
            let self = self#module_expr __x3 in
            let self = self#expression __x4 in self
        | Texp_assert __x1 -> let self = self#expression __x1 in self
        | Texp_assertfalse -> self
        | Texp_lazy __x1 -> let self = self#expression __x1 in self
        | Texp_object (__x1, __x2) ->
            let self = self#class_structure __x1 in
            let self = self#list (fun self _ -> self) __x2 in self
        | Texp_pack __x1 -> let self = self#module_expr __x1 in self
    method meth : meth -> 'self = fun __value -> self
    method class_expr : class_expr -> 'self =
      fun __value -> let self = self#class_expr_desc __value.cl_desc in self
    method class_expr_desc : class_expr_desc -> 'self =
      fun __value ->
        match __value with
        | Tcl_ident (__x1, __x2, __x3) ->
            let self = self#list (fun self -> self#core_type) __x3 in self
        | Tcl_structure __x1 -> let self = self#class_structure __x1 in self
        | Tcl_fun (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#pattern __x2 in
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#expression __x3 in self)
                __x3 in
            let self = self#class_expr __x4 in self
        | Tcl_apply (__x1, __x2) ->
            let self = self#class_expr __x1 in
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#option (fun self -> self#expression) __x2
                   in self)
                __x2
            in self
        | Tcl_let (__x1, __x2, __x3, __x4) ->
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#pattern __x1 in
                   let self = self#expression __x2 in self)
                __x2 in
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#expression __x3 in self)
                __x3 in
            let self = self#class_expr __x4 in self
        | Tcl_constraint (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#class_expr __x1 in
            let self = self#option (fun self -> self#class_type) __x2 in
            let self = self#list (fun self _ -> self) __x3 in
            let self = self#list (fun self _ -> self) __x4 in self
    method class_structure : class_structure -> 'self =
      fun __value ->
        let self = self#pattern __value.cstr_pat in
        let self =
          self#list (fun self -> self#class_field) __value.cstr_fields
        in self
    method class_field : class_field -> 'self =
      fun __value -> let self = self#class_field_desc __value.cf_desc in self
    method class_field_kind : class_field_kind -> 'self =
      fun __value ->
        match __value with
        | Tcfk_virtual __x1 -> let self = self#core_type __x1 in self
        | Tcfk_concrete __x1 -> let self = self#expression __x1 in self
    method class_field_desc : class_field_desc -> 'self =
      fun __value ->
        match __value with
        | Tcf_inher (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#class_expr __x2 in
            let self = self#option (fun self _ -> self) __x3 in
            let self = self#list (fun self (__x1, __x2) -> self) __x4 in
            let self = self#list (fun self (__x1, __x2) -> self) __x5 in self
        | Tcf_val (__x1, __x2, __x3, __x4, __x5, __x6) ->
            let self = self#class_field_kind __x5 in self
        | Tcf_meth (__x1, __x2, __x3, __x4, __x5) ->
            let self = self#class_field_kind __x4 in self
        | Tcf_constr (__x1, __x2) ->
            let self = self#core_type __x1 in
            let self = self#core_type __x2 in self
        | Tcf_init __x1 -> let self = self#expression __x1 in self
    method module_expr : module_expr -> 'self =
      fun __value ->
        let self = self#module_expr_desc __value.mod_desc in self
    method module_type_constraint : module_type_constraint -> 'self =
      fun __value ->
        match __value with
        | Tmodtype_implicit -> self
        | Tmodtype_explicit __x1 -> let self = self#module_type __x1 in self
    method module_expr_desc : module_expr_desc -> 'self =
      fun __value ->
        match __value with
        | Tmod_ident (__x1, __x2) -> self
        | Tmod_structure __x1 -> let self = self#structure __x1 in self
        | Tmod_functor (__x1, __x2, __x3, __x4) ->
            let self = self#module_type __x3 in
            let self = self#module_expr __x4 in self
        | Tmod_apply (__x1, __x2, __x3) ->
            let self = self#module_expr __x1 in
            let self = self#module_expr __x2 in
            let self = self#module_coercion __x3 in self
        | Tmod_constraint (__x1, __x2, __x3, __x4) ->
            let self = self#module_expr __x1 in
            let self = self#module_type_constraint __x3 in
            let self = self#module_coercion __x4 in self
        | Tmod_unpack (__x1, __x2) -> let self = self#expression __x1 in self
    method structure : structure -> 'self =
      fun __value ->
        let self =
          self#list (fun self -> self#structure_item) __value.str_items
        in self
    method structure_item : structure_item -> 'self =
      fun __value ->
        let self = self#structure_item_desc __value.str_desc in self
    method structure_item_desc : structure_item_desc -> 'self =
      fun __value ->
        match __value with
        | Tstr_eval __x1 -> let self = self#expression __x1 in self
        | Tstr_value (__x1, __x2) ->
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#pattern __x1 in
                   let self = self#expression __x2 in self)
                __x2
            in self
        | Tstr_primitive (__x1, __x2, __x3) ->
            let self = self#value_description __x3 in self
        | Tstr_type __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#type_declaration __x3 in self)
                __x1
            in self
        | Tstr_exception (__x1, __x2, __x3) ->
            let self = self#exception_declaration __x3 in self
        | Tstr_exn_rebind (__x1, __x2, __x3, __x4) -> self
        | Tstr_module (__x1, __x2, __x3) ->
            let self = self#module_expr __x3 in self
        | Tstr_recmodule __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3, __x4) ->
                   let self = self#module_type __x3 in
                   let self = self#module_expr __x4 in self)
                __x1
            in self
        | Tstr_modtype (__x1, __x2, __x3) ->
            let self = self#module_type __x3 in self
        | Tstr_open (__x1, __x2) -> self
        | Tstr_class __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#class_declaration __x1 in
                   let self = self#list (fun self _ -> self) __x2 in self)
                __x1
            in self
        | Tstr_class_type __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#class_type_declaration __x3 in self)
                __x1
            in self
        | Tstr_include (__x1, __x2) ->
            let self = self#module_expr __x1 in
            let self = self#list (fun self _ -> self) __x2 in self
    method module_coercion : module_coercion -> 'self =
      fun __value ->
        match __value with
        | Tcoerce_none -> self
        | Tcoerce_structure __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2) ->
                   let self = self#module_coercion __x2 in self)
                __x1
            in self
        | Tcoerce_functor (__x1, __x2) ->
            let self = self#module_coercion __x1 in
            let self = self#module_coercion __x2 in self
        | Tcoerce_primitive __x1 -> self
    method module_type : module_type -> 'self =
      fun __value ->
        let self = self#module_type_desc __value.mty_desc in self
    method module_type_desc : module_type_desc -> 'self =
      fun __value ->
        match __value with
        | Tmty_ident (__x1, __x2) -> self
        | Tmty_signature __x1 -> let self = self#signature __x1 in self
        | Tmty_functor (__x1, __x2, __x3, __x4) ->
            let self = self#module_type __x3 in
            let self = self#module_type __x4 in self
        | Tmty_with (__x1, __x2) ->
            let self = self#module_type __x1 in
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#with_constraint __x3 in self)
                __x2
            in self
        | Tmty_typeof __x1 -> let self = self#module_expr __x1 in self
    method signature : signature -> 'self =
      fun __value ->
        let self =
          self#list (fun self -> self#signature_item) __value.sig_items
        in self
    method signature_item : signature_item -> 'self =
      fun __value ->
        let self = self#signature_item_desc __value.sig_desc in self
    method signature_item_desc : signature_item_desc -> 'self =
      fun __value ->
        match __value with
        | Tsig_value (__x1, __x2, __x3) ->
            let self = self#value_description __x3 in self
        | Tsig_type __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#type_declaration __x3 in self)
                __x1
            in self
        | Tsig_exception (__x1, __x2, __x3) ->
            let self = self#exception_declaration __x3 in self
        | Tsig_module (__x1, __x2, __x3) ->
            let self = self#module_type __x3 in self
        | Tsig_recmodule __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3) ->
                   let self = self#module_type __x3 in self)
                __x1
            in self
        | Tsig_modtype (__x1, __x2, __x3) ->
            let self = self#modtype_declaration __x3 in self
        | Tsig_open (__x1, __x2) -> self
        | Tsig_include (__x1, __x2) ->
            let self = self#module_type __x1 in self
        | Tsig_class __x1 ->
            let self = self#list (fun self -> self#class_description) __x1
            in self
        | Tsig_class_type __x1 ->
            let self =
              self#list (fun self -> self#class_type_declaration) __x1
            in self
    method modtype_declaration : modtype_declaration -> 'self =
      fun __value ->
        match __value with
        | Tmodtype_abstract -> self
        | Tmodtype_manifest __x1 -> let self = self#module_type __x1 in self
    method with_constraint : with_constraint -> 'self =
      fun __value ->
        match __value with
        | Twith_type __x1 -> let self = self#type_declaration __x1 in self
        | Twith_module (__x1, __x2) -> self
        | Twith_typesubst __x1 ->
            let self = self#type_declaration __x1 in self
        | Twith_modsubst (__x1, __x2) -> self
    method core_type : core_type -> 'self =
      fun __value -> let self = self#core_type_desc __value.ctyp_desc in self
    method core_type_desc : core_type_desc -> 'self =
      fun __value ->
        match __value with
        | Ttyp_any -> self
        | Ttyp_var __x1 -> self
        | Ttyp_arrow (__x1, __x2, __x3) ->
            let self = self#core_type __x2 in
            let self = self#core_type __x3 in self
        | Ttyp_tuple __x1 ->
            let self = self#list (fun self -> self#core_type) __x1 in self
        | Ttyp_constr (__x1, __x2, __x3) ->
            let self = self#list (fun self -> self#core_type) __x3 in self
        | Ttyp_object __x1 ->
            let self = self#list (fun self -> self#core_field_type) __x1
            in self
        | Ttyp_class (__x1, __x2, __x3, __x4) ->
            let self = self#list (fun self -> self#core_type) __x3 in
            let self = self#list (fun self _ -> self) __x4 in self
        | Ttyp_alias (__x1, __x2) -> let self = self#core_type __x1 in self
        | Ttyp_variant (__x1, __x2, __x3) ->
            let self = self#list (fun self -> self#row_field) __x1 in
            let self = self#option (fun self -> self#list (fun self _ -> self)) __x3
            in self
        | Ttyp_poly (__x1, __x2) ->
            let self = self#list (fun self _ -> self) __x1 in
            let self = self#core_type __x2 in self
        | Ttyp_package __x1 -> let self = self#package_type __x1 in self
    method package_type : package_type -> 'self =
      fun __value ->
        let self =
          self#list
            (fun self (__x1, __x2) -> let self = self#core_type __x2 in self)
            __value.pack_fields
        in self
    method core_field_type : core_field_type -> 'self =
      fun __value ->
        let self = self#core_field_desc __value.field_desc in self
    method core_field_desc : core_field_desc -> 'self =
      fun __value ->
        match __value with
        | Tcfield (__x1, __x2) -> let self = self#core_type __x2 in self
        | Tcfield_var -> self
    method row_field : row_field -> 'self =
      fun __value ->
        match __value with
        | Ttag (__x1, __x2, __x3) ->
            let self = self#list (fun self -> self#core_type) __x3 in self
        | Tinherit __x1 -> let self = self#core_type __x1 in self
    method value_description : value_description -> 'self =
      fun __value ->
        let self = self#core_type __value.val_desc in
        let self = self#list (fun self _ -> self) __value.val_prim in self
    method type_declaration : type_declaration -> 'self =
      fun __value ->
        let self =
          self#list (fun self -> self#option (fun self _ -> self)) __value.typ_params in
        let self =
          self#list
            (fun self (__x1, __x2, __x3) ->
               let self = self#core_type __x1 in
               let self = self#core_type __x2 in self)
            __value.typ_cstrs in
        let self = self#type_kind __value.typ_kind in
        let self =
          self#option (fun self -> self#core_type) __value.typ_manifest in
        let self =
          self#list (fun self (__x1, __x2) -> self) __value.typ_variance
        in self
    method type_kind : type_kind -> 'self =
      fun __value ->
        match __value with
        | Ttype_abstract -> self
        | Ttype_variant __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3, __x4) ->
                   let self = self#list (fun self -> self#core_type) __x3
                   in self)
                __x1
            in self
        | Ttype_record __x1 ->
            let self =
              self#list
                (fun self (__x1, __x2, __x3, __x4, __x5) ->
                   let self = self#core_type __x4 in self)
                __x1
            in self
    method exception_declaration : exception_declaration -> 'self =
      fun __value ->
        let self = self#list (fun self -> self#core_type) __value.exn_params
        in self
    method class_type : class_type -> 'self =
      fun __value ->
        let self = self#class_type_desc __value.cltyp_desc in self
    method class_type_desc : class_type_desc -> 'self =
      fun __value ->
        match __value with
        | Tcty_constr (__x1, __x2, __x3) ->
            let self = self#list (fun self -> self#core_type) __x3 in self
        | Tcty_signature __x1 -> let self = self#class_signature __x1 in self
        | Tcty_fun (__x1, __x2, __x3) ->
            let self = self#core_type __x2 in
            let self = self#class_type __x3 in self
    method class_signature : class_signature -> 'self =
      fun __value ->
        let self = self#core_type __value.csig_self in
        let self =
          self#list (fun self -> self#class_type_field) __value.csig_fields
        in self
    method class_type_field : class_type_field -> 'self =
      fun __value ->
        let self = self#class_type_field_desc __value.ctf_desc in self
    method class_type_field_desc : class_type_field_desc -> 'self =
      fun __value ->
        match __value with
        | Tctf_inher __x1 -> let self = self#class_type __x1 in self
        | Tctf_val __x1 ->
            let self =
              (fun (__x1, __x2, __x3, __x4) ->
                 let self = self#core_type __x4 in self)
                __x1
            in self
        | Tctf_virt __x1 ->
            let self =
              (fun (__x1, __x2, __x3) ->
                 let self = self#core_type __x3 in self)
                __x1
            in self
        | Tctf_meth __x1 ->
            let self =
              (fun (__x1, __x2, __x3) ->
                 let self = self#core_type __x3 in self)
                __x1
            in self
        | Tctf_cstr __x1 ->
            let self =
              (fun (__x1, __x2) ->
                 let self = self#core_type __x1 in
                 let self = self#core_type __x2 in self)
                __x1
            in self
    method class_declaration : class_declaration -> 'self =
      fun __value -> self#class_infos (fun self -> self#class_expr) __value
    method class_description : class_description -> 'self =
      fun __value -> self#class_infos (fun self -> self#class_type) __value
    method class_type_declaration : class_type_declaration -> 'self =
      fun __value -> self#class_infos (fun self -> self#class_type) __value
    method class_infos :
      'a. ('self -> 'a -> 'self) -> 'a class_infos -> 'self =
      fun __f_a __value ->
        let self =
          (fun (__x1, __x2) -> let self = self#list (fun self _ -> self) __x1 in self)
            __value.ci_params in
        let self = __f_a self __value.ci_expr in
        let self =
          self#list (fun self (__x1, __x2) -> self) __value.ci_variance
        in self
  end
