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

(* Annotations

   Annotations are stored in .spot with their locations
*)

open Utils
open Ext
open Format

let magic_number = "OCamlSpot"
let ocaml_version = "4.00.0"
let version = "2.0.0"

module Kind = struct
  type t =
    | Value | Type | Exception
    | Module | Module_type
    | Class | Class_type

  let to_string = function
    | Value       -> "v"
    | Type        -> "t"
    | Exception   -> "e"
    | Module      -> "m"
    | Module_type -> "mt"
    | Class       -> "c"
    | Class_type  -> "ct"

  (* for messages *)
  let name = function
    | Value       -> "value"
    | Type        -> "type"
    | Exception   -> "exception"
    | Module      -> "module"
    | Module_type -> "module_type"
    | Class       -> "class"
    | Class_type  -> "class_type"

  (* used for query interface *)
  let from_string = function
    | "v"  | "value"       -> Value
    | "t"  | "type"        -> Type
    | "e"  | "exception"   -> Exception
    | "m"  | "module"      -> Module
    | "mt" | "module_type" -> Module_type
    | "c"  | "class"       -> Class
    | "ct" | "class_type"  -> Class_type
    | _ -> raise Not_found
end

(* CR jfuruse: ultimately we do not need this *)
module Abstraction = struct
  (* module definition abstraction *)

  (* CR jfuruse: types may be incompatible between compiler versions *)
  type module_expr =
    | AMod_ident      of Path.t (* module M = N *)
    | AMod_packed     of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | AMod_structure  of structure (* module M = struct ... end *)
    | AMod_functor    of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | AMod_apply      of module_expr * module_expr (* module M = N(O) *)
    | AMod_constraint of module_expr * Types.module_type
    | AMod_unpack     of module_expr
    | AMod_abstract (* used for Tmodtype_abstract *)
    | AMod_functor_parameter

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  (* modtype must be identified from module, since they can have the
     same name *)

  and structure_item =
    | AStr_value      of Ident.t
    | AStr_type       of Ident.t
    | AStr_exception  of Ident.t
    | AStr_module     of Ident.t * module_expr
    | AStr_modtype    of Ident.t * module_expr
    | AStr_class      of Ident.t
    | AStr_class_type of Ident.t
    | AStr_included   of Ident.t * module_expr * Kind.t * Ident.t

  let rec format_module_expr ppf = function
    | AMod_ident p -> fprintf ppf "%s" (Path.name p)
    | AMod_packed s -> fprintf ppf "packed(%s)" s
    | AMod_structure str -> format_structure ppf str
    | AMod_functor (id, mty, mexp) ->
        fprintf ppf "@[<4>\\(%s : %a) ->@ %a@]"
	  (Ident.name id)
          (Printtyp.modtype ~with_pos:true) mty
          format_module_expr mexp
    | AMod_apply (mexp1, mexp2) ->
        fprintf ppf "%a(%a)"
          format_module_expr mexp1
          format_module_expr mexp2
    | AMod_constraint (mexp, mty) ->
        fprintf ppf "@[%a@ :@ @[%a@]@]"
          format_module_expr mexp
          (Printtyp.modtype ~with_pos:true) mty
    | AMod_abstract -> fprintf ppf "<abst>"
    | AMod_functor_parameter -> fprintf ppf "<functor_parameter>"
    | AMod_unpack mty ->
        fprintf ppf "@[unpack@ : @[%a@]@]"
          format_module_expr mty

  and format_structure ppf items =
    fprintf ppf "{ @[<v>%a@] }"
      (list ";@," format_structure_item) items

  and format_structure_item ppf = function
    | AStr_value id -> fprintf ppf "val %s" (Ident.name id)
    | AStr_type id -> fprintf ppf "type %s" (Ident.name id) (* CR jfuruse: todo *)
    | AStr_exception id -> fprintf ppf "exception %s" (Ident.name id)
    | AStr_module (id, mexp) ->
        fprintf ppf "@[<v4>module %s =@ %a@]"
          (Ident.name id)
          format_module_expr mexp
    | AStr_modtype (id, mexp) ->
        fprintf ppf "@[<v4>module type %s =@ %a@]"
          (Ident.name id)
          format_module_expr mexp
    | AStr_class id -> fprintf ppf "class %s" (Ident.name id)
    | AStr_class_type id -> fprintf ppf "class type %s" (Ident.name id)
    | AStr_included (id, mexp, kind, id') ->
        fprintf ppf "@[<v4>included %s %a = %a@ { @[<v>%a@] }@]"
          (Kind.name kind)
          Ident.format id
          Ident.format id'
          format_module_expr mexp

  let ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option = function
    | AStr_value id        -> Some (Kind.Value, id)
    | AStr_type id         -> Some (Kind.Type, id)
    | AStr_exception id    -> Some (Kind.Exception, id)
    | AStr_module (id, _)  -> Some (Kind.Module, id)
    | AStr_modtype (id, _) -> Some (Kind.Module_type, id)
    | AStr_class id        -> Some (Kind.Class, id)
    | AStr_class_type id   -> Some (Kind.Class_type, id)
    | AStr_included (id, _, kind, _) -> Some (kind, id)

  module Module_expr = struct
    (* cache key is Typedtree.module_expr *)
    module M = struct
      type t = Typedtree.module_expr
      let equal m1 m2 = m1 == m2
      let hash_source m = m.Typedtree.mod_loc
      let hash m = Hashtbl.hash (hash_source m)
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  module Structure_item = struct
    (* cache key is Abstraction.structure_item, not Typedtree.structure_item *)
    module M = struct
      type t = structure_item
      let equal s1 s2 =
	match s1, s2 with
	| AStr_value id1, AStr_value id2
	| AStr_type id1, AStr_type id2
	| AStr_exception id1, AStr_exception id2
	| AStr_class id1, AStr_class id2
	| AStr_class_type id1, AStr_class_type id2 -> id1 = id2
	| AStr_module (id1, mexp1) , AStr_module (id2, mexp2) ->
	    id1 = id2 && Module_expr.equal mexp1 mexp2
	| AStr_modtype (id1, mty1), AStr_modtype (id2, mty2) ->
            id1 = id2 && Module_expr.equal mty1 mty2
	| AStr_included (id1, mexp1, kind1, id1'), AStr_included (id2, mexp2, kind2, id2') ->
            id1 = id2 && kind1 = kind2 && id1' = id2'
            && Module_expr.equal mexp1 mexp2
	| (AStr_value _ | AStr_type _ | AStr_exception _ | AStr_modtype _
	  | AStr_class _ | AStr_class_type _ | AStr_module _ | AStr_included _),
	  (AStr_value _ | AStr_type _ | AStr_exception _ | AStr_modtype _
	  | AStr_class _ | AStr_class_type _ | AStr_module _ | AStr_included _) -> false

      let hash = Hashtbl.hash
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  open Types
  open Typedtree
  open Asttypes

  let cache_module_expr = Module_expr.Table.create 31
  let cache_structure_item = Structure_item.Table.create 31

  let clear_cache () =
    Module_expr.Table.clear cache_module_expr;
    Structure_item.Table.clear cache_structure_item

  module T = struct
    let kident_of_sigitem = function
      | Sig_value (id, _)         -> Kind.Value, id
      | Sig_exception (id, _)     -> Kind.Exception, id
      | Sig_module (id, _, _)     -> Kind.Module, id
      | Sig_type (id, _, _)       -> Kind.Type, id
      | Sig_modtype (id, _)       -> Kind.Module_type, id
      | Sig_class (id, _, _)      -> Kind.Class, id
      | Sig_class_type (id, _, _) -> Kind.Class_type, id

    let rec signature sg = AMod_structure (List.map signature_item sg)

    and signature_item = function
      | Sig_value (id, _) -> AStr_value id
      | Sig_type (id, _, _) -> AStr_type id
      | Sig_exception (id, _) -> AStr_exception id
      | Sig_module (id, mty, _) -> AStr_module (id, module_type mty)
      | Sig_modtype (id, mdtd) -> AStr_modtype (id, modtype_declaration mdtd)
      | Sig_class (id, _, _) -> AStr_class id
      | Sig_class_type (id, _, _) -> AStr_class_type id

    and module_type = function
      | Mty_ident p -> AMod_ident p
      | Mty_signature sg -> signature sg
      | Mty_functor (id, mty1, mty2) -> AMod_functor(id, mty1, module_type mty2)

    and modtype_declaration = function
      | Modtype_abstract -> AMod_structure []
      | Modtype_manifest mty -> module_type mty
  end

  let aliases_of_include' no_value_is_not_in_ids sg ids =
    (* We cannot use kind directly since it does not distinguish normal values and primitives *)
    Debug.format "@[<2>DEBUG alias: ids=[ @[%a@] ]@ + sg=[ @[%a@] ]@]@."
      (Format.list ";@ " Ident.format) ids
      (Format.list ";@ " (fun ppf (k, id) -> Format.fprintf ppf "%s:%a" (Kind.name k) Ident.format id)) (List.map T.kident_of_sigitem sg);
    let must_be_empty, res = List.fold_left (fun (ids, res) sitem ->
      let (k,_) = T.kident_of_sigitem sitem in
      match sitem with
      | Sig_value (id, { Types.val_kind = Types.Val_prim _ })
      | Sig_type (id, _, _)
      | Sig_modtype (id, _)
      | Sig_class_type (id, _, _) when no_value_is_not_in_ids ->
          (* They have no value, so id is not listed in [ids] *)
          (ids, (Ident.unsafe_create_with_stamp (Ident0.name id) (-1), (k, id)) :: res)
      | Sig_value (id, _)
      | Sig_exception (id, _)
      | Sig_module (id, _, _)
      | Sig_class (id, _, _)
      | Sig_type (id, _, _)
      | Sig_modtype (id, _)
      | Sig_class_type (id, _, _) ->
          (* They have a value, so id must be listed in [ids] *)
          begin match ids with
          | [] -> assert false
          | id'::ids ->
              assert (Ident0.name id = Ident0.name id');
              (ids, (id', (k,id)) :: res)
          end)
      (ids, []) sg
    in
    assert (must_be_empty = []);
    res

  let aliases_of_include mexp ids =
    let sg = try match Mtype.scrape (Cmt.recover_env mexp.mod_env) mexp.mod_type with Mty_signature sg -> sg | _ -> assert false with _ -> assert false in
    aliases_of_include' true sg ids

  let rec module_expr mexp =
    try
      match Module_expr.Table.find cache_module_expr mexp with
      | None ->
          (* When a module definition finds itself in itself.
             Impossible to happen, so far. *)
          assert false
      | Some v -> v
    with
    | Not_found ->
	Module_expr.Table.replace cache_module_expr mexp None;
	let res = module_expr_desc mexp.mod_desc in
	Module_expr.Table.replace cache_module_expr mexp (Some res);
        res

  and module_expr_desc = function
    | Tmod_ident (p, _) -> AMod_ident p
    | Tmod_structure str ->
	(* This may recompute abstractions of structure_items.
	   It sounds inefficient but not so much actually, since
	   module_expr is nicely cached. *)
	structure str
    | Tmod_functor (id, _, mty, mexp) ->
	AMod_functor(id, mty.mty_type, module_expr mexp)
    | Tmod_apply (mexp1, mexp2, _mcoercion) -> (* CR jfuruse ? *)
	AMod_apply (module_expr mexp1, module_expr mexp2)
    | Tmod_constraint (mexp, mty_, _constraint, _mcoercion) ->
	AMod_constraint (module_expr mexp, mty_)
    | Tmod_unpack (_expr, mty_) ->
        AMod_unpack (T.module_type mty_) (* CR jfuruse: need to unpack, really? *)

  and structure str = AMod_structure (List.concat_map structure_item str.str_items)

  and structure_item sitem =
    (* it may recompute the same thing, but it is cheap *)
    let sitems = structure_item_desc sitem.str_desc in
    (* eq consing *)
    let equalize sitem =
      try
	Structure_item.Table.find cache_structure_item sitem
      with
      | Not_found ->
	  Structure_item.Table.replace cache_structure_item sitem sitem;
	  sitem
    in
    List.map equalize sitems

  and structure_item_desc = function
    | Tstr_eval _ -> []
    | Tstr_value (_, pat_exps) ->
	List.map (fun id -> AStr_value id) (let_bound_idents pat_exps)
    | Tstr_primitive (id, _, _vdesc) ->
	[AStr_value id]
    | Tstr_type id_descs -> List.concat_map (fun (id, _, td) -> AStr_type id :: type_declaration td) id_descs
    | Tstr_exception (id ,_ , _) ->
	[AStr_exception id]
    | Tstr_exn_rebind (id, _, _path, _) -> (* CR jfuruse: path? *)
	[AStr_exception id]
    | Tstr_module (id, _, mexp) ->
	[AStr_module (id, module_expr mexp)]
    | Tstr_recmodule (idmexps) ->
	List.map (fun (id, _, _, mexp) ->
	  AStr_module (id, module_expr mexp)) idmexps
    | Tstr_modtype (id, _, mty) -> [AStr_modtype (id, module_type mty)]
    | Tstr_open _ -> []
    | Tstr_class classdescs ->
	List.map (fun (cls, _names, _) -> AStr_class cls.ci_id_class) classdescs
    | Tstr_class_type iddecls ->
	List.map (fun (id, _, _) -> AStr_class_type id) iddecls
    | Tstr_include (mexp, idents) ->
        let id_kid_list = try aliases_of_include mexp idents with e -> prerr_endline "structure_item include failed!!!"; raise e in
        let m = module_expr mexp in
        List.map (fun (id, (k, id')) -> AStr_included (id, m, k, id')) id_kid_list


  (* CR jfuruse: caching like module_expr_sub *)
  and module_type mty = module_type_desc mty.mty_desc

  and module_type_desc = function
    | Tmty_ident (p, _) -> AMod_ident p
    | Tmty_signature sg -> signature sg
    | Tmty_functor (id, _, mty1, mty2) ->
        (* CR jfuruse: need to scrape ? but how ? *)
        AMod_functor(id, mty1.mty_type, module_type mty2)
    | Tmty_with (mty, _) -> module_type mty (* CR jfuruse: ?? *)
    | Tmty_typeof mexp ->  (* CR jfuruse: ?? *)
        T.module_type mexp.mod_type

  and signature sg = AMod_structure (List.concat_map signature_item sg.sig_items)

  and signature_item sitem =
    match sitem.sig_desc with
    | Tsig_value (id, _, _) -> [AStr_value id]
    | Tsig_exception (id, _, _) -> [AStr_exception id]
    | Tsig_module (id, _ , mty) ->
        [AStr_module (id, module_type mty)]
    | Tsig_modtype (id, _, mty_decl) ->
        [(* todo *) AStr_modtype (id, modtype_declaration mty_decl) (* sitem.sig_final_env can be used? *) ]

    | Tsig_type typs -> List.concat_map (fun (id, _, td) -> AStr_type id :: type_declaration td) typs
    | Tsig_class clses ->
        (* CR jfuruse: still not sure which one is which *)
        List.concat_map (fun cls ->
          [ AStr_class cls.ci_id_class;
            AStr_class_type  cls.ci_id_class_type;
            AStr_type cls.ci_id_object;
            AStr_type cls.ci_id_typesharp]
        ) clses
    | Tsig_class_type clses -> List.map (fun cls -> AStr_class_type cls.ci_id_class) clses

    | Tsig_recmodule lst ->
        List.map (fun (id, _, mty) -> AStr_module (id, module_type mty)) lst
    | Tsig_open _ -> []
    | Tsig_include (mty, sg) ->
        let m = module_type mty in
        let sg0 = try match Mtype.scrape (Cmt.recover_env mty.mty_env) mty.mty_type with Mty_signature sg -> sg | _ -> assert false with _ -> assert false in
        let ids = List.map (fun si -> snd (T.kident_of_sigitem si)) sg in
        let aliases = try aliases_of_include' false sg0 ids with _ -> assert false in
        List.map (fun (id, (k, id')) -> AStr_included (id, m, k, id')) aliases

  and modtype_declaration = function
    | Tmodtype_abstract -> AMod_abstract
    | Tmodtype_manifest mty -> module_type mty

  and type_declaration td = match td.typ_kind with
    | Ttype_abstract -> []
    | Ttype_variant lst -> List.map (fun (id, {loc=_loc}, _, _) -> AStr_type id) lst
    | Ttype_record lst -> List.map (fun (id, {loc=_loc}, _, _, _) -> AStr_type id) lst

  let top_structure str = clear_cache (); structure str
  let top_signature sg =  clear_cache (); signature sg
end

let protect' name f v = try f v with e ->
  Format.eprintf "Error: %s: %s@." name (Printexc.to_string e); raise e

module Annot = struct
  type t =
    | Use               of Kind.t * Path.t
    | Type              of Types.type_expr * Env.t * [`Expr of Path.t option | `Pattern of Ident.t option ]
    | Mod_type          of Types.module_type
    | Str               of Abstraction.structure_item  (* CRjfuruse: Should be Sitem *)
    | Module            of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive     of bool

  let equal t1 t2 = match t1, t2 with
    | Type (t1, _, _), Type (t2, _, _) -> t1 == t2
    | Mod_type mty1, Mod_type mty2 -> mty1 == mty2
    | Str sitem1, Str sitem2 -> Abstraction.Structure_item.equal sitem1 sitem2
    | Module mexp1, Module mexp2 -> mexp1 == mexp2
    | Use (k1,p1), Use (k2,p2) -> k1 = k2 && p1 = p2
    | Non_expansive b1, Non_expansive b2 -> b1 = b2
    | Functor_parameter id1, Functor_parameter id2 -> id1 = id2
    | (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _),
      (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _) -> false

  module Record = struct
    open Asttypes
    open Typedtree
    open Abstraction
    module K = Kind

    (* CR jfuruse: A Location.t contains a filename, though it is always
       unique. Waste of 4xn bytes. *)
  (*
    let recorded = (Hashtbl.create 1023 : (Location.t, (int * t list)) Hashtbl.t)

    let clear () = Hashtbl.clear recorded
  *)

    type location_property = Wellformed | Flipped | Over_files | Illformed

    let check_location loc =
      if loc.Location.loc_start == Lexing.dummy_pos || loc.Location.loc_end == Lexing.dummy_pos then Illformed
      else if loc.Location.loc_start = Lexing.dummy_pos || loc.Location.loc_end = Lexing.dummy_pos then Illformed
      else
        (* If the file name is different between the start and the end, we cannot tell the wellformedness. *)
        if loc.Location.loc_start.Lexing.pos_fname <> loc.Location.loc_end.Lexing.pos_fname then Over_files
        else
          (* P4 creates some flipped locations where loc_start > loc_end *)
          match compare loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum
          with
          | -1 | 0 -> Wellformed
          | _ -> Flipped

    let record tbl loc t =
      let really_record () =
        let records =
          try Hashtbl.find tbl loc with Not_found -> []
        in
(*
        (* CR jfuruse: I am not really sure the below is correct now,
           but I remember the huge compilation slow down... *)
        (* This caching works horribly when too many things are defined
           at the same location. For example, a type definition of more than
           3000 variants, with sexp camlp4 extension, the compile time explodes
           from 10secs to 4mins! Therefore this works
           only if [num_records <= 10]
        *)
        if num_records <= 10 && List.exists (equal t) records then ()
        else Hashtbl.replace tbl loc (num_records + 1, t :: records)
*)
        Hashtbl.replace tbl loc (t :: records)
      in
      match check_location loc with
      | Wellformed -> really_record ()
      | Flipped ->
          if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Flipped location.@." Location.print loc;
          really_record ()
      | Illformed ->
          if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Ill-formed location.@." Location.print loc
      | Over_files -> ()

    let record_record tbl loc typ =
      let open Types in
      let open Ctype in
      match (repr typ).desc with
      | Tconstr (path, _, _) -> record tbl loc (Use (K.Type, path))
      | _ -> (* strange.. *) ()

    class fold tbl =
      let record = record tbl in
      let record_def loc sitem = record loc (Str sitem)
      and record_use loc kind path = record loc (Use (kind, path)) in
    object
      inherit Ttfold.fold as super

      method! pattern p =
        let ident_opt = match p.pat_desc with
          | Tpat_var (id, _) -> Some id
          | Tpat_alias (_, id, _) -> Some id
          (* | Tpat_construct (path, {loc}, cdesc, _, _) ->  *)
          | _ -> None
        in
        record p.pat_loc (Type (p.pat_type, p.pat_env, `Pattern ident_opt));
        begin match p.pat_desc with
        | Tpat_record _ -> record_record tbl p.pat_loc p.pat_type
        | _ -> ()
        end;
        super#pattern p

    (* CR jfuruse: pat_extra *)

      method! pattern_desc pd =
        begin match pd with
        | Tpat_var (id, {loc})
        | Tpat_alias (_, id, {loc}) -> record_def loc (AStr_value id)
        | Tpat_construct (path, {loc}, cdesc, _, _) ->
            let kind = match cdesc.Types.cstr_tag with
              | Types.Cstr_exception _ -> K.Exception
              | _ -> K.Type
            in
            record loc (Use (kind, path))
        | Tpat_record (lst , _) ->
            List.iter (fun (path, {loc}, _, _) ->
              record_use loc K.Type path) lst
        | Tpat_any | Tpat_constant _ | Tpat_tuple _
        | Tpat_variant _ | Tpat_array _ | Tpat_or _ | Tpat_lazy _ -> ()
        end;
        super#pattern_desc pd

      method! expression e =
        let path_opt = match e.exp_desc with
          | Texp_ident (path, _, _) -> Some path
          (* | Texp_construct (path, {loc}, cdesc, _, _) -> *)
          | Texp_instvar (_path, path, _)
          | Texp_setinstvar (_path, path, _, _) -> Some path
          | _ -> None
        in
        record e.exp_loc (Type (e.exp_type, e.exp_env, `Expr path_opt));
        begin match e.exp_desc with
        | Texp_record _ -> record_record tbl e.exp_loc e.exp_type
        | _ -> ()
        end;
        super#expression e

      method! exp_extra ee =
        begin match ee with
        | Texp_constraint _ -> ()
        | Texp_open (path, {loc}, _) -> record_use loc K.Module path
        | Texp_poly _ -> ()
        | Texp_newtype _ -> ()
        end;
        super#exp_extra ee

      method !expression_desc ed =
        begin match ed with
        | Texp_ident (path, {loc}, _) ->
            record_use loc K.Value path
        | Texp_construct (path, {loc}, cdesc, _, _) ->
            let kind = match cdesc.Types.cstr_tag with
              | Types.Cstr_exception _ -> K.Exception
              | _ -> K.Type
            in
            record_use loc kind path
        | Texp_record (lst, _) ->
            List.iter (fun (path, {loc}, _, _) ->
              record_use loc K.Type path) lst
        | Texp_field (_, path, {loc}, _)
        | Texp_setfield (_, path, {loc}, _, _) ->
            record_use loc K.Type path
        | Texp_for (id, {loc}, _, _, _, _) ->
            (* CR jfuruse: add type int to id *)
            record_def loc (AStr_value id)
        | Texp_new (path, {loc}, _) ->
            record_use loc K.Class path
        | Texp_instvar (_path, path, {loc}) (* CR jfuruse: not sure! *)
        | Texp_setinstvar (_path, path, {loc}, _) ->
            record_use loc K.Value path
        | Texp_override (_path, lst) ->  (* CR jfuruse: what todo with _path? *)
            List.iter (fun (path, {loc}, _) ->
              record_use loc K.Type path) lst
        | Texp_letmodule (id, {loc}, mexp, _) ->
            record_def loc (AStr_module (id, module_expr mexp))
        | Texp_constant _ | Texp_let _ | Texp_function _
        | Texp_apply _ | Texp_match _ | Texp_try _
        | Texp_tuple _ | Texp_variant _ | Texp_array _
        | Texp_ifthenelse _ | Texp_sequence _ | Texp_while _
        | Texp_when _ | Texp_send _ | Texp_assert _ | Texp_assertfalse
        | Texp_lazy _ | Texp_object _ | Texp_pack _ -> ()
        end;
        super#expression_desc ed
(*
and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t
*)

(* CR jfuruse: Class_type
      method! class_expr ce =
        record ce.cl_loc (Class_type (ce.cl_type, ce.cl_env));
        super#class_expr ce
*)

      method! class_expr_desc ced =
        begin match ced with
        | Tcl_ident (path, {loc}, _) -> record_use loc K.Value path
        | Tcl_structure _ -> ()
        | Tcl_fun (_, _, lst , _, _)
        | Tcl_let (_, _, lst, _) ->
            List.iter (fun (id, {loc}, _) -> record_def loc (AStr_value id)) lst
        | Tcl_apply _ -> ()
        | Tcl_constraint _ -> ()
        end;
        super#class_expr_desc ced
(*

and class_structure =
  { cstr_pat : pattern; (* this is self *)
    cstr_fields: class_field list;
    cstr_type : Types.class_signature;
    cstr_meths: Ident.t Meths.t (* CR jfuruse: to be done? *) }
*)

(*
and class_field =
   {
    cf_desc : class_field_desc;
    cf_loc : Location.t;
  }
*)

(*
and class_field_kind =
  Tcfk_virtual of core_type
| Tcfk_concrete of expression
*)

      method! class_field_desc cfd =
        begin match cfd with
        | Tcf_inher (_, ce, _, ivars, cmethods) ->
            (* try to have better location *)
            let rec find ce = match ce.cl_desc with
              | Tcl_ident _
              | Tcl_structure _
              | Tcl_fun _
              | Tcl_apply _
              | Tcl_constraint _ -> ce
              | Tcl_let (_, _, _, ce) -> find ce
            in
            let loc = (find ce).cl_loc in
            List.iter (fun (_, id) -> record_def loc (AStr_value id)) ivars;
            List.iter (fun (_, id) -> record_def loc (AStr_value id)) cmethods
        | Tcf_val (_name, {loc}, _, id, _, _) -> record_def loc (AStr_value id)
        | Tcf_meth (_name, {loc=_loc}, _, _, _) -> ()
        | Tcf_constr _ -> ()
        | Tcf_init _ -> ()
        end;
        super#class_field_desc cfd

      method! module_expr me = (* CR jfuruse: me.mod_env *)
        record me.mod_loc (Mod_type me.mod_type);
        super#module_expr me

(*
and module_type_constraint =
  Tmodtype_implicit
| Tmodtype_explicit of module_type
*)

      method! module_expr_desc med =
        begin match med with
        | Tmod_ident (path, {loc}) ->
            record_use loc K.Module path
        | Tmod_functor (id, {loc}, _, _) ->
            (* CR jfuruse: must rethink *)
            record_def loc (AStr_module (id, AMod_functor_parameter));
            record loc (Functor_parameter id); (* CR jfuruse: required? *)
        | Tmod_structure _
        | Tmod_apply _
        | Tmod_constraint _
        | Tmod_unpack _ -> ()
        end;
        super#module_expr_desc med

(* CR jfuruse: I want to put the sig
and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}
*)

      method! structure_item sitem =
        begin match sitem.str_desc with (* CR jfuruse; todo add env *)
        | Tstr_include (mexp, idents) ->
            let loc = sitem.str_loc in
            let id_kid_list = try aliases_of_include mexp idents with e -> prerr_endline "structure_item include failed!!!"; raise e in
            let m = module_expr mexp in
            List.iter (fun (id, (k, id')) ->
              record_def loc (AStr_included (id, m, k, id'))) id_kid_list
        | _ -> ()
        end;
        super#structure_item sitem

      method! structure_item_desc sid =
        begin match sid with
        | Tstr_primitive (id, {loc}, _) ->
            record_def loc (AStr_value id)
        | Tstr_type lst ->
            List.iter (fun (id, {loc}, _) ->
              record_def loc (AStr_type id)) lst
        | Tstr_exception (id, {loc}, _) ->
            record_def loc (AStr_exception id)
        | Tstr_exn_rebind (id, {loc}, path, {loc=loc'}) ->
            record_def loc (AStr_exception id);
            record_use loc' K.Exception path
        | Tstr_module (id, {loc}, mexp) ->
            record loc (Mod_type mexp.mod_type);
            record_def loc (AStr_module (id, module_expr mexp))
        | Tstr_recmodule lst ->
            List.iter (fun (id, {loc}, _mty, mexp) ->
              record loc (Mod_type mexp.mod_type);
              record_def loc (AStr_module (id, module_expr mexp))) lst
        | Tstr_modtype (id, {loc}, mty) ->
            record_def loc (AStr_modtype (id, module_type mty))
        | Tstr_open (path, {loc}) ->
            record_use loc K.Module path
        | Tstr_class_type lst ->
            List.iter (fun (id, {loc}, _) ->
              record_def loc (AStr_class_type id)) lst
        | Tstr_include (_mexp, _idents) -> () (* done in #structure_item *)
        | Tstr_eval _
        | Tstr_value _
        | Tstr_class _
          -> ()
        end;
        super#structure_item_desc sid

(*
and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description
*)

(* add env?
and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t; (* BINANNOT ADDED *)
    mty_loc: Location.t }
*)

      method! module_type_desc mtd =
        begin match mtd with
        | Tmty_ident (path, {loc}) ->
            record_use loc K.Module_type path
        | Tmty_functor (id, {loc}, mty, _mty) ->
            record_def loc (AStr_module (id, module_type mty))
        | Tmty_with (_mty, lst) ->
            List.iter (fun (path, {loc}, with_constraint) ->
              record loc (Use ( (match with_constraint with
                                 | Twith_type _      -> K.Type
                                 | Twith_module _    -> K.Module
                                 | Twith_typesubst _ -> K.Type
                                 | Twith_modsubst _  -> K.Module),
                                path ))) lst
        | Tmty_typeof _
        | Tmty_signature _ -> ()
        end;
        super#module_type_desc mtd

(* add env
and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

 add env *)

      method! signature_item si =
        begin match si.sig_desc with (* CR jfuruse; todo add env *)
        | Tsig_include (mty, sg) ->
            let loc = si.sig_loc in
            let m = Abstraction.module_type mty in
            let sg0 = match Mtype.scrape (Cmt.recover_env mty.mty_env) mty.mty_type with
              | Types.Mty_signature sg -> sg
              | Types.Mty_functor _ -> assert false
              | Types.Mty_ident _path ->
                  (* Strange... failed to scrape? *)
                  assert false
            in
            let ids = List.map (fun si -> snd (T.kident_of_sigitem si)) sg in
            let aliases = try aliases_of_include' false sg0 ids with _ -> assert false in
            List.iter (fun (id, (k, id')) ->
              record_def loc (AStr_included (id, m, k, id'))) aliases
        | _ -> ()
        end;
        super#signature_item si

(*
and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }
*)

      method! signature_item_desc sid =
        begin match sid with
        | Tsig_value (id, {loc}, _) -> record_def loc (AStr_value id)
        | Tsig_type lst ->
            List.iter (fun (id, {loc}, _) ->
              record_def loc (AStr_type id)) lst
        | Tsig_exception (id, {loc}, _) -> record_def loc (AStr_exception id)
        | Tsig_module (id, {loc}, mty) ->
            record loc (Mod_type mty.mty_type);
            record_def loc (AStr_module (id, module_type mty))
        | Tsig_recmodule lst ->
            List.iter (fun (id, {loc}, mty) ->
              record loc (Mod_type mty.mty_type);
              record_def loc (AStr_module (id, module_type mty))) lst
        | Tsig_modtype (id, {loc}, mtd) ->
            record_def loc (AStr_modtype (id, modtype_declaration mtd))
        | Tsig_open (path, {loc}) -> record_use loc K.Module path
        | Tsig_include _ -> () (* done in #signature_item *)
        | Tsig_class _ -> ()
        | Tsig_class_type _ -> ()
        end;
        super#signature_item_desc sid


      method! with_constraint wc =
        begin match wc with
        | Twith_module (path, {loc}) -> record_use loc K.Module path
        | Twith_modsubst (path, {loc}) -> record_use loc K.Module path  (*?*)
        | Twith_type _ -> ()
        | Twith_typesubst _ -> ()
        end;
        super#with_constraint wc

(* add env?
and core_type =
(* mutable because of [Typeclass.declare_method] *)
  { mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t }
*)

      method! core_type_desc ctd =
        begin match ctd with
        | Ttyp_var _var -> () (* CR jfuruse: todo *)
        | Ttyp_constr (path, {loc}, _) -> record_use loc K.Type path
        | Ttyp_class (path, {loc}, _, _) -> record_use loc K.Class path
            (* CR jfuruse: or class type? *)
        | Ttyp_alias (_core_type, _var) -> () (* CR jfuruse: todo *)
        | Ttyp_poly (_vars, _core_type) -> () (* CR jfuruse; todo *)
        | Ttyp_any
        | Ttyp_arrow _
        | Ttyp_tuple _
        | Ttyp_object _
        | Ttyp_variant _
        | Ttyp_package _
            -> ()
        end;
        super#core_type_desc ctd

      method! package_type pt =
        record_use pt.pack_txt.loc K.Module_type pt.pack_name;
        super#package_type pt
(*
and package_type = {
  pack_name : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and core_field_type =
  { field_desc: core_field_desc;
    field_loc: Location.t }

and core_field_desc =
    Tcfield of string * core_type
  | Tcfield_var

and row_field =
    Ttag of label * bool * core_type list
  | Tinherit of core_type

and value_description =
  { val_desc : core_type;
    val_val : Types.value_description;
    val_prim : string list;
    val_loc : Location.t;
    }

and type_declaration =
  { typ_params: string loc option list;
    typ_type : Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_variance: (bool * bool) list;
    typ_loc: Location.t }
*)

      method! type_kind tk =
        begin match tk with
        | Ttype_abstract -> ()
        | Ttype_variant lst ->
            List.iter (fun (id, {loc}, _, _loc(*?*)) ->
              record_def loc (AStr_type id)) lst
        | Ttype_record lst ->
            List.iter (fun (id, {loc}, _, _, _loc(*?*)) ->
              record_def loc (AStr_type id)) lst
        end;
        super#type_kind tk

(*

and exception_declaration =
  { exn_params : core_type list;
    exn_exn : Types.exception_declaration;
    exn_loc : Location.t }

and class_type =
  { cltyp_desc: class_type_desc;
    cltyp_type : Types.class_type;
    cltyp_env : Env.t; (* BINANNOT ADDED *)
    cltyp_loc: Location.t }
*)

      method! class_type_desc ctd =
        begin match ctd with
        | Tcty_constr (path, {loc}, _) -> record_use loc K.Class_type path
        | Tcty_signature _
        | Tcty_fun _ -> ()
        end;
        super#class_type_desc ctd

(*

and class_signature = {
    csig_self : core_type;
    csig_fields : class_type_field list;
    csig_type : Types.class_signature;
    csig_loc : Location.t;
  }

and class_type_field = {
    ctf_desc : class_type_field_desc;
    ctf_loc : Location.t;
  }

and class_type_field_desc =
    Tctf_inher of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_virt  of (string * private_flag * core_type)
  | Tctf_meth  of (string * private_flag * core_type)
  | Tctf_cstr  of (core_type * core_type)

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos
*)

      method! class_infos f ci =
        let loc = ci.ci_id_name.loc in
        (* CR jfuruse: are they correct? *)
        record_def loc (AStr_class ci.ci_id_class);
        record_def loc (AStr_class_type ci.ci_id_class_type);
        record_def loc (AStr_type ci.ci_id_object);
        record_def loc (AStr_type ci.ci_id_typesharp);
        super#class_infos f ci

    end
  end

  let record_structure str =
    protect' "Spot.Annot.record_structure" (fun () ->
      let tbl = Hashtbl.create 1023 in
      let o = new Record.fold tbl in
      ignore (o#structure str);
      Debug.format "structure recorded: %d records@." (Hashtbl.length tbl);
      tbl)
      ()

  let record_signature sg =
    protect' "Spot.Annot.record_signature" (fun () ->
      let tbl = Hashtbl.create 1023 in
      let o = new Record.fold tbl in
      ignore (o#signature sg);
      Debug.format "signature recorded: %d records@." (Hashtbl.length tbl);
      tbl)
      ()

  let string_of_at = function
    | `Expr _ -> "Expr"
    | `Pattern _ -> "Pattern"

  let format ppf = function
    | Type (typ, _env, at) ->
	Printtyp.reset ();
	Printtyp.mark_loops typ;
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: %a@ " (Printtyp.type_scheme ~with_pos:false) typ;
	fprintf ppf "XType: %a@ " (Printtyp.type_scheme ~with_pos:true) typ;
        fprintf ppf "At: %s" (string_of_at at)
    | Mod_type mty ->
	fprintf ppf "Type: %a@ " (Printtyp.modtype ~with_pos:false) mty;
	fprintf ppf "XType: %a" (Printtyp.modtype ~with_pos:true) mty
    | Str str ->
	fprintf ppf "Str: %a"
	  Abstraction.format_structure_item str
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s"
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Module mexp ->
	fprintf ppf "Module: %a"
          Abstraction.format_module_expr mexp
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b

  let summary ppf = function
    | Type (_typ, _env, at) ->
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: ...@ ";
	fprintf ppf "XType: ...@ ";
        fprintf ppf "At: %s" (string_of_at at)
    | Mod_type _mty ->
	fprintf ppf "Type: ...@ ";
	fprintf ppf "XType: ..."
    | Str _str ->
	fprintf ppf "Str: ..."
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s"
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Module _mexp ->
	fprintf ppf "Module: ..."
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b

  let dummy = Use (Kind.Value, Path.Pident (Ident.create_persistent "dummy"))
end

module Position = struct
  open Lexing

  type t = { line_column : (int * int) option;
             bytes : int option }

  let of_lexing_position pos =
    { line_column = Some (pos.pos_lnum, pos.pos_cnum - pos.pos_bol);
      bytes = Some pos.pos_cnum }

  let compare p1 p2 = match p1, p2 with
    (* line_columns are preferrable, since bytes of mll+mly are of the generated files *)
    | { line_column = Some (l1,c1); _ }, { line_column = Some (l2,c2); _ } ->
	begin match compare l1 l2 with
	| 0 -> compare c1 c2
	| n -> n
	end
    | { bytes = Some b1; _ }, { bytes = Some b2; _ } -> compare b1 b2
    | _ -> assert false

  let to_string p = match p.line_column, p.bytes with
    | Some (l,c), Some b -> Printf.sprintf "l%dc%db%d" l c b
    | Some (l,c), None -> Printf.sprintf "l%dc%d" l c
    | None, Some b -> Printf.sprintf "b%d" b
    | None, None -> assert false

  let none = { line_column = None; bytes = None }

  exception Parse_failure of string

  let parse s =
    (* token : [a-z][0-9]+ *)
    let len = String.length s in
    let rec get_number ~num pos =
      if pos >= len then num, pos
      else
	match s.[pos] with
	| '0'..'9' ->
	    get_number
	      ~num: (num * 10 + int_of_char s.[pos] - int_of_char '0')
	      (pos + 1)
	| _ -> num, pos
    in
    let rec get_tokens pos =
      if pos >= len then []
      else
	match s.[pos] with
	| 'a'..'z' ->
	    let k = s.[pos] in
	    let pos = pos + 1 in
	    let num, pos' = get_number ~num:0 pos in
	    if pos = pos' then
	      raise (Parse_failure (Printf.sprintf "pos token has no number: '%c'"
				       k));
	    (k, num) :: get_tokens pos'
        | '0'..'9' ->
            (* Good Ol' Syntax *)
            begin try ['b', int_of_string s] with _ ->
              raise (Parse_failure
                        (Printf.sprintf "failed to parse %S as a byte position" s))
            end
	| _ ->
	    raise (Parse_failure (Printf.sprintf "illegal pos token head '%c'"
				     s.[pos]))
    in
    let tokens = get_tokens 0 in
    match tokens with
    | ['l', line; 'c', column] -> { line_column = Some (line, column);
				    bytes = None }
    | ['b', bytes] -> { line_column = None; bytes = Some bytes }
    | _ -> raise (Parse_failure "illegal pos token combination")

  let next = function
    | { bytes = Some b; _ } -> { bytes = Some (b + 1); line_column = None }
    | { line_column = Some (l,c); bytes = None; } ->
        { line_column = Some (l, c+1); bytes = None }
    | _ -> assert false

  let is_complete = function
    | { line_column = Some _ } -> true
    | _ -> false

  (* it drops one byte at the end, but who cares? *)
  let complete mlpath t = match t with
    | { line_column = Some _ } ->
        t (* already complete *)
    (* Completing of the byte part from line-column is HARD,
       for the case of auto-generated source files.
       line_column : this is of the original file
       bytes : this is of the GENERATED file
    *)
    | { line_column = None; bytes = Some bytes } ->
        let ic = open_in_bin mlpath in
        let rec iter lines remain =
          let pos = pos_in ic in
          let new_remain = bytes - pos in
          if new_remain < 0 then begin (* run over *)
            close_in ic;
            { line_column = Some (lines, remain); bytes = Some bytes }
          end else begin
            if try ignore (input_line ic); true with End_of_file -> false then
              iter (lines+1) new_remain
            else
              { line_column = Some (lines+1, new_remain); bytes = Some bytes }
          end
        in
        iter 0 bytes

    | { line_column = None; bytes = None } -> assert false

end

module Region : sig

  type t = private {
    fname : (string * (int * int) option) option;
    (* filename and device/inode. None = "_none_" *)
    start : Position.t;
    end_ : Position.t
  }

  val compare : t -> t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]

  val to_string : t -> string
  val to_string_no_path : t -> string
  val of_parsing : string -> Location.t -> t
  val split : t -> by:t -> (t * t) option
  val point_by_byte : string -> int -> t
    (** works only if bytes are available *)
  val point : string -> Position.t -> t
  val change_positions : t -> Position.t -> Position.t -> t
  val length_in_bytes : t -> int
  val is_complete : t -> bool
  val complete : string -> t -> t
  val substring : string -> t -> t * string

end = struct

  type t = {
    fname : (string * (int * int) option) option;
    (* filename and device/inode. None = "_none_" *)
    start : Position.t;
    end_ : Position.t
  }

  let cache = Hashtbl.create 1023

  let fname = function
    | "_none_" -> None
    | s ->
        let s =
          if Filename.is_relative s then
            Unix.getcwd () ^/ s
          else s
        in
        try
          Hashtbl.find cache s
        with
        | Not_found ->
            let dev_inode = Unix.dev_inode s in
            if dev_inode = None then Format.eprintf "%s does not exist@." s;
            let v = Some (s, dev_inode) in
            Hashtbl.replace cache s v;
            v

  let to_string t =
    Printf.sprintf "%s:%s:%s"
      (match t.fname with Some (fname, _) -> fname | None -> "_none_")
      (Position.to_string t.start)
      (Position.to_string t.end_)

  let to_string_no_path t =
    Printf.sprintf "%s:%s"
      (Position.to_string t.start)
      (Position.to_string t.end_)

  (* CR jfuruse: we should have path cache *)

  let of_parsing builddir l =
    let fname1 = l.Location.loc_start.Lexing.pos_fname in
    let fname2 = l.Location.loc_end.Lexing.pos_fname in
    if fname1 <> fname2 then
      Format.eprintf "Warning: A location contains strange file names %s and %s@." fname1 fname2;
    let fname = fname (if fname1 = "_none_" then fname1 else builddir ^/ fname1) in
    let start = Position.of_lexing_position l.Location.loc_start in
    let end_ = Position.of_lexing_position l.Location.loc_end in
    match Position.compare start end_ with
    | -1 | 0 -> { fname; start = start; end_ = end_ }
    | _ -> { fname; start = end_; end_ = start }

  let compare l1 l2 =
    let compare_fnames f1 f2 =
      let same_files =
        f1 == f2
        || match f1, f2 with
          | Some (_, Some di1), Some (_, Some di2) -> di1 = di2
          | Some (f1, _), Some (f2, _) -> f1 = f2 (* weak guess *)
          | None, None -> true (* ouch *)
          | _ -> false
      in
      if same_files then 0
      else match f1, f2 with
      | Some (f1, _), Some (f2, _) -> compare f1 f2
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None -> 0
    in
    (* CR jfuruse: this can be merged with same_files as compare *)
    match compare_fnames l1.fname l2.fname with
    | 1 -> `Left
    | -1 -> `Right
    | _ (* 0 *) ->
        let starts = Position.compare l1.start l2.start in
        let ends   = Position.compare l1.end_  l2.end_  in
        if starts = 0 && ends = 0 then `Same
        else if starts <= 0 && ends >= 0 then `Includes
        else if starts >= 0 && ends <= 0 then `Included
        else if Position.compare l1.end_ l2.start <= 0 then `Left
        else if Position.compare l2.end_ l1.start <= 0 then `Right
        else `Overwrap

  let split l1 ~by:l2 =
    if compare l1 l2 = `Overwrap then
      if Position.compare l1.start l2.start < 0 then
	Some ({ l1 with end_ = (* position_prev *) l2.start },
	      { l1 with start = l2.start })
      else if Position.compare l2.start l1.start < 0 then
        Some ({ l1 with end_ = l2.end_ },
	      { l1 with start = (* position_next *) l2.end_ })
      else assert false
    else None

  open Position

  let point_by_byte fn pos =
    let fname = fname fn in
    { fname;
      start = { line_column = None;
 		bytes = Some pos };
      end_ = { line_column = None;
               bytes = Some (pos + 1)} }

  let point fn pos =
    let fname = fname fn in
    { fname; start = pos; end_ = Position.next pos }

  let change_positions t p1 p2 = { t with start = p1; end_ = p2 }

  let length_in_bytes t =
    let bytes = function
      | { Position.bytes = Some bytes; _ } -> bytes
      | _ -> raise Not_found
    in
    bytes t.end_ - bytes t.start

  let is_complete t =
    Position.is_complete t.start && Position.is_complete t.end_

  (* CR jfuruse: fname is overwritten. Strange. *)
  let complete mlpath t =
    let fname = fname mlpath in
    { fname;
      start = Position.complete mlpath t.start;
      end_ = Position.complete mlpath t.end_ }

  let substring mlpath t =
    let t = complete mlpath t in
    let ic = open_in_bin mlpath in
    match t.start.Position.bytes, t.end_.Position.bytes with
    | Some start, Some end_ ->
	seek_in ic start;
	let s = String.create (end_ - start) in
	really_input ic s 0 (end_ - start);
	t, s
    | _ -> assert false

end

module Regioned = struct
  type 'a t = { region: Region.t; value: 'a }

  let compare { region = r1; _ } { region = r2; _ } = Region.compare r1 r2

  let split { region = r1; value = v } ~by:{ region = r2; _ } =
    Option.map (Region.split r1 ~by: r2) ~f:(fun (r11, r12) ->
      { region = r11; value = v },
      { region = r12; value = v })

  let format f ppf { region = r; value = v } =
    fprintf ppf "@[<2>%s:@ @[%a@]@]"
      (Region.to_string r)
      f v
end

(* annotation with region *)
module RAnnot = struct
  type t      = Annot.t list Regioned.t
  let split   = Regioned.split
  let compare = Regioned.compare
  let format  = Regioned.format (Format.list ";@ " Annot.format)
end

module Tree = struct
  include Treeset.Make(RAnnot)

  open Regioned

  (* If the region maybe splitted, the original region will be gone *)
  let add t rannot = add_elem rannot t

  let iter = iter_elem

  let find_path_contains r t =
    let probe = { region = r; value = [] (* dummy *) } in
    find_path_contains probe t

  let dump t =
    iter_elem (fun ~parent rrspot ->
	let format_parent ppf = function
	  | None -> fprintf ppf "ROOT"
	  | Some rrspot -> RAnnot.format ppf rrspot
	in
	eprintf "@[<2>@[%a@] =>@ @[%a@]@]@."
	  format_parent parent
	  RAnnot.format rrspot) t
end

(* Minimum data for spotting, which are saved into spot files *)
module File = struct
  type t = {
    modname        : string;
    builddir       : string;
    loadpath       : string list;
    args           : string array;
    path           : string; (** source path. If packed, the .cmo itself *)
    top            : Abstraction.structure;
    loc_annots     : (Location.t, Annot.t list) Hashtbl.t
  }

  let dump file =
    eprintf "@[<v2>{ module= %S;@ path= %S;@ builddir= %S;@ loadpath= [ @[%a@] ];@ argv= [| @[%a@] |];@ ... }@]@."
      file.modname
      file.path
      file.builddir
      (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) file.loadpath
      (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) (Array.to_list file.args)

  let save path t =
    let oc = open_out_bin path in
    output_string oc "spot";
    output_string oc Checksum.char16;
    output_value oc t;
    close_out oc

  let load path =
    let ic = open_in path in
    let buf = String.create 4 in
    really_input ic buf 0 4;
    if buf <> "spot" then failwithf "file %s is not a spot file" path;
    let buf = String.create 16 in
    really_input ic buf 0 16;
    if buf <> Checksum.char16 then failwithf "file %s has an incompatible checksum" path;
    let v = input_value ic in
    close_in ic;
    v

  open Cmt_format

  let abstraction cmt = match cmt.cmt_annots with
    | Implementation str ->
        let loc_annots = Annot.record_structure str in
        begin match Abstraction.top_structure str with
        | Abstraction.AMod_structure str -> str, loc_annots
        | _ -> assert false
        end
    | Interface sg ->
        let loc_annots = Annot.record_signature sg in
        begin match Abstraction.top_signature sg with
        | Abstraction.AMod_structure str -> str, loc_annots
        | _ -> assert false
        end
    | Packed (_sg, files) ->
        (List.map (fun file ->
          let fullpath = if Filename.is_relative file then cmt.cmt_builddir ^/ file else file in
          let modname = match Filename.split_extension (Filename.basename file) with
            | modname, (".cmo" | ".cmx" | ".cmi") -> String.capitalize modname
            | _ -> Format.eprintf "packed module with strange name: %s@." file; assert false
          in
          Abstraction.AStr_module (Ident.create modname (* stamp is bogus *),
                                   Abstraction.AMod_packed fullpath)) files),
        (Hashtbl.create 1 (* empty *))
    | Partial_implementation _parts | Partial_interface _parts -> assert false

  let abstraction cmt =
    let load_path = List.map (fun p ->
      cmt.cmt_builddir ^/ p) cmt.cmt_loadpath
    in
    with_ref Config.load_path load_path (fun () ->
      try abstraction cmt; with e ->
        Format.eprintf "Aiee %s@." (Printexc.to_string e);
        raise e)

  let of_cmt path (* the cmt file path *) cmt =
    let path = Option.default (Cmt.source_path cmt) (fun () ->
      let ext = if Cmt.is_opt cmt then ".cmx" else ".cmo" in
      Filename.chop_extension path ^ ext)
    in
    let top, loc_annots = abstraction cmt in
    { modname  = cmt.cmt_modname;
      builddir = cmt.cmt_builddir;
      loadpath = cmt.cmt_loadpath;
      args     = cmt.cmt_args;
      path;
      top;
      loc_annots;
    }
end

(* Spot info for each compilation unit *)
module Unit = struct

  module F = File

  type t = {
    modname        : string;
    builddir       : string;
    loadpath       : string list;
    args           : string array;
    path           : string; (** source path. If packed, the .cmo itself *)
    top            : Abstraction.structure;
    loc_annots     : (Location.t, Annot.t list) Hashtbl.t;

    flat           : Abstraction.structure lazy_t;
    id_def_regions : (Ident.t, Region.t) Hashtbl.t lazy_t;
    rannots        : Annot.t list Regioned.t list lazy_t;
    tree           : Tree.t lazy_t
  }

  (* same as F.dump, ignoring new additions in Unit *)
  let dump file =
    eprintf "@[<v2>{ module= %S;@ path= %S;@ builddir= %S;@ loadpath= [ @[%a@] ];@ argv= [| @[%a@] |];@ ... }@]@."
      file.modname
      file.path
      file.builddir
      (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) file.loadpath
      (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) (Array.to_list file.args)

  let to_file { modname; builddir; loadpath; args; path; top ; loc_annots } =
    { F.modname;
      builddir;
      loadpath;
      args;
      path;
      top;
      loc_annots;
    }

  let of_file ({ F.loc_annots; } as f) =
    let rannots = lazy (Hashtbl.fold (fun loc annots st ->
      { Regioned.region = Region.of_parsing f.F.builddir loc;  value = annots } :: st)
                          loc_annots [])
    in
    let id_def_regions = lazy (
      let tbl = Hashtbl.create 1023 in
      Hashtbl.iter (fun loc annots ->
        List.iter (function
          | Annot.Str sitem ->
              Option.iter (Abstraction.ident_of_structure_item sitem) ~f:(fun (_kind, id) ->
                Hashtbl.add tbl id (Region.of_parsing f.F.builddir loc))
          | _ -> ()) annots) loc_annots;
      tbl)
    in
    let tree = lazy begin
      Hashtbl.fold (fun loc annots st ->
        Tree.add st { Regioned.region = Region.of_parsing f.F.builddir loc; value = annots })
        loc_annots Tree.empty
    end in
    (* CR jfuruse: it is almost the same as id_def_regions_list *)
    let flat = lazy (Hashtbl.fold (fun _loc annots st ->
      List.filter_map (function
        | Annot.Str sitem -> Some sitem
        | _ -> None) annots @ st) loc_annots [])
    in
    { modname    = f.F.modname;
      builddir   = f.F.builddir;
      loadpath   = f.F.loadpath;
      args       = f.F.args;
      path       = f.F.path;
      top        = f.F.top;
      loc_annots = f.F.loc_annots;

      flat; id_def_regions; rannots; tree;
    }
end
