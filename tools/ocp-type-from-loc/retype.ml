open Types
open Typedtree
open Location
open Cmt_format

let todo msg = failwith ("TODO: " ^ msg)

let match_loc loc1 loc2 = (* TODO : do more tests *)
  let open Lexing in
  let _, st_line1, st_char1 = get_pos_info loc1.loc_start in
  let _, st_line2, st_char2 = get_pos_info loc2.loc_start in
  let _, end_line1, end_char1 = get_pos_info loc1.loc_end in
  let _, end_line2, end_char2 = get_pos_info loc2.loc_end in
  st_line1 = st_line2
  && end_line1 = end_line2
  && st_char1 <= st_char2
  && end_char1 >= end_char2

let rec from_binary_annots loc = function
  | Packed _ -> todo "Packed... "
  | Interface _ -> todo "Interface..."
  | Partial_implementation _ -> todo "Partial implementation..."
  | Partial_interface _ -> todo "Partial interface..."
  | Implementation structure ->
    from_structures loc structure.str_items

and from_structures loc = function
  | [] -> None     (* No type founded *)
  | strct :: tl ->
    match from_structure loc strct.str_desc with
    | None -> from_structures loc tl
    | Some _ as ty -> ty      (* Stop looping, type founded *)

and from_structure loc = function
  | Tstr_eval e ->
    from_loc_expr loc e
  | Tstr_value (_, pat_expr_list) ->
    from_params loc pat_expr_list
  | Tstr_modtype (_, sloc, mtype) ->
    if match_loc loc sloc.loc then todo "Tstr_modtype..."
    else from_mod_type loc mtype.mty_desc
  | Tstr_type tcl ->
    from_type_decl loc tcl
  | Tstr_primitive _ -> todo "Primitive..."
  | Tstr_exception _ -> todo "Exception..."
  | Tstr_exn_rebind _ -> todo "Exn_rebind..."
  | Tstr_module _ -> todo "Module..."
  | Tstr_recmodule _ -> todo "Recmodule..."
  | Tstr_open _ -> todo "Open..."
  | Tstr_class _ -> todo "Class..."
  | Tstr_class_type _ -> todo "Class type..."
  | Tstr_include _ -> todo "Include..."

and from_type_decl loc = function
  | [] -> None
  | (id, sloc, tdecl) :: tl ->
    if match_loc loc sloc.loc then
      todo "Type constructore name ?"
    else
      from_type_decl loc tl

and from_mod_type loc = function
  | Tmty_ident (path, lloc) -> assert false
  | Tmty_signature sign ->
    let rec loop loc = function
    | [] -> None
    | sign :: tl ->
      match from_signature loc sign.sig_desc with
      | None -> loop loc tl
      | Some _ as ty -> ty     (* Stop looping *)
    in
    loop loc sign.sig_items
  | Tmty_functor (id, sloc, mtyp1, mtyp2) ->
    begin match from_mod_type loc mtyp1.mty_desc with
    | None -> from_mod_type loc mtyp2.mty_desc
    | Some _ as ty -> ty
    end
  | Tmty_with (mtyp1, _) -> assert false
  | Tmty_typeof mexp -> assert false

and from_signature loc = function
  | Tsig_value _ -> assert false
  | Tsig_type _ -> assert false
  | Tsig_exception _ -> assert false
  | Tsig_module _ -> assert false
  | Tsig_recmodule _ -> assert false
  | Tsig_modtype _ -> assert false
  | Tsig_open _ -> assert false
  | Tsig_include _ -> assert false
  | Tsig_class _ -> assert false
  | Tsig_class_type _ -> assert false

and from_loc_expr loc tt =
  if match_loc loc tt.exp_loc then Some tt.exp_type
  else from_loc_edesc loc tt.exp_desc

and from_loc_edesc loc t =
  match t with
  | Texp_ident (path, lloc, vdesc) ->
    if match_loc loc lloc.loc then Some vdesc.val_type
    else None
  | Texp_constant _ ->
    None
  | Texp_let (_, binds, body) ->
    begin match from_params loc binds with
    | None -> from_loc_expr loc body
    | Some _ as ty -> ty
    end
  | Texp_function (_, params, _) ->
    from_params loc params
  | Texp_apply (e, args) ->
    begin match from_loc_expr loc e with
    | None ->
      let rec loop loc args =
        match args with
        | [] -> None
        | (_, None, _) :: tl -> loop loc tl
        | (_, Some e, _) :: tl ->
          begin match from_loc_expr loc e with
          | None -> loop loc tl
          | Some _ as ty -> ty
          end
      in
      loop loc args
    | Some _ as ty -> ty (* type of e *)
    end
  | Texp_match (e, cases, _) ->
    begin match from_loc_expr loc e with
    | None -> from_params loc cases
    | Some _ as ty -> ty
    end
  | Texp_try (e, cases) ->
    begin match from_loc_expr loc e with
    | None -> from_params loc cases
    | Some _ as ty -> ty
    end
  | Texp_tuple tuples ->
    from_loc_exprs loc tuples
  | Texp_construct (_, lloc, _, constrs, _) ->
    if match_loc loc lloc.loc then todo "Texp_construct cases..."
    else from_loc_exprs loc constrs

  | Texp_variant (_, None) -> None
  | Texp_variant (_, Some e) ->
    from_loc_expr loc e

  | Texp_record (lbl_exp_list, e_opt) ->
    let rec loop loc = function
      | [] ->
        begin match e_opt with
        | None -> None
        | Some e -> from_loc_expr loc e
        end
      | (_, lloc, _, e) :: tl ->
        if match_loc loc lloc.loc then todo "Texp_record cases..."
        else
          begin match from_loc_expr loc e with
          | None -> loop loc tl
          | Some _ as ty -> ty
          end in
    loop loc lbl_exp_list
  | Texp_field (e, _, lloc, _) ->
    if match_loc loc lloc.loc then todo "Texp_field cases..."
    else from_loc_expr loc e
  | Texp_setfield (record, _, lloc, _, new_val) ->
    if match_loc loc lloc.loc then todo "Texp_setfield cases..."
    else
      begin match from_loc_expr loc record with
      | None -> from_loc_expr loc new_val
      | Some _ as ty -> ty
      end
  | Texp_array vals ->
    from_loc_exprs loc vals

  | Texp_ifthenelse (cond, thenb, None) ->
    from_loc_exprs loc [cond; thenb]
  | Texp_ifthenelse (cond, thenb, Some elseb) ->
    from_loc_exprs loc [cond; thenb; elseb]

  | Texp_sequence (e1, e2) ->
    from_loc_exprs loc [e1; e2]
  | Texp_while (cond, body) ->
    from_loc_exprs loc [cond; body]
  | Texp_for _ -> todo "Texp_for cases..."
  | Texp_when (e1, e2) ->
    from_loc_exprs loc [e1; e2]

  | Texp_send (e, _, None) ->
    from_loc_expr loc e
  | Texp_send (e1, _, Some e2) ->
    from_loc_exprs loc [e1; e2]
  | Texp_assert e -> from_loc_expr loc e

  | Texp_new (_, _, _)     -> todo "Texp_new cases..."
  | Texp_instvar (_, _, _) -> todo "Texp_instvar cases..."
  | Texp_setinstvar _      -> todo "Texp_setinstvar cases..."
  | Texp_override _        -> todo "Texp_override cases..."
  | Texp_letmodule _       -> todo "Texp_letmodule cases..."
  | Texp_assertfalse       -> todo "Texp_assert false cases..."
  | Texp_lazy _            -> todo "Texp_Lazy cases..."
  | Texp_object _          -> todo "Texp_object cases..."
  | Texp_pack _            -> todo "Texp_pack cases..."

and from_params loc params =
  match params with
  | [] -> None
  | (pat, exp) :: tl ->
    if match_loc loc pat.pat_loc then Some pat.pat_type
    else
      match from_loc_expr loc exp with
      | None -> from_params loc tl
      | Some _ as ty -> ty

and from_loc_exprs loc = function
  | [] -> None
  | e :: tl ->
    match from_loc_expr loc e with
    | None -> from_loc_exprs loc tl
    | Some _ as ty -> ty (* Stop the loop, type founded *)

let from_loc loc bin_annot =
  from_binary_annots loc bin_annot

let print_ty_from_cmt loc cmtfilename =
  let cmt = Cmt_format.read_cmt cmtfilename in
  let bin_annot = cmt.cmt_annots in
  match from_loc loc bin_annot with
    | None -> Format.eprintf "No type founded...\n%!"
    | Some ty ->
      Printf.printf "Type founded...";
      Printtyp.raw_type_expr Format.std_formatter ty
