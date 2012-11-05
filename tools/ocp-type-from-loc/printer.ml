open Typedtree
open Types
open Format
open Cmt_format

let pp_ident ppf id = fprintf ppf "%s" id.Ident.name

let rec pp_path ppf = function
  | Path.Pident id      -> fprintf ppf "%a" pp_ident id
  | Path.Pdot (p,s,i)   -> fprintf ppf "%a.%s(%i)" pp_path p s i
  | Path.Papply (p1,p2) -> fprintf ppf "%a %a" pp_path p1 pp_path p2

let pp_loc = Location.print

let rec pp_longident ppf = function
  | Longident.Lident str ->
    fprintf ppf "%s" str
  | Longident.Ldot (l, str) ->
    fprintf ppf "Ldot(%a.%s)" pp_longident l str
  | Longident.Lapply (l1, l2) ->
    fprintf ppf "Lapply(%a %a)" pp_longident l1 pp_longident l2

let pp_longident_loc ppf lloc =
  fprintf ppf "%a" pp_longident lloc.Location.txt

let pp_cons ppf = function
  | Asttypes.Const_int i       -> fprintf ppf "%i" i
  | Asttypes.Const_char cons   -> fprintf ppf "%c" cons
  | Asttypes.Const_string str  -> fprintf ppf "%s" str
  | Asttypes.Const_float f     -> fprintf ppf "%s" f
  | Asttypes.Const_int32 i32   -> fprintf ppf"i32:%s" (Int32.to_string i32)
  | Asttypes.Const_int64 i64   -> fprintf ppf "i64:%s" (Int64.to_string i64)
  | Asttypes.Const_nativeint i -> fprintf ppf "nativei:%s" (Nativeint.to_string i)

let pp_vdesc ppf = function
  | Val_reg           -> ()
  | Val_prim p        -> fprintf ppf "Prim"
  | Val_self _        -> fprintf ppf "Val_self"
  | Val_anc _         -> fprintf ppf "Val_anc"
  | Val_unbound       -> fprintf ppf "Val_unbound"
  | Val_ivar (m, str) -> fprintf ppf "Val_ivar (%s)" str

let rec pp_core_type ppf = function
  | Texp_ident (path, lloc, vdesc) ->
    fprintf ppf "Ident(%a%a)" pp_longident_loc lloc pp_vdesc vdesc.val_kind
  | Texp_constant c ->
    fprintf ppf "%a" pp_cons c
  | Texp_let (rec_flag, binds, body) ->
    let rec letbody = function
      |  Texp_let (rec_flag, binds, body) ->
        fprintf ppf "@ @[<2>%a@ @]" pp_patexp binds;
        letbody body.exp_desc
      | e -> e
    in
    fprintf ppf "@[<2>Let@ @[<hv 1>(@[<2>%a @]" pp_patexp binds;
    let e = letbody body.exp_desc in
    fprintf ppf ")@]@ %a@]" pp_core_type e
  | Texp_function (lbl, args, partial) ->
    begin match partial with
    | Partial -> fprintf ppf "funP%a" pp_patexp args;
    | Total   -> fprintf ppf "funT%a" pp_patexp args;
    end
  | Texp_apply (f, param) ->
    let pp_param ppf ll =
      let first = ref true in
      List.iter (fun l ->
        match l with
        | (lbl, Some expr, _) ->
          fprintf ppf "%s %a" lbl pp_core_type expr.exp_desc;
          if not !first then begin
            fprintf ppf ",";
            first := false;
          end
        | (lbl, None, _) -> ()
      ) ll in
      fprintf ppf "@[<2>(Apply@ %a%a)@]" pp_core_type f.exp_desc pp_param param
  | Texp_match (e, cases, part) ->
    fprintf ppf "Match %a with" pp_core_type e.exp_desc;
    fprintf ppf "@ %a" pp_patexp cases
  | Texp_try (e, cases) ->
    fprintf ppf "Try@ %a with" pp_core_type e.exp_desc;
    fprintf ppf "@ %a" pp_patexp cases
  | Texp_tuple exp_list ->
    let first = ref true in
    let aux ppf l =
      List.iter (fun x ->
        if !first then begin
          fprintf ppf "%a" pp_core_type x.exp_desc;
          first := false;
        end else
          fprintf ppf ",@ %a" pp_core_type x.exp_desc) l in
    fprintf ppf "Tuple(%a)" aux exp_list
  | Texp_construct (path,loc,cnstr_desc,exp_list,_) ->
    fprintf ppf "Cons(";
    let first = ref true in
    List.iter (fun x ->
        if !first then begin
          fprintf ppf "%a" pp_core_type x.exp_desc;
          first := false;
        end else
          fprintf ppf ", %a" pp_core_type x.exp_desc
    ) exp_list;
    fprintf ppf ")"
  | Texp_variant (lbl,e_opt) -> fprintf ppf "variant"
  | Texp_record (list,e_opt) ->
      begin
        match e_opt with
          | None -> fprintf ppf "{}"
          | Some e -> fprintf ppf "{S}"
      end
  | Texp_sequence (e1,e2) ->
      fprintf ppf "%a ; %a" pp_core_type e1.exp_desc pp_core_type e2.exp_desc
  | Texp_field (e,path,loc,lbl_desc) ->
      fprintf ppf "%a.%a" pp_core_type e.exp_desc pp_path path
  | Texp_when (e1,e2) ->
    fprintf ppf "When %a -> %a"
      pp_core_type e1.exp_desc
      pp_core_type e2.exp_desc
  | Texp_ifthenelse (cond, thenb, elseb) ->
    fprintf ppf "IF(%a, %a" pp_core_type cond.exp_desc pp_core_type thenb.exp_desc;
    begin match elseb with
    | None -> fprintf ppf ")"
    | Some elseb -> fprintf ppf ",%a)" pp_core_type elseb.exp_desc
    end
  | Texp_setfield (_, _, _, _, _) -> fprintf ppf "setfield"
  | Texp_array _ -> fprintf ppf "array"
  | Texp_while (_, _) -> fprintf ppf "while"
  | Texp_for (_, _, _, _, _, _) -> fprintf ppf "for"
  | Texp_send (_, _, _) -> fprintf ppf "send"
  | Texp_new (_, _, _) -> fprintf ppf "new"
  | Texp_instvar (_, _, _) -> fprintf ppf "instvar"
  | Texp_setinstvar (_, _, _, _) -> fprintf ppf "setinstvar"
  | Texp_override (_, _) -> fprintf ppf "override"
  | Texp_letmodule (_, _, _, _) -> fprintf ppf "letmodule"
  | Texp_assert _ -> fprintf ppf "assert"
  | Texp_assertfalse -> fprintf ppf "assertfalse"
  | Texp_lazy _ -> fprintf ppf "lazy"
  | Texp_object (_, _) -> fprintf ppf "object"
  | Texp_pack _ -> fprintf ppf "pack"

and print_apply ppf (e,list) =
  let aux ppf = function
    | (lbl,Some e1,_) ->
        (* fprintf ppf "%s ," lbl *)
        fprintf ppf "%s %a," lbl pp_core_type e1.exp_desc
    | (lbl,None,_) ->
        fprintf ppf "%s," lbl
  in
  let pr_param ppf ll =
    List.iter (fun l -> fprintf ppf "@ %a" aux l) ll in
  (* fprintf ppf "%a (%a)" pp_core_type e.exp_desc aux list *)
  fprintf ppf "@[<2>(apply@ %a%a)@]" pp_core_type e.exp_desc pr_param list


and pp_pattern ppf = function
  | Tpat_any -> fprintf ppf "Any"
  | Tpat_var (id, _) -> fprintf ppf "%a" pp_ident id
  | Tpat_alias (p, _, _) -> fprintf ppf "Alias(%a)" pp_pattern p.pat_desc
  | Tpat_constant cons -> fprintf ppf "(%a)" pp_cons cons
  | Tpat_tuple pat_list -> fprintf ppf "Tuple"
  | Tpat_construct (path, loc, cnstor_desc, pat_list, _) -> fprintf ppf "Constr"
  | Tpat_variant (lbl, pat_option, row_desc) -> fprintf ppf "Variant"
  | Tpat_record (list,flag) ->
    List.iter (fun (path,_,_,pat) ->
      fprintf ppf "%a = %a," pp_path path pp_pattern pat.pat_desc) list
  | Tpat_array pat_list -> fprintf ppf "Array"
  | Tpat_or (pat1,pat2,row_desc_opt) -> fprintf ppf "Or"
  | Tpat_lazy pat -> fprintf ppf "Lazy"

and pp_patexp ppf = function
  | [] -> ()
  | (pat, e) :: tl ->
    fprintf ppf "@[<2>(%a@ %a)@]"
      pp_pattern pat.pat_desc pp_core_type e.exp_desc;
    pp_patexp ppf tl

let rec print_core_type_desc ppf = function
  | Ttyp_any -> fprintf ppf "Ttyp_any"
  | Ttyp_var s -> fprintf ppf "Ttyp_var %s" s
  | Ttyp_arrow (lbl,_,_) -> fprintf ppf "Ttyp_var %s" lbl
  | Ttyp_tuple _ -> fprintf ppf "Ttyp_tuple"
  | Ttyp_constr (path,_,_) -> fprintf ppf "constr "
  | Ttyp_object _ -> fprintf ppf "Ttyp_object"
  | Ttyp_class (path,_,_,_) -> fprintf ppf "cl%a" pp_path path
  | Ttyp_alias (_,s) -> fprintf ppf "Ttyp_alias %s" s
  | Ttyp_package _ -> fprintf ppf "Ttyp_package"
  | Ttyp_variant (_,_,_) -> fprintf ppf "Ttyp_variant"
  | Ttyp_poly (l,ct) -> fprintf ppf "%a" print_core_type ct


and print_core_type ppf ct =
  fprintf ppf "%a" print_core_type_desc ct.ctyp_desc

let print_type ppf (id,loc,typ_desc) =
  let print_type_var ppf =
    List.iter (fun (id,_,ctl,_) ->
      fprintf ppf "@ %a " pp_ident id;
      List.iter (print_core_type ppf) ctl) in
  let print_type_rec ppf =
    List.iter (fun (id,_,_,ct,_) ->
      fprintf ppf "@ %a:%a " pp_ident id print_core_type ct) in
  let print_type_desc ppf td = match td.typ_kind with
    | Ttype_variant l -> fprintf ppf "%a" print_type_var l
    | Ttype_record l -> fprintf ppf "%a" print_type_rec l
    | Ttype_abstract -> fprintf ppf "abst"
  in
  fprintf ppf "Type %a = %a@." pp_ident id print_type_desc typ_desc

let rec print_mod_expr ppf me = match me.mod_desc with
  | Tmod_ident (p, _) -> fprintf ppf "Tmod_ident (%a,_)" pp_path p
  | Tmod_structure str -> fprintf ppf "Tmod_struct(%a)" print_structure_items str.str_items
  | Tmod_functor (id, _, _, me) ->
      fprintf ppf "Tmod_fun(%a,_,%a)" pp_ident id print_mod_expr me
  | Tmod_apply (me1, me2, _) ->
      fprintf ppf "Tmod_appl((%a,%a,_)" print_mod_expr me1  print_mod_expr me2
  | Tmod_constraint (me, _, _, _) -> fprintf ppf "Tmod_constraint"
  | Tmod_unpack (_,_)  -> fprintf ppf "Tmod_unpack"


and print_struct_item_descr ppf = function
  | Tstr_eval e ->
      fprintf ppf "%a@." pp_core_type e.exp_desc
  | Tstr_value (recflag, list) ->
      fprintf ppf "@[<2>(let@ %a@]@ )@])@]@]@." pp_patexp list;
  | Tstr_primitive (_, _, _) -> fprintf ppf "Tprimitive@."
  | Tstr_type l ->  List.iter (fprintf ppf "%a@." print_type) l
  | Tstr_exception (_, _, _) -> fprintf ppf "Texception@."
  | Tstr_exn_rebind (_, _, _, _) -> fprintf ppf "Texn_rebind@."
  | Tstr_module (id, _, mod_expr) ->
      fprintf ppf "Tstr_module (%a,_,%a)@." pp_ident id print_mod_expr mod_expr
  | Tstr_recmodule _ -> fprintf ppf "Trecmodule@."
  | Tstr_modtype (_, _, _) -> fprintf ppf "Tmodtype@."
  | Tstr_open (_, _) -> fprintf ppf "Topen@."
  | Tstr_class _ -> fprintf ppf "Tclass@."
  | Tstr_class_type _ -> fprintf ppf "Tclass_type@."
  | Tstr_include (_, _) -> fprintf ppf "Tinclude@."

and print_structure_items ppf = function
  | [] -> ()
  | x::xs ->
      print_struct_item_descr ppf x.str_desc;
      print_structure_items ppf xs

let print_annot ppf =
  let open Cmt_format in
  function
  | Implementation strct ->
      fprintf ppf "%a@." print_structure_items strct.str_items
  | _ -> fprintf ppf "Can't print that"

let print ppf filename =
  let cmt_inf = Cmt_format.read_cmt filename in
  print_annot ppf cmt_inf.Cmt_format.cmt_annots
