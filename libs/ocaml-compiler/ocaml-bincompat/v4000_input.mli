val input_cmi : in_channel -> Cmi_format.cmi_infos
val input_ast_intf :
  in_channel -> string * V4000_types.Parsetree.signature
val input_ast_impl :
  in_channel -> string * V4000_types.Parsetree.structure
