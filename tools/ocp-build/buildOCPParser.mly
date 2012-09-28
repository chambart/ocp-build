/******************************************************************************/
/*                                                                            */
/*                          TypeRex OCaml Tools                               */
/*                                                                            */
/*                               OCamlPro                                     */
/*                                                                            */
/*    Copyright 2011-2012 OCamlPro                                            */
/*    All rights reserved.  See accompanying files for the terms under        */
/*    which this file is distributed. In doubt, contact us at                 */
/*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         */
/*                                                                            */
/******************************************************************************/

%{

open BuildOCPTree
  (* TODO: location of the type in ocamlyacc is erroneous, for example here token "main"
   type is located in the .mli/.ml file instead of the .mly file. *)

%}

%token <string> STRING
%token <int> INT
%token EOF
%token <float> FLOAT
%token <char> CHAR
%token SEMI
%token BEGIN
%token END
%token <string> IDENT
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token PLUSEQUAL
%token MINUSEQUAL
%token TRUE
%token FALSE
%token INCLUDE
%token <BuildOCPTree.statement list> INCLUDED
%token OBJECTS
%token LIBRARY
%token PROGRAM
%token CONFIG
%token EQUAL
%token LPAREN
%token RPAREN
%token FILES
%token REQUIRES
%token TYPE
%token FILE
%token USE
%token PACK
%token IF
%token THEN
%token ELSE
%token NOT
%token COND_OR
%token COND_AND
%token SYNTAX
%token CAMLP4
%token CAMLP5

%start main
%type <BuildOCPTree.statement list> main

%%

main:
toplevel_statements EOF { $1 }
;

toplevel_statements:
  { [] }
| INCLUDED toplevel_statements { $1 @ $2 }
| toplevel_statement toplevel_statements { $1 :: $2 }
| SEMI toplevel_statements { $2 }
;

package_type:
  PROGRAM { ProjectProgram }
| LIBRARY { ProjectLibrary }
| OBJECTS { ProjectObjects }
;

toplevel_statement:
| BEGIN CONFIG STRING options END { StmtDefineConfig ($3, $4) }
| BEGIN package_type STRING statements END { StmtDefinePackage ($2, $3, $4) }
| BEGIN toplevel_statements END { StmtBlock $2 }
| IF condition THEN one_toplevel_statement maybe_else_one_toplevel_statement { StmtIfThenElse($2,$4,$5) }
| simple_statement { $1 }

/* for backward compatibility */
| BEGIN STRING TYPE EQUAL package_type statements END { StmtDefinePackage ($5, $2, $6) }
;

one_toplevel_statement:
| toplevel_statement { [$1] }
| LBRACE toplevel_statements RBRACE { $2 }
;

statements:
| statement statements { $1 :: $2 }
| { [] }
| SEMI statements { $2 }
;

statement:
| BEGIN statements END { StmtBlock $2 }
| IF condition THEN one_statement maybe_else_one_statement { StmtIfThenElse($2, $4, $5) }
| simple_statement { $1 }
;

one_statement:
| statement { [$1] }
| LBRACE statements RBRACE { $2 }
;

simple_statement:
| simple_option { StmtOption $1 }
| FILES EQUAL list_of_files { StmtFilesSet $3 }
| FILE EQUAL STRING { StmtFilesSet [$3, []] }
| FILES PLUSEQUAL list_of_files { StmtFilesAppend $3 }
| REQUIRES list_of_strings { StmtRequiresAppend $2 }
| SYNTAX STRING EQUAL camlp4_or_camlp5 list_of_strings { StmtSyntax (Some $2, $4, $5) }
| SYNTAX EQUAL camlp4_or_camlp5 list_of_strings { StmtSyntax (None, $3, $4) }
/* The two next ones only for backward compatibility */
| REQUIRES EQUAL list_of_strings { StmtRequiresAppend $3 }
| REQUIRES PLUSEQUAL list_of_strings { StmtRequiresAppend $3 }
;

camlp4_or_camlp5:
| CAMLP4 { Camlp4 }
| CAMLP5 { Camlp5 }
;

simple_option:
| USE STRING { OptionConfigSet $2 }
| IDENT EQUAL string_or_list { OptionListSet ($1,$3) }
| IDENT PLUSEQUAL string_or_list { OptionListAppend ($1,$3) }
| IDENT MINUSEQUAL string_or_list { OptionListRemove ($1,$3) }
| IDENT EQUAL TRUE { OptionBoolSet ($1, true) }
| IDENT EQUAL FALSE { OptionBoolSet ($1, false) }
| SYNTAX EQUAL STRING { OptionListSet ("syntax", [$3]) }
;

string_or_list:
| STRING { [$1] }
| list_of_strings { $1 }
;

option:
| BEGIN options END { OptionBlock $2 }
| IF condition THEN one_option maybe_else_one_option { OptionIfThenElse($2, $4, $5) }
| simple_option { $1 }
;

one_option:
| option { [$1] }
| LBRACE options RBRACE { $2 }
;

maybe_else_one_option:
| { None }
| ELSE one_option { Some $2 }
;

maybe_else_one_statement:
| { None }
| ELSE one_statement { Some $2 }
;

maybe_else_one_toplevel_statement:
| { None }
| ELSE one_toplevel_statement { Some $2 }
;

condition:
| IDENT EQUAL string_or_list { IsEqualStringList($1, $3) }
| NOT simple_condition { NotCondition $2 }
| simple_condition COND_AND simple_condition { AndConditions ($1, $3) }
| simple_condition COND_OR simple_condition { OrConditions ($1, $3) }
| simple_condition { $1 }
;

simple_condition:
| LPAREN condition RPAREN { $2 }
| IDENT { IsTrue $1 }
;

options:
| { [] }
| option options { $1 :: $2 }
| SEMI options { $2 }
;

list_of_options:
|                        { [] }
|  LPAREN options RPAREN { $2 }
;

list_of_strings:
| LBRACKET strings RBRACKET { $2 }
;

strings:
 { [] }
| STRING strings { $1 :: $2 }
| IDENT strings { $1 :: $2 }
| SEMI strings { $2 }
;

list_of_files:
| LBRACKET files RBRACKET { $2 }
;

packer:
| PACK STRING { $2 }
| PACK IDENT  { let s = $2 in s.[0] <- Char.lowercase s.[0]; s ^ ".ml" }
;

files:
 { [] }
| STRING list_of_options files { ($1, $2) :: $3 }
| SEMI files { $2 }
| BEGIN list_of_options files END files
   { let shared_options = $2 in
     let inner_files = $3 in
     let outter_files = $5 in
     let inner_files =
       List.map (fun (file, file_options) ->
	 (file, shared_options @ file_options)
       ) inner_files in
     inner_files @ outter_files
   }
| packer list_of_options list_of_files list_of_options files {
  let packname = $1 in
  let pack_options1 = $2 in
  let files = $3 in
  let pack_options2 = $4 in
  let other_files = $5 in
  let pack_options = pack_options1 @ pack_options2 in

  let packmodname = BuildOCPTree.modname_of_fullname packname in

  let modnames = ref [] in
  let packed_files =
    List.map (fun (file, file_options) ->
      begin
        match file_options with
            OptionListAppend ( "packed", _ ) :: _ -> ()
          | _ ->
            modnames := Filename.basename file :: !modnames
      end;
      (file, OptionListAppend ("packed", [packmodname]) :: pack_options @ file_options)
  ) files;
  in
  packed_files @
    [ packname, OptionListSet ("pack", List.rev !modnames) :: pack_options] @
    other_files
}
;

%%
