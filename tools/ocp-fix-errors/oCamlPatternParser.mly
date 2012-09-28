%{

  open OcpLang
  open OCamlPattern

  let overflow_p x =
    match x with
        Approx_common.Overflow s -> Printf.kprintf failwith "Overflow %s" s
      | Approx_common.InRange x -> x

  let mkpat x = x
  let ghpat x = x

  let rec mktailpat = function
    [] ->
      ghpat(Ppat_construct([ "[]" ], None, false))
  | p1 :: pl ->
      let pat_pl = mktailpat pl in
      let arg = Ppat_tuple [p1; pat_pl] in
      Ppat_construct( [ "::" ], Some arg, false)

let pat_of_label lbl =
  Ppat_var(List.last lbl)

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char Approx_common.overflow> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token <int * int> COMMENT
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token <int> EOF_IN_COMMENT
%token <int> EOF_IN_STRING
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token <char> ILLEGAL_CHAR
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int Approx_common.overflow> INT
%token <int32 Approx_common.overflow> INT32
%token <int64 Approx_common.overflow> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint Approx_common.overflow> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PLUS
%token PLUSDOT
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUESTIONQUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
  in favor of the first rule (in source file order).  A shift/reduce conflict
      is resolved by comparing the precedence and associativity of the token to
          be shifted with those of the rule to be reduced.

              By default, a rule has the precedence of its rightmost terminal (if any).

                  When there is a shift/reduce conflict between a rule and a token that
                      have the same precedence, it is resolved using the associativity:
                        if the token is left-associative, the parser will reduce; if
                            right-associative, the parser will shift; if non-associative,
                              the parser will declare a syntax error.

                              We will only use associativities with operators of the kind  x * x -> x
                                for example, in the rules of the form    expr: expr BINOP expr
                                                                        in all other cases, we define two precedences if needed to resolve
                                                                            conflicts.

                                                                            The precedences must be listed from low to high.
                                                                                */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start pattern
%type <OCamlPattern.pattern> pattern

%%


uident:
    UIDENT { $1 }
lident:
    LIDENT { $1 }

ident:
    uident                                      { $1 }
  | lident                                      { $1 }
;
val_ident:
    lident                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
label_longident:
    LIDENT                                      {  [$1] }
  | mod_longident DOT LIDENT                    {  $1 @ [$3] }
;

operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | BANG                                        { "!" }
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident_no_loc:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
constr_ident:
    constr_ident_no_loc                         { $1 }

val_longident:
    val_ident                                   { [$1] }
  | mod_longident DOT val_ident                 { $1 @ [ $3 ] }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { ["[]"] }
  | LPAREN RPAREN                               { ["()"] }
  | FALSE                                       { ["false"] }
  | TRUE                                        { ["true"] }
;
mod_longident:
    UIDENT                                      { [$1] }
  | mod_longident DOT UIDENT                    {  $1 @ [$3] }
;

name_tag:
    BACKQUOTE ident                             { $2 }
;

constant:
    INT                                         { Const_int (overflow_p $1) }
  | CHAR                                        { Const_char (overflow_p $1) }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
  | INT32                                       { Const_int32 (overflow_p $1) }
  | INT64                                       { Const_int64 (overflow_p $1) }
  | NATIVEINT                                   { Const_nativeint (overflow_p $1) }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { Const_int(- (overflow_p $2) ) }
  | MINUS FLOAT                                 { Const_float("-" ^ $2) }
  | MINUS INT32                                 { Const_int32(Int32.neg (overflow_p $2)) }
  | MINUS INT64                                 { Const_int64(Int64.neg (overflow_p $2) ) }
  | MINUS NATIVEINT                             { Const_nativeint(Nativeint.neg (overflow_p $2) ) }
  | PLUS INT                                    { Const_int (overflow_p $2) }
  | PLUS FLOAT                                  { Const_float $2 }
  | PLUS INT32                                  { Const_int32 (overflow_p $2) }
  | PLUS INT64                                  { Const_int64 (overflow_p $2) }
  | PLUS NATIVEINT                              { Const_nativeint (overflow_p $2) }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;

/* Patterns */

pattern:
    simple_pattern      { $1 }
  | pattern AS val_ident { Ppat_alias($1, $3) }
  | pattern_comma_list  %prec below_COMMA
      { Ppat_tuple(List.rev $1) }
  | constr_longident pattern %prec prec_constr_appl
      { Ppat_construct($1, Some $2, false) }
  | name_tag pattern %prec prec_constr_appl
      { Ppat_variant($1, Some $2) }
  | pattern COLONCOLON pattern
      { Ppat_construct( ["::"],  Some(Ppat_tuple[$1;$3]), false) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { Ppat_construct( [ "::" ], Some(Ppat_tuple[$5;$7]),false) }
  | pattern BAR pattern
      { Ppat_or($1, $3) }
  | LAZY simple_pattern
      { Ppat_lazy $2 }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
/*  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 } */
  | constr_longident
      { mkpat(Ppat_construct($1, None, false)) }
  | name_tag
      { mkpat(Ppat_variant($1, None)) }
/*
  | SHARP type_longident
      { mkpat(Ppat_type $2) }
*/
  | LBRACE lbl_pattern_list record_pattern_end RBRACE
      { mkpat(Ppat_record(List.rev $2, $3)) }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { mktailpat (List.rev $2) }
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpat(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LPAREN pattern RPAREN
      { $2 }
/*
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
*/
  | LPAREN MODULE UIDENT RPAREN
      { mkpat(Ppat_unpack $3) }
/*
  | LPAREN MODULE UIDENT COLON package_type RPAREN
      { mkpat(Ppat_constraint(mkpat(Ppat_unpack $3),ghtyp(Ptyp_package $5))) }
*/
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    label_longident EQUAL pattern               { [($1, $3)] }
  | label_longident                             { [($1, pat_of_label $1)] }
  | lbl_pattern_list SEMI label_longident EQUAL pattern { ($3, $5) :: $1 }
  | lbl_pattern_list SEMI label_longident       { ($3, pat_of_label $3) :: $1 }
;
record_pattern_end:
    opt_semi                                    { true }
  | SEMI UNDERSCORE opt_semi                    { false }
;


%%
