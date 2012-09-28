open SimpleConfig
open EditOptions

open Subcommands.TYPES

let print_elist elist =
  Printf.printf "%s\n"  (String.concat "\n" elist)

let add_hook mode hook =
  [ Printf.sprintf "(add-hook '%s %s)" mode hook ]

let add_lambda_hook mode body =
  [ Printf.sprintf "(add-hook '%s '(lambda()" mode ]
  @
    (List.map (fun s -> Printf.sprintf "    %s" s) body)
  @
    [ "  ))" ]

let defun name args body =
  [
    Printf.sprintf "(defun %s (%s)" name args
  ]
  @
   (List.map (fun s -> Printf.sprintf "  %s" s) body)
  @    [ ")" ]

let message msg =
  [ Printf.sprintf "(message %s)" msg ]

let message_quoted msg = message (Printf.sprintf "\"%s\"" msg)

let exec_on_buffer arg =
  [
    "   (let ((ocp-local-name (buffer-file-name)))";
    "     (let ((ocp-local-buffer (current-buffer)))";
    "        (with-temp-buffer ";
    "           (insert \"(set-buffer ocp-local-buffer)\")";
    "           (insert";
    Printf.sprintf "             (shell-command-to-string (concat \"ocp-edit-mode emacs %s '\" ocp-local-name \"'\"))) (eval-buffer))" arg;
    "     )";
    "   )";
  ]

let prepend_to_list list_name v =
  [ Printf.sprintf "(setq %s " list_name;
    Printf.sprintf "   (cons %s" v;
    Printf.sprintf "       %s))" list_name;
  ]

let setq name body =
  [ Printf.sprintf "(setq %s" name ] @
    List.map (fun s -> Printf.sprintf "  %s" s) body @
    [ ")" ]

let load_global_config () =
  print_elist
    (
      [
        Printf.sprintf "(setq debug-on-error %s)"
          (if !!debug_on_error then "t" else "nil")
      ] @
      (message_quoted "ocp-edit-mode global config loaded") @
        prepend_to_list "load-path"
        (Printf.sprintf  "\"%s/emacs\"" !!install_directory) @
        defun "typerex-mode" ""
        (
          message "(concat \"typerex-mode called for \" (buffer-name))" @
            exec_on_buffer "-load-local-config"
        ) @
        [
          "(autoload 'auto-complete-mode \"auto-complete\" \"Minor mode for completion\" t)";
          "(autoload 'tuareg-mode \"tuareg\" \"Major mode for editing Caml code\" t)";
          "(autoload 'caml-mode \"caml\" \"Major mode for editing OCaml code.\" t)";
          "(autoload 'ocp-fix-errors \"ocp-fix-errors.el\" \"Auto fix erros.\" t)";
          "(require 'font-lock)";

          "(require 'paren)";

          "(setq ocamlspot-command \"ocp-spotter\")";
          "(setq ocamlspot-debug t)";
          "(require 'ocamlspot)";


        ] @


        prepend_to_list "auto-mode-alist"
        "'(\"\\.ml[iylp]?$\" . typerex-mode)" @
        prepend_to_list "auto-mode-alist"
        "'(\"\\.ocp$\" . typerex-mode)" @

        [ "(setq save-abbrevs nil)"    ] @
        add_lambda_hook "tuareg-mode-hook"
        (exec_on_buffer "-tuareg-mode-hook")    @
        add_lambda_hook "caml-mode-hook"
        (exec_on_buffer "-caml-mode-hook")    @
        [
          "(setq completion-ignored-extensions";
          "  (append completion-ignored-extensions";
          "    '(\".o\") '(\"~\") '(\".cmo\") '(\".cmi\") '(\".cma\")";
          "    '(\".cmx\") '(\".cmxa\") '(\".cmxs\") '(\".cmt\") '(\".cmti\")";
          "  ))";
        ]

    )

let load_local_config filename =
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"ocp-edit-mode local config loaded\")";
         "(set-buffer ocp-local-buffer)";
         Printf.sprintf "(%s)" !!emacs_major_mode;
       ]);
()

let all_mode_hook mode filename =
  print_elist (
    [
      "(local-set-key  (kbd \"C-c C-f\") 'ocp-fix-errors)";
      "(local-set-key  (kbd \"C-c C-d\") 'ocp-fix-errors)";
    ]
    @
[
  "(local-set-key \"\\C-c;\" 'ocamlspot-query)";
  "(local-set-key \"\\C-c:\" 'ocamlspot-query-interface)";
  "(local-set-key \"\\C-c'\" 'ocamlspot-query-uses)";
  "(local-set-key \"\\C-c\\C-t\" 'ocamlspot-type)";
  "(local-set-key \"\\C-c\\C-i\" 'ocamlspot-xtype)";
  "(local-set-key \"\\C-c\\C-y\" 'ocamlspot-type-and-copy)";
  "(local-set-key \"\\C-cx\" 'ocamlspot-expand)";
  "(local-set-key \"\\C-c\\C-u\" 'ocamlspot-use)";
  "(local-set-key \"\\C-ct\" 'caml-types-show-type)";
  "(local-set-key \"\\C-cp\" 'ocamlspot-pop-jump-stack)";
]
      @
      [
      "(show-paren-mode t)";
      "(setq blink-matching-paren t)";
      "(setq blink-matching-paren-on-screen t)";
      "(setq show-paren-style 'expression)";
      "(setq blink-matching-paren-dont-ignore-comments t)";
      "(font-lock-mode t)";
      "(setq font-lock-maximum-decoration t)";
      "(setq column-number-mode t)";
      "(setq require-final-newline 'query)";
      "(require 'auto-complete-config)";

      "(defun ocp-candidates()";
      "   (let ((ocp-local-name (buffer-file-name)))";
      "     (let (result)";
      "      (with-temp-buffer ";
      "        (insert";
      "          (shell-command-to-string";
      "            (concat \"ocp-edit-mode candidates -infile \" ocp-local-name";
      "              \" '\" ac-prefix \"'\")))";
      "        (eval-buffer))";
      "      result)";
      "     )";
      "   )";

      "(defun ocp-documentation(candidate)";
      "   (let ((ocp-local-name (buffer-file-name)))";
      "     (let (result)";
      "      (with-temp-buffer ";
      "        (insert";
      "          (shell-command-to-string";
      "            (concat \"ocp-edit-mode documentation -infile \" ocp-local-name";
      "              \" '\" candidate \"'\")))";
      "        (eval-buffer))";
      "      result)";
      "     )";
      "   )";

      "(defun ocp-prefix-longident ()";
      "(let ((regexp \"[^a-zA-Z0-9'.]+\"))";
      "  (message (concat \"ocp-prefix-longident of \" regexp))";
      "  (let ((point (re-search-backward regexp nil t)))";
      (* completion does not work if the identifier starts at the beginning
         of the file *)
      "  (if point (1+ point)))))";

      "(ac-define-source ocp-complete";
      "  '((candidates . ocp-candidates)";
      "    (prefix . ocp-prefix-longident)";
      "    (document . ocp-documentation)";
      "    ))";

      "(setq ac-sources '(ac-source-ocp-complete))";

      "(setq ac-auto-start nil)";
      "(ac-set-trigger-key \"TAB\")";

      "(auto-complete-mode)";


      Printf.sprintf "(setq indent-tabs-mode %s)"
        (if !!indent_use_tabs then "t" else "nil");
      ] @
      List.map (fun (s1,s2) ->
        Printf.sprintf "  (define-abbrev %s-abbrev-table %S %S)" mode
          s1 s2) !!abbrevs @
      ["(abbrev-mode 1)" ]    @
      (if !!delete_trailing_whitespaces then
          [
            "(add-hook 'write-contents-hooks 'delete-trailing-whitespace)";
            "(setq show-trailing-whitespace t)";

          ]
       else [])
  )

let tuareg_mode_hook filename =
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading tuareg-mode-hook...\")";
         "(set-buffer ocp-local-buffer)";
       ]);
  all_mode_hook "tuareg-mode" filename;
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading tuareg-mode-hook done.\")";
       ]);
  ()

let caml_mode_hook filename =
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading caml-mode-hook...\")";
         "(set-buffer ocp-local-buffer)";
       ]);
  all_mode_hook "caml-mode" filename;

  Printf.printf "(require 'caml-font)\n";
  Printf.printf "(caml-font-set-font-lock)\n";
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading caml-mode-hook done.\")";
       ]);
  ()

let arg_list = [
  "-load-global-config", Arg.Unit load_global_config,
  " : load global Emacs config";
  "-load-local-config", Arg.String load_local_config,
  " FILENAME  : load local Emacs config for file";
  "-tuareg-mode-hook", Arg.Unit tuareg_mode_hook,    "";
  "-caml-mode-hook", Arg.Unit caml_mode_hook,    "";
]

let subcmd_spec = {
  subcmd_list = arg_list;
  subcmd_usage = [];
  subcmd_help = [];
}

let subcmd_main args = ()
let subcmd_init () = ()
