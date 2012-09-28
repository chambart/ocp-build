
let arg_list = [

]

let _ =

  Subcommands.parse arg_list
    [
      "candidates", CompCandidates.subcmd_init, CompCandidates.subcmd_spec, CompCandidates.subcmd_main;
      "documentation", CompDocumentation.subcmd_init, CompDocumentation.subcmd_spec, CompDocumentation.subcmd_main;
    ]
    (String.concat "\n"
       ["ocp-complete SUB-COMMAND [OPTIONS] ARGUMENTS";
        "Complete prefixes from context";
        "where SUB-COMMAND:";
        ""
       ]
    )
