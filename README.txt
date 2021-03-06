**************************************************************************
*                                                                        *
*                        TypeRex OCaml Studio                            *
*                                                                        *
*                 Thomas Gazagnaire, Fabrice Le Fessant                  *
*                                                                        *
*  Copyright 2011-2012 OCamlPro                                          *
*  All rights reserved.  This file is distributed under the terms of     *
*  the GNU Public License version 3.0.                                   *
*                                                                        *
*  TypeRex is distributed in the hope that it will be useful,            *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*  GNU General Public License for more details.                          *
*                                                                        *
**************************************************************************

ocp-build
=========

A simplified build tool for Objective-Caml projects. Contrary to
"make" and "ocamlbuild", "ocp-build" should not be used to build other
kinds of projects.

Features:
=========

* Files are compiled in place (so that .annot files and other files
can easily be found there), but generated binaries (.cmo, .cmi, .cmx)
are moved to per-project _obuild/ directories.

* Good multicore support: building my own set of libraries (363 source
files) in bytecode and native code takes 28s with -cores 5, but 81s
with -cores 1.

* All projects of one directory are described in one file, that can be
automatically generated by the tool.

Usage:
======
* To compile a package using ocp-build:
---------------------------------------

Just run 'ocp-build -scan' in the directory. It will look for files ending
in .ocp to know what needs to be compiled.

If it tells you some dependencies are missing, you might need to add a
description of the ocaml standard library. You can find an
(incomplete) such description here in the file
'ocp-standard-ocaml.ocp'.

You can increase the verbosity using the -v option:
-v -1: no messages, except in case of errors
-v 0: just the result message, except in case of errors
-v 1: short messages (default)
-v 2: more messages, etc.

You can also select the number of concurrent processes you want to use:
-cores 1: sequential execution
-cores 4: use 4 concurrent processes
-cores 5: use 5 concurrent processes (near optimal on quadcores)

If you don't want ocp-build to use the defaults for ocaml executables,
you can force it use other executables: Use the -conf option to
generate an ocp-build.conf file (in _obuild). Modify it to fit your
needs. If such a configuration should always be used, copy it in
~/.ocp/ocp-build.conf, or use the -global -conf file to generate it in
the first place.

If you don't want ocp-build to use your global configuration, use
the -no-global option.

* To use ocp-build to compile one of your project:
--------------------------------------------------

Define your project in a file with an .ocp extension:

For a program:

begin "xyz"
  type = program
  files = [ "x.ml"; "y.ml"; "z.ml" ]
  requires = [ "unix" ]
end

This tells ocp-build that it should generate a program "xyz"
("xyz.byte" for bytecode, and "xyz.asm" for native code), by compiling
the units "x.ml", "y.ml" and "z.ml", and linking with the "unix"
library (use 'ocp-standard-ocaml.ocp' for predefined libraries).

For a library, type should be 'library' instead of 'program'.

ocp-build will correctly manage usual OCaml file extensions, like
"x.mli", "x.mll" and "x.mly".

You can specify extra options to append to the commands arguments:
pp = "camlp4o" (* use camlp4o with the -pp option *)
o = "-g" (* use the -g option with all ocaml commands *)
byte = "-g" (* use -g only for bytecode commands *)
asm = "-g" (* use -g only for native commands *)
comp = "-g" (* use -g only for compiling *)
link = "-g" (* use -g only for linking *)
bytecomp = "-g"
bytelink = "-g"
asmcomp = "-g"
asmlink = "-g"
dep = "-I foo" (* use -I foo with ocamldep *)

These options can be specified:
1/ for a set of projects "x" and "y", but not "z":

   begin
	comp = "-g"
	begin "x" ... end
	begin "y" ... end
   end
   begin "z" ... end

2/ for one project:

   begin "x"
   	 comp = "-g"
	 ...
   end

3/ for one file in a project:
   begin "x"
   	 ...
   	 files = [ "x.ml" (comp = "-g") "y.ml" "z.ml"]
	 ...
   end

