
To clone:
---------

0/ 

    git clone git@github.com:OCamlPro/typerex.git
    cd typerex
    git checkout typerex2

To build:
---------

1/ Install the sources of OCaml 4.00 as subdirectory ocaml/ocaml/

    cd ocaml
    svn checkout http://caml.inria.fr/svn/ocaml/version/4.00 ocaml
    cd ..

2/ configure

   ./configure -prefix /usr/local

3/ Build with ocp-build. You will need to create an ocp-build.conf file in 
   the top directory.

    make

4/ Install

   make install

5/ Build and read the documentation

   make doc
   cd docs/user-manual
   evince user-manual.pdf

   
