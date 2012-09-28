
To build:
---------

1/ Install the sources of OCaml 4.00 as subdirectory ocaml/ocaml/

    cd ocaml
    tar zxf ~/Downloads/ocaml-4.00.0.tar.gz
    mv ocaml-4.00.0 ocaml
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

   
