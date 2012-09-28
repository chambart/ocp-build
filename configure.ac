AC_INIT(typerex,0.1)
AC_COPYRIGHT(Copyright 2012 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

# Check that OCaml version is greater or equal to 3.12.1
if test "$VERSION_CHECK" = "yes" ; then
   AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [3.12.1],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
fi

test "x$prefix" = "xNONE" && prefix=$ac_default_prefix
test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"
libdir="`eval echo ${libdir}`"
libdir="`eval echo ${libdir}`"
datarootdir="`eval echo ${datarootdir}`"
datarootdir="`eval echo ${datarootdir}`"

AC_CONFIG_FILES(
  Makefile.config
  typerex.config
)
AC_OUTPUT

echo
echo Executables will be installed in ${bindir}
echo Libraries will be installed in ${libdir} + typerex
echo Data files will be installed in ${datarootdir} + typerex
echo Manual pages will be installed in ${mandir}
