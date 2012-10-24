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

#ifdef ALSO__CYGWIN__
#define _WIN32
#endif

#ifdef _WIN32

#include <windows.h>
#include <sys/types.h>

#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>

#ifndef CAML_UNIXSUPPORT_H
#include <caml/unixsupport.h>
#define CAML_UNIXSUPPORT_H
#endif

#ifdef _WIN32

value win32_setmode_ml(value fd_v, value binary_v)
{
  int mode = O_TEXT;
  if( binary_v == Val_true ) mode = O_BINARY;
  int oldmode = setmode(fd, mode);
  if ( oldmode == O_BINARY ) return Val_true;
  return Val_false;
}

#else

value win32_setmode_ml(value fd_v, value binary_v)
{
  return Val_true; /* always binary */  
}

#endif
