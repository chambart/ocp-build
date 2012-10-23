/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* OCamlPro: add O_TEXT and O_BINARY */
/* O_SHARE_DELETE was not defined. TODO: investigate why ! */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>


#ifndef CAML_UNIXSUPPORT_H
#include <caml/unixsupport.h>
#define CAML_UNIXSUPPORT_H
#endif

#include <string.h>
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif
#ifndef O_SHARE_DELETE
#define O_SHARE_DELETE 0
#endif
#ifndef O_BINARY
#define O_BINARY 0
#endif

static int open_flag_table[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC, O_SHARE_DELETE, O_TEXT, O_BINARY, 0
};

CAMLprim value win32_open_ml(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int ret, cv_flags;
  char * p;

  cv_flags = convert_flag_list(flags, open_flag_table);
  p = stat_alloc(string_length(path) + 1);
  strcpy(p, String_val(path));
  /* open on a named FIFO can block (PR#1533) */
  enter_blocking_section();
  ret = open(p, cv_flags, Int_val(perm));
  leave_blocking_section();
  stat_free(p);
  if (ret == -1) uerror("open", path);
  CAMLreturn (Val_int(ret));
}
