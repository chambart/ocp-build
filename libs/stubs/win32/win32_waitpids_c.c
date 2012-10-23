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
static value alloc_process_status(HANDLE pid, int status)
{
  value res, st;

  st = alloc(1, 0);
  Field(st, 0) = Val_int(status);
  Begin_root (st);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_long((intnat) pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

enum { CAML_WNOHANG = 1, CAML_WUNTRACED = 2 };

static int wait_flag_table[] = { CAML_WNOHANG, CAML_WUNTRACED };

CAMLprim value win32_waitpids_ml(value ncount_v, value pid_reqs_v)
{
  int flags,i;
  DWORD status, retcode;
  DWORD err = 0;
  int ncount = Int_val(ncount_v);
  HANDLE* pid_reqs;
  HANDLE pid_req;

  pid_reqs = malloc(sizeof(HANDLE) * ncount);
  for(i=0; i < ncount; i++){
     pid_reqs[i] = (HANDLE) Long_val(Field(pid_reqs_v,i));
  }
  enter_blocking_section();
  retcode = WaitForMultipleObjects(ncount, pid_reqs, FALSE,INFINITE);
  if (retcode == WAIT_FAILED) err = GetLastError();
  leave_blocking_section();
  if (err) {
    free(pid_reqs);
    win32_maperr(err);
    uerror("waitpids", Nothing);
  }
  pid_req = pid_reqs[retcode - WAIT_OBJECT_0];
  free(pid_reqs);
  if (! GetExitCodeProcess(pid_req, &status)) {
    win32_maperr(GetLastError());
    uerror("waitpids", Nothing);
  }
  if (status == STILL_ACTIVE)
    return alloc_process_status((HANDLE) 0, 0);
  else {
    CloseHandle(pid_req);
    return alloc_process_status(pid_req, status);
  }
}

#else

CAMLprim value win32_waitpids_ml(value ncount_v, value pid_reqs_v){
   uerror("win32_waitpids_ml", Nothing);
}

#endif

#ifdef _WIN32
extern value win_waitpid(value vflags, value vpid_req);
#else
extern value unix_waitpid(value vflags, value vpid_req);
#endif


value win32_waitpid_ml(value vflags, value vpid_req)
{
#ifdef _WIN32
  return win_waitpid(vflags, vpid_req);
#else
  return unix_waitpid(vflags, vpid_req);
#endif
}

