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

DOUBLE FileTime_to_POSIX(FILETIME ft)
{
  ULARGE_INTEGER date, adjust;
  date.HighPart = ft.dwHighDateTime;
  date.LowPart = ft.dwLowDateTime;

  // 100-nanoseconds = milliseconds * 10000
  adjust.QuadPart = 11644473600000ULL * 10000;
  // removes the diff between 1970 and 1601
  date.QuadPart -= adjust.QuadPart;
  // converts back from 100-nanoseconds to seconds
  return date.QuadPart / 1e7;
}


value win32_getFileInformationByHandle_ml(value handle_v)
{
  HANDLE handle = (HANDLE)handle_v;
  BY_HANDLE_FILE_INFORMATION fileInfo;
  CAMLparam0 ();
  CAMLlocal1 (v);
  ULARGE_INTEGER size, index;

  if( !GetFileInformationByHandle(handle, &fileInfo) ){
    DWORD err = GetLastError();
    win32_maperr(err);
    uerror("GetFileInformationByHandle", Nothing);
  }

  size.HighPart = fileInfo.nFileSizeHigh;
  size.LowPart = fileInfo.nFileSizeLow;
  index.HighPart = fileInfo.nFileIndexHigh;
  index.LowPart = fileInfo.nFileIndexLow;

  v = caml_alloc (8, 0);
  Store_field(v,0, Val_int(fileInfo.dwFileAttributes));
  Store_field(v, 1,
              caml_copy_double(FileTime_to_POSIX(fileInfo.ftCreationTime)));
  Store_field(v, 2,
              caml_copy_double(FileTime_to_POSIX(fileInfo.ftLastAccessTime)));
  Store_field(v, 3,
              caml_copy_double(FileTime_to_POSIX(fileInfo.ftLastWriteTime)));
  Store_field(v, 4, Val_int(fileInfo.dwVolumeSerialNumber));
  Store_field(v, 5, caml_copy_int64(size.QuadPart));
  Store_field(v, 6, Val_int(fileInfo.nNumberOfLinks));
  Store_field(v, 7, caml_copy_int64(index.QuadPart));

  CAMLreturn (v);
}

value win32_getFileInformationByName_ml(value filename_v)
{
  HANDLE hfile = CreateFile(String_val(filename_v), 0, 
			    FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE, 
			    NULL,   OPEN_EXISTING, 
			    FILE_FLAG_BACKUP_SEMANTICS, NULL);
  value res;

  if( hfile == INVALID_HANDLE_VALUE ){
    DWORD err = GetLastError();
    win32_maperr(err);
    uerror("GetFileInformationByName", Nothing);
  }
  res = win32_getFileInformationByHandle_ml((value)hfile);
  CloseHandle(hfile);
  return res;
}

#else

value win32_getFileInformationByHandle_ml(value handle_v)
{
  uerror("win32_getFileInformationByHandle_ml", Nothing);
}

value win32_getFileInformationByName_ml(value filename_v)
{
  uerror("win32_getFileInformationByName_ml", Nothing);
}
#endif
