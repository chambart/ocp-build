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
