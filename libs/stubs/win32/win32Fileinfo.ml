
type fileinfo = {
  dwFileAttributes : int;
  ftCreationTime : float; (* in Unix seconds *)
  ftLastAccessTime : float; (* in Unix seconds *)
  ftLastWriteTime : float; (* in Unix seconds *)
  dwVolumeSerialNumber : int;
  nFileSize : int64;
  nNumberOfLinks : int;
  nFileIndex : int64;
}

external getFileInformationByHandle : Unix.file_descr -> fileinfo
  = "win32_getFileInformationByHandle_ml"

external getFileInformationByName : string -> fileinfo
  = "win32_getFileInformationByName_ml"

