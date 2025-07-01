;+
; NAME:
;     FIND_NEAREST_MATCHING_FILES_IN_REPO
;
; PURPOSE:
;     Recursive search for "closest" files matching a pattern, starting in the
;     given directory or in this routine's directory if none is given. 
;
;     If no files are found under the starting directory, the directory above is searched
;     recursively. If still not found, the directory above that again is searched, etc.
;
;     The search stops at the (git) repository boundary, i.e. after no files
;     matching the pattern have been found under the top of the repository.
;
; CALLING SEQUENCE:
;     FILES = PTOOLS.FIND_NEAREST_MATCHING_FILES_IN_REPO(pattern [, start_directory])
;
; INPUTS:
;     PATTERN: File matching pattern (as in FILE_SEARCH)
;
; OPTIONAL INPUT:
;     START_DIRECTORY: the directory to start looking for files matching the pattern
;
; KEYWORD PARAMETERS:
;
; RETURNS:
;     Returns the list of files found, or a scalar empty string if none found
;
; MODIFICATION HISTORY:
;     Ver.1, 2025-07-01 Stein Haugan
;-
; $Id: 2025-07-01 18:14 CEST $

FUNCTION ptools::find_nearest_matching_files_in_repo, pattern, start_dir
  compile_opt static
  
  dir = keyword_set(start_dir) ? start_dir : routine_dir()
  paths = strsplit(dir, path_sep(), /extract)
  
  ; /path/to/us/ => ["path","to","us"] so /path/to/us/../../../ equals /
  ;
  max_up = n_elements(paths)-1
  
  FOR i=0, max_up DO BEGIN
     files = file_search(dir, pattern)
     IF files[0] NE "" THEN return, files
     lastdir = dir
     git_top = file_test(dir + path_sep() + ".git/config")
     svn_top = file_test(dir + path_sep() + ".svn/format")
     IF git_top OR svn_top THEN BEGIN
        print, "Repo top " + dir + " reached and no files matching " + pattern
        return, ""
     END
     dir = dir + "../"
  END
  print, "Could not find " + pattern + " up to " + lastdir
  return, ""
END
