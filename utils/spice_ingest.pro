;+
; NAME:
;      SPICE_INGEST
;
; PURPOSE:
;      This routine takes as input a SPICE filename (or list of files, or a directory)
;      and correctly places them in the SPICE data tree, creating new
;      directories if necessary. $SPICE_DATA needs to be defined.
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;	     SPICE_INGEST [, Filename, path_index=path_index, /force, /nolevel, /search_subdir, /help, $
;	                   destination=destination, file_moved=file_moved, files_found=files_found]
;
; INPUTS:
;      Filename: The name and path of a SPICE file. Can be an array of names.
;                Or a directory (scalar), then all spice files in this directory will be moved.
;                If not provided, all files from the current directory will be moved.
;
; OPTIONAL INPUTS:
;      PATH_INDEX: If $SPICE_DATA contains multiple paths, then this
;               keyword allows you to specify to which path you send
;               the file. Default is 0.
;      TOP_DIR: A path to a directory in which the file should be saved. The necessary subdirectories
;               will be created (e.g. level2/2020/06/21).
;
; KEYWORDS:
;      FORCE:   By default the routine will not overwrite a file that
;               already exists in the destination directory. Setting
;               /force allows the file to be overwritten.
;      NOLEVEL: If set, then the level part of the default path is omitted
;               (e.g. $SPICE_DATA/2020/06/21/ instead of $SPICE_DATA/level2/2020/06/21/)
;      SEARCH_SUBDIR: If set and 'Filename' is a directory, then the program looks for
;                     spice files recurrently, i.e. in all subdirectories
;      USER_DIR: If set, the file(s) will be moved into $SPICE_DATA/user/ instead of $SPICE_DATA/
;      HELP:    If set, then a help message is printed.
;      DRY_RUN: If set, the file won't be moved, no directories will be created. The optional outputs
;               will be the same as if this keyword was not set.
;      QUIET:   If set, warnings will be suppressed.
;
; OUTPUTS:
;      Moves the SPICE file(s) into the correct location in the local
;      SPICE data directory tree.
;
; OPTIONAL OUTPUTS:
;     Destination: A string array of same length than 'Filename' or, in case 'Filename' is
;                  a directory, of length of the files found in that directory. Contains
;                  the paths of each file after the move. This path is identical to its origin
;                  if the file hasn't been moved.
;     File_moved:  int array of same length than 'Destination'. Indicates which files have
;                  been moved (1=moved, 0=not moved).
;     Files_found: A string array of same length than 'Destination'. Contains all files that
;                  have been found, if input was a directory. Else it is identical to 'Filename'.
;
; RESTRICTIONS:
;      The environment variable SPICE_DATA must be defined.
;
; EXAMPLES:
;      spice_ingest        ; This moves all files from the current directory
;      spice_ingest, './'  ; This moves all files from the current directory
;
; HISTORY:
;      Ver. 1, 30-Jun-2014, Peter Young
;      Ver. 2, 22-Aug-2014, Peter Young
;         added /force keyword
;      Ver. 3, 17-Oct-2014, Peter Young
;         allow $IRIS_DATA to contain multiple paths; added INDEX=
;         keyword.
;      Ver. 4, 23-Feb-2015, Peter Young
;         added /HELP keyword.
;      10-Jun-2020 : Martin Wiesmann : iris_ingest rewritten for SPICE
;                 and renamed to spice_ingest
;-
; $Id: 2024-04-30 11:25 CEST $


PRO spice_ingest, filename, path_index=path_index, force=force, nolevel=nolevel, $
  search_subdir=search_subdir, $
  destination=destination, file_moved=file_moved, files_found=files_found, $
  user_dir=user_dir, top_dir=top_dir, dry_run=dry_run, $
  help=help, debug=debug, quiet=quiet

  IF n_params() LT 1 AND NOT keyword_set(help) THEN BEGIN
    filename = './'
  ENDIF

  IF keyword_set(top_dir) THEN BEGIN
    IF N_ELEMENTS(top_dir) NE 1 || size(top_dir, /type) NE 7 THEN BEGIN
      print, 'top_dir must be a scalar string.'
      print, 'No files moved.'
      return
    ENDIF
    topdir = top_dir
  ENDIF ELSE BEGIN ; keyword_set(top_dir)
    topdir=getenv('SPICE_DATA')
    IF topdir EQ '' THEN BEGIN
      print,'% SPICE_INGEST:  Please define the environment variable $SPICE_DATA to point to the '
        print,'               top level of your directory structure. Returning...'
      return
    ENDIF
  ENDELSE ; keyword_set(top_dir)

  spice_paths=BREAK_path(topdir,/nocurrent)
  np=n_elements(spice_paths)
  IF np EQ 1 || N_ELEMENTS(path_index) eq 0 THEN BEGIN
    topdir=spice_paths[0]
  ENDIF ELSE BEGIN
    IF path_index LT np THEN BEGIN
      topdir=spice_paths[path_index]
    ENDIF ELSE BEGIN
      print, 'path_index is out of bounds: ' + strtrim(string(path_index),2) + ' >= ' + strtrim(string(np),2)
      return
    ENDELSE
  ENDELSE

  IF keyword_set(help) THEN begin
    print,'% SPICE_INGEST: your SPICE data paths are:'
    FOR i=0,np-1 DO BEGIN
      istr=strpad(trim(i),4,fill=' ')+'. '
      print,istr+trim(spice_paths[i])
    ENDFOR
    print,'Use path_index=  to put a file in the appropriate path.'
    IF n_params() LT 1 THEN return
  ENDIF

  IF keyword_set(user_dir) && ~keyword_set(top_dir) THEN topdir = concat_dir(topdir, 'user')

  nfiles=n_elements(filename)
  IF nfiles GT 1 THEN BEGIN
    files = filename[sort(filename)]
  ENDIF ELSE BEGIN ; nfiles GT 1
    IF file_test(filename, /directory) THEN BEGIN
      IF keyword_set(search_subdir) THEN BEGIN
        files = file_search(filename, 'solo*.fits', count=nfiles)
      ENDIF ELSE BEGIN ; keyword_set(search_subdir)
        files = file_search(concat_dir(filename,'solo*.fits'), count=nfiles)
      ENDELSE ; keyword_set(search_subdir)
      IF nfiles EQ 0 THEN BEGIN
        print, 'No files found'
        return
      ENDIF ; nfiles EQ 0
      IF nfiles GT 1 THEN files = files[sort(files)]
    ENDIF ELSE BEGIN ; file_test(filename, /directory)
      files = filename
    ENDELSE ; file_test(filename, /directory)
  ENDELSE ; nfiles GT 1


  files_found = files
  destination = files
  file_moved = intarr(nfiles)
  debug = keyword_set(debug)

  FOR ifiles=0,nfiles-1 DO BEGIN
    file_info = spice_file2info(files[ifiles])

    IF ~file_info.is_spice_file THEN BEGIN
      if ~keyword_set(quiet) then print,'% SPICE_INGEST: File '+file_info.filename+' has not been moved as it is not a spice file.'
      continue
    ENDIF

    outdir = topdir
    IF ~keyword_set(nolevel) THEN outdir = concat_dir(outdir, 'level'+strtrim(string(file_info.level), 2))
    outdir = concat_dir(outdir, time2fid(file_info.datetime, /full_year, delim=path_sep()))

    ;check if file to be moved already exists
    old_files = file_search(concat_dir(outdir,'solo*.fits'))
    filechck=where(file_basename(old_files) EQ file_info.filename,nf)
    IF nf EQ 0 OR keyword_set(force) THEN BEGIN
      IF ~file_test(outdir, /directory) && ~keyword_set(dry_run) THEN BEGIN
        file_mkdir, outdir
      ENDIF
      if ~keyword_set(dry_run) then file_move,files[ifiles], outdir, /overwrite
      file_moved[ifiles] = 1
      destination[ifiles] = concat_dir(outdir, file_info.filename)
    ENDIF ELSE IF ~keyword_set(quiet) THEN BEGIN
      print,'% SPICE_INGEST: file '+file_info.filename+' was not moved '
      print,'               as it already exists in the data directory.'
      print,'               Use the keyword /FORCE to overwrite the existing file.'
    ENDIF

  ENDFOR ; ifiles=0,nfiles-1

END
