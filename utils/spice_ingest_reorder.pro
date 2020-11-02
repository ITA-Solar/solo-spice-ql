;+
; NAME:
;      SPICE_INGEST_REORDER
;
; PURPOSE:
;      This routine reorganises the files which have been ingested with
;      spice_ingest with the old version of spice_ingest, i.e. before 29-Oct-2020.
;      Before, all FITS files were stored in a subdirectory under the day directory to
;      collect all files that have the same SPIOBS. Now files will be saved according
;      to the observation date and time and saved to the respective date directoy.
;
;      This routine can also be used to reorganize spice data, e.g. if you have your data
;      organized like $SPICE_DATA/YYYY/mm/dd/ but want to mark the level as well e.g.
;      $SPICE_DATA/levelx/YYYY/mm/dd/
;      Or if $SPICE_DATA contains multiple paths, you can move the data from one to the other.
;
;      This routine will delete all empty folders under $SPICE_DATA
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;      SPICE_INGEST_REORDER [, index_origin=index_origin, index_destination=index_destination, /force, /nolevel, $
;                    destination=destination, file_moved=file_moved, files_found=files_found]
;
; OPTIONAL INPUTS:
;      Index_origin:   If $SPICE_DATA contains multiple paths, then this
;                      keyword allows you to specify from which path you move
;                      the file. Default is 0.
;      Index_destination:   If $SPICE_DATA contains multiple paths, then this
;                      keyword allows you to specify to which path you send
;                      the file. Default is 0.
;
; KEYWORDS:
;      FORCE:   By default the routine will not overwrite a file that
;               already exists in the destination directory. Setting
;               /force allows the file to be overwritten.
;      NOLEVEL: If set, then the level part of the default path is omitted
;               (e.g. $SPICE_DATA/2020/06/21/ instead of $SPICE_DATA/level2/2020/06/21/)
;
; OUTPUTS:
;      Moves the SPICE file(s) into the new location in the local
;      SPICE data directory tree.
;
; OPTIONAL OUTPUTS:
;     Destination: A string array of same length than 'Filename' or, in case 'Filename' is
;                  a directory, of length of the files found in that directory. Contains
;                  the paths of each file after the move. This path is identical to its origin
;                  if the file hasn't been moved.
;     File_moved:  int array of same length than 'Destination'. Indicates which files have
;                  been moved (1=moved, 0=not moved).
;     Files_found: A string array of same length than 'Destination'. Contains all file that
;                  have been found, if input was a directory. Else it is identical to 'Filename'.
;
; RESTRICTIONS:
;      The environment variable SPICE_DATA must be defined.
;      
; USAGE:
;     spice_ingest_reorder
;           A call without arguments is required, when you have used spice_ingest before 2-Nov-2020,
;           because the organisation of files has changed. The files are now in the folder of the day of
;           its creation, instead of in a subfolder for each SPIOBSID. This call will correct that and
;           move the files to the correct location, deleting the empty subfolders.
;     spice_ingest_reorder, index_origin=0, index_destination=1
;           This would move all data from the first path given in $SPICE_DATA to the second path
;
; HISTORY:
;      29-Oct-2020 : Martin Wiesmann
;-
; $Id: 2020-11-02 11:24 CET $


PRO spice_ingest_reorder, index_origin=index_origin, index_destination=index_destination, $
  force=force, nolevel=nolevel, $
  destination=destination, file_moved=file_moved, files_found=files_found

  topdir=getenv('SPICE_DATA')
  IF topdir EQ '' THEN BEGIN
    print,'% SPICE_INGEST:  Please define the environment variable $SPICE_DATA to point to the '
      print,'               top level of your directory structure. Returning...'
    return
  ENDIF

  spice_paths=BREAK_path(topdir,/nocurrent)
  np=n_elements(spice_paths)
  IF np EQ 1 || N_ELEMENTS(index_origin) eq 0 THEN BEGIN
    topdir=spice_paths[0]
  ENDIF ELSE BEGIN
    IF index_origin LT np THEN BEGIN
      topdir=spice_paths[index_origin]
    ENDIF ELSE BEGIN
      print, 'index is out of bounds: ' + strtrim(string(index_origin),2) + ' >= ' + strtrim(string(np),2)
      return
    ENDELSE
  ENDELSE

  spice_ingest, topdir, index=index_destination, force=force, nolevel=nolevel, $
    /search_subdir, $
    destination=destination, file_moved=file_moved, files_found=files_found


  dirsep = path_sep()
  if strmid(topdir, 0,1, /reverse_offset) ne dirsep then topdir = topdir+dirsep

  nfiles = N_ELEMENTS(files_found)
  FOR i=0,nfiles-1 DO BEGIN
    parent_dir = file_dirname(files_found[i], /mark_directory)
    while parent_dir ne topdir do begin
      file_delete, parent_dir,/allow_nonexistent,/quiet
      parent_dir = file_dirname(parent_dir, /mark_directory)
    endwhile
  ENDFOR

END
