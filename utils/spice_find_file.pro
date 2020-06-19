;+
; NAME:
;     SPICE_FIND_FILE
;
; PURPOSE:
;     This routine returns the name (or names) of SPICE raster files
;     (or optional SJI files) that correspond to the specified
;     time. The logic for what filename(s) is returned is as follows.
;
;     The data for a given observation can either be contained in a
;     single file for sit-and-stare or single raster observations, or
;     in multiple files corresponding to multiple raster repeats. In
;     the latter case, the routine will return the complete list of
;     rasters for the observation. Note that these are files with
;     r00000, r00001, r00002, etc. in their filenames.
;
;     A given observation has a start time (t0) and an end time
;     (t1). If TTIME lies between these two times, and the files are
;     present in the user's $SPICE_DATA directory, then the
;     raster files for this period are returned.
;
;     If the files are not present on the computer, then the user can
;     search for the nearest file sequence to the input time by
;     setting /NEAREST.
;
;     The routine only checks the specified day and the previous day
;     for the existence of files.
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;     Result = SPICE_FIND_FILE(Time)
;
; INPUTS:
;     Time:  This can be in any format accepted by the ANYTIM suite of
;            routines. For example, '1-jan-2010', '2010-01-01 05:00'.
;
; OPTIONAL INPUT:
;     TOP_DIR:  top directory xxxxx
;     
; KEYWORD PARAMETERS:
;     SEQUENCE: If set, then all files of the sequence that the
;               found file belongs to will be returned.
;     ALL:      If set, then all filenames for the specified day will
;               be returned.
;     QUIET:    If set, then no information messages are printed.
;
; OUTPUTS:
;     A string containing the full path to a SPICE file or files. If
;     there are no matches, then an empty string is returned.
;
; OPTIONAL OUTPUTS:
;     Count: An integer containing the number of matching files.
;
; EXAMPLE:
;     IDL> file=spice_find_file('2020/05/28 16:04:00.000')
;     IDL> file=spice_find_file('3-oct-2014 23:30',/sji)
;     IDL> file=spice_find_file('3-oct-2014 23:30',sji=2832)
;
; CALLS:
;     TIME2FID, ANYTIM2UTC
;
; MODIFICATION HISTORY:
;     Ver.1, 15-Jun-2020, Martin Wiesmann
;         iris_find_file rewritten for SPICE
;
;-
; $Id: 19.06.2020 12:08 CEST $


FUNCTION spice_find_file, ttime, $
  top_dir=top_dir, $
  count=count, quiet=quiet, $
  all=all, $
  sequence=sequence

  count = 0

  IF n_params() LT 1 THEN BEGIN
    print,'Use:  IDL> file=spice_find_file(time [, /quiet, /all, /nearest, count= ])'
    print,''
    print," Example time formats:  '1-jan-2010 05:00', '2010-01-01 05:00'"
    print,' Keywords:'
    print,'   /quiet Do not print messages.'
    print,'   /all   Return all files for the specified day.'
    print,'   count= The number of files found.'
    print,'   /nearest Search for nearest file sequence.'
    return,''
  ENDIF


  ;
  ; If a raster is repeated 25 times beginning at 23:00 on 31-Dec-2013,
  ; then all 25 files will be stored under the 2013/12/31 directory even
  ; though some of them were run on 1-Jan-2014. For this reason I have
  ; to search the previous day to the one requested in order to pick up
  ; the files correctly.
  ;
  IF keyword_set(all) THEN BEGIN
    date_start=ttime
    date_end=ttime
  ENDIF ELSE BEGIN
    tt_mjd=anytim2utc(/mjd,ttime)
    tt_mjd.mjd=tt_mjd.mjd-1
    date_start=anytim2utc(/ccsds,tt_mjd)
    date_end=ttime
  ENDELSE


  files=spice_find_seq_dir(date_start, date_end, /files, top_dir=top_dir, directories=directories, n_directories=n_directories, n_files=n_files)
  IF n_files EQ 0 THEN BEGIN
    IF NOT keyword_set(quiet) THEN print,'%SPICE_FIND_FILE: no files found.'
    return,''
  ENDIF
  count=n_files

  ;
  ; If /ALL then just return all the found files.
  ;
  IF keyword_set(all) THEN BEGIN
    IF NOT keyword_set(quiet) THEN BEGIN
      print,'% SPICE_FIND_FILE: '+trim(n_files)+' files found: '
      FOR j=0,n_files-1 DO BEGIN
        print,format='(i3,2x,a60)',j,file_basename(files[j])
      ENDFOR
    ENDIF
    return, files
  ENDIF

  ; search the file that is closest to the date
  date_tai=anytim2tai(ttime)
  file_info = spice_file2info(files)
  file_date = anytim2tai(file_info.datetime) - date_tai
  ind_older = where(file_date LT 0, n_older, complement=ind_newer, ncomplement=n_newer)
  IF n_older GT 0 THEN BEGIN
    min_older = min(abs(file_date[ind_older]), min_older_ind)
    min_older_ind = ind_older[min_older_ind]
    IF n_newer GT 0 THEN BEGIN
      min_newer = min(abs(file_date[ind_newer]), min_newer_ind)
      min_newer_ind = ind_newer[min_newer_ind]
      IF file_info[min_older_ind].spiobsid EQ file_info[min_newer_ind].spiobsid THEN BEGIN
        nearest_ind = min_older_ind
      ENDIF ELSE BEGIN ; file_info[min_older_ind].spiobsid EQ file_info[min_newer_ind].spiobsid
        IF min_older LE min_newer THEN nearest_ind = min_older_ind $
        ELSE nearest_ind = min_newer_ind
      ENDELSE ; file_info[min_older_ind].spiobsid EQ file_info[min_newer_ind].spiobsid
    ENDIF ELSE BEGIN ; n_newer GT 0
      nearest_ind = min_older_ind
    ENDELSE
  ENDIF ELSE BEGIN ; n_older GT 0
    min_newer = min(abs(file_date[ind_newer]), min_newer_ind)
    min_newer_ind = ind_newer[min_newer_ind]
    nearest_ind = min_newer_ind
  ENDELSE ; n_older GT 0

  IF keyword_set(sequence) THEN BEGIN
    ind = where(file_info.spiobsid eq file_info[nearest_ind].spiobsid, count)
    output = files[ind]
  ENDIF ELSE BEGIN
    count = 1
    output = files[nearest_ind]
  ENDELSE
  
  return,output
END

