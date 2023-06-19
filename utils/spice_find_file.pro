;+
; NAME:
;     SPICE_FIND_FILE
;
; PURPOSE:
;     This routine returns the path(s) and name(s) of SPICE raster files that correspond 
;     to the specified time or lie within a given time window. The logic for what filename(s) is returned is as follows:
;
;     If only the start time is given, then the file closest to that time is returned or, if SEQUENCE keyword is set, 
;     all files with the same SPIOBSID as the file closest to that time are returned.
;
;     If both, the end and start time are given, all files within the time window are returned or, 
;     if SEQUENCE keyword is set, all SPICE observations that have at least one file within the time window are returned.
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;     Result = SPICE_FIND_FILE(time_start [, time_end=time_end, level=level, $
;       top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, $
;       /SEQUENCE, /ALL, /NO_LEVEL, /NO_TREE_STRUCT, /USER_DIR, /SEARCH_SUBDIR, /IGNORE_TIME , /REMOVE_DUPLICATES] )
;
; INPUTS:
;     TIME_START: This can be in any format accepted by the ANYTIM suite of
;                 routines. For example, '1-jan-2010', '2010-01-01 05:00'. This is
;                 either the time for which the file closest to it is returned,
;                 or the start of the time window to be searched, in case 'time_end'
;                 is also provided.
;
; OPTIONAL INPUT:
;     TIME_END: This can be in any format accepted by the ANYTIM suite of
;               routines. For example, '1-jan-2010', '2010-01-01 05:00'. This is
;               the end of the time window to be searched.
;     LEVEL:    Desired level of the data (0, 1, 2 (default) or 3)
;     TOP_DIR:  Top directory in which the SPICE data lies. If not provided
;               the path given in $SPICE_DATA is searched.
;     PATH_INDEX: If $SPICE_DATA or TOP_DIR contains multiple paths, then this
;               keyword allows you to specify which path should be searched. Default is 0.
;
; KEYWORD PARAMETERS:
;     SEQUENCE: If set, then all files of the sequence that the found files belong to will 
;               be returned, i.e. the time window to be searched is expanded to include files 
;               outside of the given time window, but only sequences (= Spice observations) 
;               that have at least one file in the given time window are returned. 
;               If set and 'time_end' is provided, the returned value will be a LIST 
;               in which each element is a string or string array with paths to SPICE FITS files 
;               that belong to the same sequence.
;     ALL:      If set, then all filenames for the specified day will be returned.
;               Ignored if TIME_END is provided or if NO_TREE_STRUCT or SEQUENCE is set.
;     NO_LEVEL: If set, then the level part of the default path is omitted
;               (e.g. $SPICE_DATA/2020/06/21/ instead of $SPICE_DATA/level2/2020/06/21/)
;     NO_TREE_STRUCT: If set, then the tree structure won't be appended to TOP_DIR
;               (e.g. TOP_DIR/level2/ instead of TOP_DIR/level2/2020/06/21/)
;     USER_DIR: If set, the procedure searches in TOP_DIR/user/ instead of TOP_DIR/.
;     SEARCH_SUBDIR: If set then the program looks for spice files recurrently,
;               i.e. in all subdirectories
;     IGNORE_TIME: If set, TIME_START and TIME_END are ignored, and all files will be
;               returned. Ignored if NO_TREE_STRUCT is not set.
;               This keyword is used by spice_xfiles, it makes it possible to return
;               all files that are stored locally
;     REMOVE_DUPLICATES: If set, then the procedures checks whether there are any files that have same
;               SPIOBS, raster number and level. If yes, it keeps only the one with the highest version
;               number.
;
; OUTPUTS:
;     A string or string array containing the full path to a SPICE file or files. 
;     If there are no matches, then an empty string is returned. If the keyword SEQUENCE is set 
;     and time_end is provided, then a LIST is returned in which each element is a string or 
;     string array with paths to SPICE FITS files that belong to the same SPICE observation.
;
; OPTIONAL OUTPUTS:
;     COUNT_FILE:An integer containing the number of matching files.
;     COUNT_SEQ: An integer containing the number of matching sequences,
;                only non-zero if SEQUENCE is set.
;
; EXAMPLE:
;     IDL> file=spice_find_file('2020/05/28 16:04:00.000')
;               returns the closest file to this date/time
;     IDL> file=spice_find_file('28-may-2020 16:04',/all)
;               returns all files from the given date
;     IDL> file=spice_find_file('2020-05-28T16:04:00.000', /sequence)
;               returns all files of the sequence closest to this date/time
;     IDL> file=spice_find_file('2020-05-28T16:04:00.000', time_end='2020-06-03T23:59:59', /sequence)
;               returns a list, where each element is a string array containing files for one sequence
;     IDL> file=spice_find_file('2020-05028T16:04:00', /no_tree_stuct, /ignore_time, /search_subdir)
;               returns all level 2 files that are in $SPICE_DATA
;
; CALLS:
;     BREAK_path, concat_dir, ssw_time2paths, time_window, spice_file2info,
;     utc2tai
;
; MODIFICATION HISTORY:
;     Ver.1, 15-Jun-2020, Martin Wiesmann : iris_find_file rewritten for SPICE
;     Ver.2,  3-Nov-2020, Martin Wiesmann : complete overhaul of the procedure
;
;-
; $Id: 2023-06-19 11:37 CEST $


FUNCTION spice_find_file, time_start, time_end=time_end, level=level, $
  top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, $
  all=all, sequence=sequence, no_level=no_level, no_tree_struct=no_tree_struct, user_dir=user_dir, $
  search_subdir=search_subdir, ignore_time=ignore_time, remove_duplicates=remove_duplicates

  count_file = 0
  count_seq = 0
  IF N_ELEMENTS(level) EQ 0 THEN level = 2

  IF n_params() LT 1 THEN BEGIN
    print,'Use:  IDL> Result = SPICE_FIND_FILE(time_start [, time_end=time_end, level=level, '
    print,'       top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, '
    print,'       /SEQUENCE, /ALL, /NO_LEVEL, /NO_TREE_STRUCT, /SEARCH_SUBDIR, /QUIET ] )'
    print,''
    print," Example time formats:  '28-may-2020 05:00', '2020-05-28 05:00'"
    print,' Keywords:'
    print,'   top_dir= The top directory in which the SPICE data lies'
    print,'   level= The desired data level (default=2)'
    print,'   /sequence  Return all files of the sequence
    print,'   /all   Return all files for the specified day.'
    print,'   count_file= The number of files found.'
    return,''
  ENDIF


  IF N_ELEMENTS(top_dir) eq 0 THEN BEGIN
    topdir=getenv('SPICE_DATA')
    IF topdir EQ '' THEN BEGIN
      print,'% SPICE_INGEST:  Please define the environment variable $SPICE_DATA to point to the '
        print,'               top level of your directory structure.'
      print,'               Or specify TOP_DIR. Returning...'
      return,''
    ENDIF
  ENDIF ELSE BEGIN
    topdir = top_dir
  ENDELSE

  spice_paths=BREAK_path(topdir,/nocurrent)
  np=n_elements(spice_paths)
  IF np EQ 1 || N_ELEMENTS(path_index) eq 0 THEN BEGIN
    topdir=spice_paths[0]
  ENDIF ELSE BEGIN
    IF path_index LT np THEN BEGIN
      topdir=spice_paths[path_index]
    ENDIF ELSE BEGIN
      print, 'index is out of bounds: ' + strtrim(string(path_index),2) + ' >= ' + strtrim(string(np),2)
      return,''
    ENDELSE
  ENDELSE

  IF keyword_set(user_dir) THEN topdir = concat_dir(topdir, 'user')
  IF ~keyword_set(no_level) THEN topdir = concat_dir(topdir, 'level'+strtrim(string(level), 2))

  time0 = time_start
  IF N_ELEMENTS(time_end) EQ 0 THEN BEGIN
    time1 = time0
    no_endtime = 1
  ENDIF ELSE BEGIN
    time1 = time_end
    no_endtime = 0
  ENDELSE
  IF keyword_set(sequence) THEN BEGIN
    time_window, [time0,time1], timeexp0, timeexp1, days=1 ; this expands input time +/- per user window
    time0 = timeexp0
    time1 = timeexp1
  ENDIF

  IF ~keyword_set(no_tree_struct) && ~keyword_set(search_subdir) THEN BEGIN
    file_pattern = 'solo_L' + strtrim(string(level), 2) + '_spice*.fits'
    dirs = ssw_time2paths(time0, time1, topdir)
    files1 = file_list(dirs, file_pattern, /quiet)
    ind = where(files1 ne '', count1)
    file_pattern = 'solo_L' + strtrim(string(level), 2) + '_spice*.fits.gz'
    files2 = file_list(dirs, file_pattern, /quiet)
    ind = where(files2 ne '', count2)
    count0 = count1 + count2
    files = []
    IF count1 GT 0 THEN BEGIN
      files = [files, files1]
    ENDIF
    IF count2 GT 0 THEN BEGIN
      files = [files, files2]
    ENDIF
  ENDIF ELSE BEGIN
    IF ~keyword_set(no_tree_struct) THEN paths = ssw_time2paths(time0, time1, topdir) $
    ELSE paths = topdir

    file_pattern = 'solo_L' + strtrim(string(level), 2) + '_spice*.{fits,fits.gz}'
    IF keyword_set(search_subdir) THEN BEGIN
      files = file_search(paths, file_pattern, count=count0)
    ENDIF ELSE BEGIN
      files = file_search(concat_dir(paths, file_pattern), count=count0)
    ENDELSE
  ENDELSE

  IF count0 GT 0 THEN BEGIN
    fileinfo = spice_file2info(files)
    fgood = where(fileinfo.is_spice_file, fcount)
    IF fcount GT 0 THEN BEGIN
      files=files[fgood]
      fileinfo=fileinfo[fgood]
      count0=fcount
    ENDIF ELSE count0=0
  ENDIF

  IF count0 EQ 0 THEN BEGIN
    print, 'no SPICE files found'
    return,''
  ENDIF
  
  
  IF keyword_set(remove_duplicates) || $
    (no_endtime && ~keyword_set(all) && N_ELEMENTS(remove_duplicates) EQ 0) THEN BEGIN
    ; Remove duplicates of files
    ind_keep = []
    ind_keep_not = []
    FOR i=0,count0-1 DO BEGIN
      IF where(ind_keep eq i) GE 0 || where(ind_keep_not eq i) GE 0 THEN continue
      ind = where(fileinfo.spiobsid eq fileinfo[i].spiobsid AND $
        fileinfo.rasterno eq fileinfo[i].rasterno AND $
        fileinfo.level eq fileinfo[i].level, count)
      IF count GT 1 THEN BEGIN
        max_version = max(fileinfo[ind].version, max_ind)
        ind_keep = [ind_keep, ind[max_ind]]
        remove, max_ind, ind
        ind_keep_not = [ind_keep_not, ind]
      ENDIF ELSE BEGIN
        ind_keep = [ind_keep, i]
      ENDELSE
    ENDFOR
    IF N_ELEMENTS(ind_keep_not) GT 0 THEN BEGIN
      files=files[ind_keep]
      fileinfo=fileinfo[ind_keep]
      count0=N_ELEMENTS(ind_keep)
    ENDIF
  ENDIF ; keyword_set(remove_duplicates)


  IF keyword_set(all) && ~keyword_set(no_tree_struct) && no_endtime && ~keyword_set(sequence) THEN BEGIN
    ; user wants all files of one specific day
    count_file=count0
    return, files
  ENDIF


  IF ~keyword_set(ignore_time) || ~keyword_set(no_tree_struct) THEN BEGIN
    ; we do not ignore the provided date/time (window)
    startdate=utc2tai(time_start)
    filedates=utc2tai(fileinfo.datetime)

    IF no_endtime THEN BEGIN
      ; user wants only one file, or one sequence, that is closest to provided date/time
      temp = min(abs(filedates-startdate), min_index)
      IF keyword_set(sequence) THEN BEGIN
        same_obs_ind = where(fileinfo.spiobsid eq fileinfo[min_index].spiobsid, count_file)
        files = files[same_obs_ind]
        count_seq = 1
      ENDIF ELSE IF N_ELEMENTS(remove_duplicates) GT 0 && ~keyword_set(remove_duplicates) THEN BEGIN
        ind = where(fileinfo.spiobsid eq fileinfo[min_index].spiobsid AND $
          fileinfo.rasterno eq fileinfo[min_index].rasterno AND $
          fileinfo.level eq fileinfo[min_index].level, count_file)
        files = files[ind]
      ENDIF ELSE BEGIN ; keyword_set(sequence)
        files = files[min_index]
        count_file = 1
      ENDELSE ; keyword_set(sequence)
      return, files
    ENDIF ; no_endtime

    stopdate=utc2tai(time_end)
    fgood=where((filedates ge startdate) AND (filedates le stopdate), fcount)
    IF fcount EQ 0 THEN BEGIN
      print, 'no SPICE files found'
      return,''
    ENDIF
    IF keyword_set(sequence) THEN BEGIN
      all_obs = uniq(fileinfo[fgood].spiobsid, sort(fileinfo[fgood].spiobsid))
      all_obs = fileinfo[fgood[all_obs]].spiobsid
      seq_list = list()
      count_seq = N_ELEMENTS(all_obs)
      for i=0,count_seq-1 do begin
        ftemp = where(fileinfo.spiobsid eq all_obs[i])
        seq_list.add, files[ftemp]
        count_file = count_file + N_ELEMENTS(ftemp)
      endfor
      return,seq_list
    ENDIF ELSE BEGIN ; keyword_set(sequence)
      files = files[fgood]
      count_file = fcount
      return,files
    ENDELSE ; keyword_set(sequence)

  ENDIF ELSE BEGIN ; ~keyword_set(ignore_time) || keyword_set(no_tree_struct)
    ; user wants to get all files, regardless of its observation date/time
    IF keyword_set(sequence) THEN BEGIN
      all_obs = uniq(fileinfo.spiobsid, sort(fileinfo.spiobsid))
      all_obs = fileinfo[all_obs].spiobsid
      seq_list = list()
      count_seq = N_ELEMENTS(all_obs)
      for i=0,count_seq-1 do begin
        ftemp = where(fileinfo.spiobsid eq all_obs[i])
        seq_list.add, files[ftemp]
        count_file = count_file + N_ELEMENTS(ftemp)
      endfor
      return,seq_list
    ENDIF ELSE BEGIN ; keyword_set(sequence)
      count_file = count0
      return,files
    ENDELSE ; keyword_set(sequence)

  ENDELSE ; ~keyword_set(ignore_time) || keyword_set(no_tree_struct)

END

