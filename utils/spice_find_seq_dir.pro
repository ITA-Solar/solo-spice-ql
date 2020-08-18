;+
; NAME:
;      SPICE_FIND_SEQ_DIR()
;
; PURPOSE:
;      This routine finds a list of all SPICE data directories between
;      start_date and stop_date.
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;      Result = SPICE_FIND_SEQ_DIR( Start_Date, Stop_Date [ , top_dir=top_dir, level=level, $
;                                   files=files, n_files=n_files, directories=directories, n_directories=n_directories ] )
;
; INPUTS:
;      Start_Date:  A start date in a standard SSW format. For
;                   example, '2020/05/15 13:00:00'.
;      Stop_Date:   A stop date in a standard SSW format. For
;                   example, '2020/05/15 13:00:00'.
;
; OPTIONAL INPUTS:
;      level:    The desired level of the directories, or files (0, 1 or 2)
;                level 2 is default.
;      top_dir:  A string (or string array) containing the name(s) of
;                a top-level directory containing the data. For
;                example, '$HOME/spice/data'. If not set, then the routine
;                uses the top-level directories defined by the
;                $SPICE_DATA environment variable.
;
; KEYWORD PARAMETERS:
;      FILES:  If set, then the routine finds all of the FITS files in the
;              directories, and returns the filenames. Note that the
;              filenames must end in '.fits' (so they can't be
;              compressed, for example).
;
; OUTPUTS:
;      The full names of the directories that have been found. If no
;      directories are found, then an empty string is returned. If keyword FILES
;      is set, then full names of all files are returned instead of the directories.
;
; OPTIONAL OUTPUTS:
;      directories: a named variable, contains full names of the directories
;                   that have been found.
;      n_directories: a named variable, contains the number of directories found
;      n_files: a named variable, contains the number of files found, undefined
;               if keyword FILES has not been set.
;
; EXAMPLE:
;      IDL> dir=spice_find_seq_dir('1-jan-2015','31-dec-2015')
;      IDL> files=spice_find_seq_dir('1-jan-2015','31-dec-2015',/files)
;
; MODIFICATION HISTORY:
;      Ver.1, 16-Jun-2020, Martin Wiesmann
;             iris_find_obs_dir rewritten for SPICE
;-
; $Id: 18.08.2020 13:02 CEST $


FUNCTION spice_find_seq_dir, start_date, stop_date, top_dir=top_dir, level=level, $
  files=files, n_files=n_files, directories=directories, n_directories=n_directories

  n_files = 0
  n_directories = 0
  directories = ''

  IF n_params() LT 2 THEN BEGIN
    print,'Use:  IDL> dir = spice_find_seq_dir( start_date, stop_date  [ , top_dir=top_dir, level=level, files=files, n_files=n_files, directories=directories, n_directories=n_directories ] )'
    return,''
  ENDIF


  IF n_elements(top_dir) EQ 0 THEN BEGIN
    spice_data=getenv('SPICE_DATA')
    IF spice_data EQ '' THEN BEGIN
      print,'%SPICE_FIND_SEQ_DIR: $SPICE_DATA is not defined. Please specify a top-level directory using the input TOP_DIR='
        print,'                  Note that e.g. level-2 files are expected to be in $SPICE_DATA/level2.'
        print,'                    Returning...'
      return,''
    ENDIF
    spice_paths=BREAK_path(spice_data,/nocurrent)
    IF N_ELEMENTS(level) eq 0 THEN level=2
    case level of
      0: level_string = 'level0'
      1: level_string = 'level1'
      2: level_string = 'level2'
      else: BEGIN
        message, 'level ' + strtrim(string(level),2) + ' not defined, returning.', /info
        return,''
      END
    endcase
    top_dir=concat_dir(spice_paths,level_string)
  ENDIF

  ndir=n_elements(top_dir)

  ;
  ; The following creates an array of directory names of the form
  ; 'YYYY/MM/DD' for all days between start_date and stop_date. I do it
  ; by going to modified julian day format.
  ;
  t0_mjd=anytim2utc(/mjd,start_date)
  t1_mjd=anytim2utc(/mjd,stop_date)
  ;
  n0=t0_mjd.mjd
  n1=t1_mjd.mjd
  ;
  ndays=n1-n0+1
  ;
  str={mjd: 0l, time: 0l}
  mstr=replicate(str,ndays)
  ;
  mstr.mjd=n0+lindgen(ndays)
  ;
  date_dir=time2fid(mstr,/full_year,delim='/')


  ;
  ; Now go through the date directories and check if they exist
  ; on the user's computer. The array 'outdir' contains the
  ; directory names that do exist.
  ;
  outdir=''
  count=0
  FOR i=0,ndir-1 DO BEGIN
    finddir=concat_dir(top_dir[i],date_dir)
    chck=file_info(finddir)
    k=where(chck.directory EQ 1,nk)
    count=count+nk
    IF nk GT 0 THEN outdir=[outdir,finddir[k]]
  ENDFOR

  IF count GT 0 THEN BEGIN
    outdir=outdir[1:*]
    date_dir=outdir
  ENDIF ELSE BEGIN
    return,''
  ENDELSE

  ;
  ; Now go through the date directories and find the sub-directories
  ; within them.
  ;
  start_tai = utc2tai(start_date)
  stop_tai = utc2tai(stop_date)
  output=file_search(date_dir,'*',/test_directory,count=count)
  if count eq 0 then return,''
  
  folder_date =  utc2tai(file2time(file_basename(output)))
  ind = where(folder_date GE start_tai AND folder_date LE stop_tai, count)
  if count eq 0 then return,''
  
  output = output[ind]
  n_directories = count
  directories = output

  ;
  ; If /files has been set, then search the directories for '.fits'
  ; files.
  ;
  IF keyword_set(files) THEN BEGIN
    search_str='*.fits'
    search_str=concat_dir(output,search_str)
    output=file_search(search_str,count=count)
    n_files = count
  ENDIF

  return,output

END
