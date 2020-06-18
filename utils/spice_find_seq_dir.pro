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
;      Result = SPICE_FIND_SEQ_DIR( Start_Date, Stop_Date [,level=level, top_dir=top_dir, count=count, files=files, compressed=compresse] )
;
; INPUTS:
;      Start_Date:  A start date in a standard SSW format. For
;                   example, '1-Jan-2015'.
;      Stop_Date:   A stop date in a standard SSW format. For
;                   example, '1-Jan-2015'.
;
; OPTIONAL INPUTS:
;      level:    The desired level of the directories, or files (0, 1 or 2)
;                level 2 is default.
;      Top_Dir:  A string (or string array) containing the name(s) of
;                a top-level directory containing the data. For
;                example, '$HOME/spice/data'. If not set, then the routine
;                uses the top-level directories defined by the
;                $SPICE_DATA environment variable.
;
; OPTIONAL INPUTS:
;      Count:  An integer containing the number of directories that
;              have been found.
;
; KEYWORD PARAMETERS:
;      FILES:  If set, then the routine finds all of the FITS files in the
;              directories, and returns the filenames. Note that the
;              filenames must end in '.fits' (so they can't be
;              compressed, for example).
;     COMPRESSED: If set, then the routine will look in the
;              level2_compressed directory rather than level2.
;
; OUTPUTS:
;      The full names of the directories that have been found. If no
;      directories are found, then an empty string is returned.
;
; OPTIONAL OUTPUTS:
;      directories: a named variable, contains full names of the directories
;      that have been found.
;
; EXAMPLE:
;      IDL> dir=spice_find_seq_dir('1-jan-2015','31-dec-2015')
;      IDL> files=spice_find_seq_dir('1-jan-2015','31-dec-2015',/files)
;
; MODIFICATION HISTORY:
;      Ver.1, 16-Jun-2020, Martin Wiesmann
;             iris_find_obs_dir rewritten for SPICE
;-
; $Id: 18.06.2020 10:36 CEST $


FUNCTION spice_find_seq_dir, start_date, stop_date, top_dir=top_dir, count=count, $
  files=files, compressed=compressed, directories=directories, level=level

  IF n_params() LT 2 THEN BEGIN
    print,'Use:  IDL> dir = spice_find_seq_dir( start_date, stop_date [, level=, top_dir=, count=, /files, /compressed ]'
    return,''
  ENDIF


  IF n_elements(top_dir) EQ 0 THEN BEGIN
    spice_data=getenv('SPICE_DATA')
    IF spice_data EQ '' THEN BEGIN
      print,'%SPICE_FIND_SEQ_DIR: $SPICE_DATA is not defined. Please specify a top-level directory using the input TOP_DIR='
        print,'                    Returning...'
      return,''
    ENDIF
    spice_paths=BREAK_path(spice_data,/nocurrent)
    IF N_ELEMENTS(level) eq 0 THEN level=2
    case level of
      0: level_string = 'level0'
      1: level_string = 'level1'
      2: level_string = 'level2'
      else: level_string = 'level2'
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
  output=file_search(date_dir,'*',/test_directory,count=count)
  directories = output

  ;
  ; If /files has been set, then search the directories for '.fits'
  ; files.
  ;
  IF keyword_set(files) THEN BEGIN
    IF keyword_set(compressed) THEN search_str='*.gz' ELSE search_str='*.fits'
    search_str=concat_dir(output,search_str)
    output=file_search(search_str,count=count)
  ENDIF

  return,output

END
