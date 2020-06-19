;+
; NAME:
;      SPICE_INGEST
;
; PURPOSE:
;      This routine takes as input an SPICE filename (or list of files)
;      and correctly places them in the SPICE data tree, creating new
;      directories if necessary.
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;	     SPICE_INGEST, Filename [, force=force, index=index, help=help]
;
; INPUTS:
;      Filename: The name of an SPICE file. Can be an array of names.
;
; OPTIONAL INPUTS:
;      Index:   If $SPICE_DATA contains multiple paths, then this
;               keyword allows you to specify to which path you send
;               the file. Default is 0.
;
; KEYWORDS:
;      FORCE:   By default the routine will not overwrite a file that
;               already exists in the destination directory. Setting
;               /force allows the file to be overwritten.
;
;      HELP:    If set, then a help message is printed.
;
; OUTPUTS:
;      Moves the SPICE file(s) into the correct location in the local
;      SPICE data directory tree.
;
; RESTRICTIONS:
;      The environment variable SPICE_DATA must be defined.
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
; $Id: 17.06.2020 14:56 CEST $


PRO spice_ingest, filename, force=force, index=index, help=help, $
  debug=debug

  ;filename='/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083015_V202003091733C_12583153-000.fits'
  ;filename='/Users/mawiesma/spice/data/level2/solo_L2_spice-n-exp_20200421T135817961_V01_12583408-000.fits'

  IF n_params() LT 1 AND NOT keyword_set(help) THEN BEGIN
    print,''
    print,'Use:  IDL> spice_ingest, filename [, /force ]'
    print,'  - filename can be an array'
    print,'  - make sure tar files are unpacked before running'
    print,''
    return
  ENDIF

  nfiles=n_elements(filename)
  IF nfiles Gt 1 THEN BEGIN
    files = filename[sort(filename)]
  ENDIF ELSE BEGIN
    files = filename
  ENDELSE

  topdir=getenv('SPICE_DATA')
  IF topdir EQ '' THEN BEGIN
    print,'% SPICE_INGEST:  Please define the environment variable $SPICE_DATA to point to the '
      print,'               top level of your directory structure. Returning...'
    return
  ENDIF

  debug = keyword_set(debug)

  spice_paths=BREAK_path(topdir,/nocurrent)
  np=n_elements(spice_paths)

  IF np EQ 1 THEN BEGIN
    topdir=spice_paths[0]
  ENDIF ELSE BEGIN
    IF n_elements(index) NE 0 THEN BEGIN
      IF index LT np THEN BEGIN
        topdir=spice_paths[index]
      ENDIF ELSE BEGIN
        print, 'index is out of bounds: ' + strtrim(string(index),2) + ' >= ' + strtrim(string(np),2)
        return
      ENDELSE
    ENDIF ELSE BEGIN
      topdir=spice_paths[0]
    ENDELSE
  ENDELSE

  IF keyword_set(help) THEN begin
    print,'% SPICE_INGEST: your SPICE data paths are:'
    FOR i=0,np-1 DO BEGIN
      istr=strpad(trim(i),4,fill=' ')+'. '
      print,istr+trim(spice_paths[i])
    ENDFOR
    print,'Use INDEX=  to put a file in the appropriate path.'
    IF n_params() LT 1 THEN return
  ENDIF


  FOR ifiles=0,nfiles-1 DO BEGIN
    fname=file_basename(files[ifiles])
    chck=strpos(fname,'solo_L2_spice-')
    IF chck[0] NE 0 THEN BEGIN
      chck=strpos(fname,'solo_L1_spice-')
      IF chck[0] NE 0 THEN BEGIN
        chck=strpos(fname,'solo_L0_spice-')
        IF chck[0] NE 0 THEN BEGIN
          print,'% SPICE_INGEST: File '+fname+' has not been moved as it is not a spice file.'
          continue
        ENDIF ELSE BEGIN
          level=0
          outdir=concat_dir(topdir,'level0')
        ENDELSE
      ENDIF ELSE BEGIN
        level=1
        outdir=concat_dir(topdir,'level1')
      ENDELSE
    ENDIF ELSE BEGIN
      level=2
      outdir=concat_dir(topdir,'level2')
    ENDELSE

    mreadfits_header, files[ifiles], hdr, extension=0, only_tags='SPIOBSID,SEQ_BEG'

    IF ~tag_exist(hdr, 'SPIOBSID') THEN BEGIN
      message, 'SPIOBSID not found in fits header, file '+files[ifiles]+' not moved.', /informational
      continue
    ENDIF
    spiobsid = strtrim(string(hdr.spiobsid), 2)

    IF ~tag_exist(hdr, 'SEQ_BEG') THEN BEGIN
      message, 'SEQ_BEG not found in fits header, file '+files[ifiles]+'. Extracting date/time from filename.', /informational
      seq_beg_defined=0
      temp = strsplit(files[ifiles], '_', /extract)
      IF level EQ 0 THEN BEGIN
        seq_beg_date = strmid(temp[4], 1, 8)
        seq_beg_time = strmid(temp[4], 9, 4)
      ENDIF ELSE BEGIN ; level EQ 0
        seq_beg_date = strmid(temp[3], 0, 8)
        seq_beg_time = strmid(temp[3], 9, 6)
      ENDELSE ; level EQ 0
      seq_beg_datetime = seq_beg_date+'_'+seq_beg_time
      seq_beg = fid2time('_'+seq_beg_datetime)
      seq_beg = anytim2utc(seq_beg)
    ENDIF ELSE BEGIN ; ~tag_exist(hdr, 'SEQ_BEG')
      seq_beg_defined=1
      ;may have to transform date and time
      seq_beg = anytim2utc(hdr.SEQ_BEG)
      seq_beg_datetime = time2fid(seq_beg, /full_year, /time, /seconds)
    ENDELSE ; ~tag_exist(hdr, 'SEQ_BEG')
    seq_beg_dir = time2fid(seq_beg, /full_year, delim=path_sep())
    if debug then begin
      print,'seq_beg: ', seq_beg
      print,'seq_beg_dir: ',seq_beg_dir
      print,'seq_beg_datetime: ',seq_beg_datetime
    endif

    ; we have to check whether a directory for this SPIOBSID already exists
    ; with another date/time, in case a file was ingested before SEQ_BEG keyword
    ; was included in the FITS header
    ; we check +- 2 days
    old_dirs = []
    if debug then print, 'searching +-2 days'
    for day=-2,2 do begin
      tempday = seq_beg
      tempday.mjd = tempday.mjd+day
      tempdir = concat_dir(outdir, time2fid(tempday, /full_year, delim=path_sep()))
      if debug then print,tempdir
      IF file_test(tempdir, /directory) THEN BEGIN
        if debug then print,'exists'
        tempdirs = file_search(tempdir+path_sep()+'*', /test_directory, count=count_dir)
        IF count_dir GT 0 THEN BEGIN
          if debug then print,'subdirectories'
          if debug then print,tempdirs
          ind = where(strmatch(tempdirs, '*'+spiobsid+'*') eq 1, count_match)
          FOR i=0,count_match-1 DO BEGIN
            IF file_basename(tempdirs[ind[i]]) NE seq_beg_datetime+'_'+spiobsid THEN old_dirs = [old_dirs, tempdirs[ind[i]]]
          ENDFOR
          if debug then print,'old_dirs: ', old_dirs
        ENDIF
      ENDIF
    endfor

    new_file_exists=0
    IF seq_beg_defined || N_ELEMENTS(old_dirs) EQ 0 THEN BEGIN

      outdir = concat_dir(outdir, seq_beg_dir)
      outdir = concat_dir(outdir, seq_beg_datetime+'_'+spiobsid)

    ENDIF ELSE BEGIN ; seq_beg_defined

      ; we don't know exactly when the obs started
      ;  so we have to move the file to the folder with the same
      ;  SPIOBSID and the lowest date/time stamp, if an older directory exists
      old_times = []
      FOR i=0,N_ELEMENTS(old_dirs)-1 DO BEGIN
        old_times = [old_times, utc2tai(file2time(old_dirs[i]))]
      ENDFOR
      oldest = min(old_times, oldest_ind)
      new_time = utc2tai(seq_beg)
      IF new_time LT oldest THEN BEGIN
        outdir = concat_dir(outdir, seq_beg_dir)
        outdir = concat_dir(outdir, seq_beg_datetime+'_'+spiobsid)
      ENDIF ELSE BEGIN
        outdir = old_dirs[oldest_ind]
        IF N_ELEMENTS(old_dirs) eq 1 THEN BEGIN
          old_dirs=[]
        ENDIF ELSE BEGIN
          remove, oldest_ind, old_dirs
        ENDELSE
      ENDELSE

    ENDELSE ; seq_beg_defined

    ; create directory, if it doesn' exist yet
    IF ~file_test(outdir, /directory) THEN BEGIN
      message, 'created new directory: ' + outdir, /informational
      file_mkdir, outdir
    ENDIF

    ; move files from older directories to the new one
    FOR i=0,N_ELEMENTS(old_dirs)-1 DO BEGIN
      old_files = file_search(old_dirs[i], 'solo*.fits')
      ;      filechck=where(file_basename(old_files) EQ fname,nf)
      ;      IF nf GT 0 THEN new_file_exists=1
      file_move, old_files, outdir, /overwrite
      file_delete, old_dirs[i]
    ENDFOR

    ;check if file to be moved already exists
    old_files = file_search(outdir, 'solo*.fits')
    filechck=where(file_basename(old_files) EQ fname,nf)
    IF nf EQ 0 OR keyword_set(force) THEN BEGIN
      file_move,files[ifiles], outdir, /overwrite
    ENDIF ELSE BEGIN
      print,'% SPICE_INGEST: file '+fname+' was not moved '
      print,'               as it already exists in the data directory.'
      print,'               Use the keyword /FORCE to overwrite the existing file.'
    ENDELSE

  ENDFOR ; ifiles=0,nfiles-1

END
