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
; $Id: 10.06.2020 14:53 CEST $


PRO spice_ingest, filename, force=force, index=index, help=help

  IF n_params() LT 1 AND NOT keyword_set(help) THEN BEGIN
    print,''
    print,'Use:  IDL> spice_ingest, filename [, /force ]'
    print,'  - filename can be an array'
    print,'  - make sure tar files are unpacked before running'
    print,''
    return
  ENDIF

  n=n_elements(filename)

  topdir=getenv('SPICE_DATA')
  IF topdir EQ '' THEN BEGIN
    print,'% SPICE_INGEST:  Please define the environment variable $SPICE_DATA to point to the '
      PRINT,'               top level of your directory structure. Returning...'
    return
  ENDIF


  spice_paths=BREAK_path(topdir,/nocurrent)
  np=n_elements(spice_paths)

  IF np EQ 1 THEN BEGIN
    topdir=spice_paths[0]
  ENDIF ELSE BEGIN
    IF n_elements(index) NE 0 THEN BEGIN
      IF index LT np THEN topdir=spice_paths[index]
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


  FOR i=0,n-1 DO BEGIN
    fname=file_basename(filename[i])
    ;
    chck=strpos(fname,'_l2_')
    IF chck[0] GT 0 THEN BEGIN
      outdir=concat_dir(topdir,'level2')
      ;
      datestr=strmid(fname,8,8)
      dateex=anytim2utc(datestr,/ex)
      outdir=concat_dir(outdir,trim(dateex.year))
      outdir=concat_dir(outdir,strpad(trim(dateex.month),2,fill='0'))
      outdir=concat_dir(outdir,strpad(trim(dateex.day),2,fill='0'))
      ;
      fileid=strmid(fname,8,26)
      outdir=concat_dir(outdir,fileid)
      ;
      chck=file_info(outdir)
      IF chck.directory EQ 1 THEN BEGIN
        ;
        ; Check if file already exists in directory
        ;
        dirlist=file_search(outdir,'*.fits')
        filechck=where(file_basename(dirlist) EQ fname,nf)
        IF nf EQ 0 OR keyword_set(force) THEN BEGIN
          file_move,filename[i],outdir,/overwrite
        ENDIF ELSE BEGIN
          print,'% SPICE_INGEST: file '+fname+' was not moved '
          print,'               as it already exists in the data directory.'
          print,'               Use the keyword /FORCE to overwrite the existing file.'
        ENDELSE
      ENDIF ELSE BEGIN
        file_mkdir,outdir
        print,'% SPICE_INGEST:  created new directory '+outdir
        file_move,filename[i],outdir
      ENDELSE
    ENDIF ELSE BEGIN
      print,'% SPICE_INGEST: File '+fname+' has not been moved as it is not a level-2 file.'
    ENDELSE
  ENDFOR

END
