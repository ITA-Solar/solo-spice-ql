;+
; Project     : SOHO - CDS     
;                   
; Name        : SFITSLIST
;               
; Purpose     : Create/update the lfitslist.txt file.
;               
; Explanation : CDS fits file names don't tell you much about their 
;               content. This program creates/updates a file
;               called lfitslist.txt in the fits file directory, with
;               various information on the content of the files.
;               This file is used by PICKFITS in order to search
;               the list of files for those files that the user wants.
;
; Use         : SFITSLIST [,FITSDIR [,LISTDIR]]
;    
; Inputs      : None required.
;               
; Opt. Inputs : FITSDIR : The directory with the fits files to be in
;                         the list. Default "CDS_FITS_DATA"
;
;               LISTDIR : The directory to place the list in. This is
;                         also the directory where the lfitslist.txt
;                         file is created/updated. The default is
;                         "$CDS_FITS_DATA_W".
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : ANYTIM2UTC(), BREAK_FILE, CONCAT_DIR(), DEFAULT, FILE_EXIST()
;               FIND_FILES(), FXBCLOSE, FXBOPEN, FXBTDIM(), FXPAR(), RD_ASCII()
;               STRPAD(), TRIM()
;
; Common      : None.
;               
; Restrictions: Expects to find fits files in the specified directory.
;               If there is already a lfitslist.txt file in the same
;               directory, it has to have one or more entries.
;               
; Side effects: Creates/updates the file "sfitslist.txt" in the
;               specified directory.
;               
; Category    : CDS_Utility
;               
; Prev. Hist. :
;
; Written     : Stein Vidar H. Haugan, UiO, 22 March 1996
;               
; Modified    : Version 2, SVHH, 18 April 1996
;                          Changed format of fitslist.txt -> lfitslist.txt
;                          Generic format.
;               Version 3, SVHH, 19 April 1996
;                          Added check for errors after FXBOPEN call.
;               Version 4, SVHH, 22 April 1996
;                          TITLE expanded to 80 chars max
;               Version 5, SVHH, 23 April 1996
;                          Using find_files for multi-path CDS_FITS_DATA.
;                          Allowing both FITSDIR and LISTDIR to be 
;                          specified.
;                          For updates the new list is collected before
;                          it's written -- direct overwrite, no spawns.
;               Version 6, SVHH, 29 April 1996
;                          Wrote fitslist_add, fitslist_addtx to shorten
;                          sfitslist_prop. These are identical in sfitslist
;                          Tested on VMS (suman1), caught an error in the
;                          use of CATCH.
;               Version 7, Using lowercase fits file names, since that's what
;                          is used on soho-archive disks.
;
; Version     : 7, 6 August 1996
;-            


;
; Standard procedures for adding entries to the line
;

PRO fitslist_add,entry,name,text,length,form,line
  ON_ERROR,0
  IF N_PARAMS() NE 6 THEN MESSAGE,"Wrong number of parameters"
  line = line + strpad(STRMID(entry,0,length-1),length,/after)
  IF N_elements(form) GT 0 THEN BEGIN
     IF STRLEN(text) GT length-1 THEN MESSAGE,"Text longer than entry"
     new = name+'='+text+'='+trim(length)
     IF datatype(form) NE 'STR' THEN form = [new] $
     ELSE                            form = [form,new]
  END
END

PRO fitslist_addtx,header,name,text,length,form,line
  ON_ERROR,0
  IF N_PARAMS() NE 6 THEN MESSAGE,"Wrong number of parameters"
  entry = fxpar(header,name)
  fitslist_add,entry,name,text,length,form,line
END

;
; Make one line of text out of one fits file name.
; Calculate form when CALCFORM is set present
;

PRO sfitslist_prop,f,calcform=calcform,out=out
  errmsg = ''
  
  IF NOT test_open(f) THEN begin
     MESSAGE,"File "+f+" could not be processed",/continue
     out = ''
     RETURN
  END
  CATCH,ERROR
  IF error NE 0 THEN GOTO,PROCESS_ERROR

  header = headfits(f)
  
  break_file,f,disk,dir,filnam
  
  calcform = KEYWORD_SET(calcform)
  IF calcform THEN form = 0
  
  n = ''
  
  fitslist_add,filnam,'FILENAME','Filename',18,form,n
  
  ;;
  ;; Lengths etc. should be adjusted later
  ;;
  
  fitslist_addtx,header,'DETECTOR','det',9,form,n
  
  
  pointing = ("(" + STRING(FIX(fxpar(header,"XCEN")),'(I5)') $
              + ","+STRING(FIX(fxpar(header,"YCEN")),'(I5)') + ") ")
  fitslist_add,pointing,'POINTING','  pointing',14,form,n
  
  ;; Field width
  widths = ("(" + STRING(FIX(fxpar(header,"IXWIDTH")),'(I4)') $
            + ","+STRING(FIX(fxpar(header,"IYWIDTH")),'(I3)') + ") ")
  fitslist_add,widths,'WIDTH','Fieldwidth',11,form,n
  
  slit = STRMID(STRCOMPRESS(fxpar(header,"SLIT"),/remove_all),0,14)
  fitslist_add,slit,'SLIT','Slit',15,form,n
  
  exptime = STRING(fxpar(header,"EXPTIME"),'(f7.1)')
  fitslist_add,exptime,'EXPTIME','Exptime',8,form,n
  
  waverange =  ("[" + STRING(FLOAT(fxpar(header,"WAVEMIN")),'(f6.1)') $
                +","+STRING(FLOAT(fxpar(header,"WAVEMAX")),'(f6.1)') + "]")
  fitslist_add,waverange,'WAVELNTH','Wavlnth range',6+6+4,form,n
  
  fitslist_addtx,header,'OBS_SEQ','Obs.seq',9,form,n
  
  fitslist_addtx,header,'STUDY_NM','Studynam',9,form,n
  
  fitslist_addtx,header,'OBJECT','Object',25,form,n
  
  fitslist_addtx,header,'SCI_OBJ','Sci.obj',9,form,n
  
  fitslist_addtx,header,'OBS_PROG','Obs.prog',9,form,n
  
  fitslist_addtx,header,'CMP_NAME','Cmp.name',9,form,n
  
  fitslist_addtx,header,'SCIENTIS','Scientist',21,form,n
  
  fitslist_addtx,header,'PHYSCOM','Phys.comment',21,form,n
  
  fitslist_addtx,header,'PHYSDAT','Phys.data',21,form,n
  
  fitslist_addtx,header,'PROC_COM','Proc.comment',21,form,n
  
  fitslist_addtx,header,'SC_COM','Scient.comment',21,form,n
  
  fitslist_addtx,header,'T_QUAL','Target quality',21,form,n
  
  fitslist_addtx,header,'I_QUAL','Abs. quality',21,form,n
  
  fitslist_addtx,header,'ININSE','Inter.ins.event',21,form,n
  
  fitslist_addtx,header,'PROCESS','Processing',21,form,n
  
  fitslist_addtx,header,'COMPRESS','Compression',12,form,n
  
  fitslist_addtx,header,'RASTYPE','Ras.type',9,form,n
  
  fitslist_addtx,header,'PROG_NM','Progname',9,form,n
  
  IF calcform THEN begin
     frm = ''
     FOR ii = 0,N_ELEMENTS(FORM)-2 DO frm = frm + form(ii) + '='
     frm = frm + form(ii)
  END
  out = n
  IF calcform THEN out = [frm,out]
  RETURN
  
  PROCESS_ERROR:
  catch,/cancel
  MESSAGE,"File "+f+" could not be processed",/continue
  out = ''
  RETURN
END


FUNCTION sfitslist_srnumber,filename
  ON_ERROR,0
  ;; Sumer version.
  bfilename = byte(STRMID(filename,4,13))
  bfilename(WHERE(bfilename EQ (byte('_'))(0) )) = (byte('.'))(0)
  num = dblarr(N_ELEMENTS(filename))
  READS,STRING(bfilename),num
  RETURN, num
END

;
; Incremental update
;
PRO incsfitslist,fitsdir,file
  
  default,maxfiles,300
  
  ;;----------------------------------------------------
  ;; Find the actual file list
  
  a_filelist = find_files("sum_*_*.fits",fitsdir)  ;*
  
  break_file,a_filelist,disk,dir,a_filename
  
  ;; Calculate srnum
  a_srnum = sfitslist_srnumber(a_filename)
  
  ;; Sort actual file list according to snum
  ix = SORT(a_srnum)
  a_filelist = a_filelist(ix)
  a_filename = a_filename(ix)
  a_srnum = a_srnum(ix)
  
  ;;---------------------------------------------------
  ;; Find the original file list (assumed sorted)
  
  IF NOT file_exist(file) THEN  $
     MESSAGE,"There's no lfitslist.txt file here"
  o_list = rd_ascii(file)
  
  IF N_ELEMENTS(o_list) LT 2 THEN  $
     MESSAGE,"I need an existing, nonempty cds fitslist"
  
  ;; Chop it's head off and calculate srnum (for identification)
  head = o_list(0)
  o_list = o_list(1:*)
  namelen = FIX((str_sep(head,'='))(2))-1
  o_srnum = sfitslist_srnumber(STRMID(o_list,0,namelen))
  
  ;; Loop variable initialization
  
  changed = 0
  currenti = 0 
  newlist = [head]
  
  ;; Check each existing file if it's in the list
  filesdone = 0
  FOR i = 0L,N_ELEMENTS(a_filelist)-1 DO BEGIN
     IF (WHERE(o_srnum EQ a_srnum(i)))(0) EQ -1 THEN BEGIN
        changed = 1
        ;; Find out where to place it to keep list sorted
        ix = (WHERE(o_srnum GT a_srnum(i)))(0)
        IF ix EQ -1 THEN ix = N_ELEMENTS(o_list)
        ;; Create the text entry
        sfitslist_prop,a_filelist(i),out=out
        IF out(0) NE '' THEN begin
           ;; Place it in the new list, along with all higher-ranking 
           ;; existing entries
           IF ix EQ currenti THEN newlist = [newlist,out] $
           ELSE newlist = [newlist,o_list(currenti:ix-1),out]
           ;; These have been accounted for
           currenti = ix
           PRINT,"Added "+a_filelist(i)
        END
        filesdone = filesdone+1
        IF filesdone EQ maxfiles THEN GOTO,ENOUGH_FILES
     END
  END
  
  enough_files:
  PRINT,"Finished processing list"
  
  IF NOT changed THEN BEGIN
     ;; Njet, nada nothing
     PRINT,"No changes"
     RETURN
  END
  
  ;; Add the rest of the original list that wasn't already copied
  
  IF currenti LT N_ELEMENTS(o_list) THEN  $
     newlist = [newlist,o_list(currenti:*)]
  
  ;; Write new list
  
  OPENW,flun,file,/GET_LUN,error = error
  IF error NE 0 THEN BEGIN
     MESSAGE,"Could not open "+file+" for writing -- aborting", $
        /continue
     RETURN
  END
  ;; The format is necessary to avoid a space in front lines
  PRINTF,flun,newlist,FORMAT='(A)'
  CLOSE,flun
  FREE_LUN,flun
END



PRO sfitslist,fitsdir,listdir
  ON_ERROR,2
  
  default,maxfiles,20  ;; Testing purposes at GODDARD
  
;  Does SUMER have !DEBUG ?
;  IF !debug GT 0 THEN ON_ERROR,0
  
  sfitsdata = getenv("SUM_FITS_DATA")
  
  vms = !version.os EQ 'vms'
  
  IF sfitsdata EQ '' THEN BEGIN
     IF vms THEN sfitsdata = "fits_nfs,fits30"  $
     ELSE sfitsdata = $
        "/mn/rigil/u1/paalb/sumer/afits,/mn/rigil/u1/paalb/sumer/mats"
  END
   
  
  default,fitsdir,sfitsdata
  
  IF vms THEN default,listdir,"sys$login"
  default,listdir,"$HOME"       ;*
  
  parcheck,fitsdir,1,typ(/str),0,'FITSDIR'
  parcheck,listdir,2,typ(/str),0,'LISTDIR'
  
  ;; This is where the list should be
  file = concat_dir(listdir,'sfitslist.txt')
  
  ;; Do an incremental update if it's not there.
  IF file_exist(file) THEN BEGIN
     incsfitslist,fitsdir,file
     RETURN
  END
  PRINT,"No file "+file+" found"
  ;; Get the full list
  PRINT,"Getting file list"
  filelist = find_files("sum_*_*.fits",fitsdir)     ;*
  IF filelist(0) EQ '' THEN MESSAGE,"No files found"
  
  PRINT,"About to create new "+file+" with "+ $
     trim(N_ELEMENTS(filelist))+" elements"
  
  break_file,filelist,disk,path,filename
  
  ;; Calculate srnum and sort accordingly
  n = sfitslist_srnumber(filename)
  filelist = filelist(SORT(n))
  
  ;; Just do it.
  
  OPENW,flun,file,/GET_LUN
  calcform = 1
  filesdone = 0
  FOR i = 0,N_ELEMENTS(filelist)-1 DO BEGIN
     sfitslist_prop,filelist(i),calcform=calcform,out=out
     IF out(0) NE '' THEN BEGIN
        PRINTF,flun,out,FORMAT='(A)'
        calcform = 0
     END
     PRINT,"Files done :",filesdone
     filesdone = filesdone + 1
     IF filesdone EQ maxfiles THEN GOTO,ENOUGH_FILES
  END
  
  ENOUGH_FILES:
  
  CLOSE,flun
  FREE_LUN,flun
END

