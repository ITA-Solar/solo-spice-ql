;+
; Project     : SOLAR ORBITER - SPICE     
;                   
; Name        : SPICE_FITSLIST
;               
; Purpose     : Create/update the spice_fitslist.txt file.
;               
; Explanation : This program creates/updates a file
;               called spice_fitslist.txt in the fits file directory, with
;               various information on the content of the files.
;               This file is used by PICKFITS in order to search
;               the list of files for those files that the user wants.
;
; Use         : SPICE_FITSLIST [,FITSDIR [,LISTDIR]]
;    
; Inputs      : None required.
;               
; Opt. Inputs : FITSDIR : The directory with the fits files to be in
;                         the list. Default "SPICE_FITS_DATA"
;
;               LISTDIR : The directory to place the list in. This is
;                         also the directory where the spice_fitslist.txt
;                         file is created/updated. The default is
;                         "$SPICE_FITS_DATA_W".
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : ?
;
; Common      : None.
;               
; Restrictions: Expects to find fits files in the specified directory.
;               If there is already a spice_fitslist.txt file in the same
;               directory, it has to have one or more entries.
;               
; Side effects: Creates/updates the file "spice_fitslist.txt" in the
;               specified directory.
;               
; Category    : SPICE_Utility
;               
; Prev. Hist. : 
;
; Written     : Stein Vidar H. Haugan, UiO, 9 August 2020
;               
; Modified    : Version 1, SVHH, 9 August 2020
;                          Initial version based on sfitslist.pro
;
; Version     : Version 1, SVHH, 9 August 2020
;-            

FUNCTION spice_fitslist__line,header,keyword_info
  line = []
  keyword_array = keyword_info.keys()
  foreach keyword,keyword_array DO line =  [line, trim(fxpar(header,keyword))]
  RETURN,strjoin(line,string(9b))
END


FUNCTION spice_fitslist__unique_key,line
  return, line.extract("solo_.*spice.*[0-9]{8}T[0-9]{6}.*V[0-9]+")
END


FUNCTION spice_fitslist__stash_in_hash,lines
  keys = spice_fitslist__unique_key(lines)
  return,hash(keys,lines) ;; Wow....
END


FUNCTION spice_fitslist__get_header,filename
     openr,lun,filename,/get_lun
     fxhread,lun,header
     free_lun,lun
     return,header
END


PRO spice_fitslist,spice_datadir,listdir,reset=reset,maxfiles=maxfiles
  ON_ERROR,0
  
  keyword_info = spice_keyword_info(/all)
  
  default,reset,1
  default,maxfiles,2000
  default,spice_datadir,getenv("SPICE_DATA")
  default,listfiledir,spice_datadir
  
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  
  IF keyword_set(reset) THEN file_delete,listfilename,/allow_nonexistent
  
  IF file_exist(listfilename) THEN BEGIN
     list =  rd_ascii(listfilename)
     stash = spice_fitslist__stash_in_hash(list[1:*])
     print,"Found list, with "+trim(stash.count())+" elements"
  END ELSE BEGIN
     PRINT,"No file "+listfilename+" found"
     PRINT,"Creating one from scratch"
     stash = hash()
  END
  
  fits_filelist = file_search(spice_datadir,"*.fits")
  IF fits_filelist(0) EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END
  
  PRINT,"About to create new " + listfilename + " with "+ $
     trim(N_ELEMENTS(fits_filelist))+" elements"
  
  FOREACH fits_filename, fits_filelist, index DO BEGIN
     key = spice_fitslist__unique_key(fits_filename)
     IF stash.haskey(key) THEN BEGIN
        print,"Skipping "+key
        CONTINUE
     END
     
     header = spice_fitslist__get_header(fits_filename)
     
     stash[key] = spice_fitslist__line(header,keyword_info)
     PRINT,"Files done :",index+1," "+key
     IF index GE maxfiles-1 THEN BREAK
  END
  keyword_list = keyword_info.keys()
  keyword_array = keyword_list.toArray()
  comma_separated_keywords = keyword_array.join(",")
  keys = (stash.keys()).sort()
  OPENW,fitslist_lun,listfilename,/get_lun
  printf,fitslist_lun,comma_separated_keywords
  foreach key,keys DO printf,fitslist_lun,stash[key],format="(a)"
  FREE_LUN,fitslist_lun
END

spice_fitslist
END
