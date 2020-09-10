;+
; Project     : SOLAR ORBITER - SPICE     
;                   
; Name        : SPICE_GEN_FITSLIST
;               
; Purpose     : Create/update the spice_fitslist.txt file.
;               
; Explanation : This program creates a file called spice_fitslist.txt in the
;               $SPICE_DATA/ directory (but other paths can be specified),
;               with various information on the content of the files found in
;               the directory hierarchy below that path.
;
;               This file is used by SPICE_CAT in order to search/filter
;               the list of files for those files that the user wants.
;
; Use         : SPICE_GEN_FITSLIST [,FITSDIR [,LISTDIR]]
;    
; Inputs      : None required.
;               
; Opt. Inputs : FITSDIR : The top of the directory tree containing the fits
;                         files to be included in the list. Default is taken
;                         from $SPICE_DATA
;
;               LISTDIR : The directory to place the list in. The default is
;                         to put the file at the top level of the scanned
;                         directory tree
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : fake_factor: Repeat all files fake_factor times (testing)
;               reset: Default is true, reset=0 may have side effects 
;
; Category    : SPICE_UTILITY
;               
; Prev. Hist. : 
;
; Written     : Stein Vidar H. Haugan, UiO, 9 August 2020
;               
; Modified    : Version 1, SVHH, 9 August 2020
;                          Initial version based on sfitslist.pro
;               Version 2, SVHH, 9 September 2020
;                          Rewritten from scratch
;
; Version     : Version 2, SVHH, 9 September 2020
;-            

FUNCTION spice_gen_fitslist__line,header,keyword_info
  line = []
  keyword_array = keyword_info.keys()
  foreach keyword,keyword_array DO line =  [line, trim(fxpar(header,keyword))]
  RETURN,strjoin(line,string(9b))
END


FUNCTION spice_gen_fitslist__unique_key,line
  level_1_to_3 = line.extract("solo_L[1-3]_spice.*_[0-9]{8}T[0-9]{6}.*V[0-9]+")
  IF level_1_to_3 NE "" THEN return, level_1_to_3
  return, line.extract("solo_L0_spice.*V[0-9]+")
END


FUNCTION spice_gen_fitslist__stash_lines_in_hash,lines
  keys = spice_gen_fitslist__unique_key(lines)
  return,hash(keys,lines) ;; Wow....
END


FUNCTION spice_gen_fitslist__get_header,filename
     openr,lun,filename,/get_lun
     fxhread,lun,header
     free_lun,lun
     return,header
END


PRO spice_gen_fitslist,spice_datadir,listdir, reset=reset, fake_factor=fake_factor
  ON_ERROR,0
  
  keyword_info = spice_keyword_info(/all)
  
  default, reset,1
  default,fake_factor, 1
  default,spice_datadir,getenv("SPICE_DATA")
  default,listfiledir,spice_datadir
  
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  
  IF keyword_set(reset) THEN file_delete,listfilename,/allow_nonexistent
  
  IF file_exist(listfilename) THEN BEGIN
     list = rd_ascii(listfilename)
     lines_in_hash = spice_gen_fitslist__stash_lines_in_hash(list[1:*])
     print,"Found list, with "+trim(lines_in_hash.count())+" elements"
  END ELSE BEGIN
     PRINT,"No file "+listfilename+" found"
     PRINT,"Creating one from scratch"
     lines_in_hash = hash()
  END
  
  fits_filelist = file_search(spice_datadir,"*.fits")
  IF fits_filelist(0) EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(fits_filelist)).toString() + " files"
  END
  
  PRINT,"About to create new " + listfilename + " with "+ $
     trim(N_ELEMENTS(fits_filelist))+" elements"
  
  FOREACH fits_filename, fits_filelist, index DO BEGIN
     key = spice_gen_fitslist__unique_key(fits_filename)
     IF lines_in_hash.haskey(key) THEN BEGIN
        print,"Skipping "+key
        CONTINUE
     END
     
     header = spice_gen_fitslist__get_header(fits_filename)
     
     lines_in_hash[key] = spice_gen_fitslist__line(header,keyword_info)
     PRINT,"Files done :",index+1," "+key
  END
  keyword_list = keyword_info.keys()
  keyword_array = keyword_list.toArray()
  comma_separated_keywords = keyword_array.join(",")
  keys = (lines_in_hash.keys()).sort()
  OPENW,fitslist_lun,listfilename,/get_lun
  printf,fitslist_lun,comma_separated_keywords
  FOR fake=0, fake_factor-1 DO BEGIN 
     foreach key,keys DO printf,fitslist_lun,lines_in_hash[key],format="(a)"
  END
  FREE_LUN,fitslist_lun
END

;spice_gen_fitslist
;END
