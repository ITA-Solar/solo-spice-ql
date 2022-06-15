;+
; Project     : SOLAR ORBITER - SPICE     
;                   
; Name        : SPICE_GEN_CAT
;               
; Purpose     : Create/update the spice_catalog.txt file.
;               
; Explanation : This program creates a file called spice_catalog.txt in the
;               $SPICE_DATA/ directory (but other paths can be specified),
;               with various information on the content of the files found in
;               the directory hierarchy below that path.
;
;               This file is used by SPICE_CAT in order to search/filter
;               the list of files for those files that the user wants.
;
; Use         : SPICE_GEN_CAT [,FITSDIR [,CATALOG_DIR]]
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
;               quiet: Set to suppress message for each added line
;
; Category    : SPICE_UTILITY
;               
; Prev. Hist. : 
;
; Written     : Stein Vidar H. Haugan, UiO, 9 August 2020
;               
; Modified    : Version 1, SVHH, 9 August 2020
;                          Initial version based on sfitslist.pro
;               Version 2, SVHH, 11 September 2020
;                          Rewritten from scratch
;               Version 3, TF, 8 February 2022
;                          When keyword RESET is set: do not delete old
;                          catalog file before new catalog file is generated
;               Version 4, SVHH, 31 May 2022
;                          Populate FILE_PATH and ICON_PATH with file path
;                          relative to SPICE_DATA
;
; Version     : Version 4, SVHH, 31 May 2022
;
; $Id: 2022-06-15 15:19 CEST $
;-            

FUNCTION spice_gen_cat__line,header,keyword_info, relative_path
  line = []
  keyword_array = keyword_info.keys()
  foreach keyword,keyword_array DO BEGIN
     keyword_type = keyword_info[keyword].type
     missing = keyword_type EQ 't' ? 'MISSING' : 999999
     value = (fxpar(header,keyword, missing=missing)).tostring()
     IF keyword EQ "FILE_PATH" OR keyword EQ "ICON_PATH" THEN BEGIN
        value = relative_path
     END
     line =  [line, value]
  END
  RETURN,strjoin(line,string(9b))
END


FUNCTION spice_gen_cat__unique_key,line
  level_1_to_3 = line.extract("solo_L[1-3]_spice.*_[0-9]{8}T[0-9]{6}.*V[0-9]+")
  IF level_1_to_3 NE "" THEN return, level_1_to_3
  return, line.extract("solo_L0_spice.*V[0-9]+")
END


FUNCTION spice_gen_cat__stash_lines_in_hash,lines
  keys = spice_gen_cat__unique_key(lines)
  return,hash(keys,lines) ;; Wow....
END


FUNCTION spice_gen_cat__get_header,filename
     openr,lun,filename,/get_lun
     fxhread,lun,header
     free_lun,lun
     return,header
END


PRO spice_gen_cat,spice_datadir,catalog_dir, fake_factor=fake_factor, quiet=quiet
  ON_ERROR,0
  
  quiet = keyword_set(quiet)
  spice_default,fake_factor, 1
  spice_default,spice_datadir,getenv("SPICE_DATA")
  spice_default,catalog_dir,spice_datadir
  
  spice_datadir = expand_path(spice_datadir)
  catalog_filename = concat_dir(catalog_dir,'spice_catalog.txt')
  catalog_tmp_filename = catalog_filename + ".tmp"
  keyword_info_filename = concat_dir(catalog_dir, 'keyword_info.txt')
  keyword_info_tmp_filename = keyword_info_filename + ".tmp"
  
  lines_in_hash = hash()
  
  fits_filelist = file_search(spice_datadir,"*.fits")
  IF fits_filelist(0) EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(fits_filelist)).toString() + " files"
  END
  
  PRINT,"About to create new " + catalog_filename + " with "+ $
        (N_ELEMENTS(fits_filelist)).tostring()+" elements"
  
  keyword_info = spice_keyword_info(/all)
  FOREACH fits_filename, fits_filelist, index DO BEGIN
     key = spice_gen_cat__unique_key(fits_filename)
     IF lines_in_hash.haskey(key) THEN BEGIN
        print,"Skipping "+key
        CONTINUE
     END
     
     header = spice_gen_cat__get_header(fits_filename)
     relative_filename = fits_filename.replace(spice_datadir + "/", "")
     relative_path = file_dirname(relative_filename)
     lines_in_hash[key] = spice_gen_cat__line(header,keyword_info, relative_path)
     IF NOT quiet THEN PRINT,"Files done :",(index+1).toString("(i6)")," "+key
  END
  
  comma_separated_keywords = ((keyword_info.keys()).toArray()).join(",")
  
  OPENW,catalog_lun,catalog_tmp_filename,/get_lun
  printf,catalog_lun,comma_separated_keywords
  keys = (lines_in_hash.keys()).sort()
  FOR fake=0, fake_factor-1 DO BEGIN 
     foreach key,keys DO BEGIN
        printf,catalog_lun,lines_in_hash[key],format="(a)"
     END
  END
  FREE_LUN,catalog_lun
  file_move, catalog_tmp_filename, catalog_filename,/overwrite
  
  openw, lun, keyword_info_tmp_filename, /get_lun
  printf, lun, json_serialize(keyword_info, /lowercase)
  free_lun, lun
  file_move, keyword_info_tmp_filename, keyword_info_filename, /overwrite
END

spice_gen_cat, "$HOME/tmp/spice_data/level0"
END
