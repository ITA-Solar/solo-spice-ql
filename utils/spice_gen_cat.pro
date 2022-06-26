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
; $Id: 2022-06-26 14:56 CEST $
;-            

FUNCTION spice_gen_cat__unique_key,line
  level_1_to_3 = line.extract("solo_L[1-3]_spice.*_[0-9]{8}T[0-9]{6}.*V[0-9]+")
  IF level_1_to_3 NE "" THEN return, level_1_to_3
  return, line.extract("solo_L0_spice.*V[0-9]+")
END


FUNCTION spice_gen_cat__get_header,filename
     openr,lun,filename,/get_lun
     fxhread,lun,header
     free_lun,lun
     return,header
END


;;
;; WRITING:
;;
PRO spice_gen_cat__write_keyword_info, filename, keyword_info
  openw, lun, filename + '.tmp', /get_lun
  printf, lun, json_serialize(keyword_info, /lowercase)
  free_lun, lun
  file_move, filename + '.tmp', filename, /overwrite
END

PRO spice_gen_cat__write_plaintext, filename, keyword_info, file_hash
  comma_separated_keywords = ((keyword_info.keys()).toArray()).join(",")
  tmp_filename = filename + '.tmp'
  OPENW,lun, tmp_filename, /get_lun
  printf,lun,comma_separated_keywords
  keys = (file_hash.keys()).sort()
  foreach key,keys DO BEGIN
     printf,lun,file_hash[key],format="(a)"
  END
  FREE_LUN,lun
  file_move, tmp_filename, filename,/overwrite  
END


PRO spice_gen_cat__write_csv, filename, keyword_info, file_array
  header = (keyword_info.keys()).toarray()
  write_csv, filename + '.tmp', file_array, header=header
  file_move, filename + '.tmp', filename, /overwrite
END


PRO spice_gen_cat__write_json, filename
  
  
END

;;
;; Generating catalog
;;
FUNCTION spice_gen_cat__line, header, keyword_info, relative_path, array=array
  array = []
  keyword_array = keyword_info.keys()
  foreach keyword,keyword_array DO BEGIN
     keyword_type = keyword_info[keyword].type
     missing = keyword_type EQ 't' ? 'MISSING' : 999999
     value = (fxpar(header,keyword, missing=missing)).tostring()
     IF keyword EQ "FILE_PATH" OR keyword EQ "ICON_PATH" THEN BEGIN
        value = relative_path
     END
     array =  [array, value]
  END
  RETURN,strjoin(array,string(9b))
END


FUNCTION spice_gen_cat__add_file, file_hash, file_arr, fits_filename, keyword_info, path_prefix=path_prefix
     key = spice_gen_cat__unique_key(fits_filename)
     IF file_hash.haskey(key) THEN BEGIN
        print,"Skipping "+key
        return, !null
     END
     
     header = spice_gen_cat__get_header(fits_filename)
     relative_filename = fits_filename.replace(path_prefix + "/", "")
     relative_path = file_dirname(relative_filename)
     file_hash[key] = spice_gen_cat__line(header,keyword_info, relative_path, array=array)
     file_arr = [[file_arr], [array]]
     return, key
END

PRO spice_gen_cat,spice_datadir,catalog_dir, fake_factor=fake_factor, quiet=quiet
  ON_ERROR,0
  
  quiet = keyword_set(quiet)
  spice_default,spice_datadir,getenv("SPICE_DATA")
  spice_default,catalog_dir,spice_datadir
  
  spice_datadir = expand_path(spice_datadir) ; Must have explicit path to find relative paths
  
  catalog_basename = concat_dir(catalog_dir,'spice_catalog')
  keyword_info_filename = concat_dir(catalog_dir, 'keyword_info.json')
  
  fits_filelist = file_search(spice_datadir,"*.fits")
  IF fits_filelist(0) EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(fits_filelist)).toString() + " files"
  END
  
  PRINT,"About to create new " + catalog_basename + ".[txt|csv] with "+ $
        (N_ELEMENTS(fits_filelist)).tostring()+" elements"
  
  keyword_info = spice_keyword_info(/all)
  file_hash = hash()
  file_array = []
  
  FOREACH fits_filename, fits_filelist, index DO BEGIN
     key = spice_gen_cat__add_file(file_hash, file_array, fits_filename, keyword_info, path_prefix=spice_datadir)
     IF NOT quiet THEN PRINT,"Files done :",(index+1).toString("(i6)")," "+key
  END
  
  spice_gen_cat__write_keyword_info, keyword_info_filename, keyword_info
  
  spice_gen_cat__write_plaintext, catalog_basename + '.txt', keyword_info, file_hash
  spice_gen_cat__write_csv, catalog_basename + '.csv', keyword_info, file_array
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   spice_gen_cat
END

END
