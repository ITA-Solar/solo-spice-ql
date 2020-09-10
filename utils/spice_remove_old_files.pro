PRO spice_remove_old_files, top_dir, do_delete=do_delete
  IF n_elements(top_dir) EQ 0 THEN top_dir = getenv("SPICE_DATA")
  files = file_search(top_dir, "solo_L?_spice*.fits", count=count, /expand_environment)
  IF count EQ 0 THEN BEGIN
     print, "No L0 files found under " + top_dir
     return
  END
  
  files_to_delete = []
  files_by_key = hash()
  
  foreach file, files DO BEGIN
     obsid_rasterno = file.extract("_[0-9]+-[0-9]{3}\.fits")
     level = file.extract("solo_L._spice-")
     key = level + obsid_rasterno
     IF NOT files_by_key.haskey(key) THEN files_by_key[key] = file $
     ELSE BEGIN 
        IF file GT files_by_key[key] THEN BEGIN
           files_to_delete = [files_to_delete, files_by_key[key]]
           files_by_key[key] = file
        END ELSE BEGIN
           files_to_delete = [files_to_delete, file]
        END
     END
  END
  
  IF n_elements(files_to_delete) EQ 0 THEN BEGIN
     print, "No files found to delete"
     return
  END
  
  foreach file_to_delete, files_to_delete DO BEGIN
     print, "Delete: " + file_to_delete
     IF keyword_set(do_delete) THEN file_delete, file_to_delete
  END
END
