FUNCTION prits_tools::vso_cached_search, date_beg, date_end,  $
                                         instrument=instrument, wave=wave, sample=sample, urls=urls
  search_string = date_beg + "-"
  search_string += date_end + "-" 
  search_string += instrument + "-"
  search_string += wave + "-"
  search_string += sample.tostring() + '-' 
  search_string += urls.tostring()  
  savefile = self.vso.cache_dir + "/" + search_string + ".sav"
  
  IF file_test(savefile) THEN BEGIN
     box_message, "Found cached vso search in " + savefile
     restore, savefile
     IF size(results, /tname) NE 'STRUCT' THEN results = !null
     return, results
  END
  
  results = vso_search(date_beg, date_end, instrument=instrument, wave=wave, urls=urls, sample=sample)
  IF size(results, /tname) NE 'STRUCT' THEN BEGIN
     box_message, "VSO_SEARCH did not work: " + search_string
     results = 0
     save, results, filename=savefile
     return, !null
  END
  
  save, results, filename=savefile
  return, results
END



FUNCTION prits_tools::vso_cached_get, result
  
  ; We don't know file name until we've gotten the file, so construct the name
  ; of a link with a name that we can construct from the result, check if it
  ; exists already
  
  fileid = file_basename(result.fileid) ;; AIA fileid isn't a file name, but does not hurt
  fileid_link = self.vso.cache_dir + "/" + fileid + ".lnk"
  link_status = file_info(fileid_link)
  have_file = link_status.symlink AND NOT link_status.dangling_symlink
  IF have_file THEN BEGIN
     box_message, "Already have " + fileid_link
     return, fileid_link
  END
  status = vso_get(result, out_dir=self.vso.cache_dir, filename=file, /use_network)
  IF status.info NE '' OR file EQ '' THEN return, ""

  box_message, "Linking " + file + " -> " + fileid_link
  file_link, file, fileid_link
  return, fileid_link
END



PRO prits_tools::vso_addons_init
  self.vso.search_strings = ptr_new([""])
  self.vso.search_results = ptr_new([ptr_new()])
  IF NOT file_test("$HOME/vso-cache") THEN message, "You must create a VSO cache: mkdir $HOME/vso-cache"
  self.vso.cache_dir = expand_path("$HOME/vso-cache")
END

FUNCTION prits_tools_vso_addons
  compile_opt static
  vso = {prits_tools_vso_info, $
         search_strings:ptr_new(), $
         search_results:ptr_new(), $
         cache_dir:"" $
        }
  return, vso
END
