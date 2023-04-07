FUNCTION prits_tools::vso_cached_search, date_beg, date_end, $
                                         instrument=instrument, wave=wave, $
                                         sample=sample, urls=urls
  compile_opt static
  COMMON vso_cached_search_common, search_strings, search_results
  COMMON prits_tools, ptools
  
  ptools.default, search_strings, []
  ptools.default, search_results, []
  
  search_string = date_beg + "-" + date_end + "-" + instrument + $
                  "-" + wave + "-" + sample.tostring() + '-' + urls.tostring()
  ix = where(search_strings EQ search_string, count)
  IF count EQ 1 THEN BEGIN
     print, "Found cached search for " + search_string
     return, *search_results[ix[0]]
  END 
  results = vso_search(date_beg, date_end, instrument=instrument, wave=wave, urls=urls, sample=sample)
  search_strings = [search_strings, search_string]
  search_results = [search_results, ptr_new(results)]
  return, results
END
