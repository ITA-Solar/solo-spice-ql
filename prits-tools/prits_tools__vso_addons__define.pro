FUNCTION prits_tools::vso_cached_search_use_savefile_or_not, date_beg, savefile, redo_recent_days
  file_info = file_info(savefile)
  IF NOT file_info.exists THEN return, 0
  
  file_utc = unixtai2utc(file_info.ctime)
  get_utc, current_utc
  
  file_age_days = current_utc.mjd - file_utc.mjd
  
  file_is_old_enough = file_age_days GT redo_recent_days - 1
  
  return, file_is_old_enough
END

FUNCTION prits_tools::vso_cached_search, date_beg, date_end, instrument, wave_str_in, sample, include_urls, $
                                         accept_failure=accept_failure, $
                                         redo_recent_days=redo_recent_days, $
                                         quiet=quiet
  IF n_params() LT 6 THEN BEGIN
     message, "Must have six arguments!"
  END
  
  wave_str = wave_str_in
  quiet = keyword_set(quiet) 
  
  self.default, redo_recent_days, 5
  self.default, accept_failure, 0
  
  IF instrument EQ 'eit' THEN BEGIN
     IF wave_str EQ '193' THEN wave_str = '195'
     IF wave_str EQ '211' THEN wave_str = '284'
  END
  IF instrument EQ 'aia' THEN BEGIN
     IF wave_str EQ '195' THEN wave_str = '193'
     IF wave_str EQ '284' THEN wave_str = '211'
  END
  
  IF instrument EQ 'aia' AND date_end LT '2010' THEN return, !null
  
  savefile_name = date_beg + "--"
  savefile_name += date_end + "--" 
  savefile_name += instrument + "-"
  savefile_name += wave_str + "-"
  savefile_name += sample.tostring() + '-' 
  savefile_name += include_urls.tostring()  
  IF NOT file_test(self.vso.cache_dir, /directory) THEN BEGIN
     print
     m = ['', $
          'Could not find VSO cache directory ' + self.vso.cache_dir, $
          'Set VSO_CACHE_DIR to point to it, or have it in $HOME/vso-cache', $
          '', $
          'To change this object''s VSO cache directory use "self->vso.cache_dir = <path>"', $
          'at the prompt below', '' $
         ]
     box_message, m
     message, "See box above"
  END
  
  savefile = self.vso.cache_dir + "/" + savefile_name + ".sav"
  
  use_savefile = self.vso_cached_search_use_savefile_or_not(date_beg, savefile, redo_recent_days)
  print, "redo_recent_days/use_savefile:", redo_recent_days, use_savefile
  IF use_savefile THEN BEGIN
     IF NOT quiet THEN message, /info, "Using cached vso search in " + savefile
     restore, savefile
     IF size(results, /tname) EQ 'STRUCT' THEN return, results
     IF NOT keyword_set(retry) THEN return, !null
     IF quiet THEN message, /info, "Retrying: " + savefile
  END
  
  box_message, "Calling vso_search(), may take some time"
  results = vso_search(date_beg, date_end, instrument=instrument, wave=wave_str, urls=inclulde_urls, sample=sample)
  IF size(results, /tname) NE 'STRUCT' THEN BEGIN
     IF NOT quiet THEN box_message, "VSO_SEARCH did not work: " + savefile_name
     results = 0
     save, results, filename=savefile
     return, !null
  END
  
  save, results, filename=savefile
  return, results
END



FUNCTION prits_tools::vso_cached_get, result, quiet=quiet
  quiet = keyword_set(quiet)
  
  ; We don't know file name until we've gotten the file, so construct the name
  ; of a link with a name that we can construct from the result, check if it
  ; exists already
  
  fileid = file_basename(result.fileid) ;; AIA fileid isn't a file name, but does not hurt
  fileid_link_name = self.vso.cache_dir + "/" + fileid + ".lnk"
  link_status = file_info(fileid_link_name)
  have_file = link_status.symlink AND NOT link_status.dangling_symlink
  IF have_file THEN BEGIN
     IF NOT quiet THEN box_message, "Already have " + fileid_link_name
     return, fileid_link_name
  END
  status = vso_get(result, out_dir=self.vso.cache_dir, filename=file, /use_network)
  IF status.info NE '' OR file EQ '' THEN BEGIN
     IF NOT quiet THEN box_message, "VSO_GET failed, status.info:" + status.info
     return, ""
  END
  
  shortened_destination = self->shorten_symlink(file, fileid_link_name)
  IF NOT quiet THEN box_message, "Linking " + fileid_link_name + " -> " + shortened_destination
  file_link, shortened_destination, fileid_link_name
  return, fileid_link_name
END

PRO prits_tools::vso_rename_cache_entries_ad_hoc
  
  f = file_search(self.vso.cache_dir, '*-aia-195-*.sav')
  IF f[0] NE '' THEN BEGIN 
     foreach file, f DO BEGIN
        file_move, file, file.replace('-aia-195-', '-aia-193-')
     END
  END
  
  f = file_search(self.vso.cache_dir, '*-aia-284-*.sav')
  IF f[0] NE '' THEN BEGIN
     foreach file, f DO BEGIN
        file_move, file, file.replace('-aia-284-', '-aia-211-')
     END
  END
  
  f = file_search(self.vso.cache_dir, '*-eit-193-*.sav')
  IF f[0] NE '' THEN BEGIN
     foreach file, f DO BEGIN
        file_move, file, file.replace('-eit-193-', '-eit-195-')
     END
  END
  
  f = file_search(self.vso.cache_dir, '*-eit-211-*.sav')
  IF f[0] NE '' THEN BEGIN
     foreach file, f DO BEGIN
        file_move, file, file.replace('-eit-211-', '-eit-284-')
     END
  END
  
  f = file_search(self.vso.cache_dir, '*T23:59-*.sav')
  IF f[0] NE '' THEN BEGIN
     foreach file, f DO BEGIN
        destination = file.replace('T23:59-', 'T23:59.99-')
        IF NOT file_test(destination) THEN file_move, file, destination $
        ELSE file_delete, file
     END
  END
  
;  f = file_search(self.vso.cache_dir, '*-1.sav')
;  IF f[0] NE '' THEN BEGIN
;     foreach file,  f DO BEGIN
;        cache_dir_len = strlen(self.vso.cache_dir)
;        start = strmid(file, 0, cache_dir_len + 12)
;        rest = strmid(file, cache_dir_len + 12-1, 1000)
;        new = start + rest
;        start = strmid(new, 0, cache_dir_len + 12 + 21)
;        rest = strmid(new, cache_dir_len + 12 + 21 - 1, 1000)
;        new = start + rest
;        print, new
;        file_move, file, new
;     END
;  END
  f = file_search(self.vso.cache_dir, '*---*')
  IF f[0] NE '' THEN BEGIN
     foreach file, f DO BEGIN
        new = file.replace('---', '--')
        new = new.replace('.999', '.99')
        IF NOT file_test(new) THEN file_move, file, new $
        ELSE file_delete, file
     END
  END
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          CACHE FILLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO prits_tools::vso_fill_day_cache, results, hours
  results = results[sort(results.time.start)]
  day = strmid(results[0].time.start, 0, 10)
  FOREACH hour, hours DO BEGIN
     date_hour = day + "T" + hour
     ix = where(results[*].time.start GT date_hour)
     IF ix[0] EQ -1 THEN CONTINUE
     result = results[ix[0]]
     file_link = self.vso_cached_get(result)
  END
END 

PRO prits_tools::vso_fill_cache, start, final, reverse=reverse, waves_str=waves_str, $
                                 instruments=instruments, hours=hours, retry=retry, quiet=quiet
  
  self.default, start, '2006/10/18' ;; First Hinode obs
  self.default, final, 'today'      ;; Calculated later
  self.default, reverse, 0
  self.default, waves_str, ['171', '193', '304', '211']
  self.default, instruments, ['aia', 'eit']
  self.default, hours, ['00', '12']
  self.default, retry, 0
  
  IF final EQ 'today' THEN BEGIN
     jd2ymd, systime(/julian), y, m, d
     ymd = trim(y) + '-' + trim(m, '(I02)') + '-' + trim(d, '(I02)')
     final = ymd
  END
  
  days = self.list_of_days(start, final, reverse=reverse)
  
  print, "FILLING CACHE"
  print, "FROM " + days[0]
  print, "TO   " + days[-1]

  FOR instrument_ix=0, n_elements(instruments)-1 DO BEGIN
     instrument = instruments[instrument_ix]
     FOR day_ix=0, n_elements(days)-1 DO BEGIN
        day = days[day_ix]
        day_end = day + 'T23:59.99'
        FOREACH wave_str, waves_str DO BEGIN
           extra = { instrument:instrument, wave:wave_str, urls:1, sample:3600}
           catch, err
           IF err NE 0 THEN BEGIN
              catch, /cancel
              IF NOT quiet THEN message, "Caught error:" + !error_state.msg, /continue
              CONTINUE
           END
           results = self.vso_cached_search(day, day_end, _extra = extra, /retry)
           IF n_elements(results) GT 0 THEN  self.vso_fill_day_cache, results, hours
        END
     END
  END 
END


PRO prits_tools::vso_addons_init
  vso_cache_dir = getenv("VSO_CACHE_DIR")
  IF vso_cache_dir EQ "" THEN vso_cache_dir = "$HOME/vso-cache"
  
  ;; Make sure we have a fully qualified PHYSICAL path i.e., no symlinks
  cd, vso_cache_dir, current=cwd
  cd, cwd, current=vso_cache_dir
  
  self.vso.cache_dir = vso_cache_dir
  box_message, 'VSO CACHE DIRECTORY: ' + self.vso.cache_dir
END

PRO prits_tools__vso_addons__define
  compile_opt static
  vso = {prits_tools__vso_addons, $
         cache_dir:"" $
        }
END
;pt = prits_tools()
;pt.rename_cache_entries_ad_hoc
;END
