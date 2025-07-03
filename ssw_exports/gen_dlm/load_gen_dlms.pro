PRO load_gen_dlms, redo = redo, retry = retry, fail = fail
  COMPILE_OPT IDL3
  COMMON load_gen_dlms, loaded

  IF keyword_set(fail) THEN BEGIN
    loaded = 0
    return
  END

  prior_failure = n_elements(loaded) EQ 1 && loaded EQ 0
  IF prior_failure AND NOT keyword_set(retry) THEN return

  prior_success = n_elements(loaded) EQ 1 && loaded EQ 1
  IF prior_success AND NOT keyword_set(redo) THEN return

  ; We will try:
  loaded = 0

  catch, err
  IF n_elements(err) && err NE 0 THEN BEGIN
    catch, /cancel
    box_message, ["ERROR LOADING DLMs from $SSW/gen/dlm DLMs", "Please see $SSW/gen/dlm/AAA-README.txt for instructions"]
    return
  END

  dlm_home = routine_dir()
  dlms = ["cfit", "fmedian"]
  FOREACH dlm, dlms DO BEGIN
    dlm_source_file = dlm_home + path_sep() + dlm + path_sep() + dlm + ".dlm"
    dlm_dest_file = !make_dll.compile_directory + path_sep() + dlm + ".dlm"
    file_copy, dlm_source_file, dlm_dest_file, /overwrite
    source_dir = dlm_home + path_sep() + dlm
    make_dll, dlm, "IDL_Load", input_directory = source_dir, output_directory = !make_dll.compile_directory, $
      /verbose, extra_cflags = "-O3"
    DLM_LOAD, !make_dll.compile_directory + path_sep() + dlm + ".dlm"
  END
  loaded = 1
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
  ; Test if restart is needed for testing:
  help, /dlm, "cfit", out = out
  IF n_elements(out) GT 1 THEN BEGIN
    message, "DLM was loaded before tests - restart IDL"
  END

  print, "", "Testing load_gen_dlms with /fail and /redo", "", format = "(A)"

  ; Try failing (to ensure it does not load)
  load_gen_dlms, /fail
  help, /dlm, "cfit", out = out
  IF n_elements(out) EQ 1 THEN BEGIN
    print, "cfit.dlm *not* loaded with /fail - ok"
    print
  END ELSE BEGIN
    message, "cfit.dlm was loaded with /fail - not ok"
  END

  ; Try loading after failure (should not load)
  load_gen_dlms
  help, /dlm, "cfit", out = out
  IF n_elements(out) EQ 1 THEN BEGIN
    print, "cfit.dlm not loaded after repeat call - ok"
    print, "."
  END ELSE BEGIN
    print, "cfit.dlm was loaded after repeat call - not ok"
    message, "cfit.dlm was loaded after repeat call - not ok"
  END

  ; Try loading with /retry (should load)
  load_gen_dlms, /retry
  help, /dlm, "cfit", out = out1
  help, /dlm, "fmedian", out = out2
  IF n_elements(out1) GT 1 AND n_elements(out2) GT 1 THEN BEGIN
    print, "cfit.dlm and fmedian.dlm loaded with /redo - ok"
    print
  END ELSE BEGIN
    message, "cfit.dlm and/or fmedian.dlm not loaded with /redo - not ok"
  END
ENDIF
END
