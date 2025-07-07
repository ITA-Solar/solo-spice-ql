


function load_gen_dlms_version_file, version_filename
  return, !make_dll.compile_directory + path_sep() + version_filename
end

function load_gen_dlms_must_update, version_filename
  version_file = load_gen_dlms_version_file(version_filename)
  must_update = ~file_test(version_file)
  return, must_update
end

pro load_gen_dlms_touch_version_file, version_filename
  version_file = load_gen_dlms_version_file(version_filename)
  openw, lun, version_file, /get_lun
  printf, lun, version_filename
  close,lun
  free_lun, lun
end

PRO load_gen_dlms, success=success, redo = redo, retry = retry, test_failure = test_failure
  COMPILE_OPT IDL3
  COMMON load_gen_dlms, loaded

  ; If update not needed, can we short-circuit?
  version="gen_idl_dlms_v1.01"
  must_update = load_gen_dlms_must_update(version)
  reuse_existing = ~must_update
 
  prior_failure = n_elements(loaded) EQ 1 && loaded EQ 0
  IF NOT must_update AND prior_failure AND NOT keyword_set(retry) THEN return

  prior_success = n_elements(loaded) EQ 1 && loaded EQ 1
  IF NOT must_update AND prior_success AND NOT keyword_set(redo) THEN return

  ; Ok so we will try:
  loaded = 0
  success = 0

  cc = !make_dll.cc
  tried_gcc = 0
  o3_flag = "-O3"

  TRY_NATIVE_COMPILER_WITH_O3_FLAG:
  TRY_NATIVE_COMPILER_WITHOUT_O3_FLAG:
  TRY_WITH_GCC:

  err = 0
  IF 1 THEN catch, err

  IF err NE 0 THEN BEGIN
    catch, /cancel
    box_message, !error_state.msg
    IF o3_flag THEN BEGIN
      o3_flag = ""
      GOTO, TRY_NATIVE_COMPILER_WITHOUT_O3_FLAG
    END

    IF tried_gcc THEN BEGIN
      box_message, ["ERROR LOADING DLMs from $SSW/gen/dlm", $
        "Please see $SSW/gen/dlm/AAA-README.txt for instructions", $
        "Do you have a C compiler installed? Native or at least gcc?"]
      return
    END

    INCLUDE = STREGEX(!make_dll.cc, '-I[^ ]+', /EXTRACT)
    CC = "gcc -c -fPIC " + INCLUDE + " -O3 %C -o %O"
    tried_gcc = 1
    GOTO, TRY_WITH_GCC
  END

  ; Now we try to do it:
  dlm_home = routine_dir()
  dlms = ["cfit", "fmedian"]
  FOREACH dlm, dlms DO BEGIN
    file_mkdir, !make_dll.compile_directory

    source_dir = dlm_home + path_sep() + dlm

    dlm_file = source_dir + path_sep() + dlm + ".dlm"
    dlm_dest_file = !make_dll.compile_directory + path_sep() + dlm + ".dlm"
    file_copy, dlm_file, dlm_dest_file, /overwrite

    box_message, "LOAD_GEN_DLMS trying MAKE_DLL with O3_FLAG=" + o3_flag + " and CC=" + cc
    make_dll, dlm, "IDL_Load", input_directory = source_dir, output_directory = !make_dll.compile_directory, $
      EXTRA_CFLAGS = o3_flag, cc = cc, reuse_existing = reuse_existing

    IF keyword_set(test_failure) THEN BEGIN
      IF o3_flag THEN message, "Simulating -O3 failure" ; => Catch
      IF tried_gcc EQ 0 THEN message, "Simulating lack of native compiler" ; => catch
      message, "Simulating utter failure"
    END

    DLM_LOAD, !make_dll.compile_directory + path_sep() + dlm + ".dlm"
  END
  loaded = 1
  success = 1
  load_gen_dlms_touch_version_file, version
END

FUNCTION load_gen_dlms_loaded
  help, /dlm, "cfit", out = out1
  help, /dlm, "fmedian", out = out2
  return, n_elements(out1) GT 1 AND n_elements(out2) GT 1
END

;
; Tests:
;
PRO load_gen_dlms_test
  ;
  ; Test if restart is needed for testing:
  ;
  IF load_gen_dlms_loaded() THEN BEGIN
    print
    box_message, "DLMs already loaded, can't test - RESTART IDL"
    message, "DLMs already loaded, can't test - RESTART IDL"
  END

  box_message, "Testing load_gen_dlms"

  ; Try failing (to ensure it does not load)
  ;
  load_gen_dlms, /test_failure
  IF NOT load_gen_dlms_loaded() THEN BEGIN
    box_message, "cfit.dlm *not* loaded with /test_failure - OK"
    print
  END ELSE BEGIN
    box_message, "cfit.dlm was loaded with /test_failure - NOT OK"
    message, "cfit.dlm was loaded with /test_failure - NOT OK"
  END

  ; Try loading after failure (should not load)
  load_gen_dlms
  IF NOT load_gen_dlms_loaded() THEN BEGIN
    box_message, "cfit.dlm not loaded after repeat call - OK"
    print, "."
  END ELSE BEGIN
    box_message, "cfit.dlm was loaded after repeat call - NOT OK"
    message, "cfit.dlm was loaded after repeat call - NOT OK"
  END

  ; Try loading with /retry (should load)
  load_gen_dlms, /retry
  IF load_gen_dlms_loaded() THEN BEGIN
    box_message, "cfit.dlm and fmedian.dlm loaded with /retry - OK"
    print
  END ELSE BEGIN
    message, "cfit.dlm and/or fmedian.dlm not loaded with /retry - NOT OK"
  END

  print
  box_message, ["YOU SHOULD NOW SEE WARNINGS ABOUT REDEFINITIONS:"]
  load_gen_dlms, /redo
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
  ; load_gen_dlms_test
  load_gen_dlms
  ; load_gen_dlms_func_test
END
END
