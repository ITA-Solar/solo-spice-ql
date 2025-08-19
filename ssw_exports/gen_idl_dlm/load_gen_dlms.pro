;+
; Project     : GEN_DLMS
;
; Name        : LOAD_GEN_DLMS
;
; Purpose     : Load DLMs contained in $SSW/gen/idl/dlm
;
; Explanation : DLMs need to be compiled, and this routine will try to do it
;               automatically if needed.
;
;               * If we can pick up a site installation *and* it has the correct versions,
;               that takes precedence b/c it seems like the user knows what she/he is doing
;
;               * If not, we try the distribution version (which may or may not exist for
;               this architecture!)
;
;               * If still no dice, we try compilation on the fly. We reuse earlier compilation
;               unless /retry is set
;
;
; Use         : LOAD_GEN_DLMS
;
; Inputs      : None.
;
; Keywords    : SUCCESS: Will be set to 1 on success, 0 otherwise
;               RETRY: Try again if it failed last time
;               REDO: Do over even if it succeeded last time
;               TEST_FAILURE: Pretend everything goes wrong (for testing)
;
; Common      : COMMON load_gen_dlms, loaded
;
;               The variable loaded will be undefined if LOAD_GEN_DLMS has not
;               been run. It will be 1 if last call succeeded, and 0 if last
;               call failed. Can be used for a superfast check of whether there
;               is any point in calling LOAD_GEN_DLMS or not
;
; Restrictions: None.
;
; Side effects: None.
;
; Category    : DLM utility
;
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan (prits-group@astro.uio.no), 9. July 2025
;
; Modified    :
;
; $Id: 2025-08-19 18:58 CEST $
;-

FUNCTION lgdlms_check_if_ok, dlm, version, distribution_path
  ; Check presence (we'v already added fallback distrib. path to !dlm_path)
  help, /dlm, dlm, out = out
  IF n_elements(out) EQ 1 THEN return, !false

  ; Check version:
  version_string = (out[1].toupper()).trim()
  match_against = "VERSION: " + version + ","
  IF ~version_string.startsWith(match_against) THEN return, !false

  ; DLM found, but is it loadable without errors? Could be architecture mismatch.
  catch, err
  IF err NE 0 THEN GOTO, CHECK_IDL_GEN_DLM_DISTRIBUTION
  dlm_load, dlm
  return, !true

  ; There might still be hope for the distribution version, if that was
  ; overshadowed by a wrong version in site/dlm
  ;
  CHECK_IDL_GEN_DLM_DISTRIBUTION:
  catch, err
  IF err NE 0 THEN return, !false
  dlm_load, distribution_path + "/" + dlm
  ; All ok:
  return, !true
END

FUNCTION lgdlms_find_dlms_to_do, dlms_to_check, versions
  ; We add distribution DLM path at *end* of !dlm_path once to pick up
  ; first fall-back source right away
  ssw_binary_type = !version.os + "." + !version.arch
  distribution_path = routine_dir() + "/binaries/" + ssw_binary_type
  !dlm_path = !dlm_path + ":" + distribution_path
  lacking_dlms = []
  FOR i = 0, n_elements(dlms_to_check) - 1 DO BEGIN
    ok = lgdlms_check_if_ok(dlms_to_check[i], versions[i], distribution_path)
    IF ~ok THEN BEGIN
      print, "DLM " + dlms_to_check[i] + " not found or version mismatch"
      lacking_dlms = [lacking_dlms, dlms_to_check[i]]
    END
  END
  return, lacking_dlms
END

PRO lgdlms_try_single_compilation, dlm, cc = cc, o3_flag = o3_flag, $
  redo = redo, test_failure = test_failure, tried_gcc = tried_gcc
  ;
  source_dir = routine_dir() + "/sources/" + dlm

  dlm_file = source_dir + "/" + dlm + ".dlm"
  dlm_dest_file = !make_dll.compile_directory + "/" + dlm + ".dlm"
  file_copy, dlm_file, dlm_dest_file, /overwrite

  box_message, "LOAD_GEN_DLMS trying MAKE_DLL with O3_FLAG=" + o3_flag + " and CC=" + cc
  make_dll, dlm, "IDL_Load", input_directory = source_dir, output_directory = !make_dll.compile_directory, $
    EXTRA_CFLAGS = o3_flag, cc = cc, reuse_existing = ~keyword_set(redo)

  IF keyword_set(test_failure) THEN BEGIN
    IF o3_flag THEN message, "Simulating -O3 failure" ; => Catch
    IF tried_gcc EQ 0 THEN message, "Simulating lack of native compiler" ; => catch
    message, "Simulating utter failure"
  END

  DLM_LOAD, !make_dll.compile_directory + "/" + dlm + ".dlm"
END

PRO lgdlms_try_compilations, dlms_to_do, $
  success = success, redo = redo, test_failure = test_failure
  COMMON load_gen_dlms, loaded

  cc = !make_dll.cc
  tried_gcc = 0
  o3_flag = "-O3"

  ; On error we jump back here so we can try the entire process including
  ; error catching over again with different settings. First label simply
  ; explains what we are doing the first time around
  ;
  TRY_NATIVE_COMPILER_WITH_O3_FLAG:
  TRY_NATIVE_COMPILER_WITHOUT_O3_FLAG:
  TRY_WITH_GCC:

  err = 0
  catch, err

  IF err NE 0 THEN BEGIN
    catch, /cancel
    box_message, !error_state.msg
    IF o3_flag THEN BEGIN
      o3_flag = ""
      GOTO, TRY_NATIVE_COMPILER_WITHOUT_O3_FLAG
    END

    IF tried_gcc THEN BEGIN
      box_message, ["***** ERROR LOADING DLMs *****", $
        "Please see " + routine_dir() + "/sources/AAA-README.txt for instructions", $
        "Do you have a C compiler installed? Native, or at least gcc?"]
      return
    END

    INCLUDE = STREGEX(!make_dll.cc, '-I[^ ]+', /EXTRACT)
    CC = "gcc -c -fPIC " + INCLUDE + " -O3 %C -o %O"
    tried_gcc = 1
    GOTO, TRY_WITH_GCC
  END

  file_mkdir, !make_dll.compile_directory

  FOREACH dlm, dlms_to_do DO BEGIN
    lgdlms_try_single_compilation, dlm, cc = cc, o3_flag = o3_flag, test_failure = test_failure, redo = redo, tried_gcc = tried_gcc
  END
  loaded = 1
  success = 1
END

;
;
PRO load_gen_dlms, success = success, redo = redo, retry = retry, test_failure = test_failure
  COMMON load_gen_dlms, loaded

  prior_failure = n_elements(loaded) EQ 1 && loaded EQ 0
  IF prior_failure AND NOT keyword_set(retry) THEN BEGIN
    success = 0
    return
  END

  prior_success = n_elements(loaded) EQ 1 && loaded EQ 1
  IF prior_success AND NOT keyword_set(redo) THEN BEGIN
    success = 1
    return
  END

  ; Ok so we will try:
  loaded = 0
  success = 0

  dlms_to_check = ["cfit", "fmedian"]
  dlms_to_do = dlms_to_check
  versions = ["1.0", "2.0"]
  IF ~keyword_set(test_failure) THEN BEGIN
    dlms_to_do = lgdlms_find_dlms_to_do(dlms_to_check, versions)

    IF keyword_set(redo) THEN dlms_to_do = dlms_to_check

    IF n_elements(dlms_to_do) EQ 0 THEN BEGIN
      box_message, "DLMs already loaded, no need to load again"
      loaded = 1
      success = 1
      return
    END
  END
  lgdlms_try_compilations, dlms_to_do, redo = redo, success = success, test_failure = test_failure
END

FUNCTION lgdlms_loaded
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
  IF lgdlms_loaded() THEN BEGIN
    print
    box_message, "DLMs already loaded, can't test - RESTART IDL"
    message, "DLMs already loaded, can't test - RESTART IDL"
  END

  box_message, "Testing load_gen_dlms"

  ; Try failing (to ensure it does not load)
  ;
  load_gen_dlms, /test_failure
  IF NOT lgdlms_loaded() THEN BEGIN
    box_message, "cfit.dlm *not* loaded with /test_failure - OK"
    print
  END ELSE BEGIN
    box_message, "cfit.dlm was loaded with /test_failure - NOT OK"
    message, "cfit.dlm was loaded with /test_failure - NOT OK"
  END

  ; Try loading after failure (should not load)
  load_gen_dlms
  IF NOT lgdlms_loaded() THEN BEGIN
    box_message, "cfit.dlm not loaded after repeat call - OK"
    print, "."
  END ELSE BEGIN
    box_message, "cfit.dlm was loaded after repeat call - NOT OK"
    box_message, "If the version has been updated (or not registered), do that first"
    message, "cfit.dlm was loaded after repeat call - NOT OK"
  END

  ; Try loading with /retry (should load)
  load_gen_dlms, /retry
  IF lgdlms_loaded() THEN BEGIN
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
  load_gen_dlms_test
  load_gen_dlms
  load_gen_dlms_func_test
END
END
