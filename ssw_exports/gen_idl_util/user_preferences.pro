;+
; Project     : CFIT
;
; Name        : USER_PREFERENCES
;
; Purpose     : Store and retrieve user preferences
;
; Explanation : User preferences can be set with:
;
;                    user_preferences,key,value,/set
;
;               and later retrieved with:
;
;                    user_preferences,key,value,/get
;
;               If a user preference is not defined, value is undefined on return
;
; Use         : USER_PREFERENCES,key,value, ( /set | /get )
;
; Inputs      : KEY : String
;
;               VALUE : Any IDL value other than undefined
;
; Outputs     : VALUE : Undefined or stored preference value
;
; Opt. Outputs: None.
;
; Keywords    : GET/SET: One and only one must be set
;
; Written     : S.V.H.Haugan, UiO, 30. June 2025
;
; Modified    : Version 1, SVHH, 30. June 2025
;
; Version     : 1
; $Id: 2025-07-03 18:11 CEST $
;-

PRO user_preferences, key, value, set = set, get = get
  on_error, 2
  IF n_params() LT 0 THEN BEGIN
    message, /continue, "user_preferences must be called with: "
    message, "  user_preferences, key, value, ( /set | /get )"
  END

  IF n_elements(key) NE 1 OR datatype(key) NE 'STR' THEN BEGIN
    message, "user_preferences must be called with a single, defined string key"
  END

  keywords_set = total([keyword_set(set), keyword_set(get)])
  IF keywords_set NE 1 THEN BEGIN
    print, "ERROR: user_preferences must be called with either /set or /get"
    return
  END

  on_error, 0
  ; First make sure we fail gracefully, like if there is no pref.
  IF keyword_set(get) THEN value = !null
  err = 0
  ; catch,err
  IF err NE 0 THEN BEGIN
    message, /info, "ERROR: user_preferences failed with error code: ", err
    return
  END
  
  file_mkdir, !make_dll.compile_directory
  savefile = !make_dll.compile_directory + path_sep() + ".." + path_sep() + "user_preferences.sav"
  IF file_test(savefile) EQ 0 THEN BEGIN
    preferences = hash()
    save, preferences, filename = savefile
  END
  restore, savefile
  IF keyword_set(get) THEN BEGIN
    IF NOT preferences.haskey(key) THEN BEGIN
      return
    END
    value = preferences[key]
  END ELSE IF keyword_set(set) THEN BEGIN
    preferences[key] = value
    save, preferences, filename = savefile
  END
END

PRO user_preferences_test
  set_value = "HELLO"
  user_preferences, "PREF_TEST", set_value, /set
  user_preferences, "PREF_TEST", get_value, /get
  IF get_value NE set_value THEN BEGIN
    print, "ERROR: user_preferences did not set the value correctly"
    print, "  Expected: ", set_value
    print, "  Got: ", get_value
  END ELSE BEGIN
    print, "user_preferences test passed"
  END

  user_preferences, "NON_EXISTENT", value, /get
  IF n_elements(value) NE 0 THEN BEGIN
    print, "ERROR: user_preferences should not return a value for a non-existent key"
  END ELSE BEGIN
    print, "user_preferences correctly returned no value for non-existent key"
  END
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
  user_preferences_test
END
END
