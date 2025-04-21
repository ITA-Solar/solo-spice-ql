;+
; Name       : prits_tools::add_python_paths
;
; Purpose    : Add python subdirectories of IDL directories to $PYTHONPATH
;              Paths are appended by default, set prepend=1 to prepend
;
; Opt. Inputs: prepend = 1 : prepend python paths to PYTHONPATH
;
; Method:
;
;     foreach path p in !path:
;       check if p/python exists
;       if it does, append or prepend p/python to the PYTHONPATH
;     end
;
; Version: 1, SVHH
;
; $Id: 2025-04-21 15:29 CEST $
;-

PRO prits_tools::add_python_paths, prepend = prepend
  COMPILE_OPT STATIC
  prepend = keyword_set(prepend)

  existing = getenv('PYTHONPATH')
  IF existing EQ "" THEN python_paths = [] $
  ELSE python_paths = strsplit(existing, ':', /extract)

  paths = strsplit(!path, ':', /extract)
  FOR i = 0, n_elements(paths) - 1 DO BEGIN
    pythonpath = paths[i] + '/python'
    IF file_test(pythonpath, /directory) THEN BEGIN
      IF prepend THEN python_paths = [pythonpath, python_paths] $
      ELSE python_paths = [python_paths, pythonpath]
    ENDIF
  ENDFOR

  setenv, "PYTHONPATH=" + strjoin(python_paths, ':')
  print, "PYTHONPATH set to: " + getenv('PYTHONPATH')
END
