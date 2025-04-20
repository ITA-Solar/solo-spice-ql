;+
; Name       : prits_tools::add_pythonpaths
;
; Purpose    : Add python subdirectories of IDL directories to $PYTHONPATH
;              Paths are appended by default, set prepend=1 to prepend
;
; Opt. Inputs: prepend = 1 : prepend python paths to PYTHONPATH
;
; Method:
;
;     Foreach path p in !path:
;       check if p/python exists
;       if it does, append or prepend p/python to the PYTHONPATH
;     end
;
; Version: 1, SVHH
;
; $Id: $
;-

PRO prits_tools::add_pythonpaths, prepend = prepend
  COMPILE_OPT STATIC
  prepend = keyword_set(prepend)

  existing = getenv('PYTHONPATH')
  IF existing EQ "" THEN pythonpaths = [] $
  ELSE pythonpaths = strsplit(existing, ':', /extract)

  paths = strsplit(!path, ':', /extract)
  FOR i = 0, n_elements(paths) - 1 DO BEGIN
    pythonpath = paths[i] + '/python'
    IF file_test(pythonpath, /directory) THEN BEGIN
      IF prepend THEN pythonpaths = [pythonpath, pythonpaths] $
      ELSE pythonpaths = [pythonpaths, pythonpath]
    ENDIF
  ENDFOR

  setenv, "PYTHONPATH=" + strjoin(pythonpaths, ':')
END
