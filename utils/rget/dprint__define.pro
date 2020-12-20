FUNCTION dprint::init, debug=debug, verbose=verbose, quiet=quiet
  self.dprint_data = dictionary()
  debug = n_elements(debug) EQ 1 ? debug : 0
  verbose = n_elements(verbose) EQ 1 ? verbose : 0
  IF keyword_set(quiet) THEN BEGIN
     debug = -1
     verbose = -1
  END
  self.dprint_data.debug_level = debug
  self.dprint_data.verbosity = verbose
  return, 1
END

;; Debug print only if debug level is GE message level (default 1)
;;
PRO dprint::dprint, p1, p2, p3, p4, p5, p6, _extra=_extra, level=level
  IF n_elements(level) EQ 0 THEN level = 1
  IF self.dprint_data.debug_level LT level THEN return
  CASE n_params() OF 
     0: print, _extra=_extra
     1: print, p1, _extra=_extra
     2: print, p1, p2, _extra=_extra
     3: print, p1, p2, p3, _extra=_extra
     4: print, p1, p2, p3, p4, _extra=_extra
     5: print, p1, p2, p3, p4, p5, _extra=_extra
     6: print, p1, p2, p3, p4, p5, p6, _extra=_extra
  END
END

;; Info print only if verbose level GE message level (default 0)
;; Thus verbosity=0 means "normal" info w/level 0 comes through
;;
PRO dprint::info, p1, p2, p3, p4, p5, p6, _extra=_extra, level=level
  IF n_elements(level) EQ 0 THEN level = 0
  IF self.dprint_data.verbosity LT level THEN return
  CASE n_params() OF 
     0: print, _extra=_extra
     1: print, p1, _extra=_extra
     2: print, p1, p2, _extra=_extra
     3: print, p1, p2, p3, _extra=_extra
     4: print, p1, p2, p3, p4, _extra=_extra
     5: print, p1, p2, p3, p4, p5, _extra=_extra
     6: print, p1, p2, p3, p4, p5, p6, _extra=_extra
  END
END

FUNCTION dprint::debug, level=level
  IF keyword_set(level) THEN return, self.dprint_data.debug_level
  return, self.dprint_data.debug_level GT 0
END

FUNCTION dprint::verbose, level=level
  IF keyword_set(level) THEN return, self.dprint_data.verbosity
  return, self.dprint_data.verbosity GT 0
END

PRO dprint__define
  dummy = {dprint, dprint_data: dictionary() }
END
