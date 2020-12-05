;; TODO: Add
;; self.info, ... , level=level        ; Informative, only when threshold <= verbose

FUNCTION dprint::init, debug=debug, verbose=verbose, quiet=quiet
  self.dprint_data = dictionary()
  self.dprint_data.debug = keyword_set(debug) ? debug : 0
  self.dprint_data.verbose = keyword_set(verbose) ? verbose : 0
  self.dprint_data.quiet = keyword_set(quiet) ? quiet : 0
  return, 1
END

PRO dprint::dprint, p1, p2, p3, p4, p5, p6, _extra=_extra, level=level
  IF n_elements(level) EQ 0 THEN level = 1
  IF level GT self.dprint_data.debug THEN return
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

PRO dprint::info, p1, p2, p3, p4, p5, p6, _extra=_extra, threshold=threshold
  IF n_elements(threshold) EQ 0 THEN threshold = 0
  IF threshold GT self.dprint_data.verbose THEN return
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
  IF keyword_set(level) THEN return, self.dprint_data.debug
  return, self.dprint_data.debug GT 0
END

FUNCTION dprint::verbose, level=level
  IF keyword_set(level) THEN return, self.dprint_data.verbose
  return, self.dprint_data.verbose GT 0
END

PRO dprint__define
  dummy = {dprint, dprint_data: dictionary() }
END
