;+
; NAME:
;     spice_browser_oplot_line_ids
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_oplot_line_ids, wrange, yrange, idstr, velocity=velocity, refwvl=refwvl
;
; INPUTS:
;     input:  XXX
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     None.
;
; OUTPUT:
;     XXX
;
; EXAMPLE:
;
; INTERNAL ROUTINES:
;
; PROGRAMMING NOTES:
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-


FUNCTION spice_browser_oplot_line_ids, wrange, yrange, idstr, velocity=velocity, refwvl=refwvl
  ;
  ; Overplots line IDs on the spectra
  ;
  n=n_elements(idstr)
  FOR i=0,n-1 DO BEGIN
    wvl=idstr[i].wvl/10.0 ; id strings are Angstrom, but SPICE uses nm
    ion=idstr[i].ion
    IF keyword_set(velocity) THEN wvl=lamb2v(wvl-refwvl,refwvl)
    ;
    IF wvl GE wrange[0] AND wvl LE wrange[1] THEN BEGIN
      y75=0.75*yrange[1]+0.25*yrange[0]
      y25=0.25*yrange[1]+0.75*yrange[0]
      ypos=(10-(i MOD 10))*(y75-y25)/10. + y25
      ;
      xyouts,wvl,ypos,trim(ion),charsiz=1.2
      oplot,[1,1]*wvl,[-(y75-y25)/15.,0]+ypos
    ENDIF
  ENDFOR

  return,0

END
