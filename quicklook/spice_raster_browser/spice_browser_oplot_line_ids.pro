;+
; NAME:
;     spice_browser_oplot_line_ids
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     Writes the names of some chosen lines in the spectrum plot.
;     The lines are taken from spice_line_list.pro
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_oplot_line_ids, wrange, yrange, velocity=velocity, refwvl=refwvl
;
; INPUTS:
;     wrange: 2-element vector. The x-range of the spectrum plot (wavelength or velocity).
;     yrange: 2-element vector. The y-range of the spectrum plot (intensity).
;     refwvl: A reference wavelength used to calculate the velocity from the wavelengths in the line list.
;             Ignored if /VELOCITY has not been set.
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     velocity: If set then the x-range is assumed to be a velocity, and the velocities will be calculated
;               from the wavelengths in the line list.
;
; INTERNAL ROUTINES:
;     SPICE_LINE_LIST, LAMB2V
;
; PROGRAMMING NOTES:
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;     Ver. 2, 23-May-2023, Martin Wiesmann
;       uses now spice_line_list() instead of user defined line list.
;-
; $Id: 2023-05-23 11:35 CEST $


PRO spice_browser_oplot_line_ids, wrange, yrange, velocity=velocity, refwvl=refwvl
  ;
  ; Overplots line IDs on the spectra
  ;
  line_list = spice_line_list()
  lines = line_list.keys()
  lines = lines.toArray()
  IF keyword_set(velocity) THEN wvls = lamb2v(lines-refwvl,refwvl) $
  ELSE wvls = lines
  ind_lines = where(wvls GE wrange[0] AND wvls LE wrange[1], npeaks)

  for iline=0,npeaks-1 do begin
    wvl = wvls[ind_lines[iline]]
    ion = line_list[lines[ind_lines[iline]]]

    y75=0.75*yrange[1]+0.25*yrange[0]
    y25=0.25*yrange[1]+0.75*yrange[0]
    ypos=(10-(iline MOD 10))*(y75-y25)/10. + y25
    ;
    xyouts,wvl,ypos,trim(ion),charsiz=1.2
    oplot,[1,1]*wvl,[-(y75-y25)/15.,0]+ypos
  endfor

END
