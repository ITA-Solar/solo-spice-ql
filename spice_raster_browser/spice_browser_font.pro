;+
; NAME:
;     spice_browser_font
;
; PURPOSE:
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_font, font, big=big, fixed=fixed, retina=retina
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


PRO spice_browser_font, font, big=big, fixed=fixed, retina=retina
  ;+
  ;  Defines the fonts to be used in the widgets. Allows for Windows and Unix
  ;  operating systems.
  ;
  ;  14-Jun-2015: I've added /retina to shrink the fonts for a
  ;  Mac retina display.
  ;-
  CASE !version.os_family OF

    'unix': BEGIN
      IF keyword_set(fixed) THEN fstr='-*-courier-' ELSE $
        fstr='-adobe-helvetica-'
      IF keyword_set(retina) THEN BEGIN
        IF keyword_set(big) THEN str='14' ELSE str='10'
      ENDIF ELSE BEGIN
        IF keyword_set(big) THEN str='18' ELSE str='12'
      ENDELSE
      font=fstr+'bold-r-*-*-'+str+'-*'
    END

    ELSE: BEGIN
      IF keyword_set(fixed) THEN fstr='Courier' ELSE $
        fstr='Arial'
      IF keyword_set(big) THEN str='20' ELSE str='16'
      font=fstr+'*bold*'+str
    END

  ENDCASE

END
