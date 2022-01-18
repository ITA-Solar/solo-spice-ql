;+
; NAME:
;     SPICE_BROWSER_FONT
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     Defines the fonts to be used in the widgets. Allows for Windows and Unix
;     operating systems.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_font, font [, big=big, fixed=fixed, retina=retina]
;
; INPUTS:
;     None
;     
; KEYWORDS:
;     BIG: Bigger font
;     FIXED: Font with fixed width-letters (Courier)
;     RETINA: The widget was too big for my MacBook Pro retina screen,
;             so I've added this keyword to shrink the fonts.
;
; OUTPUT:
;     font: string containing the font
;
; EXAMPLE:
;
; DEPENDENCIES:
;     None
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-


PRO spice_browser_font, font, big=big, fixed=fixed, retina=retina
  COMPILE_OPT IDL2

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
