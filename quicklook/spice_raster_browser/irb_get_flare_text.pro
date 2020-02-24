;+
; NAME:
;     irb_get_flare_text
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     irb_get_flare_text, input
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
; $Id: 24.02.2020 20:49 CET $


FUNCTION irb_get_flare_text, input
  ;
  ; To get this far, we already know that input is defined, so
  ; don't have to check.
  ;
  n=n_elements(input)
  out_string=strarr(n+1)
  out_string[0]='    Date    Peak-time Class      X       Y'
  FOR i=0,n-1 DO BEGIN
    t_date=anytim2utc(input[i].peaktime,/vms,/date)
    t_time=anytim2utc(input[i].peaktime,/ccsds,/truncate,/time)
    xstr=string(format='(f8.1)',input[i].x)
    ystr=string(format='(f8.1)',input[i].y)
    out_string[i+1]=t_date+'  '+t_time+'  '+trim(input[i].fl_goescls)+xstr+ystr
  ENDFOR

  return,out_string

END
