;+
; NAME:
;     spice_browser_make_windata
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_make_windata, data, windata
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


PRO spice_browser_make_windata, data, windata
  ;
  ; This is meant to reduce the size of the data files by extracting
  ; narrow windows centered on key lines. Not implemented yet, though.
  ;
  str=[ {wvl: 1334.532, ion: 'c_2', wind: -1}, $
    {wvl: 1335.708, ion: 'c_2', wind: -1}, $
    {wvl: 1399.766, ion: 'o_4', wind: -1}, $
    {wvl: 1401.158, ion: 'o_4', wind: -1}, $
    {wvl: 1393.757, ion: 'si_4', wind: -1}, $
    {wvl: 1402.772, ion: 'si_4', wind: -1}, $
    {wvl: 2796.352, ion: 'mg_2', wind: -1}, $
    {wvl: 2803.531, ion: 'mg_2', wind: -1}, $
    {wvl: 1355.598, ion: 'o_1', wind: -1}, $
    {wvl: 1354.067, ion: 'fe_21', wind: -1} ]

  nwin=data->get_number_windows()
  FOR i=0,nwin-1 DO BEGIN
    lam=data->get_lambda_vector(i)
    k=where(str.wvl GE min(lam) AND str.wvl LE max(lam),nk)
    IF nk NE 0 THEN str[k].wind=i
  ENDFOR

  k=where(str.wind NE -1,nk)
  str=str[k]

  FOR i=0,nk-1 DO BEGIN

  ENDFOR

END
