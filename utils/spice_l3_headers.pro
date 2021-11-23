;+
; NAME:
;     SPICE_L3_HEADERS
;
; PURPOSE:
;     spice_create_l3 creates a SPICE level 3 file from a level 2 file
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     The SPICE_L3_HEADERS procedure is
;                 spice_create_l3
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     Level 3 files
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; HISTORY:
;     23-Nov-2021: Martin Wiesmann
;-
; $Id: 2021-11-23 21:01 CET $


FUNCTION spice_l3_headers, ana_headers, l2_header, sit_and_stare=sit_and_stare

  ; Header 1: Results of fitting plus fit parameters plus level 2 keywords
  ; Add time to DATE



  ; Header 2: Data



  ; Header 3: Lambda



  ; Header 4: Residuals



  ; Header 5: Weights



  ; Header 6: Include



  ; Header 7: Const



  return, headers

end