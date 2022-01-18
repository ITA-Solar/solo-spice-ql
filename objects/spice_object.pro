;+
; NAME:
;     SPICE_OBJECT
;
; PURPOSE:
;     spice_object returns a 'spice_data' object.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_object, file [, verbose=verbose]
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; KEYWORD PARAMETERS:
;     verbose : if set, the initiation of the object prints out some information
;
; OUTPUTS:
;     Object of type SPICE_DATA which describes and contains
;     a SPICE raster
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;     The function checks whether 'file' is a string ending with '.fits' and if yes
;     creates a SPICE_DATA object and returns it. This function exists for the user's
;     convenience and simplifies the creation of a new object. See spice_data__define.pro
;     for more information on the SPICE_DATA object.
;
; RESTRICTIONS:
;
; HISTORY:
;     27-Nov-2019: Martin Wiesmann
;-


FUNCTION spice_object, file, verbose=verbose
  COMPILE_OPT IDL2
  
  IF N_ELEMENTS(file) NE 1 || $ 
    SIZE(file, /TYPE) NE 7 || $
    ~ strmatch(file, '*.fits', /fold_case) THEN BEGIN
      print, 'file input must be a scalar string ending with .fits'
      return, -1
  ENDIF ELSE BEGIN
    return, obj_new('spice_data', file, verbose=verbose)
  ENDELSE

END
