;+
; NAME:
;      SPICE_OBJECT
;
; PURPOSE:
;      This function returns a SPICE_DATA or a SPICE_DATA_L3 object, depending on the input.
;      If the input is a string, the function assumes that the input is a path to a SPICE FITS file,
;      and tries to return either a SPICE_DATA or a SPICE_DATA_L3 object.
;      If input is not a string, the function checks whether the input is one of these two objects,
;      the result of this check is returned in IS_SPICE. The input is returned unaltered.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      object = spice_object(input [, is_spice=is_spice, object_created=object_created])
;
; INPUTS:
;      input: The name and path of a SPICE file (level 2 or 3) or a SPICE_DATA or a SPICE_DATA_L3 object
;
; KEYWORDS:
;     quiet : If set, then warnings are suppressed.
;
; OUTPUTS:
;      a SPICE_DATA or SPICE_DATA_L3 object
;
; OPTIONAL OUTPUTS:
;      is_spice: is 2 (or 3) if 'input' is a string and this function creates the SPICE_DATA(_L3) object,
;                is 2 (or 3) if 'input' is an object of type SPICE_DATA(_L3),
;                0 otherwise.
;      object_created: is 1 if 'input' is a string and this function creates a SPICE_DATA(_L3) object,
;                0 otherwise.
;
; HISTORY:
;      Ver. 1, 22-Oct-2020, Martin Wiesmann (prits-group@astro.uio.no)
;-
; $Id: 2025-07-31 13:25 CEST $

FUNCTION spice_object, input, is_spice = is_spice, object_created = object_created, quiet = quiet
  is_spice = 0
  object_created = 0
  type = size(input, /type)
  IF type EQ 7 THEN BEGIN
    object = spice_data(input, /quiet)
    IF typename(object) EQ 'SPICE_DATA' THEN BEGIN
      object_created = 1
      is_spice = 2
      return, object
    ENDIF ELSE BEGIN
      object = spice_data_l3(input)
      IF typename(object) EQ 'SPICE_DATA_L3' THEN BEGIN
        object_created = 1
        is_spice = 3
        return, object
      ENDIF
    ENDELSE
  ENDIF ELSE IF type EQ 11 THEN BEGIN
    IF typename(input) EQ 'SPICE_DATA' THEN BEGIN
      is_spice = 2
      return, input
    ENDIF ELSE BEGIN
      IF typename(input) EQ 'SPICE_DATA_L3' THEN BEGIN
        is_spice = 3
        return, input
      ENDIF
    ENDELSE

    IF ~keyword_set(quiet) THEN box_message, 'Input is not a SPICE_DATA or SPICE_DATA_L3 object'
  ENDIF ELSE BEGIN
    IF ~keyword_set(quiet) THEN box_message, 'Input must be either path to SPICE FITS file or SPICE_DATA or SPICE_DATA_L3 object'
  ENDELSE
  return, input
END
