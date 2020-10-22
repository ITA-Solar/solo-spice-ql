;+
; NAME:
;      SPICE_GET_OBJECT
;
; PURPOSE:
;      This function is used to make sure that the input is a spice_data object.
;      If input is a string, it is assumed that this is a path to a spice
;      FITS file, a spice_data object of this file is then returned. If input is
;      other than a string, it is returned unaltered.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      object = spice_get_object(file)
;
; INPUTS:
;      file: The name and path of a SPICE file or a spice_data object
;
; OUTPUTS:
;      a spice_data object
;
; HISTORY:
;      Ver. 1, 22-Oct-2020, Martin Wiesmann
;-
; $Id: 22.10.2020 13:21 CEST $


FUNCTION spice_get_object, file, is_spice=is_spice, object_created=object_created
  
  is_spice=0
  object_created=0
  type = size(file, /type)
  if type eq 7 then begin
    object_created=1
    is_spice=1
    return, spice_object(file)
  endif else if type eq 11 then begin
    if typename(file) ne 'SPICE_DATA' then begin
      box_message,'input is not a SPICE_DATA object'
    endif else begin
      is_spice=1
    endelse
  endif else begin
    box_message, 'input must be either path to spice file or SPICE_DATA object'
  endelse
  return, file
END
