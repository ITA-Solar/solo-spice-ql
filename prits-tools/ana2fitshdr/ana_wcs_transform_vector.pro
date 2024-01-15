;+
; NAME:
;      ANA_WCS_TRANSFORM_VECTOR
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function moves one element of a vector to another place within the vector.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      new_wcs = ana_wcs_transform_vector(vector, move_dim, dest_dim, naxis)
;
; INPUTS:
;      vector: The vector that should be manipulated.
;      MOVE_DIM: Integer. The index of the element to be moved.
;      DEST_DIM: Integer. The destination index the element should be moved to.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      NAXIS: Integer. Number of elements in the vector.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;      WCS: Structure containing World Coordinate System information.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;
; HISTORY:
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;-
; $Id: 2023-11-30 15:21 CET $


FUNCTION ana_wcs_transform_vector, vector, move_dim, dest_dim, naxis
  IF N_ELEMENTS(naxis) EQ 0 THEN NAXIS = N_ELEMENTS(vector)
  new_vector = vector
  move_i = 0
  FOR i=0,naxis-1 DO BEGIN
    IF move_i EQ move_dim THEN move_i++
    IF i EQ dest_dim THEN BEGIN
      new_vector[i] = vector[move_dim]
    ENDIF ELSE BEGIN
      new_vector[i] = vector[move_i]
      move_i++
    ENDELSE
  ENDFOR
  return, new_vector
END
