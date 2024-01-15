;+
; NAME:
;      ANA_WCS_TRANSFORM_ARRAY
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function manipulates a 2-dimensional array such that row and column 'move_dim'
;      will be moved to row and column 'dest_dim'.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      new_wcs = ana_wcs_transform(wcs, move_dim, dest_dim)
;
; INPUTS:
;      ARRAY: 2-dimensional array with same size in both dimensions.
;      MOVE_DIM: Integer. The index of the dimension to be moved.
;      DEST_DIM: Integer. The index of the destination dimension.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      NAXIS: Scalar integer. Number of elements in each dimension.
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


FUNCTION ana_wcs_transform_array, array, move_dim, dest_dim, naxis
  IF N_ELEMENTS(naxis) EQ 0 THEN NAXIS = N_ELEMENTS(array[0,*])
  new_array = array
  move_i = 0
  FOR i=0,naxis-1 DO BEGIN
    IF move_i EQ move_dim THEN move_i++
    IF i EQ dest_dim THEN BEGIN
      new_array[i,*] = array[move_dim,*]
    ENDIF ELSE BEGIN
      new_array[i,*] = array[move_i,*]
      move_i++
    ENDELSE
  ENDFOR
  array1 =new_array
  move_i = 0
  FOR i=0,naxis-1 DO BEGIN
    IF move_i EQ move_dim THEN move_i++
    IF i EQ dest_dim THEN BEGIN
      new_array[*,i] = array1[*,move_dim]
    ENDIF ELSE BEGIN
      new_array[*,i] = array1[*,move_i]
      move_i++
    ENDELSE
  ENDFOR
  return, new_array
END
