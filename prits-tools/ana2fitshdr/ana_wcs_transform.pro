;+
; NAME:
;      ANA2FITSHDR_WCSHDR
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the const cube of an ANA object or file.
;      It will return an empty string if all values in the cube are zero or if
;      CONST is not provided.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_const(datetime=datetime, data_id=data_id, CONST=CONST, $
;        header_l2=header_l2)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      CONST: Array to keep the CONST status of each parameter at each point.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      header_l2: The header (string array) of the SPICE level 2 file.
;
; OUTPUTS:
;      a fits header (string array), may be an empty string.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, fxpar
;
; HISTORY:
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;-
; $Id: 2023-11-20 13:13 CET $


FUNCTION ana_wcs_transform_vector, vector, move_dim, dest_dim, naxis
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


FUNCTION ana_wcs_transform, wcs, move_dim, dest_dim

  prits_tools.parcheck, wcs, 1, 'wcs', 8, 0
  prits_tools.parcheck, move_dim, 2, 'move_dim', 'INTEGERS', 0
  prits_tools.parcheck, dest_dim, 3, 'dest_dim', 'INTEGERS', 0

  IF move_dim EQ dest_dim THEN return, wcs
  naxis = N_ELEMENTS(wcs.naxis)
  IF move_dim LT 0 || move_dim GE naxis|| dest_dim LT 0 || dest_dim GE naxis THEN BEGIN
    message, ['At least one of the indices is out of range', $
      'NAXIS    : ' + strtrim(naxis), $
      'MOVE_DIM : ' + strtrim(move_dim), $
      'DEST_DIM : ' + strtrim(dest_dim) ]
    return, wcs
  ENDIF

  new_wcs = wcs

END
