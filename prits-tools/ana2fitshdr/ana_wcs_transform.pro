;+
; NAME:
;      ANA_WCS_TRANSFORM
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function takes a WCS structure, transforms it and then returns it.
;      It transforms it thus that one of the dimension is moved to another place.
;      This can be  used to move the absorbed dimension to the first dimension. Or vice versa.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      new_wcs = ana_wcs_transform(wcs, move_dim, dest_dim)
;
; INPUTS:
;      WCS: Structure containing World Coordinate System information.
;      MOVE_DIM: Integer. The index of the dimension to be moved.
;      DEST_DIM: Integer. The index of the destination dimension.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
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
;      prits_tools.parcheck
;
; HISTORY:
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;      Ver. 2, 25-Jan-2023, Terje Fredvik - update wcs.ix and wcs.iy if needed
;-
; $Id: 2024-01-25 15:52 CET $


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
  new_wcs.NAXIS = ana_wcs_transform_vector(wcs.NAXIS, move_dim, dest_dim, naxis)
  new_wcs.CRPIX = ana_wcs_transform_vector(wcs.CRPIX, move_dim, dest_dim, naxis)
  new_wcs.CRVAL = ana_wcs_transform_vector(wcs.CRVAL, move_dim, dest_dim, naxis)
  new_wcs.CTYPE = ana_wcs_transform_vector(wcs.CTYPE, move_dim, dest_dim, naxis)
  new_wcs.CNAME = ana_wcs_transform_vector(wcs.CNAME, move_dim, dest_dim, naxis)
  new_wcs.CUNIT = ana_wcs_transform_vector(wcs.CUNIT, move_dim, dest_dim, naxis)
  IF tag_exist(wcs, 'CDELT') THEN $
    new_wcs.CDELT = ana_wcs_transform_vector(wcs.CDELT, move_dim, dest_dim, naxis)
  IF tag_exist(wcs, 'PC') THEN $
    new_wcs.PC = ana_wcs_transform_array(wcs.PC, move_dim, dest_dim, naxis)
  IF tag_exist(wcs, 'CD') THEN $
    new_wcs.CD = ana_wcs_transform_array(wcs.CD, move_dim, dest_dim, naxis)

  new_wcs.ix = where(new_wcs.ctype EQ wcs.ctype[wcs.ix])
  new_wcs.iy = where(new_wcs.ctype EQ wcs.ctype[wcs.iy])

  return, new_wcs
END
