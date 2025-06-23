;+
; NAME:
;      ANA_WCS_GET_TRANSFORM
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function extracts WCS parameters from HEADERS_INPUT_DATA and puts the
;      dimension with CTYPE=XDIM1_TYPE as the first dimension.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      new_wcs = ana_wcs_get_transform(XDIM1_TYPE, HEADERS_INPUT_DATA)
;
; INPUTS:
;      XDIM1_TYPE: CTYPE of the absorbed dimension (e.g. 'WAVE'). If not found in HEADERS_INPUT_DATA,
;             !NULL will be returned.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      HEADERS_INPUT_DATA: String array. The header from which the WCS parameters should be taken.
;              This may be the header from the progenitor data, i.e. absorbed dimension does not need
;              to be in the first dimension.
;              If not provided or if NAXIS=0, !NULL  will be returned.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;      WCS: Structure containing World Coordinate System information, with XDIM1_TYPE
;            in the first dimension.
;            Or !NULL if HEADERS_INPUT_DATA is not provided or NAXIS=0 therein, or does not
;            contain a CTYPE with value XDIM1_TYPE.
;
; OPTIONAL OUTPUTS:
;      ind_xdim1: The index of the absorbed dimension before transformation.
;
; CALLS:
;      ptools.parcheck, fxpar, fitshead2wcs, ana_wcs_transform, fitshead2struct
;
; HISTORY:
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;-
; $Id: 2025-06-17 11:04 CEST $

FUNCTION ana_wcs_get_transform, xdim1_type, headers_input_data, ind_xdim1 = ind_xdim1
  ptools.parcheck, xdim1_type, 1, 'XDIM1_TYPE', 'STRING', 0
  ptools.parcheck, headers_input_data, 2, 'HEADERS_INPUT_DATA', 'STRING', 1, /optional

  IF n_elements(headers_input_data) EQ 0 THEN return, !NULL
  naxis = fxpar(headers_input_data, 'NAXIS', missing = 0)
  IF naxis EQ 0 THEN return, !NULL

  ctypes = strtrim(fxpar(headers_input_data, 'CTYPE*', missing = 'xx'), 2)
  ind_xdim1 = where(strcmp(ctypes, xdim1_type, /fold_case), count)
  IF count EQ 0 THEN BEGIN
    message, 'Did not find CTYPEn with value: ' + xdim1_type, /informational
    return, !NULL
  ENDIF
  ind_xdim1 = ind_xdim1[0]

  IF size(headers_input_data, /type) EQ 7 THEN hdr = fitshead2struct(headers_input_data, /multivalue, /silent) $
  ELSE hdr = headers_input_data
  wcs_original = fitshead2wcs(hdr)
  wcs_transformed = ana_wcs_transform(wcs_original, ind_xdim1, 0)

  return, wcs_transformed
END
