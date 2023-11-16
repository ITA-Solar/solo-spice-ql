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
; $Id: 2023-11-16 14:23 CET $


FUNCTION ana2fitshdr_wcshdr, HDR, HEADERS_INPUT_DATA, XDIM1_TYPE=XDIM1_TYPE, $
  RESULT=RESULT, XDIM1=XDIM1, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, CONST=CONST

  IF N_ELEMENTS(HEADERS_INPUT_DATA) EQ 0 THEN return, HDR
  naxis = fxpar(HEADERS_INPUT_DATA, 'NAXIS', missing=0)
  IF naxis EQ 0 THEN return, HDR

  ctypes = strtrim(fxpar(HEADERS_INPUT_DATA, 'CTYPE*', missing='xx'), 2)
  ind_xdim1 = where(ctypes eq XDIM1_TYPE, count)
  IF count EQ 0 THEN BEGIN
    message, 'Did not find CTYPEn with value: ' + XDIM1_TYPE, /informational
    return, HDR
  ENDIF
  ind_xdim1 = ind_xdim1[0]
  ind_xdim1_str = strtrim(ind_xdim1+1,2)
  IF ind_xdim1 EQ 0 THEN xcfit_compatible=1 ELSE xcfit_compatible=0

  ; Add WCS keywords
  fits_util = obj_new('oslo_fits_util')
  fits_util->add_description, hdr, 'World Coordinate System (WCS) keywords'

  FOR iaxis=0,naxis-1 DO BEGIN
    IF iaxis EQ 0 THEN BEGIN

      IF keyword_set(RESULT) || keyword_set(CONST) || keyword_set(INCLUDE) THEN BEGIN
        IF keyword_set(INCLUDE) THEN BEGIN
          fits_util->add, hdr, 'CTYPE1', 'FIT COMPONENT', 'Type of 1st coordinate'
          fits_util->add, hdr, 'CNAME1', 'Component', 'Name of 1st coordinate'
        ENDIF ELSE BEGIN
          fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
          fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
        ENDELSE
        fits_util->add, hdr, 'CUNIT1', ' ', 'Units for 1st coordinate (for CRVAL1, CDELT1)'
        fits_util->add, hdr, 'CRVAL1', 1.0, '[] 1st coordinate of reference point'
        fits_util->add, hdr, 'CDELT1', 1.0, '[] Increment of 1st coord at ref point'
        fits_util->add, hdr, 'CRPIX1', 1.0, '[pixel] 1st pixel index of reference point'
        fits_util->add, hdr, 'PC1_1', 1.0, 'Default value, no rotation'

      ENDIF ELSE BEGIN

        ; XDIM1 or WEIGHTS
        ctype1 = fxpar(HEADERS_INPUT_DATA, 'CTYPE'+ind_xdim1_str, missing='')
        fits_util->add, hdr, 'CTYPE1', ctype1, 'Type of 1st coordinate'

        cname1 = fxpar(HEADERS_INPUT_DATA, 'CNAME'+ind_xdim1_str, missing='')
        fits_util->add, hdr, 'CNAME1', cname1, 'Name of 1st coordinate'

        cunit1 = fxpar(HEADERS_INPUT_DATA, 'CUNIT'+ind_xdim1_str, missing='')
        fits_util->add, hdr, 'CUNIT1', cunit1, 'Units for 1st coordinate (for CRVAL1, CDELT1)'

        crval1 = fxpar(HEADERS_INPUT_DATA, 'CRVAL'+ind_xdim1_str, missing=0)
        fits_util->add, hdr, 'CRVAL1', crval1, '[' + cunit1 + '] 1st coordinate of reference point'

        cdelt1 = fxpar(HEADERS_INPUT_DATA, 'CDELT'+ind_xdim1_str, missing=0)
        fits_util->add, hdr, 'CDELT1', cdelt1, '[' + cunit1 + '] Increment of 1st coord at ref point'

        crpix1 = fxpar(HEADERS_INPUT_DATA, 'CRPIX'+ind_xdim1_str, missing=0)
        fits_util->add, hdr, 'CRPIX1', crpix1, '[pixel] 1st pixel index of reference point '

        pc1 = fxpar(HEADERS_INPUT_DATA, 'PC'+ind_xdim1_str+'_*', missing=0)
        fits_util->add, hdr, 'PC1_1', fxpar(HEADERS_INPUT_DATA, 'PC3_3', missing=0), 'Default value, no rotation'

      ENDELSE

      fits_util->add, hdr, '', ' '

    ENDIF ELSE BEGIN ; iaxis EQ 0

    ENDELSE ; iaxis EQ 0

  ENDFOR ; iaxis=0,naxis-1

  return, HDR
END
