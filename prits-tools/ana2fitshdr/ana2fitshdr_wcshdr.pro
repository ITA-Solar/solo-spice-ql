;+
; NAME:
;      ANA2FITSHDR_WCSHDR
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function adds the WCS parameters to the input header, given by 
;      HEADERS_INPUT_DATA. The WCS parameters are transformed according to
;      which keyword is set.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ANA2FITSHDR_WCSHDR( HDR [, HEADERS_INPUT_DATA] [, XDIM1_TYPE=XDIM1_TYPE] $
;         [, /RESULT] [, /XDIM1] [, /WEIGHTS] [, /INCLUDE] [, /CONST] )
;
; INPUTS:
;      HDR: String array. The header to which the WCS parameters should be added.
;      XDIM1_TYPE: String. The CTYPE of the absorbed dimension (e.g. 'WAVE').
;
; OPTIONAL INPUTS:
;      HEADERS_INPUT_DATA: String array. The header from which the WCS parameters
;             should be taken. If not provided HDR will be returned unaltered.
;
; KEYWORDS:
;      Exactly one of these keywords must be set.
;      
;      RESULT: If set, the WCS parameters will have 'FIT PARAMETER' as the
;             first dimension's type. The absorbed dimension is not included.
;      XDIM1: If set, the WCS parameters will have the absorbed dimension as the
;             first dimension.
;      WEIGHTS: If set, the WCS parameters will have the absorbed dimension as the
;             first dimension.
;      INCLUDE: If set, the WCS parameters will have 'FIT COMPONENT' as the
;             first dimension's type. The absorbed dimension is not included.
;      CONST: If set, the WCS parameters will have 'FIT PARAMETER' as the
;             first dimension's type. The absorbed dimension is not included.
;
; OUTPUTS:
;      String array. The HDR with the added WCS parameters.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, fxpar
;
; HISTORY:
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;-
; $Id: 2023-11-17 15:27 CET $


FUNCTION ana2fitshdr_wcshdr, HDR, HEADERS_INPUT_DATA, XDIM1_TYPE=XDIM1_TYPE, $
  RESULT=RESULT, XDIM1=XDIM1, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, CONST=CONST

  prits_tools.parcheck, HDR, 1, 'HDR', 'STRING', 1
  prits_tools.parcheck, HEADERS_INPUT_DATA, 2, 'HEADERS_INPUT_DATA', 'STRING', 1, /optional
  prits_tools.parcheck, XDIM1_TYPE, 0, 'XDIM1_TYPE', 'STRING', 0
  IF total( [ keyword_set(RESULT), keyword_set(XDIM1), keyword_set(WEIGHTS), $
    keyword_set(INCLUDE), keyword_set(CONST) ] ) NE 1 THEN BEGIN
    message, ['You must set exactly one of the keywords', $
      'RESULT, XDIM1, WEIGHTS, INCLUDE, CONST'], $
      /informational
    return, HDR
  ENDIF

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
        fits_util->add, hdr, 'PC1_1', pc1[ind_xdim1], 'Contribution of dim 1 to coord 1'
        iplus = 1
        FOR i=0,N_ELEMENTS(pc1)-1 DO BEGIN
          IF i EQ ind_xdim1 THEN BEGIN
            iplus = 0
            continue
          ENDIF
          IF pc1[i] EQ 0 && $
            fxpar(HEADERS_INPUT_DATA, 'PC'+ind_xdim1_str+'_'+strtrim(i+1,2), missing=1) EQ 1 $
            THEN continue
          fits_util->add, hdr, 'PC1_'+strtrim(i+iplus+1,2), pc1[i], 'Contribution of dim '+strtrim(i+iplus+1,2)+' to coord 1'
        ENDFOR

      ENDELSE

      fits_util->add, hdr, '', ' '

    ENDIF ELSE BEGIN ; iaxis EQ 0

    ENDELSE ; iaxis EQ 0

  ENDFOR ; iaxis=0,naxis-1

  fits_util->clean_header, hdr
  return, HDR
END
