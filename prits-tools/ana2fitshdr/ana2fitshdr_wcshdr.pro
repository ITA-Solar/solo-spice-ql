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
; $Id: 2023-11-21 11:15 CET $


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
  
  print,''
  print,'ana2fitshdr_wcshdr, HDR'
  print,hdr
  wcs_original = fitshead2wcs(HEADERS_INPUT_DATA)
  print,''
  print, 'wcs_original'
  help, wcs_original
  wcs_transformed = ana_wcs_transform(wcs_original, ind_xdim1, 0)
  print,''
  print, 'wcs_transformed'  
  help, wcs_transformed
  new_hdr = wcs2fitshead(wcs_transformed, oldhead=hdr)
  print,''
  print,'ana2fitshdr_wcshdr, new_hdr'
  print,new_hdr
  
  IF keyword_set(RESULT) || keyword_set(CONST) || keyword_set(INCLUDE) THEN BEGIN
    fits_util = obj_new('oslo_fits_util')
    IF keyword_set(INCLUDE) THEN BEGIN
      fits_util->add, new_hdr, 'CTYPE1', 'FIT COMPONENT', 'Type of 1st coordinate'
      fits_util->add, new_hdr, 'CNAME1', 'Component', 'Name of 1st coordinate'
    ENDIF ELSE BEGIN
      fits_util->add, new_hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
      fits_util->add, new_hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
    ENDELSE
    fits_util->add, new_hdr, 'CUNIT1', ' ', 'Units for 1st coordinate (for CRVAL1, CDELT1)'
    fits_util->add, new_hdr, 'CRVAL1', 1.0, '[] 1st coordinate of reference point'
    fits_util->add, new_hdr, 'CDELT1', 1.0, '[] Increment of 1st coord at ref point'
    fits_util->add, new_hdr, 'CRPIX1', 1.0, '[pixel] 1st pixel index of reference point'
    fits_util->add, new_hdr, 'PC1_1', 1.0, 'Default value, no rotation'
    FOR i=2,naxis DO BEGIN
      fits_util->add, new_hdr, 'PC1_'+strtrim(i,2), 0.0, 'Contribution of dim '+strtrim(i,2)+' to coord 1'
    ENDFOR
    fits_util->clean_header, new_hdr
  ENDIF
  
  return, new_hdr
END
