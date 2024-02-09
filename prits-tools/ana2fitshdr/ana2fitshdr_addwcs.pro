;+
; NAME:
;      ANA2FITSHDR_ADDWCS
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function adds the WCS parameters to the input header, given by WCS. 
;      The WCS parameters are transformed according to which keyword is set.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ANA2FITSHDR_ADDWCS( HDR [, WCS] $
;         [, /RESULT] [, /XDIM1] [, /WEIGHTS] [, /INCLUDE] [, /CONST] )
;
; INPUTS:
;      HDR: String array. The header to which the WCS parameters should be added.
;
; OPTIONAL INPUTS:
;      WCS: Structure. The structure from which the WCS parameters
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
;      oslo_fits_util, prits_tools.parcheck
;
; HISTORY:
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;-
; $Id: 2024-02-09 14:36 CET $


FUNCTION ana2fitshdr_addwcs, HDR, WCS, $
  RESULT=RESULT, XDIM1=XDIM1, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, CONST=CONST

  prits_tools.parcheck, HDR, 1, 'HDR', 'STRING', 1
  prits_tools.parcheck, WCS, 2, 'WCS', 8, 0, /optional
  IF total( [ keyword_set(RESULT), keyword_set(XDIM1), keyword_set(WEIGHTS), $
    keyword_set(INCLUDE), keyword_set(CONST) ] ) NE 1 THEN BEGIN
    message, ['You must set exactly one of the keywords', $
      'RESULT, XDIM1, WEIGHTS, INCLUDE, CONST'], $
      /informational
    return, HDR
  ENDIF

  IF N_ELEMENTS(WCS) EQ 0 THEN return, HDR
  new_hdr = hdr

  IF tag_exist(wcs, 'CDELT') THEN cdelt_exists = 1 ELSE cdelt_exists = 0
  matrix_exists = 1
  IF tag_exist(wcs, 'PC') THEN rot_array = wcs.pc $
  ELSE IF tag_exist(wcs, 'CD') THEN rot_array = wcs.cd $
  ELSE matrix_exists = 0

  fits_util = obj_new('oslo_fits_util')

  ; Add WCS keywords
  fits_util->add_description, new_hdr, 'World Coordinate System (WCS) keywords'
  fits_util->add, new_hdr, 'WCSNAME', wcs.WCSNAME, 'Name of coordinate system'
  IF tag_exist(wcs, 'PROJ_NAMES') && tag_exist(wcs, 'PROJ_VALUES') THEN BEGIN
    FOR i=0,min([N_ELEMENTS(wcs.PROJ_NAMES),N_ELEMENTS(wcs.PROJ_VALUES)])-1 DO BEGIN
      fits_util->add, new_hdr, wcs.PROJ_NAMES[i], wcs.PROJ_VALUES[i]
    ENDFOR
  ENDIF
  IF tag_exist(wcs, 'spectrum') THEN BEGIN
    IF tag_exist(wcs.spectrum, 'SPECSYS') THEN $
      fits_util->add, new_hdr, 'SPECSYS', wcs.spectrum.SPECSYS, 'Spectral coord. not corrected for S/C velocity'
    IF tag_exist(wcs.spectrum, 'VELOSYS') THEN $
      fits_util->add, new_hdr, 'VELOSYS', wcs.spectrum.VELOSYS, "[m/s] Default for SPECSYS='TOPOCENT'"
  ENDIF
  fits_util->add, new_hdr, '', ' '

  naxis = N_ELEMENTS(wcs.naxis)
  FOR iaxis=0,naxis-1 DO BEGIN
    iaxis_str = strtrim(string(iaxis+1), 2)
    case iaxis of
      0: axis_name = '1st'
      1: axis_name = '2nd'
      2: axis_name = '3rd'
      else: axis_name = iaxis_str+'th'
    end

    IF iaxis EQ 0 && (keyword_set(RESULT) || keyword_set(CONST) || keyword_set(INCLUDE)) THEN BEGIN

      IF keyword_set(INCLUDE) THEN BEGIN
        fits_util->add, new_hdr, 'CTYPE1', 'FIT COMPONENT', 'Type of 1st coordinate'
        fits_util->add, new_hdr, 'CNAME1', 'Component', 'Name of 1st coordinate'
      ENDIF ELSE BEGIN ; keyword_set(INCLUDE)
        fits_util->add, new_hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
        fits_util->add, new_hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
      ENDELSE ; keyword_set(INCLUDE)
      fits_util->add, new_hdr, 'CUNIT1', ' ', 'Units for 1st coordinate (for CRVAL1, CDELT1)'
      fits_util->add, new_hdr, 'CRVAL1', 1.0, '[] 1st coordinate of reference point'
      IF cdelt_exists THEN $
        fits_util->add, new_hdr, 'CDELT1', 1.0, '[] Increment of 1st coord at ref point'
      fits_util->add, new_hdr, 'CRPIX1', 1.0, '[pixel] 1st pixel index of reference point'

    ENDIF ELSE BEGIN ; iaxis EQ 0 && (keyword_set(RESULT) || keyword_set(CONST) || keyword_set(INCLUDE))

      fits_util->add, new_hdr, 'CTYPE'+iaxis_str, wcs.CTYPE[iaxis], 'Type of '+axis_name+' coordinate'
      fits_util->add, new_hdr, 'CNAME'+iaxis_str, wcs.CNAME[iaxis], 'Name of '+axis_name+' coordinate'
      fits_util->add, new_hdr, 'CUNIT'+iaxis_str, wcs.CUNIT[iaxis], 'Units for '+axis_name+' coordinate (for CRVAL'+iaxis_str+', CDELT'+iaxis_str+')'
      fits_util->add, new_hdr, 'CRVAL'+iaxis_str, wcs.CRVAL[iaxis], '['+wcs.CUNIT[iaxis]+'] '+axis_name+' coordinate of reference point'
      IF cdelt_exists THEN $
        fits_util->add, new_hdr, 'CDELT'+iaxis_str, wcs.CDELT[iaxis], '['+wcs.CUNIT[iaxis]+'] Increment of '+axis_name+' coord at ref point'
      fits_util->add, new_hdr, 'CRPIX'+iaxis_str, wcs.CRPIX[iaxis], '[pixel] '+axis_name+' pixel index of reference point'

      IF iaxis EQ 0 && keyword_set(XDIM1) THEN BEGIN
        fits_util->add, new_hdr, 'BTYPE', wcs.CTYPE[iaxis], 'Type of data'
        fits_util->add, new_hdr, 'BUNIT', wcs.CUNIT[iaxis], 'Physical units of data'
      ENDIF

    ENDELSE ; iaxis EQ 0 && (keyword_set(RESULT) || keyword_set(CONST) || keyword_set(INCLUDE))

    IF matrix_exists THEN BEGIN
      FOR i=0,naxis-1 DO BEGIN
        IF rot_array[iaxis, i] NE 0 THEN $
          fits_util->add, new_hdr, 'PC'+iaxis_str+'_'+strtrim(i+1,2), rot_array[iaxis, i], 'Contribution of dim '+strtrim(i+1,2)+' to coord '+iaxis_str
      ENDFOR ; i=0,naxis-1
    ENDIF ; matrix_exists

    fits_util->add, new_hdr, '', ' '
  ENDFOR ; iaxis=0,naxis-1

  return, new_hdr
END
