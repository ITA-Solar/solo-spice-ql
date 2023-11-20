;+
; NAME:
;      ANA2FITSHDR_CONST
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
;      HEADERS_INPUT_DATA: The header (string array) of the SPICE level 2 file.
;
; OUTPUTS:
;      a fits header (string array), may be an empty string.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, mkhdr, fxpar
;
; HISTORY:
;      Ver. 1, 2-Dec-2021, Martin Wiesmann
;-
; $Id: 2023-11-20 14:45 CET $


FUNCTION ana2fitshdr_const, DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, XDIM1_TYPE=XDIM1_TYPE, $
  CONST=CONST, HEADERS_INPUT_DATA=HEADERS_INPUT_DATA

  prits_tools.parcheck, DATETIME, 0, 'DATETIME', 'STRING', 0
  prits_tools.parcheck, EXTENSION_NAMES, 0, 'EXTENSION_NAMES', 'STRING', 1, VALID_NELEMENTS=6
  prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional

  IF N_ELEMENTS(CONST) EQ 0 THEN return, ''
  min_const = min(CONST, max=max_const)
  IF min_const EQ 0 && max_const EQ 0 THEN return, ''

  n_dims = size(CONST, /n_dimensions)

  fits_util = obj_new('oslo_fits_util')
  mkhdr, hdr, CONST, /image

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', extension_names[5], 'Extension name'

  fits_util->add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
  fits_util->add, hdr, 'XDIMXT1', extension_names[2], 'Extension name of 1st dim absorbed by analysis'
  fits_util->add, hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'

  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'BTYPE', 'WEIGHTS', 'Type of data'
  fits_util->add, hdr, 'BUNIT', ' ', 'Physical units of data'

  fits_util->clean_header, hdr

  new_hdr = ana2fitshdr_wcshdr(HDR, HEADERS_INPUT_DATA, XDIM1_TYPE=XDIM1_TYPE, /CONST)
    
  return, new_hdr
  
  
  
  
  
  IF keyword_set(header_l2) THEN BEGIN

    ; Add WCS keywords
    fits_util->add_description, hdr, 'World Coordinate System (WCS) keywords'
    fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
    fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
    fits_util->add, hdr, 'CUNIT1', ' ', 'Units for 1st coordinate (for CRVAL1, CDELT1)'
    fits_util->add, hdr, 'CRVAL1', 1.0, '[] 1st coordinate of reference point'
    fits_util->add, hdr, 'CDELT1', 1.0, '[] Increment of 1st coord at ref point'
    fits_util->add, hdr, 'CRPIX1', 1.0, '[pixel] 1st pixel index of reference point'
    fits_util->add, hdr, 'PC1_1', 1.0, 'Default value, no rotation'
    fits_util->add, hdr, '', ' '

    fits_util->add, hdr, 'CTYPE2', fxpar(header_l2, 'CTYPE1', missing=''), 'Type of 2nd coordinate'
    fits_util->add, hdr, 'CNAME2', fxpar(header_l2, 'CNAME1', missing=''), 'Name of 2nd coordinate'
    fits_util->add, hdr, 'CUNIT2', fxpar(header_l2, 'CUNIT1', missing=''), 'Units for 2nd coordinate (for CRVAL2, CDELT2)'
    fits_util->add, hdr, 'CRVAL2', fxpar(header_l2, 'CRVAL1', missing=0), '[arcsec] 2nd coordinate of reference point'
    fits_util->add, hdr, 'CDELT2', fxpar(header_l2, 'CDELT1', missing=0), '[arcsec] Increment of 2nd coord at ref point'
    fits_util->add, hdr, 'CRPIX2', fxpar(header_l2, 'CRPIX1', missing=0), '[pixel] 2nd pixel index of reference point '
    fits_util->add, hdr, 'PC2_2', fxpar(header_l2, 'PC1_1', missing=0), 'Non-default value due to CROTA degrees S/C roll'
    fits_util->add, hdr, 'PC2_3', fxpar(header_l2, 'PC1_2', missing=0), 'Contribution of dim 3 to coord 2 due to roll'
    fits_util->add, hdr, '', ' '

    fits_util->add, hdr, 'CTYPE3', fxpar(header_l2, 'CTYPE2', missing=''), 'Type of 3rd coordinate'
    fits_util->add, hdr, 'CNAME3', fxpar(header_l2, 'CNAME2', missing=''), 'Name of 3rd coordinate'
    fits_util->add, hdr, 'CUNIT3', fxpar(header_l2, 'CUNIT2', missing=''), 'Units for 3rd coordinate (for CRVAL3, CDELT3)'
    fits_util->add, hdr, 'CRVAL3', fxpar(header_l2, 'CRVAL2', missing=0), '[arcsec] 3rd coordinate of reference point'
    fits_util->add, hdr, 'CDELT3', fxpar(header_l2, 'CDELT2', missing=0), '[arcsec] Increment of 3rd coord at ref point'
    fits_util->add, hdr, 'CRPIX3', fxpar(header_l2, 'CRPIX2', missing=0), '[pixel] 3rd pixel index of reference point '
    fits_util->add, hdr, 'PC3_2', fxpar(header_l2, 'PC2_1', missing=0), 'Contribution of dim 2 to coord 3 due to roll'
    fits_util->add, hdr, 'PC3_3', fxpar(header_l2, 'PC2_2', missing=0), 'Non-default value due to CROTA degrees S/C roll'
    fits_util->add, hdr, '', ' '

    fits_util->add, hdr, 'CTYPE4', fxpar(header_l2, 'CTYPE4', missing=''), 'Type of 4th coordinate'
    fits_util->add, hdr, 'CNAME4', fxpar(header_l2, 'CNAME4', missing=''), 'Name of 4th coordinate'
    fits_util->add, hdr, 'CUNIT4', fxpar(header_l2, 'CUNIT4', missing=''), 'Units for 4th coordinate (for CRVAL4, CDELT4)'
    fits_util->add, hdr, 'CRVAL4', fxpar(header_l2, 'CRVAL4', missing=0), '[s] 4th coordinate of reference point'
    fits_util->add, hdr, 'CDELT4', fxpar(header_l2, 'CDELT4', missing=0), '[s] Increment of 4th coord at ref point'
    fits_util->add, hdr, 'CRPIX4', fxpar(header_l2, 'CRPIX4', missing=0), '[pixel] 4th pixel index of reference point '
    fits_util->add, hdr, 'PC4_4', fxpar(header_l2, 'PC4_4', missing=0), 'Default value, no rotation'
    pc4_1 = fxpar(header_l2, 'PC4_1', missing=-9999)
    if pc4_1 gt -9998 then begin
      fits_util->add, hdr, 'PC4_2', fxpar(header_l2, 'PC4_1', missing=0), 'Contribution of dim 2 to coord 4 due to roll'
    endif

  ENDIF ELSE BEGIN ; header_l2

    fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
    fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
    for idim=1,n_dims-1 do begin
      idim_str = strtrim(string(idim+1), 2)
      case idim of
        1: dim_name = '2nd'
        2: dim_name = '3rd'
        else: dim_name = idim_str+'th'
      end
      fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
      fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
    endfor ; idim=1,n_dims-1

  ENDELSE ; header_l2

end
