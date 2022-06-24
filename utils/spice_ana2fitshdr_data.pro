;+
; NAME:
;      SPICE_ANA2FITSHDR_DATA
;
; PURPOSE:
;      This function returns a fits header made from the data of an ANA object or file.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      header = spice_ana2fitshdr_data(header_l2=header_l2, datetime=datetime, $
;      extension_name_prefix=extension_name_prefix, $
;      INPUT_DATA=INPUT_DATA)
;
; INPUTS:
;      header_l2: The header (string array) of the level 2 file.
;      datetime: Date and time string.
;      extension_name_prefix: A string defining the prefix to the names of the 7 extensions
;      INPUT_DATA: Data Array. Up to 7-dimensional data array, with spectra
;            along the first dimension.
; 
; KEYWORDS:
; 
; OPTIONAL INPUTS:
;
; OUTPUTS:
;      a fits header (string array)
;
; OPTIONAL OUTPUTS:
;
; HISTORY:
;      Ver. 1, 1-Dec-2021, Martin Wiesmann
;-
; $Id: 2022-06-24 11:10 CEST $


FUNCTION spice_ana2fitshdr_data, header_l2=header_l2, datetime=datetime, $
  extension_name_prefix=extension_name_prefix, $
  INPUT_DATA=INPUT_DATA
  

  fits_util = obj_new('oslo_fits_util')
  mkhdr, hdr, INPUT_DATA, /image

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', extension_name_prefix+'data', 'Extension name'

  fits_util->add, hdr, 'RESEXT', extension_name_prefix+'results', 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', extension_name_prefix+'data', 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', extension_name_prefix+'lambda', 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', extension_name_prefix+'residuals', 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', extension_name_prefix+'weights', 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', extension_name_prefix+'includes', 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', extension_name_prefix+'constants', 'Extension name of constants'

  ; Add WCS keywords
  fits_util->add_description, hdr, 'World Coordinate System (WCS) keywords'
  fits_util->add, hdr, 'CTYPE1', fxpar(header_l2, 'CTYPE3', missing=''), 'Type of 1st coordinate'
  fits_util->add, hdr, 'CNAME1', fxpar(header_l2, 'CNAME3', missing=''), 'Name of 1st coordinate'
  fits_util->add, hdr, 'CUNIT1', fxpar(header_l2, 'CUNIT3', missing=''), 'Units for 1st coordinate (for CRVAL1, CDELT1)'
  fits_util->add, hdr, 'CRVAL1', fxpar(header_l2, 'CRVAL3', missing=0), '[nm] 1st coordinate of reference point'
  fits_util->add, hdr, 'CDELT1', fxpar(header_l2, 'CDELT3', missing=0), '[nm] Increment of 1st coord at ref point'
  fits_util->add, hdr, 'CRPIX1', fxpar(header_l2, 'CRPIX3', missing=0), '[pixel] 1st pixel index of reference point '
  fits_util->add, hdr, 'PC1_1', fxpar(header_l2, 'PC3_3', missing=0), 'Default value, no rotation'
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
  fits_util->add, hdr, '', ' '

  btype = fxpar(header_l2, 'BTYPE', missing='', comment=comment)
  fits_util->add, hdr, 'BTYPE', btype, comment
  ucd = fxpar(header_l2, 'UCD', missing='', comment=comment)
  fits_util->add, hdr, 'UCD', ucd, comment
  bunit = fxpar(header_l2, 'BUNIT', missing='', comment=comment)
  fits_util->add, hdr, 'BUNIT', bunit, comment

  fits_util->clean_header, hdr

  return, hdr
end
