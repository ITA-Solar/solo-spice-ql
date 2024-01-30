;+
; NAME:
;      ANA2FITSHDR_CONST
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the CONST cube of an ANA object or file.
;      It will return an empty string if all values in the CONST cube are zero or if
;      CONST is not provided.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_const(datetime=datetime, data_id=data_id, CONST=CONST, WCS=WCS)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      CONST: Array to keep the CONST status of each parameter at each point. If not provided, or if
;             all values are zero, an empty string will be returned.
;      WCS: Structure. The structure from which the WCS parameters
;             should be taken. If not provided the header won't include any WCS parameters.
;
; KEYWORDS:
;
; OUTPUTS:
;      a fits header (string array), may be an empty string.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, mkhdr, prits_tools.parcheck, ana2fitshdr_addwcs
;
; HISTORY:
;      Ver. 1, 2-Dec-2021, Martin Wiesmann
;-
; $Id: 2024-01-30 14:25 CET $


FUNCTION ana2fitshdr_const, DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, CONST=CONST, WCS=WCS

  prits_tools.parcheck, DATETIME, 0, 'DATETIME', 'STRING', 0
  prits_tools.parcheck, EXTENSION_NAMES, 0, 'EXTENSION_NAMES', 'STRING', 1, VALID_NELEMENTS=6
  prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  prits_tools.parcheck, WCS, 0, 'WCS', 8, 0, /optional

  IF N_ELEMENTS(CONST) EQ 0 THEN return, ''
  min_const = min(CONST, max=max_const)
  IF min_const EQ 0 && max_const EQ 0 THEN return, ''

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

  hdr = ana2fitshdr_addwcs(HDR, WCS, /CONST)

  fits_util->add, hdr, ' ', ' '
  fits_util->add, hdr, 'BTYPE', 'BOOL', 'Type of data'
  fits_util->add, hdr, 'UCD', ' ', 'Unified Content Descriptors v1.23'
  fits_util->add, hdr, 'BUNIT', ' ', 'Units of the data'

  fits_util->clean_header, hdr
  return, hdr

END
