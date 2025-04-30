;+
; NAME:
;      ANA2FITSHDR_INCLUDE
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the INCLUDE cube of an ANA object or file.
;      It will return an empty string if all values in the INCLUDE cube are one or if
;      INCLUDE is not provided.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_include(datetime=datetime, data_id=data_id, INCLUDE=INCLUDE, WCS=WCS)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      INCLUDE: Array to keep the INCLUDE status of each component at each point. If not provided, or if
;             all values are one, an empty string will be returned.
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
; $Id: 2025-04-30 11:49 CEST $

FUNCTION ana2fitshdr_include, datetime = datetime, extension_names = extension_names, include = include, wcs = wcs
  prits_tools.parcheck, datetime, 0, 'DATETIME', 'STRING', 0
  prits_tools.parcheck, extension_names, 0, 'EXTENSION_NAMES', 'STRING', 1, valid_nelements = 6
  prits_tools.parcheck, include, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  prits_tools.parcheck, wcs, 0, 'WCS', 8, 0, /optional

  IF n_elements(include) EQ 0 THEN return, ''
  min_include = min(include, max = max_include)
  IF min_include EQ 1 && max_include EQ 1 THEN return, ''

  fits_util = obj_new('oslo_fits_util')
  mkhdr, hdr, include, /image

  fits_util.add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util.add, hdr, '', ' '

  fits_util.add, hdr, 'EXTNAME', extension_names[3], 'Extension name'

  fits_util.add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util.add, hdr, 'DATAEXT', extension_names[1], 'Extension name of original data'
  fits_util.add, hdr, 'WGTEXT', extension_names[2], 'Extension name of weights'
  fits_util.add, hdr, 'INCLEXT', extension_names[3], 'Extension name of includes'
  fits_util.add, hdr, 'CONSTEXT', extension_names[4], 'Extension name of constants'
  fits_util.add, hdr, 'RESIDEXT', extension_names[5], 'Extension name of residuals'

  fits_util.add, hdr, '', ' '
  fits_util.add, hdr, 'BTYPE', 'BOOL', 'Type of data'
  fits_util.add, hdr, 'BUNIT', ' ', 'Physical units of data'

  hdr = ana2fitshdr_addwcs(hdr, wcs, /include)

  fits_util.add, hdr, ' ', ' '
  fits_util.add, hdr, 'BTYPE', 'BOOL', 'Type of data'
  fits_util.add, hdr, 'UCD', ' ', 'Unified Content Descriptors v1.23'
  fits_util.add, hdr, 'BUNIT', ' ', 'Units of the data'

  fits_util.clean_header, hdr
  return, hdr
END
