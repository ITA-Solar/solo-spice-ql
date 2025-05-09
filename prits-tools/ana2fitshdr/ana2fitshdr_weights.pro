;+
; NAME:
;      ANA2FITSHDR_WEIGHTS
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the WEIGHTS cube of an ANA object.
;      It will return an empty string if all values in the WEIGHTS cube are identical or if
;      WEIGHTS is not provided.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_weights(datetime=datetime, data_id=data_id, WEIGHTS=WEIGHTS, WCS=WCS)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: String array with the names of the 6 other extensions of the same dataset/window.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      WEIGHTS: Weights to use in the fitting process for each point. If not provided, or if
;             all values are equal, an empty string will be returned.
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
;      oslo_fits_util, mkhdr, ptools.parcheck, ana2fitshdr_addwcs
;
; HISTORY:
;      Ver. 1, 2-Dec-2021, Martin Wiesmann
;-
; $Id: 2025-05-09 13:28 CEST $

FUNCTION ana2fitshdr_weights, datetime = datetime, extension_names = extension_names, weights = weights, wcs = wcs
  ptools.parcheck, datetime, 0, 'DATETIME', 'STRING', 0
  ptools.parcheck, extension_names, 0, 'EXTENSION_NAMES', 'STRING', 1, valid_nelements = 6
  ptools.parcheck, weights, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  ptools.parcheck, wcs, 0, 'WCS', 8, 0, /optional

  IF n_elements(weights) EQ 0 THEN return, ''
  min_weights = min(weights, max = max_weights)
  IF min_weights EQ max_weights THEN return, ''

  fits_util = obj_new('oslo_fits_util')
  mkhdr, hdr, weights, /image

  fits_util.add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util.add, hdr, '', ' '

  fits_util.add, hdr, 'EXTNAME', extension_names[2], 'Extension name'

  fits_util.add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util.add, hdr, 'DATAEXT', extension_names[1], 'Extension name of original data'
  fits_util.add, hdr, 'WGTEXT', extension_names[2], 'Extension name of weights'
  fits_util.add, hdr, 'INCLEXT', extension_names[3], 'Extension name of includes'
  fits_util.add, hdr, 'CONSTEXT', extension_names[4], 'Extension name of constants'
  fits_util.add, hdr, 'RESIDEXT', extension_names[5], 'Extension name of residuals'

  hdr = ana2fitshdr_addwcs(hdr, wcs, /weights)

  fits_util.add, hdr, ' ', ' '
  fits_util.add, hdr, 'BTYPE', 'WEIGHT', 'Type of data'
  fits_util.add, hdr, 'UCD', 'stat.weight', 'Unified Content Descriptors v1.23'
  fits_util.add, hdr, 'BUNIT', ' ', 'Units of the data'

  fits_util.clean_header, hdr
  return, hdr
END
