;+
; NAME:
;      ANA2FITSHDR_XDIM
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the XDIM1 cube of an ANA object or file.
;      It will return an empty string if both XDIM1 and WCS are not provided or if the 
;      keyword SAVE_XDIM1 is NOT set.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_xdim(datetime=datetime, data_id=data_id, XDIM1=XDIM1, WCS=WCS, /SAVE_XDIM1)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;
; KEYWORDS:
;      SAVE_XDIM1: If set, the XDIM1 will be saved, otherwise not. XDIM1 can usually be
;             calculated from the WCS parameters from the data array.
;
; OPTIONAL INPUTS:
;      XDIM1: Array of same size as the input data to xcfit_block. It contains the values of the
;             absorbed dimension for each point (e.g wavelength).
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
;      Ver. 1.1, 9-Feb-2024, Terje Fredvik - fixed typo in TYPE_XDIM1 parcheck
;-
; $Id: 2024-02-16 13:09 CET $


FUNCTION ana2fitshdr_xdim, DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, XDIM1=XDIM1, WCS=WCS, $
  SAVE_XDIM1=SAVE_XDIM1, TYPE_XDIM1=TYPE_XDIM1

  prits_tools.parcheck, DATETIME, 0, 'DATETIME', 'STRING', 0
  prits_tools.parcheck, EXTENSION_NAMES, 0, 'EXTENSION_NAMES', 'STRING', 1, VALID_NELEMENTS=6
  prits_tools.parcheck, XDIM1, 0, 'XDIM', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  prits_tools.parcheck, WCS, 0, 'WCS', 8, 0, /optional
  prits_tools.parcheck, TYPE_XDIM1, 0, 'TYPE_XDIM1', 'STRING', 0, /optional

  IF ~keyword_set(SAVE_XDIM1) || (N_ELEMENTS(XDIM1) EQ 0 && N_ELEMENTS(WCS) EQ 0) THEN return, ''
  IF N_ELEMENTS(XDIM1) EQ 0 THEN xdim_array = fltarr(wcs.naxis, /nozero) $
  ELSE xdim_array = XDIM1

  fits_util = obj_new('oslo_fits_util')
  mkhdr, hdr, xdim_array, /image

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', extension_names[2], 'Extension name'

  fits_util->add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
  fits_util->add, hdr, 'XDIMXT1', extension_names[2], 'Extension name of 1st dim absorbed by analysis'
  fits_util->add, hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'

  hdr = ana2fitshdr_addwcs(HDR, WCS, /XDIM1)

  fits_util->add, hdr, ' ', ' '
  fits_util->add, hdr, 'BTYPE', keyword_set(TYPE_XDIM1) ? TYPE_XDIM1 : ' ', 'Type of data'
  fits_util->add, hdr, 'UCD', ' ', 'Unified Content Descriptors v1.23'
  fits_util->add, hdr, 'BUNIT', ' ', 'Units of the data'

  fits_util->clean_header, hdr
  return, hdr

END
