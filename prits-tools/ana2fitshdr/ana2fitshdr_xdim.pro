;+
; NAME:
;      ANA2FITSHDR_XDIM
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the XDIM cube of an ANA object or file.
;      It will return an empty string if both XDIM and WCS are not provided or if the 
;      keyword SAVE_XDIM is NOT set.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_xdim(datetime=datetime, data_id=data_id, XDIM=XDIM, WCS=WCS)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;
; KEYWORDS:
;      SAVE_XDIM: If set, the XDIM will be saved, otherwise not. XDIM can usually be
;             calculated from the WCS parameters from the data array.
;
; OPTIONAL INPUTS:
;      XDIM: Array to keep the CONST status of each parameter at each point. If not provided, or if
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
; $Id: 2023-11-22 10:47 CET $


FUNCTION ana2fitshdr_xdim, DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, XDIM=XDIM, WCS=WCS, $
  SAVE_XDIM=SAVE_XDIM

  prits_tools.parcheck, DATETIME, 0, 'DATETIME', 'STRING', 0
  prits_tools.parcheck, EXTENSION_NAMES, 0, 'EXTENSION_NAMES', 'STRING', 1, VALID_NELEMENTS=6
  prits_tools.parcheck, XDIM, 0, 'XDIM', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  prits_tools.parcheck, WCS, 0, 'WCS', 8, 0, /optional

  IF ~keyword_set(SAVE_XDIM) || (N_ELEMENTS(XDIM) EQ 0 && N_ELEMENTS(WCS) EQ 0) THEN return, ''
  IF N_ELEMENTS(XDIM) EQ 0 THEN xdim_array = fltarr(wcs.naxis, /nozero) $
  ELSE xdim_array = xdim

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

  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'BTYPE', 'BOOL', 'Type of data'
  fits_util->add, hdr, 'BUNIT', ' ', 'Physical units of data'

  hdr = ana2fitshdr_addwcs(HDR, WCS, /XDIM1)

  fits_util->clean_header, hdr
  return, hdr

END
