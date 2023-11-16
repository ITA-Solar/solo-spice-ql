;+
; NAME:
;      ANA2FITSHDR_WCSHDR
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
;      header_l2: The header (string array) of the SPICE level 2 file.
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
;      Ver. 1, 16-Nov-2023, Martin Wiesmann
;-
; $Id: 2023-11-16 12:14 CET $


FUNCTION ana2fitshdr_wcshdr, HEADERS_INPUT_DATA, IS_PROGENITOR_DATA=IS_PROGENITOR_DATA, $
  RESULT=RESULT, XDIM1=XDIM1, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, CONST=CONST

  return, ''
END
