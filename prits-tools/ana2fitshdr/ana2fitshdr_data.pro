;+
; NAME:
;      ANA2FITSHDR_DATA
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the data array of an ANA object
;      and the optionally provided header. If either PROGENITOR_DATA is not provided or
;      provided and not a scalar number, then HEADER_INPUT_DATA first will be stripped of
;      the keywords that mkhdr populates, and then it will be added to the new header.
;      These keywords will be added/updated in the header in any case:
;      DATE, EXTNAME, RESEXT, DATAEXT, WGTEXT, INCLEXT, CONSTEXT, RESIDEXT.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_data(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INPUT_DATA=INPUT_DATA, $
;        HEADER_INPUT_DATA=HEADER_INPUT_DATA, PROGENITOR_DATA=PROGENITOR_DATA)
;
; PARAMETERS:
;     Most parameters are described in ANA2FITS.
;     The only difference is that most of the parameters in ANA2FITS can be arrays, i.e. contain multiple
;     datasets/windows, whereas the parameters in this function are for one dataset/window only.
;     Parameters not described in ANA2FITS are described here.
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: String array with the names of the 6 other extensions of the same dataset/window.
;
; OUTPUTS:
;      a fits header (string array), may be an empty string.
;
; OPTIONAL OUTPUTS:
;      DATA_ARRAY: Contains the data array that will be saved into the data extension, if any.
;                  I.e. if SAVE_DATA is set, this will be INPUT_DATA or, if provided, PROGENITOR_DATA.
;                  This will be zero if neither of these is provided or if SAVE_DATA is not set.
;
; CALLS:
;      ptools.parcheck, oslo_fits_util, mkhdr, fxpar
;
; HISTORY:
;      Ver. 1, 1-Dec-2021, Martin Wiesmann
;-
; $Id: 2025-08-12 11:47 CEST $

FUNCTION ana2fitshdr_data, datetime = datetime, extension_names = extension_names, input_data = input_data, $
  header_input_data = header_input_data, progenitor_data = progenitor_data, SAVE_DATA = SAVE_DATA, $
  data_array = data_array
  ptools.parcheck, datetime, 0, 'DATETIME', 'STRING', 0
  ptools.parcheck, extension_names, 0, 'EXTENSION_NAMES', 'STRING', 1, valid_nelements = 6
  ptools.parcheck, input_data, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  ptools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'NUMERIC', [0, 2, 3, 4, 5, 6, 7], /optional
  ptools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'STRING', 1, /optional

  IF n_elements(progenitor_data) GT 0 THEN BEGIN
    data_array = progenitor_data
    IF n_elements(progenitor_data) EQ 1 THEN no_data = 1 ELSE no_data = 0
  ENDIF ELSE IF n_elements(input_data) GT 0 THEN BEGIN
    data_array = input_data
    no_data = 0
  ENDIF ELSE BEGIN
    data_array = 0
    no_data = 1
  ENDELSE
  IF ~keyword_set(SAVE_DATA) THEN BEGIN
    data_array = 0
    no_data = 1
  ENDIF
  IF no_data && n_elements(header_input_data) EQ 0 THEN return, ''

  fits_util = obj_new('oslo_fits_util')

  IF keyword_set(header_input_data) && no_data THEN BEGIN
    CASE fxpar(header_input_data, 'BITPIX', missing = 0) OF
      8: data_array = fix(data_array, type = 1)
      16: data_array = fix(data_array, type = 2)
      32: data_array = fix(data_array, type = 3)
      - 32: data_array = fix(data_array, type = 4)
      - 64: data_array = fix(data_array, type = 5)
      ELSE: data_array = fix(data_array, type = 1)
    ENDCASE
  ENDIF
  mkhdr, hdr, data_array, /image

  fits_util.add, hdr, 'DATE', fxpar(header_input_data, 'DATE', missing = ''), 'Date and time of parent FITS file creation'
  fits_util.add, hdr, '', ' '

  fits_util.add, hdr, 'EXTNAME', extension_names[1], 'Extension name'
  fits_util.add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util.add, hdr, 'DATAEXT', extension_names[1], 'Extension name of original data'
  fits_util.add, hdr, 'INCLEXT', extension_names[3], 'Extension name of includes'
  fits_util.add, hdr, 'CONSTEXT', extension_names[4], 'Extension name of constants'
  fits_util.add, hdr, 'RESIDEXT', extension_names[5], 'Extension name of residuals'

  fits_util.remove_keyword, hdr, 'PCOUNT'
  fits_util.remove_keyword, hdr, 'GCOUNT'

  IF keyword_set(header_input_data) THEN BEGIN
    hdr_addition = header_input_data

    fits_util.add, hdr, 'XNAXIS', fxpar(header_input_data, 'NAXIS', missing = 0), 'Number of data axes in external extension'
    naxisn = fxpar(header_input_data, 'NAXIS*', missing = 0)
    FOR i = 0, n_elements(naxisn) - 1 DO fits_util.add, hdr, 'XNAXIS' + strtrim(i + 1, 2), naxisn[i]

    fits_util.remove_keyword, hdr_addition, 'SIMPLE'
    fits_util.remove_keyword, hdr_addition, 'XTENSION'
    fits_util.remove_keyword, hdr_addition, 'BITPIX'
    fits_util.remove_keyword, hdr_addition, 'EXTEND'
    fits_util.remove_keyword, hdr_addition, 'DATE'
    fits_util.remove_keyword, hdr_addition, 'NAXIS'
    fits_util.remove_keyword, hdr_addition, 'NAXIS1'
    fits_util.remove_keyword, hdr_addition, 'NAXIS2'
    fits_util.remove_keyword, hdr_addition, 'NAXIS3'
    fits_util.remove_keyword, hdr_addition, 'NAXIS4'
    fits_util.remove_keyword, hdr_addition, 'NAXIS5'
    fits_util.remove_keyword, hdr_addition, 'NAXIS6'
    fits_util.remove_keyword, hdr_addition, 'NAXIS7'
    fits_util.remove_keyword, hdr_addition, 'NAXIS8'
    fits_util.remove_keyword, hdr_addition, 'NAXIS9'
    fits_util.remove_keyword, hdr_addition, 'EXTNAME'

    ind_end = where(strmatch(hdr, 'END *') EQ 1, count_hdr)
    IF count_hdr GT 0 THEN BEGIN
      ind_end = ind_end[0]
      hdr_end = hdr[ind_end : *]
    ENDIF ELSE BEGIN
      ind_end = n_elements(hdr)
    ENDELSE
    hdr = [hdr[0 : ind_end - 1], hdr_addition]
    !NULL = where(strmatch(hdr_addition, 'END *') EQ 1, count_hdr_addition)
    IF count_hdr_addition EQ 0 && count_hdr GT 0 THEN BEGIN
      hdr = [hdr, hdr_end]
    ENDIF
  ENDIF ; keyword_set(HEADER_INPUT_DATA)

  fits_util.clean_header, hdr

  return, hdr
END
