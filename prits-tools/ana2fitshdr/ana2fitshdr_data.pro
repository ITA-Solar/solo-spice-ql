;+
; NAME:
;      ANA2FITSHDR_DATA
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the data array of an ANA object or file
;      and the optionally provided header. If either PROGENITOR_DATA is not provided or
;      provided and not a scalar number, then HEADER_INPUT_DATA first will be stripped of
;      the keywords that mkhdr populates, and then it will be added to the new header.
;      These keywords will be added/updated in the header in any case:
;      DATE, EXTNAME, RESEXT, DATAEXT, XDIMXT1, WGTEXT, INCLEXT, CONSTEXT
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_data(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INPUT_DATA=INPUT_DATA, $
;        HEADER_INPUT_DATA=HEADER_INPUT_DATA, PROGENITOR_DATA=PROGENITOR_DATA)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;
; KEYWORDS:
;      SAVE_DATA: If set, then the data cube is saved into the data extension.
;              Default is not to save it and use the external extension mechanism instead.
;
; OPTIONAL INPUTS:
;      HEADER_INPUT_DATA: The header (string array), that belongs to either INPUT_DATA or PROGENITOR_DATA,
;            respectively. If not provided a generic header will be created.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array.
;            This is the data array that was used in xcfit_block and comes with the ANA structure or file.
;            The absorbed dimension (e.g. spectrum) must be in the first dimension.
;            This input will be ignored if PROGENITOR_DATA is provided.
;      PROGENITOR_DATA: If this is provided, this data array will be saved into the data extension.
;            This is used to store the original progenitor data, instead of the possibly transformed
;            data array, that xcfit_block requires. I.e. the absorbed dimension (e.g. spectrum) does
;            not need to be in the first dimension.
;            FITS2ANA will then transform the data cube when read back into memory,
;            so that it can be used in xcfit_block.
;
;            PROGENITOR_DATA may also be a scalar number, in that case, it is assumed that HEADER_INPUT_DATA
;            contains a link to the data (e.g. url). And the program uses HEADER_INPUT_DATA as it is
;            and won't create a new header. If PROGENITOR_DATA is a scalar number and HEADER_INPUT_DATA
;            is not provided, the data extension is not saved at all.
;      If neither INPUT_DATA nor PROGENITOR_DATA is provided or PROGENITOR_DATA is a scalar number, then
;      HEADER_INPUT_DATA is assumed to include a link to PROGENITOR_DATA, if this is a scalar number,
;      or to INPUT_DATA if neither is provided. And only a scalar number will be saved as the data.
;      In this case the keywords NAXIS and NAXISn will be kept from HEADER_INPUT_DATA.
;      If none of the optional inputs are provided, the data extension will not be saved.
;
; OUTPUTS:
;      a fits header (string array), may be an empty string.
;
; OPTIONAL OUTPUTS:
;      DATA_ARRAY: Contains the data array that should be saved into the data extension, if any.
;
; CALLS:
;      prits_tools.parcheck, oslo_fits_util, mkhdr, fxpar
;
; HISTORY:
;      Ver. 1, 1-Dec-2021, Martin Wiesmann
;-
; $Id: 2025-04-24 12:04 CEST $

FUNCTION ana2fitshdr_data, datetime = datetime, extension_names = extension_names, input_data = input_data, $
  header_input_data = header_input_data, progenitor_data = progenitor_data, SAVE_DATA = SAVE_DATA, $
  data_array = data_array
  prits_tools.parcheck, datetime, 0, 'DATETIME', 'STRING', 0
  prits_tools.parcheck, extension_names, 0, 'EXTENSION_NAMES', 'STRING', 1, valid_nelements = 6
  prits_tools.parcheck, input_data, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional
  prits_tools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'NUMERIC', [0, 2, 3, 4, 5, 6, 7], /optional
  prits_tools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'STRING', 1, /optional

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
  fits_util.add, hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
  fits_util.add, hdr, 'XDIMXT1', extension_names[2], 'Extension name of 1st dim absorbed by analysis'
  fits_util.add, hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
  fits_util.add, hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
  fits_util.add, hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'

  fits_util.remove_keyword, hdr, 'PCOUNT'
  fits_util.remove_keyword, hdr, 'GCOUNT'

  IF keyword_set(header_input_data) THEN BEGIN
    hdr_addition = header_input_data

    fits_util.add, hdr, 'EXTNAME', fxpar(header_input_data, 'EXTNAME', missing = ''), 'Extension name'
    fits_util.add, hdr, 'XDIMNA', fxpar(header_input_data, 'NAXIS', missing = 0), 'Number of data axes in external extension'
    naxisn = fxpar(header_input_data, 'NAXIS*', missing = 0)
    FOR i = 0, n_elements(naxisn) - 1 DO fits_util.add, hdr, 'XDIMNA' + strtrim(i + 1, 2), naxisn[i]

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
