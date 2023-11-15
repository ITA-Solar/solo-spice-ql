;+
; NAME:
;      ANA2FITSHDR_DATA
;
; PURPOSE:
;      This function returns a fits header made from the data array of an ANA object or file
;      and the optionally provided header. If either PROGENITOR_DATA is not provided or
;      provided and not a scalar number, then HEADER_INPUT_DATA first will be stripped of 
;      the keywords that mkhdr populates, and then it will be added to the new header.
;      These keywords will be added/updated in the header:
;      DATE, EXTNAME, RESEXT, DATAEXT, XDIMXT1, WGTEXT, INCLEXT, CONSTEXT, PRGDATA
;
; CATEGORY:
;      FITS -- utility
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_data(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INPUT_DATA=INPUT_DATA, $
;        HEADER_INPUT_DATA=HEADER_INPUT_DATA, PROGENITOR_DATA=PROGENITOR_DATA)
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: A string array containing the names of the 6 possible extensions.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array.
;            This is the data array that was used in xcfit_block and comes with the ANA structure or file.
;            The absorbed dimension (e.g. spectrum) must be in the first dimension.
;            This input will be ignored if PROGENITOR_DATA is provided.
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;      HEADER_INPUT_DATA: The header (string array), that belongs to either INPUT_DATA or PROGENITOR_DATA,
;            respectively. If not provided a generic header will be created.
;            If either PROGENITOR_DATA is not provided or provided and not a scalar number, then 
;            HEADER_INPUT_DATA first will be stripped of the keywords that mkhdr populates, 
;            and then it will be added to the new header.
;      PROGENITOR_DATA: If this is provided, this data array will be saved into the data extension.
;            This is used to store the original progenitor data, instead of the possibly transformed
;            data array, that xcfit_block requires. I.e. the absorbed dimension (e.g. spectrum) does
;            not need to be in the first dimension. FITS2ANA will then transform the data cube
;            so that it can be used in xcfit_block.
;            PROGENITOR_DATA may also be a scalar number, in that case, it is assumed that HEADER_INPUT_DATA
;            contains a link to the data (e.g. url). And the program uses HEADER_INPUT_DATA as it is
;            and won't create a new header. If PROGENITOR_DATA is a scalar number and HEADER_INPUT_DATA
;            is not provided, the data extension is not saved at all.
;
; OUTPUTS:
;      a fits header (string array), may be an empty string.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, mkhdr
;
; HISTORY:
;      Ver. 1, 1-Dec-2021, Martin Wiesmann
;-
; $Id: 2023-11-15 15:30 CET $


FUNCTION ana2fitshdr_data, DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INPUT_DATA=INPUT_DATA, $
  HEADER_INPUT_DATA=HEADER_INPUT_DATA, PROGENITOR_DATA=PROGENITOR_DATA

  no_data = 0
  IF N_ELEMENTS(PROGENITOR_DATA) GT 0 THEN BEGIN
    IF N_ELEMENTS(PROGENITOR_DATA) EQ 1 THEN BEGIN
      IF ~keyword_set(HEADER_INPUT_DATA) THEN BEGIN
        return, ''
      ENDIF
      no_data = 1
    ENDIF
    data_array = PROGENITOR_DATA
    PRGDATA = 'T'
  ENDIF ELSE BEGIN
    data_array = INPUT_DATA
    PRGDATA = 'F'
  ENDELSE

  fits_util = obj_new('oslo_fits_util')
  
  IF no_data THEN BEGIN
    hdr = HEADER_INPUT_DATA
  ENDIF ELSE BEGIN
    mkhdr, hdr, data_array, /image
  ENDELSE

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', extension_names[1], 'Extension name'

  fits_util->add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
  fits_util->add, hdr, 'XDIMXT1', extension_names[2], 'Extension name of xdim1'
  fits_util->add, hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'

  fits_util->add, hdr, 'PRGDATA', PRGDATA, 'Indicator whether data is in its original form'

  IF keyword_set(HEADER_INPUT_DATA) && ~no_data THEN BEGIN

    hdr_addition = HEADER_INPUT_DATA

    fits-util->remove_keyword, hdr_addition, 'SIMPLE'
    fits-util->remove_keyword, hdr_addition, 'XTENSION'
    fits-util->remove_keyword, hdr_addition, 'BITPIX'
    fits-util->remove_keyword, hdr_addition, 'EXTEND'
    fits-util->remove_keyword, hdr_addition, 'DATE'
    fits-util->remove_keyword, hdr_addition, 'NAXIS'
    fits-util->remove_keyword, hdr_addition, 'NAXIS1'
    fits-util->remove_keyword, hdr_addition, 'NAXIS2'
    fits-util->remove_keyword, hdr_addition, 'NAXIS3'
    fits-util->remove_keyword, hdr_addition, 'NAXIS4'
    fits-util->remove_keyword, hdr_addition, 'NAXIS5'
    fits-util->remove_keyword, hdr_addition, 'NAXIS6'
    fits-util->remove_keyword, hdr_addition, 'NAXIS7'
    fits-util->remove_keyword, hdr_addition, 'NAXIS8'
    fits-util->remove_keyword, hdr_addition, 'NAXIS9'

    ind_end = where(strmatch(hdr, 'END *') eq 1, count_hdr)
    if count_hdr gt 0 then begin
      ind_end = ind_end[0]
      hdr_end = hdr[ind_end:*]
    endif else begin
      ind_end = N_ELEMENTS(hdr)
    endif
    hdr = [hdr[0:ind_end-1], hdr_addition]
    ind_end_addition = where(strmatch(hdr_addition, 'END *') eq 1, count_hdr_addition)
    if count_hdr_addition eq 0 && count_hdr gt 0 then begin
      hdr = [hdr, hdr_end]
    endif
    
  ENDIF ; keyword_set(HEADER_INPUT_DATA) && ~no_data

  fits_util->clean_header, hdr

  return, hdr
end
