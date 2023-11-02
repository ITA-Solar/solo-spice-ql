;+
; NAME:
;     FITS2ANA_GET_DATA_ID
;
; PURPOSE:
;     fits2ana_get_data_id extracts the data_id from the structure returned by fits_open.
;     This is a helper function for FITS2ANA. It assumes that EXTNAME is constructed with
;     data_id + extension_type. It searches for the extension_type 'results' and returns a
;     list of data_ids.
;
; CATEGORY:
;     FITS -- utility
;
; CALLING SEQUENCE:
;     data_ids = fits2ana(fits_content)
;
; INPUTS:
;     fits_content : The structure returned by fits_open, with the content of a FITS file.
;
; OUTPUT:
;     String array. The data_ids of the different windows.
;
; CALLS:
;     prits_tools.parcheck
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; HISTORY:
;     2-Nov-2023: Martin Wiesmann
;-
; $Id: 2023-11-02 14:19 CET $


FUNCTION fits2ana_get_data_id, fits_content

  prits_tools.parcheck, fits_content, 1, "fits_content", 8, 0, /optional
  IF N_ELEMENTS(fits_content) EQ 0 THEN return, ['']

  ind_results = where(fits_content.extname.Contains('results'), count)
  IF count EQ 0 THEN return, ['']
  
  data_ids = strsplit(fits_content.extname[ind_results], ' results', /extract, /regex)
  data_ids = data_ids.ToArray(/no_copy)
  
  return, data_ids
END
