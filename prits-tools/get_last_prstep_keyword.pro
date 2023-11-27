;+
; NAME:
;     get_last_prstep_keyword
;
; PURPOSE:
;     get_last_prstep_keyword searches a FITS keyword header for all processing step keywords defined in the
;     Solarnet_Metadata_Recommendations document.
;
; CATEGORY:
;     FITS -- utility
;
; CALLING SEQUENCE:
;     max_version_number = get_last_prstep_keyword(header [, count=count] [, pr_keywords=pr_keywords] [, $
;       ind_pr_keywords=ind_pr_keywords] [, pr_versions=pr_versions] [, pr_types=pr_types] )
;
; INPUTS:
;     header : A FITS keyword header as a string array
;
; OPTIONAL INPUTS:
;
; OUTPUT:
;     An integer, this is the highest processing step keyword found in the header.
;
; OPTIONAL OUTPUT:
;     count: An integer, the number of processing step keywords found.
;     pr_keywords: A string array, all processing step keywords found.
;     ind_pr_keywords: An integer array, indices of found processing step keywords in the header.
;     pr_versions: An integer array, version numbers of the found processing step keywords.
;               Note: Some keywords have letters after the version number.
;     pr_types: A string array, indicating the types of processing step keyword.
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; HISTORY:
;     22-Jun-2023: Martin Wiesmann
;-
; $Id: 2023-11-27 13:42 CET $


FUNCTION get_last_prstep_keyword, header, count=count, pr_keywords=pr_keywords, ind_pr_keywords=ind_pr_keywords, $
  pr_versions=pr_versions, pr_types=pr_types
  compile_opt idl2

  prits_tools.parcheck, header, 1, "header", 'string', 1

  pr_keywords = header.extract('^PR(STEP|PROC|PVER|MODE|PARA|REF|LOG|ENV|VER|HSH|BRA|LIB)[1-9][0-9]{0,1}[^ =]?')
  ind_pr_keywords = where(pr_keywords NE '', count)
  if count eq 0 then begin
    pr_keywords = ''
    pr_versions = 0
    pr_version_max = 0
    pr_types = ''
  endif else begin
    ind_pr_keywords = ind_pr_keywords[sort(ind_pr_keywords)]
    pr_keywords = pr_keywords[ind_pr_keywords]
    pr_versions = fix(pr_keywords.extract('[1-9][0-9]{0,1}'))
    pr_version_max = max(pr_versions)
    pr_types = pr_keywords.extract('(STEP|PROC|PVER|MODE|PARA|REF|LOG|ENV|VER|HSH|BRA|LIB)')
  endelse

  return, pr_version_max
END
