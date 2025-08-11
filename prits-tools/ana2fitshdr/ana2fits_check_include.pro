;+
; NAME:
;      ANA2FITS_CHECK_INCLUDE
;
; PURPOSE:
;      This is a subfunction of ANA2FITS.
;      This function checks the INCLUDE parameters of each fit component and
;      the INCLUDE cube. If a fit component is not included, then the corresponding
;      parameter in the result cube is set to NAN.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS
;
; CALLING SEQUENCE:
;      see function definition
;
; PARAMETERS:
;     All parameters are described in ANA2FITS.
;     The only difference is that most of the parameters in ANA2FITS can be arrays, i.e. contain multiple
;     datasets/windows, whereas the parameters in this function are for one dataset/window only.
;
; OUTPUTS:
;     result: 4D array with the edited results of the fit.
;     The procedure will set values in the result cube to NAN if the corresponding fit component is not included.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;     ptools.parcheck
;
; HISTORY:
;      Ver. 1, 11-Aug-2025, Martin Wiesmann
;-
; $Id: 2025-08-11 14:58 CEST $

FUNCTION ANA2FITS_CHECK_INCLUDE, fit = fit, result = result, include = include
  ptools.parcheck, result, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7]
  ptools.parcheck, fit, 0, 'FIT', 'STRUCT', 0
  ptools.parcheck, include, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], result = error
  IF error[0] NE '' THEN return, result

  n_components = n_tags(fit)
  parnr = 0
  FOR itag = 0, n_components - 1 DO BEGIN
    fit_cur = fit.(itag)
    n_params = n_elements(fit_cur.param)
    IF ~fit_cur.INCLUDE THEN BEGIN
      result[parnr : parnr + n_params - 1, *, *, *] = !VALUES.f_nan
    ENDIF ELSE BEGIN
      ind = where(include[itag, *, *, *] EQ 0, n_ind)
      IF n_ind GT 0 THEN BEGIN
        FOR i = 0, n_params - 1 DO BEGIN
          res_temp = result[parnr + i, *, *, *]
          res_temp[ind] = !VALUES.f_nan
          result[parnr + i, *, *, *] = res_temp
        ENDFOR
      ENDIF
    ENDELSE
    parnr += n_params
  ENDFOR ; itag=0,n_components-1

  return, result
END
