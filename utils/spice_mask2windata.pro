FUNCTION spice_mask2windata, wd, spec
  ;+
  ; NAME:
  ;     SPICE_MASK2WINDATA
  ;
  ; PURPOSE:
  ;     Inserts a mask spectrum into a SPICE windata structure, which is
  ;     a necessary step for using an auto_fit template for the mask
  ;     spectrum (see spice_mask_auto_fit.pro).
  ;
  ; CATEGORY:
  ;     SPICE; windata; mask.
  ;
  ; CALLING SEQUENCE:
  ;     Result = SPICE_MASK2WINDATA( Wd, Spec )
  ;
  ; INPUTS:
  ;     Wd:    A windata structure in the format produced by
  ;            eis_getwindata.pro. Either SWSPEC, or LWSPEC must also be
  ;            specified.
  ;     Spec:    A mask spectrum as produced by spice_mask_spectrum.pro.
  ;              It must be from the same file as WD.
  ;
  ; OUTPUTS:
  ;     A windata structure identical to WD, except for the following
  ;     changes:
  ;     * the int tag has been filled with the part of the mask spectrum
  ;       that corresponds to the original WD structure. The inserted
  ;       spectrum is identical for each spatial pixel.
  ;     * the err tag has also been replaced using the error array from
  ;       the mask spectrum.
  ;     * the three wave_corr tag arrays have been set to zero.
  ;     * the time_stamp tag is updated to the current time.
  ;
  ;     If a problem is found, then WD is returned.
  ;
  ; MODIFICATION HISTORY:
  ;     Ver.1, 08-Nov-2024, Peter Young
  ;       Adapted from eis_mask2windata.pro.
  ;-

  IF n_params() LT 1 THEN BEGIN
    print, 'Use:  IDL> wdout=spice_mask2windata(wd, spec )'
    return, wd
  ENDIF

  wdout = wd

  ;
  ; The original code (commented out) did not work for windows joined
  ; with eis_join_windata.
  ;
  nw = n_elements(wd.wvl)
  wpix = intarr(nw)
  FOR i = 0, nw - 1 DO BEGIN
    getmin = min(abs(spec.wvl - wd.wvl[i]), imin)
    IF getmin GE 0.02 THEN message, /info, /cont, 'Warning - wavelength arrays seem to be mis-matched.'
    wpix[i] = imin
  ENDFOR
  int = spec.int[wpix]
  err = spec.err[wpix]

  nx = wd.nx
  ny = wd.ny

  FOR i = 0, nx - 1 DO BEGIN
    FOR j = 0, ny - 1 DO BEGIN
      wdout.int[*, i, j] = int
      wdout.err[*, i, j] = err
    ENDFOR
  ENDFOR

  wdout.wave_corr = 0.
  wdout.wave_corr_tilt = 0.
  wdout.wave_corr_t = 0.

  wdout.time_stamp = systime()

  return, wdout
END