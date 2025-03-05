FUNCTION spice_lw_fit_fn, x, p, _EXTRA=extra

;+
; NAME:
;     SPICE_LW_FIT_FN
;
; PURPOSE:
;     Defines a function for fitting emission lines observed by the Solar
;     Orbiter SPICE instrument in the long wavelength (LW) channel. It is
;     composed of two Gaussians with the same centroid. One Gaussian is
;     narrow with a high amplitude; the other is wide with a low
;     amplitude. The ratios of the widths and the amplitudes are fixed.
;
; CATEGORY:
;     SPICE; fitting.
;
; CALLING SEQUENCE:
;     Result = SPICE_LW_FIT_FN( X, P )
;
; INPUTS:
;     X:   A 1D array giving the wavelength values for which the function
;          is required.
;     P:   A three element array giving the peak, centroid and Gaussian
;          width. These are treated as the values corresponding to the
;          narrow Gaussian.
;
; OUTPUTS:
;     A 1D array of same size as X giving the function values at the
;     wavelengths X. The function is defined in such a way that
;     2.50*p[0]*p[2] corresponds to the integrated intensity of the function.
;
; EXAMPLE:
;     IDL> p=[100.,1031.91,0.3]
;     IDL> wvl=findgen(101)/100.*3.+p[1]-1.5
;     IDL> f=spice_lw_fit_fn(wvl,p)
;
;     Compare with a regular Gaussian:
;     IDL> f2=gauss_sg(wvl,p)
;
; MODIFICATION HISTORY:
;     Ver.1, 05-Nov-2024, Peter Young
;     Ver.2, 05-Mar-2025, Peter Young
;       Changed name to spice_lw_fit_fn.
;-

;
; These parameters were derived from a two Gaussian fit to O VI 1032. The
; two Gaussians have the same amplitude.
;
amp_scale=0.07
wid_scale=2.27

z=( (x-(p[1]) )/abs(p[2]) )^2
z2=( (x-(p[1]) )/abs(p[2]*wid_scale) )^2
f=p[0] * exp(-0.5*temporary(z)) + $
  p[0] * amp_scale * exp(-0.5*temporary(z2))


;
; The scaling factor below means that the two Gaussian intensity will be
; exactly the same as the single Gaussian intensity derived from P.
;
return,f/(1.+amp_scale*wid_scale)

END


