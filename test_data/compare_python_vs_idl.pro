obj = spice_data(file)
; Set various calibration parameters:
alpha = obj.get_header_keyword('radcal', window_index)
nbin_total = obj.get_header_keyword('nbin', window_index)
nbin_dispersion = obj.get_header_keyword('nbin3', window_index)
nbin_slit = obj.get_header_keyword('nbin2', window_index)
t = obj.get_header_keyword('xposure', window_index)

lam = obj.get_lambda_vector(window_index)
IF mean(lam) GT 900. THEN BEGIN
  noise_factor = 1.6 ; noise factor
  gain = 0.57 ; gain
  read_noise = 6.9 ; read noise
  i_dark = 0.54 ; dark current
  quantum_efficiency = 0.25
ENDIF ELSE BEGIN
  noise_factor = 1.0 ; noise factor
  gain = 3.58 ; gain
  read_noise = 6.9 ; read noise
  i_dark = 0.89 ; dark current
  IF mean(lam) LT 740. THEN quantum_efficiency = 0.12 ELSE quantum_efficiency = 0.1 ; not perfectly consistent with Python code
ENDELSE

data = obj.get_window_data(iwin, no_masking = no_masking, approximated_slit = approximated_slit)
missing_val = -100.
k = where(~finite(data) OR data LE 0., nk)
IF nk NE 0 THEN data[k] = missing_val
ind_good = where(data NE missing_val, n_good, complement = ind_miss, ncomplement = n_miss)
IF n_good GT 0 THEN err[ind_good] = sqrt(noise_factor ^ 2 * alpha * data[ind_good] * gain + nbin_total * sig_read ^ 2 + nbin_total * i_dark * t) / alpha
IF n_miss GT 0 THEN err[ind_miss] = missing_val

idl = sqrt( $
  noise_factor ^ 2 * alpha * data * gain + $
  read_noise ^ 2 * nbin_total + $
  i_dark * t * nbin_total) $
  / alpha

python = sqrt( $
  noise_factor ^ 2 * alpha * data * gain + $
  read_noise ^ 2 * nbin_total * 2 + $
  i_dark * t * nbin_total * 2) $
  / alpha


I looked at the IDL code and the Python code, and they are not exactly the same. 

By IDL code, I mean the code in the IDL function `SPICE_GETWINDATA`, in which Peter Young has implemented the
algorithm described in the article by Huang et al. (2023) (https://arxiv.org/pdf/2303.15979).

idl = sqrt( $
  noise_factor ^ 2 * alpha * data * gain + $
  read_noise ^ 2 * nbin_total + $
  i_dark * t * nbin_total) $
  / alpha

By Python code, I mean the code in the Python function `SPICE_ERROR` in the file `uncertainties.py` of the SOSPICE package.
This function was written by Eric Buchlin, as far as I can tell.

This is the Python code, translated to IDL syntax for comparison:
python = sqrt( $
  noise_factor ^ 2 * alpha * data * gain + $
  read_noise ^ 2 * nbin_total * 2 + $
  i_dark * t * nbin_total * 2) $
  / alpha

See at the end of the mail for an explanation of the variables.

The differences between the IDL and Python code are as follows:

1) The IDL code follows the algorithm given in the article by Huang et al. (2023) (https://arxiv.org/pdf/2303.15979)
The python code multiplies the read noise and dark current with `sqrt(2)`.
This results in slightly higher noise values in the Python code compared to the IDL code.
This difference is less significant for pixels with high signal values, but it can be noticeable for low signal values.

2) The python code has a 'Background' term that is not present in the IDL code. However, this term is set to zero in the Python code, 
so it does not affect the final noise values. Is the background noise eventually non-zero in the Python code?

3) The IDL codes treats negative values as missing, whereas the python code states:

Negative values of the signal are considered to be 0 for the purpose of
    computing the noise on the signal. However, the total uncertainty is
    then set to |signal_mean| + RSS (other noises), to ensure that the
    error bars are still compatible with expected fitted functions.
    We suggest users to replace large negative values of the signal
    (e.g. < -3 * RSS(other noises)) by NaNs.

    Selecting only above-zero pixels inevitably forces the fit to end up higher than it would otherwise be. 
    Sure, negative values are not *real*, but there is noise which becomes negative when subtracting dark etc. 
    The continuum is obviously not negative, though, but we can deal with that by having a minimum value during 
    the line fitting. However... to be a nitpick... if one is to handle errors absolutely correct: for some 
    spatial pixels, after dark subtraction, the continuum should be negative! So a picture of the continuum 
    (the constant coefficient in the line fit) where it has been forced to be negative won't have the "correct" 
    noise properties - and same as for the NaN pixels, the average background will be forced up.