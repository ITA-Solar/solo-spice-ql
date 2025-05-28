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
The python code multiplies the read noise and dark current with `sqrt(2)`.
This results in slightly higher noise values in the Python code compared to the IDL code.
This difference is less significant for pixels with high signal values, but it can be noticeable for low signal values.

The IDL code follows the algorithm given in the article by Huang et al. (2023) (https://arxiv.org/pdf/2303.15979)