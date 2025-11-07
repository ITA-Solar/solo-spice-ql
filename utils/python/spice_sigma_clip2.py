import numpy as np
from scipy.ndimage import generic_filter
from numpy import ma
from crtools import fmedian, fsigma

"""_summary_
IDL:

.r
pro zero_where_both_are_nan, r1, r2
  ; Set to zero the elements where both r1 and r2 are NaNs
  nanix = where((r1 ne r1) and (r2 ne r2), count)
  if count gt 0 then begin
    arr1[nanix] = 0.0
    arr2[nanix] = 0.0
  endif
end

  clip1 = python.import('spice_sigma_clip') & clip1 = clip1.sigma_clip
  clip2 = python.import('spice_sigma_clip2') & clip2 = clip2.sigma_clip

  seed = 124
  n = 100
  sigma = 3
  border = 1
  maxiters = 5
  masked = 0
  ret_center = 0
  ret_stddev = 0
  
  data = double(randomn(seed, n, n))
  if border gt 0 then data[0:border-1,*] = 0.0
  if border gt 0 then data[*,0:border-1] = 0.0
  if border gt 0 then data[n-border:n-1,*] = 0.0
  if border gt 0 then data[*,n-border:n-1] = 0.0
  t1 = systime(1)
  r1 = clip1(data, 3, sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
  t1 = systime(1) - t1
  t2 = systime(1)
  r2 = clip2(data, 3, sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
  t2 = systime(1) - t2
  data = float(data)
  rr1 = (r1 = float(r1))
  rr2 = (r2 = float(r2))
  nanix1 = where(r1 ne r1, nan_count1)
  nanix2 = where(r2 ne r2, nan_count2)
  r1[nanix1] = 0.0
  r2[nanix2] = 0.0
  print,max(abs(r1-r2))
  diff = r1-r2
  window,0,xsize=800,ysize=800
  plot_image, diff
  if nan_count1 ne nan_count2 then begin & print & print, '***Different number of NaNs: ', nan_count1, nan_count2 & print & end
  print,"","Total NaNs in input data: " + trim(total(data ne data)),"",$
        "","Total NaNs after clip1: " + trim(nan_count1),"",$
        "","Total NaNs after clip2: " + trim(nan_count2),"",$
        "","Max difference: " + trim(max(abs(diff))),"",$
        "","Pixels with differences > 1e-6: " + trim(total(abs(diff) gt 1e-6)),"",$
        "","Speedup factor (clip1 / clip2): " + trim(t1 / t2), "", format='(a)'
 print,diff
;  print,rr1
  
;  window,1
;  plot_image, r1 - data
;  window,2
;  plot_image, r2 - data
  
"""

def sigma_clip(
    data,
    size,
    sigma=3,
    sigma_lower=None,
    sigma_upper=None,
    maxiters=5,
    centerfunc="median",
    masked=True,
    ret_center=False,
    ret_stddev=False,
):
    """
     Performs sigma-clipping of the input array.

     Parameters
     ----------
    data: numpy.ndarray
        Input array
    size: int or tuple[int]
        Size of the kernel used to compute the running median (or mean) and standard deviation
    sigma: float
        The number of standard deviations to use for both the lower and upper clipping limit.
        This is overriden by `sigma_lower` and `sigma_upper`
    sigma_lower: float
        Low threshold, in units of the standard deviation of the local intensity distribution
    sigmer_upper: float
        High threshold, in units of the standard deviation of the local intensity distribution
    maxiters: int
        Maximum number of iterations to perform
    centerfunc: str
        Method used to estimate the center of the local intensity distribution ("median" (default) or "mean")
    masked: bool
        Return a `numpy.ma.MaskedArray` (default) instead of an `numpy.array`

    Returns
    -------
    numpy.ndarray
        Filtered array, with clipped pixels replaced by the estimated value of the center of the
        local intensity distribution (either median or mean).
    """
    output = np.copy(data)
    if type(size) is int:
        size = (size,) * data.ndim
    sigma_lower = sigma_lower or sigma
    sigma_upper = sigma_upper or sigma
    maxiters = maxiters or np.inf
    nchanged = 1
    iteration = 0
    while nchanged != 0 and (iteration < maxiters):
        iteration += 1
        center = fmedian(output, size, size, 0)
        if ret_center and iteration == maxiters:
            return center
        stddev = fsigma(output, size, size, 0)
        if ret_stddev and iteration == maxiters:
            return stddev
        diff = output - center
        new_mask = (diff > sigma_upper * stddev) | (diff < -sigma_lower * stddev)
        output[new_mask] = np.nan
        nchanged = np.count_nonzero(new_mask)
    nan = np.isnan(output)
    output[nan] = center[nan] # Last value for center used for filling
    if masked:
        return ma.masked_array(output, mask=nan)
    else:
        return output

    
