pro zero_where_both_are_nan, r1, r2
  ; Set to zero the elements where both r1 and r2 are NaNs
  nanix = where((r1 ne r1) and (r2 ne r2), count)
  if count gt 0 then begin
    arr1[nanix] = 0.0
    arr2[nanix] = 0.0
  endif
end

pro test_spice_sigma_clip,seed=seed, n=n, sigma=sigma, border=border, maxiters=maxiters, masked=masked, ret_center=ret_center, ret_stddev=ret_stddev, print_diff=print_diff
  compile_opt idl2
  clip1 = python.import('spice_sigma_clip')
  clip1 = clip1.sigma_clip
  clip2 = python.import('spice_sigma_clip2')
  clip2 = clip2.sigma_clip

  ptools.default,seed,124
  ptools.default,n,100
  ptools.default,sigma,3
  ptools.default,border,1
  ptools.default,maxiters,5
  ptools.default,masked,0
  ptools.default,ret_center,0
  ptools.default,ret_stddev,0
  ptools.default,print_diff,0
  
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
  if have_windows() then window,0,xsize=800,ysize=800
  if have_windows() then plot_image, diff
  if nan_count1 ne nan_count2 then begin & print & print, '***Different number of NaNs: ', nan_count1, nan_count2 & print & end
  print,"","Total NaNs in input data: " + trim(total(data ne data)),"",$
        "","Total NaNs after clip1: " + trim(nan_count1),"",$
        "","Total NaNs after clip2: " + trim(nan_count2),"",$
        "","Max difference: " + trim(max(abs(diff))),"",$
        "","Pixels with differences > 1e-6: " + trim(total(abs(diff) gt 1e-6)),"",$
        "","Time for clip1: " + trim(t1),"",$
        "","Time for clip2: " + trim(t2),"",$
        "","Speedup factor (clip1 / clip2): " + trim(t1 / t2), "", format='(a)'
  if keyword_set(print_diff) then print,diff
end

pro test_spice_sigma_clip3, seed=seed, n=n, sigma=sigma, border=border, maxiters=maxiters, masked=masked, ret_center=ret_center, ret_stddev=ret_stddev, print_diff=print_diff
  compile_opt idl2
  clip1 = python.import('spice_sigma_clip')
  clip1 = clip1.sigma_clip
  clip2 = python.import('spice_sigma_clip2')
  clip2 = clip2.sigma_clip
  ptools.default,seed,124
  ptools.default,n,250
  ptools.default,sigma,3
  ptools.default,border,1
  ptools.default,maxiters,5
  ptools.default,masked,1
  ptools.default,ret_center,0
  ptools.default,ret_stddev,0
  ptools.default,print_diff,0
  data = double(randomn(seed, n, n, n))
  if border gt 0 then data[0:border-1,*,*] = 0.0
  if border gt 0 then data[*,0:border-1,*] = 0.0
  if border gt 0 then data[*,*,0:border-1] = 0.0
  if border gt 0 then data[n-border:n-1,*,*] = 0.0
  if border gt 0 then data[*,n-border:n-1,*] = 0.0
  if border gt 0 then data[*,*,n-border:n-1] = 0.0
  t1 = systime(1)
  r1 = clip1(data, 3, sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
  t1 = systime(1) - t1
  t2 = systime(1)
  r2 = clip2(data, [3,3,3], sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
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
  if have_windows() then window,0,xsize=800,ysize=800
  if have_windows() then plot_image, diff[*,*,n/2]
  if nan_count1 ne nan_count2 then begin & print & print, '***Different number of NaNs: ', nan_count1, nan_count2 & print & end
  print,"","Total NaNs in input data: " + trim(total(data ne data)),"",$
        "","Total NaNs after clip1: " + trim(nan_count1),"",$
        "","Total NaNs after clip2: " + trim(nan_count2),"",$
        "","Max difference: " + trim(max(abs(diff))),"",$
        "","Pixels with differences > 1e-6: " + trim(total(abs(diff) gt 1e-6)),"",$
        "","Time for clip1: " + trim(t1),"",$
        "","Time for clip2: " + trim(t2),"",$
        "","Speedup factor (clip1 / clip2): " + trim(t1 / t2), "", format='(a)'
  if keyword_set(print_diff) then print,diff
  stop
end

test_spice_sigma_clip3,$
  seed=124,$
  n=5,$
  sigma=3,$
  border=1,$
  maxiters=5,$
  masked=0,$
  ret_center=0,$
  ret_stddev=0,$
  print_diff=0

stop

test_spice_sigma_clip,$
  seed=124,$
  n=250,$
  sigma=3,$
  border=1,$
  maxiters=5,$
  masked=1,$
  ret_center=0,$
  ret_stddev=0,$
  print_diff=0

  end