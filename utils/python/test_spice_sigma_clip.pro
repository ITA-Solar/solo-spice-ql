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
  box_message, '2D test'
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
  catch, err
  if err eq 0 then begin
    print,'Running clip1...'
    t1 = systime(1)
    r1 = clip1(data, [3,3], sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
    t1 = systime(1) - t1
  end else begin
    print,'Error in clip1: ',!error_state.msg
  end
  catch, err
  if err eq 0 then begin
    print,'Running clip2...'
    t2 = systime(1)
    r2 = clip2(data, [3,3], sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
    t2 = systime(1) - t2
  end else begin
    print,'Error in clip2: ',!error_state.msg
  end
  catch,/cancel
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
  print,"Total NaNs in input data: " + trim(total(data ne data)),""
  print,"Total NaNs after clip1: " + trim(nan_count1),""
  print,"Total NaNs after clip2: " + trim(nan_count2),""
  print,"Max difference: " + trim(max(abs(diff))),""
  print,"Pixels with differences > 1e-6: " + trim(total(abs(diff) gt 1e-6)),""
  print,"Time for clip1: " + trim(t1),""
  print,"Time for clip2: " + trim(t2),""
  changes = data ne r1
  print, "Total changes made by clip1: " + trim(total(changes))
  print, "", "Speedup factor (clip1 / clip2): " + trim(t1 / t2), "", format='(a)'
  if total(abs(diff) gt 1e-6) gt 0 then begin
    print,'Differences found between clip1 and clip2!!!!'
  end
  if keyword_set(print_diff) then print,diff
  stop
end

pro test_spice_sigma_clip3, seed=seed, n=n, sigma=sigma, border=border, maxiters=maxiters, masked=masked, ret_center=ret_center, ret_stddev=ret_stddev, print_diff=print_diff
  compile_opt idl2
  box_message, '3D test'
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
  catch, err
  if err eq 0 then begin
    print,'Running clip1...'
    t1 = systime(1)
    r1 = clip1(data, 3, sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
    t1 = systime(1) - t1
  end else begin
    print,'Error in clip1: ',!error_state.msg
  end
  catch, err
  if err eq 0 then begin
    print,'Running clip2...'
    t2 = systime(1)
    r2 = clip2(data, 3, sigma=sigma, masked=masked,maxiters=maxiters, ret_center=ret_center, ret_stddev=ret_stddev)
    t2 = systime(1) - t2
  end else begin
    print,'Error in clip2: ',!error_state.msg
  end
  catch,/cancel
  data = float(data)
  rr1 = (r1 = float(r1))
  rr2 = (r2 = float(r2))
  nanix1 = where(r1 ne r1, nan_count1)
  nanix2 = where(r2 ne r2, nan_count2)
  r1[nanix1] = 0.0
  r2[nanix2] = 0.0
  diff = r1-r2
  ;if have_windows() then window,0,xsize=800,ysize=800
  ;if have_windows() then plot_image, diff[*,*,n/2]
  if nan_count1 ne nan_count2 then begin & print & print, '***Different number of NaNs: ', nan_count1, nan_count2 & print & end
  print,"Total NaNs in input data: " + trim(total(data ne data)),""
  print,"Total NaNs after clip1: " + trim(nan_count1),""
  print,"Total NaNs after clip2: " + trim(nan_count2),""
  print,"Max difference: " + trim(max(abs(diff))),""
  print,"Pixels with differences > 1e-6: " + trim(total(abs(diff) gt 1e-6)),""
  print,"Time for clip1: " + trim(t1),""
  print,"Time for clip2: " + trim(t2),""
  changes = data ne r1
  print, "Total changes made by clip1: " + trim(total(changes))

  print,"","Speedup factor (clip1 / clip2): " + trim(t1 / t2), "", format='(a)'
  if total(abs(diff) gt 1e-6) gt 0 then begin
    print,'Differences found between clip1 and clip2!!!!'
  end
  if keyword_set(print_diff) then print,diff
  stop
end

test = 2

if test eq 2 then test_spice_sigma_clip,$
  seed=124,$
  n=100,$
  sigma=3,$
  border=1,$
  maxiters=5,$
  masked=1,$
  ret_center=0,$
  ret_stddev=0,$
  print_diff=0

if test eq 3 then test_spice_sigma_clip3,$
  seed=123,$
  n=30,$
  sigma=3,$
  border=1,$
  maxiters=5,$
  masked=0,$
  ret_center=0,$
  ret_stddev=0,$
  print_diff=0

  end