pro test_spice_errors_python

file = 'solo_L2_spice-n-ras_20230405T165232_V02_184549674-000'
;file = 'solo_L2_spice-n-exp_20230216T032801_V22_167772583-007'
;file = 'solo_L2_spice-n-sit_20250327T044323_V03_318767233-000'
file = spice_find_file(file)
file = file[0]

help,file
print,file

d=readfits(file,h)

ptools.add_python_paths ;; Skal automatisk legge solo-spice-ql/utils/python i $PYTHONPATH

clip = python.import('spice_sigma_clip')
a=clip.sigma_clip(dist(50),3)

help,a

sospice = python.import('sospice')
help,sospice
help,sospice.calibrate
help,sospice.calibrate.uncertainties
help,sospice.calibrate.uncertainties.spice_error

fits = python.import('astropy.io.fits')
help,fits
;fits.__doc__

;pyfile = fits.util.get_testdata_filepath(file)
;help,pyfile
hdul = fits.open(file)
help,hdul
help,hdul[0]

spice_uncertainties = python.import('sospice.calibrate.uncertainties')
help,spice_uncertainties
result = spice_uncertainties.spice_error(hdul[0])
help,result
print,result[0]
print,result[1]


dpy=(result[1])['Total']



;help, WEIGHTS
;maxweights = max(WEIGHTS, min = minweights)
;errors = 1.0 / sqrt(WEIGHTS)
;ind = where(~finite(dpy), nbad)
;IF nbad GT 0 THEN dpy[ind] = 0.0001

maxerrors = max(dpy, min = minerrors)
print, 'ERRORS      min: ', minerrors, ' max: ', maxerrors
;print, 'DATA        min: ', min(DATA), ' max: ', max(DATA)
;print, 'WEIGHTS     min: ', minweights, ' max: ', maxweights
;rel_err = dpy / DATA * 100
;maxrel_err = max(rel_err, min = minrel_err)
;print, 'RELATIVE ERRORS min: ', minrel_err, ' max: ', maxrel_err

;ind = where(WEIGHTS EQ WEIGHTS, countw)
;ind = where(DATA EQ DATA, countd)
;print, 'WEIGHTS == WEIGHTS: ', countw
;print, 'DATA == DATA: ', countd
;print, 'DIFFERENCE: ', countw - countd




print,''
print,'  ----------- '
print,''


test_spice_errors_idl, file


print,''
print,'  ----------- '
print,''


residl = spice_getwindata(file,0)
help,residl


dpy=(result[1])['Total']
maxdpy=max(dpy,min=mindpy)

didl=residl.err

print,''
help,d
help,didl
help,dpy

if file.contains('spice-n-sit') then begin
  ;for cfit
  didl=transpose(didl,[0,2,1])
  sdidl=size(didl)
  didl=reform(didl,sdidl[1],1,sdidl[2],sdidl[3])

  ;didl=transpose(didl,[2,0,1])
  ;sdidl=size(didl)
  ;didl=reform(didl,1,sdidl[1],sdidl[2],sdidl[3])
endif else begin
  didl=transpose(didl,[1,2,0])
endelse

help,didl

indbad = where(didl LT -99.9, count)
if count gt 0 then didl[indbad]=!values.f_nan
didl=didl/100.
maxdidl=max(didl,min=mindidl)

diffd = (dpy-didl)/dpy*100.0
maxdiff = max(diffd, min=mindiff)

maxdata = max(d,min=mindata)

ind = where(didl EQ didl,count_idl)
ind = where(dpy eq dpy,count_py)

print,''
print,'Valid IDL : ', count_idl
print,'Valid PY  : ', count_py

print,''
print,'Python: Min: ', mindpy, '  Max: ',maxdpy
print,'IDL   : Min: ', mindidl, '  Max: ',maxdidl
print,'Diff %: Min: ', mindiff, '  Max: ',maxdiff
print,''
print,'Data  : Min: ', mindata, '  Max: ',maxdata
print,''


ind = where(d LT 1, count)

print,count
diff0_idl = mean(didl[ind],/nan)
diff0_py = mean(dpy[ind],/nan)
print,'Diff_0_IDL      : ',diff0_idl
print,'Diff_0_PY       : ',diff0_py
print,'Diff_0_IDL * |2 : ', diff0_idl*sqrt(2)
print,'PY / IDL        : ', diff0_py / diff0_idl

print,''
box_message,['yerr   = 1. / sqrt(weight)',$
'weight = 1. / yerr^2',$
'yerr = 0  <->  weight = 0']
print,''


;stop
end
