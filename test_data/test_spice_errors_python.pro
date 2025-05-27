pro test_spice_errors_python

n=1
window_index=0

case n of
  1: begin
    file = 'solo_L2_spice-n-ras_20230405T165232_V02_184549674-000'
    xrange = [0,2.5]
    yrange1 = [0,100]
    yrange2 = [1,1.4]
    end
    
  2: begin
    file = 'solo_L2_spice-n-exp_20230216T032801_V22_167772583-007'
    xrange = [0,8]
    yrange1 = [0,100]
    yrange2 = [1,1.4]
    end
      
    3: begin
      file = 'solo_L2_spice-n-sit_20250327T044323_V03_318767233-000'
      xrange = [0,5]
      yrange1 = [0,300]
      yrange2 = [1,1.4]
    end

endcase
file = spice_find_file(file)
file = file[0]

help,file
print,file





;;;;;;;;;;;;;;;;;;;;;;

obj=spice_data(file)


;a=obj.get_header_keyword('radcal',window_index,variable_values = variable_values)
;help,a
;help,variable_values
;stop



;;;;;;;;;;;;;;;;







;ptools.add_python_paths ;; Skal automatisk legge solo-spice-ql/utils/python i $PYTHONPATH

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
hdul = fits.open(file,lazy_load_hdus=0)
help,hdul
;print,python.dir(hdul)
;a=hdul.info()
help,hdul[window_index]
print,''
print,python.dir(hdul)
print,''
print,python.dir(hdul[window_index])
print,''
print,hdul[window_index].name

;return
spice_uncertainties = python.import('sospice.calibrate.uncertainties')
help,spice_uncertainties
result = spice_uncertainties.spice_error(hdul[window_index])
help,result
print,result[0]
print,result[1]


dpy=(result[1])['Total']

;stop

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


;test_spice_errors_idl, file

dpy_new = test_spice_error_python(file,window_index)
help,dpy_new
help,dpy

diff_new = dpy-dpy_new
print,min(diff_new)
print,max(diff_new)

;stop
dpy=dpy_new
print,''
print,'  ----------- '
print,''


data=readfits(file,h,ext=window_index)


residl = spice_getwindata(file,window_index)
help,residl


;dpy=(result[1])['Total']
maxdpy=max(dpy,min=mindpy)

didl=residl.err

print,''
help,data
help,didl
help,dpy
stop
if file.contains('spice-n-sit') then begin
  ;for cfit
  ;didl=transpose(didl,[0,2,1])
  ;sdidl=size(didl)
  ;didl=reform(didl,sdidl[1],1,sdidl[2],sdidl[3])

  didl=transpose(didl,[2,0,1])
  sdidl=size(didl)
  didl=reform(didl,1,sdidl[1],sdidl[2],sdidl[3])
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

maxdata = max(data,min=mindata)

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


ind0 = where(data LT 1, count0)

print,count0
diff0_idl = mean(didl[ind0],/nan)
diff0_py = mean(dpy[ind0],/nan)
print,'Diff_0_IDL      : ',diff0_idl
print,'Diff_0_PY       : ',diff0_py
print,'Diff_0_IDL * |2 : ', diff0_idl*sqrt(2)
print,'PY / IDL        : ', diff0_py / diff0_idl

print,''
box_message,['yerr   = 1. / sqrt(weight)',$
'weight = 1. / yerr^2',$
'yerr = 0  <->  weight = 0']
print,''


;print,''
;x=108
;y=483
;print,'Sigma for all lambda at x = ',x,' and y = ',y
;print,'IDL'
;print,' ======================================================'
;print,reform(dpy[x,y,*])
;print,' ======================================================'
;print,'PYTHON'
;print,' ======================================================'
;print,reform(didl[x,y,*])
;print,' ======================================================'
;print,'DIFF'
;print,' ======================================================'
;print,' ======================================================'
;print,reform(dpy[x,y,*]) - reform(didl[x,y,*])
;print,''



print,''
print,'IDL - sigma from IDL function spice_getwindata'
print,'sigma mean     ',mean(didl,/nan)
print,'sigma stddev   ',stddev(didl,/nan)
print,'sigma variance ',variance(didl,/nan)
print,'sigma min      ',min(didl)
print,'sigma max      ',max(didl)

print,''
print,'PYTHON - sigma from python function spice_error'
print,'sigma mean     ',mean(dpy,/nan)
print,'sigma stddev   ',stddev(dpy,/nan)
print,'sigma variance ',variance(dpy,/nan)
print,'sigma min      ',min(dpy)
print,'sigma max      ',max(dpy)

print,''
print,'absolute difference'
print,'Diff = sigmaPython - sigmaIDL'
diffd = dpy - didl
print,'Diff mean     ',mean(diffd,/nan)
print,'Diff stddev   ',stddev(diffd,/nan)
print,'Diff variance ',variance(diffd,/nan)
print,'Diff min      ',min(diffd)
print,'Diff max      ',max(diffd)

print,''
print,'relative difference [%]'
print,'Diff = (sigmaPython - sigmaIDL) / sigmaIDL * 100'
diffd = (dpy - didl)/didl*100
print,'Diff mean     ',mean(diffd,/nan)
print,'Diff stddev   ',stddev(diffd,/nan)
print,'Diff variance ',variance(diffd,/nan)
print,'Diff min      ',min(diffd)
print,'Diff max      ',max(diffd)





;;;; relative erros
;; and plot as function of datavalue

sdata = size(data,/n_elements)
vec_data = reform(data,sdata)
vec_idl = reform(didl,sdata)
vec_py = reform(dpy,sdata)

window,0
plot,vec_data,vec_idl,psym=3,xrange=[0,2.5],ytitle='Sigma',xtitle='DataIntensity'
oplot,vec_data,vec_py,psym=3,color=111


diffidl = didl / data * 100
vec_diffidl = reform(diffidl,sdata)

diffpy = dpy / data * 100
vec_diffpy = reform(diffpy,sdata)


window,1
plot,vec_data,vec_diffidl,psym=3,yrange=yrange1,xrange=xrange,ytitle='Sigma/DataIntensity [%]',xtitle='DataIntensity'
oplot,vec_data,vec_diffpy,psym=3,color=111


vec_diff = vec_py - vec_idl

window,2
plot,vec_data,vec_diff,psym=3,xrange=xrange,ytitle='SigmaPython - SigmaIDL',xtitle='DataIntensity'


vec_diffrel = vec_diff / vec_idl * 100

window,3
plot,vec_data,vec_diffrel,psym=3,xrange=xrange,ytitle='(SigmaPython - SigmaIDL) / SigmaIDL [%]',xtitle='DataIntensity'


vec_diffrel2 = vec_py / vec_idl

window,4
plot,vec_data,vec_diffrel2,psym=3,yrange=yrange2,xrange=xrange,ytitle='SigmaPython / SigmaIDL',xtitle='DataIntensity'





return
;;;;;;;;;;;;;



WEIGHTS = 1.0 / dpy ^ 2.0
index = where(~finite(WEIGHTS), count)
IF count GT 0 THEN WEIGHTS[index] = 0.0
weights=transpose(weights,[2,0,1])
help,WEIGHTS


ana = obj.mk_analysis(window_index, /init_all_cubes)

help,ana
;stop
handle_value,ana.weights_h,weights,/set


  origin = [(obj.get_lambda_vector(window_index))[0], (obj.get_instr_x_vector(window_index))[0], (obj.get_instr_y_vector(window_index))[0]]
  scale = [obj.get_resolution(window_index, /lambda), obj.get_resolution(window_index, /x), obj.get_resolution(window_index, /y)]

;spice_xcfit_block, ana = ana, origin = origin, scale = scale, phys_scale = [0, 1, 1], image_dim = [1, 2]




;stop
end
