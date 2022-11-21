pro spice_calc_margin,margin0,nfig ,xleft=xleft,xright=xright,xdist=xdist
;+
;   spice_calc_margin,margin0,nfig ,xleft=xleft,xright=xright,xdist=xdist
;
;            calculates xmargin so that:
;            xleft    : space in characters left of leftmost figure
;            xright   : space in characters right of rightmost figure
;            xdist    : space in characters between figures
;         or
;            the same for ymargin with
;            xleft    : space in characters below bottom figure
;            xright   : space in characters above top figure
;
;            margin(*,i) is xmargin for figure i from left or
;                           ymargin for figure i from bottom
;
; Version     : Version 1, Martin Wiesman, 27. July 2021
;                          Copied from calc_margin.pro
;
; $Id: 2021-07-27 12:33 CEST $
;-
if(n_params(0) lt 2) then begin
  print,'spice_calc_margin,margin0,nfig [,xleft=xleft,xright=xright,xdist=xdist]'
  return
endif

if(n_elements(xleft) eq 0)  then xleft=10.
if(n_elements(xright) eq 0) then xright=0.
if(n_elements(xdist) eq 0)  then xdista=replicate(0.,nfig-1) $
else if(n_elements(xdist) eq 1)  then begin
  if(nfig gt 1) then xdista=replicate(xdist,nfig-1) else xdista=[xdist]
endif else begin
  xdista=xdist
endelse

margin0=fltarr(2,nfig)
ndim=nfig*2
a=fltarr(ndim,ndim)
b=fltarr(ndim)

for i=0,nfig-2 do begin
  a(i*2,i*2:i*2+1)=1.0
  a(i*2,(i+1)*2:(i+1)*2+1)=-1.0
  b(i*2)=0.0
endfor

for i=1,nfig-1 do begin
  a(i*2+1,i*2-1:i*2)=1.0
  b(i*2+1)=xdista(i-1)
endfor

a(1,0)=1.0
b(1)=xleft
a(ndim-2,ndim-1)=1.
b(ndim-2)=xright

aa=a
bb=b
ludcmp,aa,index,d
lubksb,aa,index,bb

for i=0,nfig-1 do begin
  margin0(0,i)=bb(i*2)
  margin0(1,i)=bb(i*2+1)
endfor

return
end
