pro spice_br_Panel,xnr,ynr,nx=nx,ny=ny,reset=reset,xlabel=xlabel,ylabel=ylabel,$
          xleft=xleft,xright=xright,ybottom=ybottom,ytop=ytop,$
          xdist=xdist,ydist=ydist,order=order
;+
;   spice_br_panel,xnr,ynr,nx=nx,ny=ny,reset=reset,xlabel=xlabel,ylabel=ylabel,$
;          xleft=xleft,xright=xright,ybottom=ybottom,ytop=ytop,$
;          xdist=xdist,ydist=ydist,order=order
;
;            sets system variables for a nxxny panel plot with no
;            intervening blanks between panels and labels only
;            at far left and bottom.xnr,ynr is 0,0 for top left
;            and nx-1,ny-1 for bottom right
;            default label handling can be overridden with keywords
;            xlabel and ylabel
;            order=order  1 for filling columns first (default), 0
;            otherwise
;
; Version     : Version 1, Martin Wiesman, 27. July 2021
;                          Copied from br_panel.pro
;
; $Id: 2021-07-27 12:33 CEST $
;-
common cpanel,xmargin,ymargin,xmargin_old,ymargin_old,xtitle,ytitle
;
if(n_elements(reset) ne 0) then begin
  !p.multi=0
  !x.margin=xmargin_old
  !y.margin=ymargin_old
  !x.ticks=0
  !x.tickname=replicate('',30)
  !x.title=''
  !y.ticks=0
  !y.tickname=replicate('',30)
  !y.title=''
  return
endif

if(n_params(0) lt 2) then begin
  print,'spice_br_panel,xnr,ynr,nx=nx,ny=ny,reset=reset,xlabel=xlabel,ylabel=ylabel,$'
  print,'      xleft=xleft,xright=xright,ybottom=ybottom,ytop=ytop,$'
  print,'      xdist=xdist,ydist=ydist,order=order
  return
endif

if(n_elements(nx) eq 0) then nx=3
if(n_elements(ny) eq 0) then nx=4
if(n_elements(xleft) eq 0) then xleft=8
if(n_elements(xright) eq 0) then xright=0
if(n_elements(ybottom) eq 0) then ybottom=4
if(n_elements(ytop) eq 0) then ytop=0
if(n_elements(xdist) eq 0) then xdist=0
if(n_elements(ydist) eq 0) then ydist=0
if(n_elements(order) eq 0) then order=1


if(xnr eq 0) and (ynr eq 0) then begin
  spice_calc_margin,xmargin,nx,xleft=xleft,xright=xright,xdist=xdist
  spice_calc_margin,ymargin,ny,xleft=ybottom,xright=ytop,xdist=ydist
  !p.multi=[0,nx,ny,0,order]
  xmargin_old=!x.margin
  ymargin_old=!y.margin
  xtitle=!x.title
  ytitle=!y.title
endif

if(n_elements(ylabel) eq 0) then begin
  if(xnr eq 0) then ylabel=1 else ylabel=0
endif
if(n_elements(xlabel) eq 0) then begin
  if(ynr eq ny-1) then xlabel=1 else xlabel=0
endif

!x.margin=xmargin(*,xnr)
!y.margin=ymargin(*,ny-1-ynr)
if(xlabel ne 0) then begin
  !x.tickname=replicate('',30)
  !x.title=xtitle
endif else begin
  !x.tickname=replicate(' ',30)
  !x.title=''
endelse
if(ylabel ne 0) then begin
  !y.tickname=replicate('',30)
  !y.title=ytitle
endif else begin
  !y.tickname=replicate(' ',30)
  !y.title=''
endelse

end
