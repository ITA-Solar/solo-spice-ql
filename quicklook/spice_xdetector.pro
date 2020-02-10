;+
; NAME:
;       SPICE_XDETECTOR
;
; PURPOSE:
;
;       SPICE_XDETECTOR is used to display 2-D (or higher) data. It is a
;       widget based program with several options and functions that
;       allows data to be displayed in a nukmber of modes.
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xdetector, data, lindx, group_leader = groupleader, ncolors=ncolors
;
; INPUTS:
;       data: Data object. Must follow the structure of the superclass
;              HW_DATA (more or less).
;       lindx: Line index array
;
; KEYWORD PARAMETERS:
;       group_leader: Widget parent (if any).
;       ncolors: Number of colors for xdetector. Default is
;                !d.n_colors<256.
;
; OUTPUTS:
;       None
;
; CALLS:
;       xzoom, iris_ximovie
;       
; COMMON BLOCKS:
;
; PROCEDURE:
;       SPICE_XDETECTOR defines the widgets and displays data. It has several
;       options for displaying data in different modes, zooming,
;       selecting colors, file output etc. It is a QL-tool for
;       displaying data of 2 dimensions or higher. 
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       12-Sep-2002: Oivind Wikstol: First version - xdetector for EIS
;        2-Jan-2013: Viggo Hansteen: Rewritten for IRIS data
;       19-Aug-2015: Martin Wiesmann: Bugfixes
;       10-Feb-2020: Martin Wiesmann: Rewritten for SPICE data
;
;-
;
; save as postscript file
pro spice_xdetector_ps,event
  thisfile=dialog_pickfile(/write,file='spice_xdetector.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xdetector_draw, pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro spice_xdetector_jpeg,event
  thisfile=dialog_pickfile(/write,file='spice_xdetector.jpg')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  wset,(*info).wid
  snapshot=tvrd()
  tvlct,r,g,b,/get
  s=size(snapshot)
  image24=bytarr(3,s[1],s[2])
  image24(0,*,*)=r(snapshot)
  image24(1,*,*)=g(snapshot)
  image24(2,*,*)=b(snapshot)
  write_jpeg,thisfile,image24,true=1,quality=75
end

;display image in the draw window:
pro spice_xdetector_draw, event
  widget_control, event.top, get_uvalue = info
  if !d.name ne 'PS' then begin
    wset, (*info).wid
    bgblack=1
    if (*info).realsize then ticklen=0.01 else ticklen=0.02
  endif else begin
    bgblack=0
    ticklen=-0.02
  endelse
  erase
  if (*info).log then begin
    im_min=*(*info).data->datamin()>1.
    ymax = alog10(max(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing())))
    ymin = alog10(min(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing())))
    mplot_image,alog10(*(*info).drawimage>im_min), $
         *(*info).xscale, *(*info).yscale, $
         min=ymin,max=ymax, $
         xstyle = 1, ystyle = 1, pos=(*info).imagepos, $
         xtitle = (*info).xtitle, ytitle = (*info).ytitle,bgblack=bgblack,ticklen=ticklen
  endif else begin
    im_min=*(*info).data->datamin()
    ymin = min(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing()))
    ymax = max(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing()))
    mplot_image, *(*info).drawimage, $
         *(*info).xscale, *(*info).yscale, $
         min=ymin,max=ymax, $
         xstyle = 1, ystyle = 1, pos=(*info).imagepos, $
         xtitle = (*info).xtitle, ytitle = (*info).ytitle,bgblack=bgblack,ticklen=ticklen
  endelse
  if (*info).lineplot then begin
    spectrum=*(*info).drawimage
    spectrum[where(finite(spectrum) eq 0)]=0.0
    spectrum=total(spectrum,2)/((size(*(*info).drawimage))[2])
    axis,0.8,/normal,/yaxis,yr=[min(spectrum),max(spectrum)],/save, $
      ytitle='!3 Average spectrum'
    oplot,*(*info).xscale,spectrum
  endif
  if ymax-ymin eq 0.0 then ymax=ymin+1
  format='(f10.1)'
  if ymax-ymin lt 10 then format='(f7.4)'
  hw_colorbar, position = [((*info).imagepos)[2]+0.01, $
                           ((*info).imagepos)[1], $
                           ((*info).imagepos)[2]+0.02, $
                           ((*info).imagepos)[3]], range = [ymin, ymax], $
            /vertical , /right, format=format, title=(*info).colorbar_title
end

; get the value of the draw window option menu:
function spice_xdetector_dwoption, event
  widget_control, event.top, get_uvalue = info
  (*info).dwoption = event.value
  return, 0
end

; wavelength selection buttons
function spice_xdetector_wloption, event
  widget_control, event.top,get_uvalue=info
  case event.value of
  0: spice_xdetector_wpix, event
  1: spice_xdetector_wangstr, event
  endcase
  return,0
end

; slit scale selection buttons
function spice_xdetector_sloption, event
  widget_control, event.top,get_uvalue=info
  case event.value of
  0: spice_xdetector_spix, event
  1: spice_xdetector_sarcsec, event
  endcase
  return,0
end

; slider to select exposure number
pro spice_xdetector_expslider, event
  widget_control, event.top,get_uvalue=info
  (*info).expnr=event.value
  nr=(*info).expnr
  ; read new data (for selected position) into detector variable
  yw=*(*info).data->getyw()
  ywmax=max(*(*info).data->getyw(),ic)
  ysmax=*(*info).data->getys(ic)
  xw=*(*info).data->getxw()
  xs=*(*info).data->getxs()
  ys=*(*info).data->getys()-ysmax
  for i=0,(*info).nwin-1 do begin
    lindxi=(*info).lindx[i]
    wd=*(*info).data->getvar(lindxi)
    xss=0
    if *(*info).data->getline_id(i) eq 'FULL CCD FUV2' then xss=xs[i]-xs[i-1]-1
    bin_sp=*(*info).data->binning_spectral(lindxi)
;    bin_sp=*(*info).data->getinfo('SUMSPTRL')
    pos0 = (xs[lindxi]-1)*bin_sp mod (*(*info).data->getccd_sz('FUV1'))[0]
    pos1 = xw[lindxi]
    pos2 = ys[lindxi]
    pos3 = yw[lindxi]
; assuming that all slit positions (on each CCD) start at same
; position: if this is not the case more logic is needed above
    (*info).detector[pos0:pos0+pos1*bin_sp-1,0:pos3-1]= $
       congrid(*(*info).data->descale_array(wd[xss:xss+pos1-1, *, nr]),pos1*bin_sp,pos3)
  endfor

; update exposure time and fine mirror
  
  widget_control,(*info).exposuretext, $
         set_value=strtrim('Exp time: '+string((*(*info).data->getexp())[nr], $
                           format='(f7.1)')+' s',2)
  time=*(*info).data->gettime()
  widget_control,(*info).timetext, $
         set_value=strtrim('Time    : '+string(time[nr], $
                           format='(f7.1)')+' s',2)
  pztx=*(*info).data->getpztx()
  widget_control,(*info).fmirrxtext, $
    set_value = 'PZTX: '+ string(pztx[nr],format='(f7.2)')+' arcsec'
  pzty=*(*info).data->getpzty()
  widget_control,(*info).fmirrytext, $
    set_value = 'PZTY: '+ string(pzty[nr],format='(f7.2)')+' arcsec'
    
; display new raster position
  
  pseudoevent={widget_base,id:0L, $
    top:event.top, handler:0l, x:(*info).tlb_xsz,  y:(*info).tlb_ysz}
  widget_control, event.top, set_uvalue=info
  spice_xdetector_resize, pseudoevent
end

; slider to select exposure within a raster pos (if multiple)
pro spice_xdetector_expprp_slider, event
  widget_control, event.top,get_uvalue=info
  (*info).exprp=event.value
  (*info).expnr = (*info).nexpprp*((*info).rpos - 1) + (*info).exprp - 1
  nr=(*info).expnr
; read new data (for selected exposure) into detector variable
  yw=*(*info).data->getyw()
  ywmax=max(*(*info).data->getyw(),ic)
  ysmax=*(*info).data->getys(ic)
  xw=*(*info).data->getxw()
  xs=*(*info).data->getxs()
  ys=*(*info).data->getys()-ysmax
  for i=0,(*info).nwin-1 do begin
    lindxi=(*info).lindx[i]
    wd=*(*info).data->getvar(lindxi)
    xss=0
    if *(*info).data->getline_id(i) eq 'FULL CCD FUV2' then xss=xs[i]-xs[i-1]-1
    pos0 = xs[lindxi] mod (*(*info).data->getccd_sz('FUV1'))[0]
    pos1 = xw[lindxi]
    pos2 = ys[lindxi]
    pos3 = yw[lindxi]
    (*info).detector[pos0:pos0+pos1-1,pos2:pos2+pos3-1] = $
       *(*info).data->descale_array(wd[xss:xss+pos1-1, *, (*info).expnr])
  endfor
  
; update exposure time and fine mirror

  widget_control,(*info).exposuretext, $
         set_value=strtrim('Exp time: '+string((*(*info).data->getexp())[nr], $
                           format='(f7.1)')+' ms',2)
  time=*(*info).data->gettime()
  widget_control,(*info).timetext, $
         set_value=strtrim('Time    : '+string(time[nr]/1000., $
                           format='(f7.1)'+' s'),2)
  pztx=*(*info).data->getpztx()
  widget_control,(*info).fmirrxtext, $
    set_value = 'PZTX: '+ string(pztx[nr],format='(f7.2)')+' arcsec'
  pzty=*(*info).data->getpzty()
  widget_control,(*info).fmirrytext, $
    set_value = 'PZTY: '+ string(pzty[nr],format='(f7.2)')+' arcsec'

; display new raster position
      
  pseudoevent={widget_base,id:0L, $
    top:event.top, handler:0l, x:(*info).tlb_xsz,  y:(*info).tlb_ysz}
  widget_control, event.top, set_uvalue=info
  spice_xdetector_resize, pseudoevent
end

; set screen size to preset value
function spice_xdetector_drawsizeoption, event
  widget_control, event.top, get_uvalue = info
  w_ysz=(*info).tlb_ysz-(*info).d_ysz
  case event.value of
    0: sizemode='standard'
    1: sizemode='big'
  endcase
  aspect=float((*info).screensize[0])/float((*info).screensize[1])
  xysz=(*(*info).data->getaux())->getdrawsize(sizemode,aspect=aspect)
  pseudoevent={widget_base,id:0l,top:(*info).tlb, handler:0l, $
               x:xysz[0]+(*info).lcol_xsz, y:xysz[1]+w_ysz}
  spice_xdetector_resize, pseudoevent
  return, 0
end
; slider to select raster position (used only when multiple exp
; pr. rast. pos)
pro spice_xdetector_rast_slider, event
  widget_control, event.top,get_uvalue=info
  (*info).rpos = event.value
  (*info).expnr = (*info).nexpprp*((*info).rpos - 1) + (*info).exprp - 1
  nr=(*info).expnr
  
; read new data (for selected exposure) into detector variable
  
  yw=*(*info).data->getyw()
  ywmax=max(*(*info).data->getyw(),ic)
  ysmax=*(*info).data->getys(ic)
  xw=*(*info).data->getxw()
  xs=*(*info).data->getxs()
  ys=*(*info).data->getys()-ysmax
  for i=0,(*info).nwin-1 do begin
    lindxi=(*info).lindx[i]
    wd=*(*info).data->getvar(lindxi)
    xss=0
    if *(*info).data->getline_id(i) eq 'FULL CCD FUV2' then xss=xs[i]-xs[i-1]-1
    pos0 = xs[lindxi] mod (*(*info).data->getccd_sz('FUV1'))[0]
    pos1 = xw[lindxi]
    pos2 = ys[lindxi]
    pos3 = yw[lindxi]
    (*info).detector[pos0:pos0+pos1-1,pos2:pos2+pos3-1] = $
      *(*info).data->descale_array(wd[xss:xss+pos1-1,*,(*info).expnr])
  endfor
  
; update exposure time and fine mirror

  widget_control,(*info).exposuretext, $
         set_value=strtrim('Exp. time: '+string((*(*info).data->getexp())[nr], $
                           format='(f5.2)'+' ms'),2)   
  time=*(*info).data->gettime()
  widget_control,(*info).timetext, $
         set_value=strtrim('Time     : '+string(time[nr], $
                           format='(f5.2)')+' ms',2)
  pztx=*(*info).data->getpztx()
  widget_control,(*info).fmirrxtext, $
    set_value = 'PZTX: '+ string(pztx[nr],format='(f7.2)')+' arcsec'
  pzty=*(*info).data->getpzty()
  widget_control,(*info).fmirrytext, $
    set_value = 'PZTY: '+ string(pzty[nr],format='(f7.2)')+' arcsec'            
    
  ; display new raster position
  pseudoevent={widget_base,id:0L, $
    top:event.top, handler:0l, x:(*info).tlb_xsz,  y:(*info).tlb_ysz}
  widget_control, event.top, set_uvalue=info
  spice_xdetector_resize, pseudoevent
end

; zoom in draw window:
pro spice_xdetector_zoom, event
  widget_control,event.top,get_uvalue=info
  if event.type gt 2 then return

  ;set up axis titles for line plots (options 1 or 2 below)
  varname = *(*info).data->getvariablename()
  varname = varname[0] +': column average'
;
  events=['down','up','motion']
  thisevent=events[event.type]
;
  if (*info).realsize then begin 
    window, /pixmap, /free, xsize = (*info).xvs, ysize = (*info).yvs
    xs = ((*info).imagepos)[0]*(*info).xvs
    ys = ((*info).imagepos)[1]*(*info).yvs
  endif else begin
    window, /pixmap, /free, xsize = (*info).d_xsz, ysize = (*info).d_ysz
    xs = ((*info).imagepos)[0]*(*info).d_xsz
    ys = ((*info).imagepos)[1]*(*info).d_ysz
  endelse
  if (*info).log then begin
    im_min=*(*info).data->datamin()>1.
    ymax = alog10(max(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing())))
    ymin = alog10(min(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing())))
    mplot_image,alog10(*(*info).drawimage), $
         *(*info).xscale, *(*info).yscale, $
         min=ymin,max=ymax, $
         xstyle = 1, ystyle = 1, pos=(*info).imagepos, $
         xtitle = (*info).xtitle, ytitle = (*info).ytitle,/bgblack
  endif else begin
    im_min=*(*info).data->datamin()
    ymin = min(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing()))
    ymax = max(iris_histo_opt(*(*info).drawimage>im_min,missing=*(*info).data->missing()))
    mplot_image,*(*info).drawimage, $
          *(*info).xscale, *(*info).yscale, $
         min=ymin,max=ymax, $
         xstyle = 1, ystyle = 1, pos=(*info).imagepos, $
         xtitle = (*info).xtitle, ytitle = (*info).ytitle,/bgblack
  endelse
  if ymax-ymin eq 0.0 then ymax=ymin+1
  if ymax-ymin lt 10 then format='(f7.4)' else format='(f10.1)'
  hw_colorbar, position = [((*info).imagepos)[2]+0.01, $
                           ((*info).imagepos)[1], $
                           ((*info).imagepos)[2]+0.02, $
                           ((*info).imagepos)[3]], range = [ymin, ymax], $
            /vertical , /right, format=format, title=(*info).colorbar_title
  (*info).pixid = !d.window
  case thisevent of
    'down': begin
    ;  turn motion events on2
    ;  set static corner
    widget_control,(*info).drawid,draw_motion_events=1
    (*info).sx=event.x
    (*info).sy=event.y
  endcase
    'up': begin
      ;  erase last box
      ;  turn motion events off
       device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0, $
       (*info).pixid]
       widget_control,(*info).drawid,draw_motion_events=0
       sx = (*info).sx - xs
       sy = (*info).sy - ys
       dx = event.x - xs
       dy = event.y - ys
       sx = (sx < ((*info).xps - 1)) > 0
       sy = (sy < ((*info).yps - 1)) > 0
       dx = (dx < ((*info).xps - 1)) > 0
       dy = (dy < ((*info).yps - 1)) > 0
;
       detsize=size((*info).detector)
       dposx=(*info).imagepos[2]-(*info).imagepos[0]
       dposy=(*info).imagepos[3]-(*info).imagepos[1]
       xrescale = float(detsize[1])/float((*info).xvs)/dposx
       yrescale = float(detsize[2])/float((*info).yvs)/dposy
       detsx = round(sx * xrescale)     
       detsy = round(sy * yrescale)     
       detdx = round(dx * xrescale)     
       detdy = round(dy * yrescale)     
       x1 = (detsx<detdx) > 0
       x2 = (detsx>detdx) < detsize[1]-1 > x1
       y1 = (detsy<detdy) > 0   
       y2 = (detsy>detdy) < detsize[2]-1 > y1
;
       if x1 eq x2 or y1 gt y2 then goto,noaction
       xscale = (*info).lambda[x1:x2]
       yscale = indgen(y2-y1+1)+y1+(*(*info).yscale)[0]
       image = ((*info).detector)[x1:x2,y1:y2]
       sz=size(image)
       mind = min(sz[0:2])
       pos=[x1,x2,y1,y2]
       case (*info).dwoption of
            0:begin
                if mind ge 2 then begin
                  xmax = (*info).screensize[0]
                  ymax = (*info).screensize[1]
                  image=congrid(image,sz[1]*2 < xmax, sz[2]*2 < ymax)
                  xscale = interpol(xscale, sz[1]*2 < xmax)
                  yscale = interpol(yscale, sz[2]*2 < ymax)
                  iris_xzoom, image, xscale, yscale, xtitle = (*info).xtitle, $
                         ytitle = (*info).ytitle, group_leader=event.top
                endif
              end
            1:begin
                ;set up axis titles for line plots (options 1 or 2 below)
                varname = *(*info).data->getvariablename()
                varname = varname[0] +': column average'
                dmean = total(image, 1)/sz[1]
                  iris_xlineplot, dmean, xscale = yscale, $
                             title = varname, $
                             xtitle = (*info).ytitle, $
                             ytitle = varname, $
                             groupl = event.top
              end
            2:begin
                ;set up axis titles for line plots (options 1 or 2 below)
                varname = *(*info).data->getvariablename()
                varname = varname[0] +': row average'
                if y1 eq y2 then dmean=image else dmean = total(image, 2)/sz[2]
                iris_xlineplot, dmean, xscale = xscale, $
                             title = varname, $
                             xtitle = (*info).xtitle, $
                             ytitle = varname, $
                             groupl = event.top
              end
            endcase
         endcase
       'motion':  begin
       ;  erase previous box
       ;  draw new box
         dx=event.x
         dy=event.y
         sx=(*info).sx
         sy=(*info).sy
         wset,(*info).wid
         device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0,(*info).pixid]
         if (*info).realsize then $
           device,copy=[0,0,(*info).xvs,(*info).yvs,0,0,(*info).pixid]
         plots,[sx,sx,dx,dx,sx],[sy,dy,dy,sy,sy],/device, $
            color=(*info).drawcolor
       endcase
    endcase
  wdelete, (*info).pixid
  noaction:
end

; Popup window for line selection
pro spice_xdetector_pickline, event
  widget_control, event.top ,get_uvalue = info
  ; open window for line selection
  lineselect_widget = widget_base(title = 'Select line', $
                group_leader = (*info).tlb,/row,xoff=200,yoff=200)
  closefield = widget_base(lineselect_widget,/column)
  closebutton = widget_button(closefield, value = 'OK', $
                              event_pro = 'spice_xdetector_pickline_destroy')
  line_base = widget_base(lineselect_widget,/column,/frame)
  linelist = cw_bgroup(line_base, (*info).linelist, /return_index, $
                       /exclusive, event_func = 'spice_xdetector_pickline_pick')
  widget_control, lineselect_widget, set_uvalue = info
  widget_control, lineselect_widget, /realize
  xmanager, 'Select line', lineselect_widget, $
             /no_block, group_leader = (*info).tlb
end

; get the value of the selected line from the line list:
function spice_xdetector_pickline_pick, event
  widget_control, event.top, get_uvalue = info
  defdir = ''
  (*info).line = event.value + (*info).lindx[0]
  return, 0
end

; close Line selection widget
pro spice_xdetector_pickline_destroy, event
  widget_control, event.top, get_uvalue = info
   spice_xdetector_anim, event
  widget_control, event.top,/destroy
end

; Controls the animation event: If only one line,
; start animation, if several line, then pop up
; line selection window first.
pro spice_xdetector_control_anim,event
  widget_control, event.top, get_uvalue = info
  if (*info).nwin lt 2 then begin
    (*info).line=(*info).lindx[0]
    spice_xdetector_anim, event
  endif else begin
    spice_xdetector_pickline, event
  endelse
end

; create animation widget and launch animation
pro spice_xdetector_anim, event
  widget_control, event.top, get_uvalue = info
  magnification=0.95
  minsize=400.0
  maxsize=800.0
  xsize=*(*info).data->getxw((*info).line)
  ysize=*(*info).data->getyw((*info).line)
  if xsize lt minsize then magnification=minsize/xsize
  if xsize gt maxsize then magnification=maxsize/xsize
  if ysize*magnification lt minsize then magnification=minsize/ysize
  if ysize*magnification gt maxsize then magnification=maxsize/ysize
  if 1.0 eq swap_endian(1.0,/swap_if_big_endian) then swap=1
  iris_ximovie,*(*info).data->getfilename(),group_leader=(*info).tlb, $
          *(*info).data->getxw((*info).line),*(*info).data->getyw((*info).line), $
          nframes=*(*info).data->getnraster((*info).line), $
          offset=*(*info).data->getposition((*info).line),swap=swap, $
          magnification=magnification,missing=*(*info).data->missing(), $
          time=*(*info).data->ti2utc(),scaling1=*(*info).data->scaling(), $
          type=*(*info).data->getdatatype()

;;   *(*info).data-> getwin, (*info).line, wd, pos
;;   if (*info).log then wd=alog10(wd>(mean(wd)>1.)/100.)
;;   sz = size(wd)
;;   ndim = sz[0]
;;   xsize = sz[1]
;;   ysize = sz[2]

;;   if ndim lt 3 then begin
;;     ok = dialog_message('Data array must be 3-D to make animation!',/center)
;;     return
;;   endif

;;   ; bytscale data to save time in animation tool
;; ;  wdb = bytscl(iris_histo_opt(wd,1.e-2,missing=*(*info).data->missing()))

;;   ; write data to assoc file:
;;   ct=0
;;   repeat begin
;;     ct=ct+1
;;     assoc_file = IRISxfiles_appReadme()+'/spice_xdetector_ximovie_'+strtrim(string(ct),2)+'.tmp'
;;   endrep until ((findfile(assoc_file))[0] eq '')
;;   if ct gt 99 then begin
;;     message,'more than 100 temporary assoc files stored in',/info
;;     message,IRISxfiles_appReadme()+'/spice_xdetector_ximovie_XX.tmp. Consider purge!',/info
;;   endif
  
;;   openw, lu, assoc_file, /get_lun
;;   rec = assoc(lu, wd)
;;   rec[0] = wd
;;   close, lu & free_lun, lu
;; ;
;;   magnification=0.95
;;   minsize=400.0
;;   maxsize=800.0
;;   if xsize lt minsize then magnification=minsize/xsize
;;   if xsize gt maxsize then magnification=maxsize/xsize
;;   if ysize*magnification lt minsize then magnification=minsize/ysize
;;   if ysize*magnification gt maxsize then magnification=maxsize/ysize
;;   ; start iris_ximovie, with the delete keyword (afile is removed from disc
;;   ; when iris_ximovie is closed
;;   iris_ximovie assoc_file, xsize, ysize, magnification=magnification, /float, $
;;     title=*(*info).data->getfilename(),time=*(*info).data->ti2utc(), $
;;     group_leader = (*info).tlb, /fdelete, missing=*(*info).data->missing()
  return
end

; change wavelength scale to pixels
pro spice_xdetector_wpix, event
  widget_control, event.top, get_uvalue = info
  ; change titles in aux object
  (*(*info).data->getaux())->setwscale,'pixels'
  (*(*info).data->getaux())->setxytitle,wscale='pixels'
  ; set titles for image plots
  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
  ; set scale for images
  xscale = *(*info).data->getlambda(*(*info).data->getregion((*info).lindx[0],/full))
  (*info).lambda = xscale
  pseudoevent={widget_base,id:0L, $
               top:event.top, handler:0l, x:(*info).tlb_xsz, y:(*info).tlb_ysz}
  spice_xdetector_resize, pseudoevent
end

; change wavelength scale to Angstrom
pro spice_xdetector_wangstr, event
  widget_control, event.top, get_uvalue = info
   ; change titles in aux object
  (*(*info).data->getaux())->setwscale,string("305B) 
  (*(*info).data->getaux())->setxytitle,wscale=string("305B)
  ; set titles for image plots
  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]

  xscale = *(*info).data->getlambda(*(*info).data->getregion((*info).lindx[0],/full))
  (*info).lambda = xscale

  pseudoevent={widget_base,id:0L, $
               top:event.top, handler:0l, x:(*info).tlb_xsz, y:(*info).tlb_ysz}
  spice_xdetector_resize, pseudoevent
end

pro spice_xdetector_spix, event
  widget_control, event.top, get_uvalue = info
; change titles in aux object
  (*(*info).data->getaux())->setsscale,'pixels'
  (*(*info).data->getaux())->setxytitle,sscale='pixels'
; set titles for image plots
  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]

  pseudoevent={widget_base,id:0L, $
               top:event.top, handler:0l, x:(*info).tlb_xsz, y:(*info).tlb_ysz}
  spice_xdetector_resize, pseudoevent
end

pro spice_xdetector_sarcsec, event
  widget_control, event.top, get_uvalue = info
; change titles in aux object
  (*(*info).data->getaux())->setsscale,'arcsec'
  (*(*info).data->getaux())->setxytitle,sscale='arcsec'
; set titles for image plots
  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]

  pseudoevent={widget_base,id:0L, $
               top:event.top, handler:0l, x:(*info).tlb_xsz, y:(*info).tlb_ysz}
  spice_xdetector_resize, pseudoevent
end

; select color table
pro spice_xdetector_colors, event
  widget_control, event.top, get_uvalue=info
  thisevent = tag_names(event, /structure_name)
  case thisevent of
  'WIDGET_BUTTON': begin
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'spice_xdetector colors (' + strtrim((*info).wid, 2) + ')', $
        group_leader = event.top, notifyid = [event.id, event.top]
      endcase
  'XCOLORS_LOAD': begin
      (*info).r = event.r((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).g = event.g((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).b = event.b((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      if !d.n_colors gt 256 then begin
        pseudoevent={widget_button,id:0L, $
                     top:event.top, handler:0l, select:1}
        spice_xdetector_draw, pseudoevent
      endif
    end
  endcase

  widget_control, event.top, set_uvalue = info
end

; protect colors
pro spice_xdetector_protect_colors,event
  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
end

; resize main window
pro spice_xdetector_resize, event
  widget_control, event.top ,get_uvalue = info
  w_ysz=(*info).tlb_ysz-(*info).d_ysz
  (*info).tlb_xsz = event.x
  (*info).tlb_ysz = event.y
  (*info).d_xsz = (event.x - (*info).lcol_xsz) > 0
  (*info).d_ysz = (event.y-w_ysz)
  if(*info).realsize then begin
    (*info).imagepos = [0.03, 0.15, 0.95, 0.95]
    xvs = (*info).ccd_xsz*(1.+(*info).imagepos[0]+(1.-(*info).imagepos[2]))
    yvs = (*info).ccd_ysz*(1.+(*info).imagepos[2]+(1.-(*info).imagepos[3]))
    widget_control, (*info).drawid, $
                    xsize = (*info).d_xsz, ysize = (*info).d_ysz, $
                    draw_xsize = xvs,  draw_ysize = yvs
    (*info).xvs = xvs
    (*info).yvs = yvs
    (*info).xps = (*info).ccd_xsz  ; x-plot-size 
    (*info).yps = (*info).ccd_ysz  ; y-plot-size
    drawimage = (*info).detector
    xscale = (*info).lambda
    ywmax=max(*(*info).data->getyw(),ic)
    ysmax=*(*info).data->getys(ic)
    if (*(*info).data->getaux())->getsscale() eq 'pixels' then yscale = findgen((*info).ccd_ysz)+ysmax $
    else yscale=*(*info).data->getypos()
    ptr_free,(*info).xscale
    ptr_free,(*info).yscale
    ptr_free,(*info).drawimage
    (*info).xscale = ptr_new((*info).ccd_xsz)
    (*info).yscale = ptr_new((*info).ccd_ysz)
    (*info).drawimage = ptr_new(uintarr((*info).ccd_xsz, (*info).ccd_ysz))
    *(*info).drawimage = drawimage
    *(*info).xscale = xscale
    *(*info).yscale = yscale
  endif else begin
    (*info).imagepos = [0.1, 0.1, 0.9, 0.95]
    (*info).xps = (*info).d_xsz*(1.+(*info).imagepos[0]+(1.-(*info).imagepos[2]))
    (*info).yps = (*info).d_ysz*(1.+(*info).imagepos[1]+(1.-(*info).imagepos[3]))
    widget_control, (*info).drawid, $
                    xsize = (*info).d_xsz, ysize = (*info).d_ysz, $
                    draw_xsize = (*info).d_xsz, draw_ysize = (*info).d_ysz
    (*info).xvs = (*info).d_xsz
    (*info).yvs = (*info).d_ysz
    xscale = (*info).lambda
    ywmax=max(*(*info).data->getyw(),ic)
    ysmax=*(*info).data->getys(ic)
    if (*(*info).data->getaux())->getsscale() eq 'pixels' then $
           yscale = findgen((*info).ccd_ysz)+ysmax $
    else yscale=*(*info).data->getypos()
    if (*info).ccd_ysz gt 1024 then begin
      detector=congrid((*info).detector[*, *],(*info).ccd_xsz/4, $
                    (*info).ccd_ysz/4)
    endif else begin
      detector = (*info).detector
    endelse
    ptr_free,(*info).xscale
    ptr_free,(*info).yscale
    ptr_free,(*info).drawimage
    drawimage = congrid(detector, (*info).xps, (*info).yps)
    (*info).xscale = ptr_new((*info).yps)
    (*info).yscale = ptr_new((*info).xps)
    *(*info).xscale = interpol(xscale, (*info).xps)
    *(*info).yscale = interpol(yscale, (*info).yps)
    (*info).drawimage = ptr_new(uintarr((*info).xps, (*info).yps))
    *(*info).drawimage = drawimage
  endelse

  pseudoevent={widget_button,id:0L, $
               top:event.top,handler:0l,select:1}
  spice_xdetector_draw, pseudoevent
end

pro spice_xdetector_lineplot, event
  widget_control, event.top, get_uvalue = info
  thisevent=tag_names(event,/structure_name)
  case thisevent of
    'WIDGET_DROPLIST': begin
      mode = event.index
      end
      else:
  endcase
  ; set up titles for plot
  varname = *(*info).data-> getvariablename()
  varname = varname[0]
  case mode of
    0:begin
        return
      end
    1: begin
            data = (*info).detector[*, *]
            iris_xlineplot, data, xscale = (*info).lambda, $
                       xtitle = (*info).xtitle, $
                       cslider_title = (*info).ytitle, $
                       ytitle = varname, $
                       groupl = (*info).tlb
       end
    2: begin
            data = transpose((*info).detector[*, *])
            iris_xlineplot, data, xtitle = (*info).ytitle, $
                       cslider_title = (*info).xtitle, $
                       ytitle = varname, $
                       groupl = (*info).tlb
       end
  endcase
end

; turn on/off log scaling
pro spice_xdetector_log,event
  widget_control, event.top, get_uvalue = info
  (*info).log=event.select
  if (*info).log then begin
    (*info).colorbar_title=*(*info).data->gettitle()+' '+'!3log!D10!N('+(*(*info).data->getvariableunit())+')'
  endif else begin
    (*info).colorbar_title=*(*info).data->gettitle()+' '+(*(*info).data->getvariableunit())
  endelse
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  spice_xdetector_draw, pseudoevent
end

; overplot average line spectrum
pro spice_xdetector_line,event
  widget_control, event.top, get_uvalue = info
  (*info).lineplot=event.select
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  spice_xdetector_draw, pseudoevent
end

; control real size ccd display or not:
pro spice_xdetector_realsize, event
  widget_control, event.top, get_uvalue = info
  (*info).realsize = event.select
; ghost the 'big' and 'standard' choices if realsize
  if (*info).realsize then begin
    widget_control,(*info).drawsizeoption_menu,sensitive=0
  endif else begin
    widget_control,(*info).drawsizeoption_menu,sensitive=1
  endelse
; create resize event to resize draw widget
  pseudoevent={widget_base,id:0l, $
               top:(*info).tlb, handler:0l, x:(*info).tlb_xsz, y:(*info).tlb_ysz}
  spice_xdetector_resize, pseudoevent
end

; close spice_xdetector
pro spice_xdetector_destroy, event
  widget_control, event.top,/destroy
end

pro spice_xdetector_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  wdelete, (*info).mainpixid
  ptr_free, (*info).data
  ptr_free, (*info).drawimage 
  ptr_free, (*info).xscale 
  ptr_free, (*info).yscale 
  ptr_free, info
end

pro spice_xdetector, data, lindx, group_leader = group_leader, $
               ncolors = ncolors, filename = filename
  if n_params() lt 2 then begin
    message, $
      'spice_xdetector: A data object and line index array must be given', $
       /cont
    return
  endif

  if n_elements(ncolors) eq 0 then ncolors = (!d.n_colors < 256)
  if n_elements(drawcolor) eq 0 then drawcolor=!p.color
; drawing window size in relation to screen
  screensize=get_screen_size()
  aspect=float(screensize[0])/float(screensize[1])
  xysz=(data->getaux())->getdrawsize('standard',aspect=aspect)
  d_xsz = xysz[0]
  d_ysz = xysz[1]
  nwin = n_elements(lindx)
  nslit = max(data->getnslit())
  nraster = data->getnraster(0)   ; number of raster positions
  nexp = data->getnexp(0)
  ntime = data->getntime(0)      ;
  nexpprp = data->getnexp_prp(0)  ; number of exp pr. raster pos.
; so far QL can not handle sit-and-stare with different exposure times 
; (when it is run as "multiple exp pr rast. pos. 
; Will have to deal with that...
; OW 14-april 2005.
  sit_and_stare = data->getsit_and_stare()
  if sit_and_stare then nexpprp = 1  
;
  yw=data->getyw()
  ywmax=max(data->getyw(),ic)
  ysmax=data->getys(ic)
  xw=data->getxw()
  xs=data->getxs()
  ys=data->getys()-ysmax
; Find out which detector is being viewed.
  ccd_sz=data->getccd_sz(data->getregion(lindx[0],/full))
  ccd_sz[1]=ywmax
  detector = fltarr(ccd_sz[0], ywmax)
  detector[*] = data->missing()
;
  for i=0,nwin-1 do begin
    wd=data->getvar(lindx[i])
    xss=0
    if data->getline_id(lindx[i]) eq 'FULL CCD FUV2' then xss=xs[lindx[i]]-xs[lindx[i-1]]-1
    bin_sp=data->binning_spectral(lindx[i])
;    bin_sp=data->getinfo('SUMSPTRL')
; assuming that this routine will look at FUV windows seperately
    pos0 = (xs[lindx[i]]*bin_sp+1) mod (data->getccd_sz('FUV1'))[0]
    pos1 = xw[lindx[i]]
    pos2 = ys[lindx[i]]
    pos3 = yw[lindx[i]]
; assuming that all slit positions (on each CCD) start at same
; position: if this is not the case more logic is needed above
    detector[pos0:pos0+pos1*bin_sp-1,0:pos3-1]= $
      congrid(data->descale_array(wd[xss:xss+pos1-1, *, 0]),pos1*bin_sp,pos3)
  endfor
  lambda=data->getlambda(data->getregion(lindx[0],/full),wscale=data->getwscale())
  wnames=data->getline_id(lindx)
; x and y titles for axis plots:
  xdim = 0   ; wavelength
  ydim = 1   ; slit pos
  xtitle = data->getxytitle(xdim) ; wavelength
  ytitle = data->getxytitle(ydim) ; slit position
; initialize size of draw window (ccd display):
  window,/pixmap,/free,xsize=d_xsz,ysize = d_ysz
  pixid = !d.window
; =======================================================================================
; Set up the widgets
; base widget:
  xwt = 'spice_xdetector - ' + data->getregion(lindx[0],/full) + ' '+data->getfilename()  ; spice_xdetector window title
  tlb = widget_base(/row, title=xwt, tlb_size_events = 1, mbar=menubar, $
                    xoffset = 100, yoffset=100, group_leader=group_leader)
  
  lcol = widget_base(tlb, /frame, /column)      ;left column.
  rcol = widget_base(tlb, /column)              ;right column.

; create pulldown menus on the base widget menubar
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', uvalue='save', /menu)
  psmenu=widget_button(savemenu, value='Postscript', $
                       event_pro = 'spice_xdetector_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'spice_xdetector_jpeg')
  exitmenu=widget_button(filemenu, value='Close', $
                         event_pro='spice_xdetector_destroy')

  optmenu=widget_button(menubar,value='Options', uvalue='options')
  colmenu=widget_button(optmenu, value='Colour table', $
                                 event_pro='spice_xdetector_colors')
  animenu=widget_button(optmenu, value='Create Animation', $
                                 event_pro='spice_xdetector_control_anim')
  wscalemenu=widget_button(optmenu, value='Change wavelength scale',/menu)
  angstr = string("305B)+'ngstr'+string("370B)+'m'
  pixmenu=widget_button(wscalemenu, value='Pixels',event_pro='spice_xdetector_wpix')
  angstrmenu=widget_button(wscalemenu, value=angstr,event_pro='spice_xdetector_wangstr')

;display window:
  displaybase = widget_base(rcol, /row)
  drawid=widget_draw(displaybase, retain = 2,$
                     xsize = d_xsz, ysize = d_ysz, $
                     x_scroll_size = d_xsz, y_scroll_size = d_ysz, $
                     /button_events, event_pro='spice_xdetector_zoom')
  colorbar_title=data->gettitle()+' '+(data->getvariableunit())
; create menu for controlling action in draw window
  dwoption = widget_base(lcol, /column, /frame)
  dwoption_title = widget_label(dwoption, value = 'Window action')
  dwoption_names=['Zoom','Average along wavelength', 'Average along slit']
  dwoption_menu = cw_bgroup(dwoption, dwoption_names, /return_index, $
                            /exclusive, set_value = 0, $
                            event_func = 'spice_xdetector_dwoption')

  titletext = widget_label(lcol,value = data->getdate_obs()+' '+data->getobsid(),/align_center)
  
  lsubcol0 = widget_base(lcol, /row)
  sliderbase = widget_base(lsubcol0,/col)
  expslider = -1
  if nexp gt 1 then begin
    if nexpprp le 1 then begin 
      nr = nexp
      title = 'Exposure nr'
      expslider = widget_slider(sliderbase, xsize=90, $
                              minimum=0, maximum=nr-1, $
                              title='Exp # ', $
                              value=0, $
                              event_pro='spice_xdetector_expslider',/drag)
    endif else begin
      exprp = 1
      expprpslider = widget_slider(sliderbase, xsize = 120, $
                                minimum = 1, maximum = nexpprp, $
                                title = 'Exp # at rast. pos.', $
                                value = 1, $
                                event_pro = 'spice_xdetector_expprp_slider')

      rpos = 1
      rastposslider = widget_slider(sliderbase, xsize = 120, $
                                minimum = 1, maximum = nraster, $
                                title = 'Raster position', $
                                value = 1, $
                                event_pro = 'spice_xdetector_rast_slider')
    endelse
  endif
  
  exposurebase = widget_base(lsubcol0,/col)
  exposuretext = widget_label(exposurebase, $
    value = strtrim('Exp time: '+string(data->getexp(0),format='(f7.1)')+' s',2), $
    /align_left)
  
  time=data->gettime()
  timebase = widget_base(exposurebase,/col)
  timetext = widget_label(timebase, $
    value = strtrim('Time    : '+string(time[0],format='(f7.1)')+' s',2), $
    /align_left)
  
  pztx=data->getpztx()
  fmirrbase = widget_base(exposurebase,/col)
  fmirrxtext = widget_label(fmirrbase, $
    value = strtrim('PZTX: '+ string(pztx[0],format='(f7.2)'),2)+' arcsec', $
    /align_left)
  
  pzty=data->getpzty()
  fmirrytext = widget_label(fmirrbase, $
    value = strtrim('PZTY: '+ string(pzty[0],format='(f7.2)'),2)+' arcsec', $
    /align_left)
    
  xycenbase = widget_base(exposurebase,/col)
  xycentext = widget_label(xycenbase, $
      value = 'Xcen: '+ string((data->getxcen()),format='(i4)')+ $
             ' Ycen: '+ string((data->getycen()),format='(i4)'), $
      /align_left)

  pixplotfield = widget_base(lcol, /column, /frame)
  pixnames = ['Not active', 'Row plot', 'Column plot']
  pixelplot = widget_droplist(pixplotfield, value = pixnames, $
                              title = 'Plot pixel values', $
                              event_pro = 'spice_xdetector_lineplot')
                              
  lsubcol1 = widget_base(lcol, /row)
  colorbase = widget_base(lsubcol1,/col)
  colorbutton=widget_button(colorbase, value='Colour table', $
                                 event_pro='spice_xdetector_colors')
  animbase = widget_base(lsubcol1,/col)
  animbutton=widget_button(animbase, value='Create Animation', $
                                event_pro='spice_xdetector_control_anim')
                  
  lsubpix=widget_base(lcol,/row)              
  wlbase = widget_base(lsubpix, /column, /frame)
  angstr = string("305B)+'ngstr'+string("370B)+'m'
  wl_names = ['Pixels', angstr]
  wlbutton = cw_bgroup(wlbase, wl_names, /return_index, $
                            /exclusive, set_value = 0, $
                            event_func = 'spice_xdetector_wloption')
                    
  slbase = widget_base(lsubpix, /column, /frame)
  sl_names = ['Pixels', 'arcsec']
  slbutton = cw_bgroup(slbase, sl_names, /return_index, $
                            /exclusive, set_value = 0, $
                            event_func = 'spice_xdetector_sloption')
                                
  lsubcol2 = widget_base(lcol, /row)
  linefield = widget_base(lsubcol2, /column, /nonexclusive)
  linebutton = widget_button(linefield, $
                   value = 'Line Plot', $
                    event_pro = 'spice_xdetector_line')
                               
  logfield = widget_base(lsubcol2, /column, /nonexclusive)
  logbutton = widget_button(logfield, $
                   value = 'log(image)', $
                    event_pro = 'spice_xdetector_log')

  realsizefield = widget_base(lcol, /column, /nonexclusive)
  realsizebutton = widget_button(realsizefield, $
                   value = 'Real size CCD display', $
                    event_pro = 'spice_xdetector_realsize')

  drawsizeoption = widget_base(lcol, /column, /frame)
;  drawsizeoption_title = widget_label(drawsizeoption, value = 'Resize widget')
  menu = ['Standard','Big']
  drawsizeoption_names = menu
  drawsizeoption_menu = cw_bgroup(drawsizeoption, drawsizeoption_names, $
                            /return_index, $
                            /exclusive, set_value = 0, $
                            event_func = 'spice_xdetector_drawsizeoption')

  closefield = widget_base(lcol, /column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'spice_xdetector_destroy')
                              
  ;; eis_icon_base=widget_base(lcol,/col)
  ;; eis_icon                   = widget_draw(eis_icon_base  ,             $
  ;;                               retain = 2,                             $
  ;;                               XSize       = 210,                      $
  ;;                               YSize       = 105,                      $
  ;;                               frame       = 1)  

  ; realize main window:
  widget_control, tlb, /realize, tlb_get_size = tlb_sz

  ; set realsizebutton to de-select:
  widget_control, realsizebutton, set_button = 0
  imagepos = [0.1, 0.1, 0.9, 0.95]
  ; define size of widget and the menu column
  tlb_xsz = tlb_sz[0]  ; xsize of whole widget in pixels
  tlb_ysz = tlb_sz[1]  ; ysize of whole widget in pixels
  lcol_xsz = tlb_xsz - d_xsz

  ; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid

  ;get and save color table
  tvlct, r, g, b, /get
  bottom = 0
  if (!d.n_colors le 256) then begin
    r = r[bottom:ncolors-1+bottom]
    g = g[bottom:ncolors-1+bottom]
    b = b[bottom:ncolors-1+bottom]
  endif

  ; define the info structure, used send information around
  info = {detector:detector, $
          drawimage:ptr_new(), $
          xscale:ptr_new(), $
          yscale:ptr_new(), $
          data:ptr_new(), $
          exposuretext:exposuretext, $
          timetext:timetext, $
          fmirrxtext:fmirrxtext, $
          fmirrytext:fmirrytext, $
          xycentext:xycentext, $
          ccd_xsz:ccd_sz[0], $
          ccd_ysz:ccd_sz[1], $
          xdim:0, $
          ydim:1, $
          nwin:nwin, $
          lindx:lindx, $
          line:0, $
          nraster:nraster, $
          nexp:0, $
          nexpprp:nexpprp, $
          exprp:1, $
          rpos:1, $
          ntime:ntime,  $
          nslit:nslit, $
          expnr:0, $
          imagepos:imagepos, $
          lambda:lambda, $
          screensize:screensize, $
          log:0, $
          lineplot:0, $
          realsize:0, $
          lcol_xsz:lcol_xsz, $
          d_xsz:d_xsz, $
          d_ysz:d_ysz, $
          xps:0, $
          yps:0, $
          xvs:0, $
          yvs:0, $
          tlb:tlb, $
          tlb_xsz:tlb_xsz, $
          tlb_ysz:tlb_ysz, $
          lcol:lcol, $
          rcol:rcol,  $
          r:r, g:g, b:b, $
          bottom:bottom, $
          ncolors:ncolors, $
          sx:0, $
          sy:0, $
          linelist:wnames, $
          dwoption:0, $
          dwoption_menu:dwoption_menu, $
          drawsizeoption_menu:drawsizeoption_menu, $
          animenu:animenu, $
          drawid:drawid, $
          colorbar_title:colorbar_title,$
          pixelplot:pixelplot,  $
          drawcolor:drawcolor, $
          mainpixid:pixid, $
          pixid:pixid, $
          xtitle:xtitle, $
          ytitle :ytitle, $
          wid:wid}

  info = ptr_new(info, /no_copy)
  (*info).data=ptr_new(data)
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info

  ; create pseudoevent and send this event to spice_xdetector_draw,
  ; in order to draw the image
  pseudoevent={widget_base,id:0l, $
               top:tlb, handler:0l, x:tlb_xsz, y:tlb_ysz}
  spice_xdetector_resize, pseudoevent
;  spice_xdetector_draw, pseudoevent

                       
  ;; widget_control, eis_icon , get_value = drawID
  ;; wset,drawID
  ;; fileName = concat_dir(GETENV('ancillary') , 'eis_logo_sarah_small.jpg')
  ;; read_jpeg , filename , icon
  ;; icon_resized = CONGRID(icon,3,210,105)
  ;; tvscl,icon_resized , true = 1

  xmanager, 'ql', tlb, /no_block, event_handler = 'spice_xdetector_resize', $
            group_leader = group, cleanup = 'spice_xdetector_cleanup'

end

