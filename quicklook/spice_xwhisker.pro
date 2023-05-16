;+
; NAME:
;       SPICE_XWHISKER
;
; PURPOSE:
;
;       spice_xwhisker is used to display 2-D spectroscopic data as whisker plots
;       (images). I.e. Intensity[wavelength, y].
;       Typically y will be solar_x (for a raster) or
;       for sit-and-stare observations it will be time.
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xwhisker, data, line [, group_leader = group_leader, ncolors = ncolors]
;
; INPUTS:
;       data: Can be either the name and path of a SPICE data file,
;             or a SPICE data object.
;       line: The index of the line window to be displayed
;
; KEYWORD PARAMETERS:
;       group_leader: Widget parent (if any).
;       ncolors: Number of colors for xraster. Default is !d.n_colors<256.
;
; OUTPUTS:
;       None
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       spice_xwhisker defines the widgets and displays data. It has several
;       options for displaying data in different modes, zooming,
;       selecting colors, file output etc. It is a QL-tool for
;       displaying data of 2 dimensions. spice_xwhisker consists
;       of many functions that is called whenever the user does
;       something in the widget program.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       2002-august: Oivind Wikstol. 1. version
;       20-APR-2004: Oivind Wikstol - Added funtions to change wavl.
;                                     scale [pix/Angstr.]
;       29-Sep-2007: Alessandro Gardini - Pointers freed by cleanup. Other
;                                     changes already made on Jun-2007.
;        3-Dec-2007: A. Gardini     - Freed pointers.
;       21-Jan-2013: V. Hansteen    - Rewritten for IRIS as iris_xwhisker
;       28-Jan-2020: M. Wiesmann    - Rewritten for SPICE as spice_xwhisker
;
;-
; $Id: 2023-05-16 15:03 CEST $


; save as postscript file
pro spice_xwhisker_ps,event
  thisfile=dialog_pickfile(/write,file='spice_xwhisker.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xwhisker_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro spice_xwhisker_jpeg,event
  thisfile=dialog_pickfile(/write,file='spice_xwhisker.jpg')
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
pro spice_xwhisker_draw, event
  widget_control, event.top, get_uvalue = info
  if !d.name ne 'PS' then begin
    wset, (*info).wid
    bgblack=1
    ticklen=0.02
  endif else begin
    bgblack=0
    ticklen=-0.02
  endelse
  widget_control, (*info).drawid, xsize = (*info).d_xsz, $
    ysize = (*info).d_ysz
  ; make new drawimage and axes
  sz = size((*info).image)
  ;region=*(*info).data->getregion((*info).line,/full)
  ;px=*(*info).data->getlambda(region,wscale='pixels')
  ;xscale=*(*info).data->getlambda(region)
  ;pos=(*info).pos
  ;  pos[0]=pos[0]-(*(*info).data->getccd(region))[0]
  ;xscale=xscale[pos[0]-px[0]:pos[0]-px[0]+pos[1]-1]
  sit_and_stare=(*info).sit_and_stare
  if ~(*info).xdim_unit then begin
    if sz[0] eq 1 then xpos = [-0.5, 0.5] $
    else xpos=indgen(sz[1])
  endif else begin
    xpos=*(*info).data->get_lambda_vector((*info).line)
    if N_ELEMENTS(xpos) eq 1 then begin
      cdelt = *(*info).data.get_resolution((*info).line,/lambda) / 2.0
      xpos = [xpos-cdelt, xpos+cdelt]
    endif
  endelse
  xscale=xpos
  if ~(*info).ydim_unit then begin
    ypos=indgen(sz[2])
  endif else begin
    if sit_and_stare then ypos=*(*info).data->get_time_vector((*info).line) $
    else ypos=*(*info).data->get_instr_x_vector((*info).line)
  endelse
  yscale=ypos
  if sz[0] eq 1 then begin
    drawimage = [[(*info).image], [(*info).image]]
    drawimage = transpose(drawimage, [1,0])
  endif else drawimage = (*info).image
  drawimage = congrid(drawimage, (*info).d_xsz, (*info).d_ysz)
  (*info).xticks=fix((*info).d_xsz/100)
  sz = size(drawimage)
  ptr_free,(*info).xscale
  ptr_free,(*info).yscale
  ptr_free,(*info).ypscale
  ptr_free,(*info).drawimage
  (*info).drawimage = ptr_new(uintarr(sz(1), sz(2)))
  *(*info).drawimage = drawimage^(*info).gamma
  (*info).xscale = ptr_new(sz(1))
  (*info).ypscale = ptr_new(sz(1))
  (*info).yscale = ptr_new(sz(2))
  *(*info).xscale = interpol(xscale, sz(1))
  *(*info).yscale = interpol(yscale, sz(2))
  *(*info).ypscale = interpol(yscale, sz(1))
  mplot_image,*(*info).drawimage,min=(*info).imin,max=(*info).imax, $
    *(*info).xscale, *(*info).yscale, $
    xstyle = 1, ystyle = 1, position = (*info).imagepos, $
    xtitle = (*info).xtitle, ytitle = (*info).ytitle, $
    xticks=(*info).xticks, xminor =(*info).xticks*2,bgblack=bgblack,ticklen=ticklen, /old
  ;create colorbar:
  ymin = (*info).imin
  ymax = (*info).imax
  if ymax-ymin eq 0.0 then ymax=ymin+1
  format='(f10.1)'
  if ymax-ymin lt 10 then format='(f7.4)'
  hw_colorbar, position = [((*info).imagepos)[2]+0.02, $
    ((*info).imagepos)[1], $
    ((*info).imagepos)[2]+0.05, $
    ((*info).imagepos)[3]], range = [ymin, ymax], $
    /vertical , /right, format=format, title=(*info).colorbar_title

end

function spice_xwhisker_gamma, event
  widget_control, event.top,get_uvalue=info
  (*info).gamma=event.value
  im_min=0.0
  (*info).imin=min(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing)>im_min)^(*info).gamma
  (*info).imax=max(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing)>im_min)^(*info).gamma
  if (*info).imax eq im_min then begin
    (*info).imin=min(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing))
    (*info).imax=max(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing))
    (*info).gamma=1.0
    text='All data < im_min '+strtrim(string(im_min,format='(f4.2)'),2)+' gamma reset to 1.0'
    message,text,/info
    ok = dialog_message(text,dialog_parent=(*info).tlb)
  endif
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xwhisker_draw,pseudoevent
  return,0
end

function spice_xwhisker_histoopt, event
  widget_control, event.top,get_uvalue=info
  (*info).histo_lim=10.0^(event.value)
  im_min=0.0
  (*info).imin=min(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing)>im_min)^(*info).gamma
  (*info).imax=max(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing)>im_min)^(*info).gamma
  if (*info).imax eq im_min then begin
    (*info).imin=min(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing))
    (*info).imax=max(iris_histo_opt((*info).image,(*info).histo_lim,missing=(*info).missing))
  endif
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xwhisker_draw,pseudoevent
  return,0
end

; get the value of the draw window option menu:
function spice_xwhisker_dwoption, event
  widget_control, event.top, get_uvalue = info
  (*info).dwoption = event.value
  return, 0
end

; slider to select exposure within a raster pos (if multiple)
pro spice_xwhisker_expprp_slider, event
  widget_control, event.top,get_uvalue=info
  (*info).exprp=event.value
  (*info).expindx = indgen((*info).nraster)*(*info).nexpprp + (*info).exprp - 1
  nr=(*info).exprp-1
  wd=*(*info).wd
  (*info).image = reform(wd[*,(*info).slitpos,(*info).expindx])
  good=finite((*info).image)
  if (where(good))[0] eq -1 then begin
    message,'All data is NaN for expprp '+string(nr),/info
    sz=size((*info).image)
    image=fltarr(sz[1],sz[2])+(*info).missing
  endif
  widget_control,(*info).exposuretext, $
    set_value=strtrim('Exp time: '+string((*(*info).data->getexp())[nr], $
    format='(f7.1)')+' s',2)
  rot=round(*(*info).data->getinfo('SAT_ROT'))
  if rot < 0 then rot=360+rot
  if rot eq 90 or rot eq 270 then begin
    pzty=(*(*info).data->getxpos((*info).line))[(*info).slitpos]
  endif else begin
    pzty=(*(*info).data->getypos((*info).line))[(*info).slitpos]
  endelse
  widget_control,(*info).fmirrytext, $
    set_value = 'Y: '+ string(pzty,format='(f10.3)')+' arcsec'
  ; display new exposure nr
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xwhisker_draw,pseudoevent
end

; slider to select raster position
pro spice_xwhisker_slitslider, event
  widget_control, event.top,get_uvalue=info
  (*info).slitpos=event.value
  wd=*(*info).wd
  if (*info).nexpprp le 1 then begin
    (*info).image = reform(wd[*,(*info).slitpos,*])
  endif else begin
    (*info).image = reform(wd[*,(*info).slitpos,(*info).expindx])
  endelse
  good=finite((*info).image)
  if (where(good))[0] eq -1 then begin
    message,'All data is NaN for expprp '+string(nr),/info
    sz=size((*info).image)
    image=fltarr(sz[1],sz[2])+(*info).missing
  endif
  rot=round(*(*info).data->get_satellite_rotation())
  if rot < 0 then rot=360+rot
  if rot eq 90 or rot eq 270 then begin
    pzty=*(*info).data->get_instr_x_vector((*info).line)
    slittxt='X: '
  endif else begin
    pzty=*(*info).data->get_instr_y_vector((*info).line)
    slittxt='Y: '
  endelse
  widget_control,(*info).fmirrytext, $
    set_value = slittxt + string(pzty[(*info).slitpos],format='(f10.3)')+' arcsec'
  ; display new raster position
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xwhisker_draw,pseudoevent
end

; zoom in draw window:
pro spice_xwhisker_zoom, event
  widget_control,event.top,get_uvalue=info
  if event.type gt 2 then return
  events=['down','up','motion']
  thisevent=events[event.type]
  window, /pixmap, /free, xsize = (*info).d_xsz, ysize = (*info).d_ysz
  xs = ((*info).imagepos)[0]*(*info).d_xsz
  ys = ((*info).imagepos)[1]*(*info).d_ysz
  mplot_image,*(*info).drawimage,min=(*info).imin,max=(*info).imax, $, $
    *(*info).xscale, *(*info).yscale, $
    xstyle = 1, ystyle = 1, position = (*info).imagepos, $
    xtitle = (*info).xtitle, ytitle = (*info).ytitle, $
    xticks=(*info).xticks, xminor =(*info).xticks*2,/bgblack
  ;
  ymin = (*info).imin
  ymax = (*info).imax
  if ymax-ymin eq 0.0 then ymax=ymin+1
  format='(f10.1)'
  if ymax-ymin lt 10 then format='(f7.4)'
  hw_colorbar, position = [((*info).imagepos)[2]+0.02, $
    ((*info).imagepos)[1], $
    ((*info).imagepos)[2]+0.05, $
    ((*info).imagepos)[3]], range = [ymin, ymax], $
    /vertical , /right, format=format, title=(*info).colorbar_title
  ;
  imagepos=(*info).imagepos
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
      image = *(*info).drawimage
      sz=size(image)
      dxfac = float(sz[1])/(imagepos[2]-imagepos[0])/float((*info).d_xsz)
      dyfac = float(sz[2])/(imagepos[3]-imagepos[1])/float((*info).d_ysz)
      sx=((*info).sx-imagepos[0]*(*info).d_xsz)*dxfac
      sy=((*info).sy-imagepos[1]*(*info).d_ysz)*dyfac
      dx=(event.x-imagepos[0]*(*info).d_xsz)*dxfac
      dy=(event.y-imagepos[1]*(*info).d_ysz)*dyfac
      sx = (sx < (*info).d_xsz - 1) > 0
      sy = (sy < (*info).d_ysz - 1) > 0
      dx = (dx < (*info).d_xsz - 1) > 0
      dy = (dy < (*info).d_ysz - 1) > 0
      image=image[sx<dx:sx>dx,sy<dy:sy>dy]
      xscale = *(*info).xscale
      yscale = *(*info).yscale
      xscale = xscale[sx<dx:sx>dx]
      yscale = yscale[sy<dy:sy>dy]
      sz=size(image)
      mind = min(sz[0:2])
      pos=[sx<dx,sx>dx,sy<dy,sy>dy]
      case (*info).dwoption of
        0:begin
          if mind ge 2 then begin
            xmax = (*info).screensize[0]
            ymax = (*info).screensize[1]
            image=congrid(image,sz[1]*2 < xmax, sz[2]*2 < ymax)
            xscale = interpol(xscale, sz[1]*2 < xmax)
            yscale = interpol(yscale, sz[2]*2 < ymax)
            spice_xzoom, image, xscale, yscale, xtitle = (*info).xtitle, $
              ytitle = (*info).ytitle, group_leader=event.top, n_subplot=(*info).n_subplot
          endif
        end
        1:begin
          ;set up axis titles for line plots (options 1 or 2 below)
          varname = *(*info).data->get_variable_type()
          varname = varname[0] +': column average'
          dmean = total(image, 1)/sz[1]
          if sz[0] ge 2 then begin
            spice_xlineplot, dmean, xscale = yscale, $
              title = varname, $
              xtitle = (*info).xtitle, $
              ytitle = varname, $
              groupl = event.top, n_subplot=(*info).n_subplot
          endif
        end
        2:begin
          ;set up axis titles for line plots (options 1 or 2 below)
          varname = *(*info).data->get_variable_type()
          varname = varname[0] +': row average'
          dmean = total(image, 2)/sz[2]
          if sz[0] ge 2 then begin
            spice_xlineplot, dmean, xscale = xscale, $
              title = varname, $
              xtitle = (*info).xtitle, $
              ytitle = varname, $
              groupl = event.top, n_subplot=(*info).n_subplot
          endif
        end
      endcase
      (*info).n_subplot = (*info).n_subplot + 1
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
      plots,[sx,sx,dx,dx,sx],[sy,dy,dy,sy,sy],/device, $
        color=(*info).drawcolor
    endcase
  endcase
  wdelete, (*info).pixid
  noaction:
end

; create animation widget and launch animation
pro spice_xwhisker_anim, event
  print,'does NOT work yet'
  return
  widget_control, event.top, get_uvalue = info
  if 1.0 eq swap_endian(1.0,/swap_if_big_endian) then swap=1
  iris_ximovie,*(*info).data->get_filename(),group_leader=(*info).tlb, $
    *(*info).data->getxw((*info).line),*(*info).data->getyw((*info).line), $
    nframes=*(*info).data->getnraster((*info).line), $
    offset=*(*info).data->getposition((*info).line),/float,swap=swap, $
    magnification=0.9,missing=*(*info).data->missing()

  ;;   stop
  ;;   wd=(*(*info).data)->getvar((*info).line,/load)

  ;;   sz = size(wd)
  ;;   ndim = sz[0]
  ;;   xsize = sz[1]
  ;;   ysize = sz[2]

  ;;   if ndim lt 3 then begin
  ;;     ok = dialog_message('Data array must be 3-D to make animation!')
  ;;     return
  ;;   endif
  ;; ; bytscale data to save time in animation tool
  ;; ;  wdb = bytscl(iris_histo_opt(wd,1.e-2,missing=*(*info).data->missing()))
  ;; ; write data to assoc file:
  ;;   ct=0
  ;;   repeat begin
  ;;     ct=ct+1
  ;;     assoc_file = IRISxfiles_appReadme()+'/spice_xwhisker_ximovie_'+strtrim(string(ct),2)+'.tmp'
  ;;   endrep until ((findfile(assoc_file))[0] eq '')
  ;;   if ct gt 99 then begin
  ;;     message,'more than 100 temporary assoc files stored in',/info
  ;;     message,IRISxfiles_appReadme()+'/spice_xdetector_ximovie_XX.tmp. Consider purge!',/info
  ;;   endif
  ;;   openw, lu, assoc_file, /get_lun
  ;;   rec = assoc(lu, wd)
  ;;   rec[0] = wd
  ;;   close, lu & free_lun, lu
  ;; ; start iris_ximovie, with the delete keyword (afile is removed from disc
  ;; ; when iris_ximovie is closed
  ;;   iris_ximovie, assoc_file, xsize, ysize, group_leader = (*info).tlb, $
  ;;     /fdelete,magnification=0.9,missing=*(*info).data->missing(),/float
  return
end

; change spatial scale to pixels
pro spice_xwhisker_spix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data->get_axis_title((*info).ydim, /pixels)
  (*info).ydim_unit = 0
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xwhisker_draw, pseudoevent
end

; change spatial scale to arcsec
pro spice_xwhisker_sarcsec, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data->get_axis_title((*info).ydim)
  (*info).ydim_unit = 1
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xwhisker_draw, pseudoevent
end

; change wavelength scale to pixels
pro spice_xwhisker_wpix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data->get_axis_title((*info).xdim, /pixels)
  (*info).xdim_unit = 0
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xwhisker_draw, pseudoevent
end

; change wavelength scale to Angstrom
pro spice_xwhisker_wangstr, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data->get_axis_title((*info).xdim)
  (*info).xdim_unit = 1
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xwhisker_draw, pseudoevent
end

; select color table
pro spice_xwhisker_colors, event
  widget_control, event.top, get_uvalue=info
  thisevent = tag_names(event, /structure_name)
  case thisevent of
    'WIDGET_BUTTON': begin
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'spice_xwhisker colors (' + strtrim((*info).wid, 2) + ')', $
        group_leader = event.top, notifyid = [event.id, event.top]
    endcase
    'XCOLORS_LOAD': begin
      (*info).r = event.r((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).g = event.g((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).b = event.b((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      if !d.n_colors gt 256 then begin
        pseudoevent={widget_button,id:0L, $
          top:event.top, handler:0l, select:1}
        spice_xwhisker_draw, pseudoevent
      endif
    endcase
  endcase
  widget_control, event.top, set_uvalue = info
end

; protect colors
pro spice_xwhisker_protect_colors,event
  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
end

; resize main window
pro spice_xwhisker_resize, event
  widget_control, event.top ,get_uvalue = info
  (*info).d_xsz = (event.x - (*info).lcol_xsz) > 0
  (*info).d_ysz = event.y
  widget_control, (*info).drawid, xsize = (*info).d_xsz, $
    ysize = (*info).d_ysz
  pseudoevent={widget_button,id:0L, $
    top:event.top,handler:0l,select:1}
  spice_xwhisker_draw, pseudoevent
end

pro spice_xwhisker_lineplot, event
  widget_control, event.top, get_uvalue = info
  thisevent=tag_names(event,/structure_name)
  case thisevent of
    'WIDGET_DROPLIST': begin
      mode = event.index
    end
    else:
  endcase
  ; set up titles for plot
  varname = *(*info).data->get_variable_type()
  varname = varname[0]
  case mode of
    0: begin
      return
    end
    1: begin
      data = (*info).image[*,*]
      spice_xlineplot, data, xscale = *(*info).xscale, $
        xtitle = (*info).xtitle, $
        cslider_title = (*info).ytitle, $
        ytitle = varname, groupl = (*info).tlb
    end
    2: begin
      data = transpose((*info).image[*,*])
      spice_xlineplot, data, xtitle = (*info).ytitle, $
        cslider_title = (*info).xtitle, ytitle = varname, $
        groupl = (*info).tlb
    end
  endcase
end


pro spice_xwhisker_mask, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).maskbutton, get_value=masking
  print,masking
  wd = *(*info).data->get_window_data((*info).line, no_masking=masking eq 0)
  image = reform(wd[*,(*info).slitpos,*,*])
  if *(*info).data->get_missing_value() ne *(*info).data->get_missing_value() then missing=-99999L $
  else missing=*(*info).data->get_missing_value()
  wd=iris_histo_opt(wd,missing=missing)
  imin=min(wd)
  imax=max(wd)
  image=iris_histo_opt(image)
  *(*info).wd = wd
  (*info).image = image
  pseudoevent={widget_button,id:0L, $
    top:event.top,handler:0l,select:1}
  spice_xwhisker_draw, pseudoevent
end


; close spice_xwhisker
pro spice_xwhisker_destroy, event
  widget_control, event.top,/destroy
end


pro spice_xwhisker_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  wdelete, (*info).mainpixid
  ;  free_lun, (*info).alu
  ptr_free, (*info).data
  ptr_free, (*info).wd
  ptr_free, (*info).drawimage
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, (*info).ypscale
  ptr_free, info
end


pro spice_xwhisker , input_data, line, group_leader = group_leader, $
  ncolors = ncolors

  if n_params() lt 2 then begin
    message, $
      'spice_xwhisker,data,line, group_leader = group,ncolors = ncolors',/cont
    return
  endif

  data = spice_get_object(input_data, is_spice=is_spice, object_created=object_created)
  if ~is_spice then return

  if n_elements(ncolors) eq 0 then ncolors = (!d.n_colors < 256)
  if n_elements(drawcolor) eq 0 then drawcolor=!p.color
  ; drawing window size in relation to screen
  if n_elements(scfac) eq 0 then scfac=0.6
  screensize=get_screen_size()
  sz=screensize*scfac
  d_xsz = sz[1]/1.5
  d_ysz = sz[0]/1.4
  ;
  sit_and_stare = data->get_sit_and_stare()
  nslit=data->get_header_keyword('NAXIS2', line)
  nraster = data->get_number_exposures(line)
  nexpprp = 1 ;data->getnexp_prp(line)  ; number of exp pr. raster pos.
  slitpos = nslit/2
  ; so far QL can not handle sit-and-stare with different exposure times
  ; (when it is run as "multiple exp pr rast. pos.)
  ; Will have to deal with that...
  ; OW 14-april 2005.
  if sit_and_stare then nexpprp = 1
  message = ['Loading data into memory...','...this may take some time']
  xmessage,message,wbase=wbase,font='helvetica'
  widget_control,/hourglass
  wd = data->get_window_data(line)
  xkill,wbase
  if nexpprp le 1 then begin
    image = reform(wd[*,slitpos,*,*])
    expindx = 0
  endif else begin
    expnr_at_rp = 1 ; intitialize first exp at each raster pos.
    expindx = indgen(nraster)*nexpprp + expnr_at_rp - 1
    image = reform(wd[*, slitpos, expindx])
  endelse
  if data->get_missing_value() ne data->get_missing_value() then missing=-99999L $
  else missing=data->get_missing_value()
  wd=iris_histo_opt(wd,missing=missing)
  imin=min(wd)
  imax=max(wd)
  image=iris_histo_opt(image)
  ; initialize size of draw window
  sz = size(wd)
  ndim = sz[0]
  xsz = sz[1]
  nlam = xsz
  ysz = sz[3]
  ;
  xdim = 2
  if sit_and_stare then ydim = 3 else ydim = 0
  xtitle = data->get_axis_title(xdim)
  ytitle = data->get_axis_title(ydim)
  window, /pixmap, /free, xsize = xsz, ysize = ysz
  tv, bytscl(image, top = ncolors)
  pixid = !d.window
  ; base widget:
  xwt = 'SPICE_Xwhisker - '+data->get_filename()   ; spice_xwhisker window title
  tlb = widget_base(/row, title=xwt, tlb_size_events = 1, $
    mbar=menubar, xoffset=100, yoffset=100,group_leader=group_leader)
  lcol = widget_base(tlb, /frame, /column)      ;left column.
  rcol = widget_base(tlb, /column)      ;right column.

  ; create pulldown menus on the base widget menubar
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', uvalue='save', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'spice_xwhisker_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'spice_xwhisker_jpeg')
  exitmenu=widget_button(filemenu, value='Close', event_pro='spice_xwhisker_destroy')

  optmenu=widget_button(menubar,value='Options', uvalue='options')
  colmenu=widget_button(optmenu, value='Colour table', $
    event_pro='spice_xwhisker_colors')
  ;animenu=widget_button(optmenu, value='Create Animation', $
  ;  event_pro='spice_xwhisker_anim')
  wscalemenu=widget_button(optmenu, value='Change wavelength scale',/menu)
  angstr = string("305B)+'ngstr'+string("370B)+'m'
  pixmenu=widget_button(wscalemenu, value='Pixels',event_pro='spice_xwhisker_wpix')
  angstrmenu=widget_button(wscalemenu, value=angstr,event_pro='spice_xwhisker_wangstr')
  sscalemenu=widget_button(optmenu, value='Change spatial scale',/menu)
  pixmenu=widget_button(sscalemenu, value='Pixels',event_pro='spice_xwhisker_spix')
  angstrmenu=widget_button(sscalemenu, value='arcsec',event_pro='spice_xwhisker_sarcsec')
  ; display window:
  displaybase = widget_base(rcol, /row)
  drawid=widget_draw(displaybase, retain = 2,$
    xsize = d_xsz, ysize = d_ysz, $
    /button_events, event_pro='spice_xwhisker_zoom')
  ;
  colorbar_title=data->get_title()+' '+(data->get_variable_unit())
  ; create menu for controlling action in draw window
  dwoption = widget_base(lcol, /column, /frame)
  dwoption_title = widget_label(dwoption, value = 'Window action')
  menu = ['Zoom','Average along wavelength', 'Average along slit',$
    'Average along raster posistion', 'Average in Time']
  if sit_and_stare then begin
    dwoption_names = [menu[0], menu[2], menu[4]]
  endif else begin
    dwoption_names = [menu[0], menu[2], menu[3]]
  endelse
  dwoption_menu = cw_bgroup(dwoption, dwoption_names, /return_index, $
    /exclusive, set_value = 0, $
    event_func = 'spice_xwhisker_dwoption')

  titletext = widget_label(lcol,value = data->get_start_time()+' '+data->get_obs_id(),/align_center)

  lsubcol = widget_base(lcol, /row)
  sliderbase = widget_base(lsubcol,/col)

  if nexpprp gt 1 then begin
    exprp = 1
    expprpslider = widget_slider(sliderbase, xsize = 90, $
      minimum = 1, maximum = nexpprp, $
      title = 'Exp.# at rast. pos.', $
      value = 1, $
      event_pro = 'spice_xwhisker_expprp_slider')
  endif

  id = data->get_window_id(line)
  idbase = widget_base(lsubcol,/col)
  idtext = widget_label(idbase,value = strtrim(id,2),/align_left)

  exposurebase = widget_base(idbase,/col)
  exposuretext = widget_label(exposurebase, $
    value = strtrim('Exp time: '+string((data->get_exposure_time(line)),format='(f7.1)')+' s',2), $
    /align_left)

  xycenbase = widget_base(exposurebase,/col)
  xycentext = widget_label(xycenbase, $
    value = 'Xcen: '+ string((data->get_header_keyword('crval1', line)),format='(f10.3)')+ $
    ' Ycen: '+ string((data->get_header_keyword('crval2', line)),format='(f10.3)'), $
    /align_left)

  rot=data->get_satellite_rotation()
  if N_ELEMENTS(rot) eq 0 then rot=0
  rot=round(rot)
  if rot lt 0 then rot=360+rot
  if rot eq 90 or rot eq 270 then begin
    pzty=data->get_instr_x_vector(line)
    slittxt='X: '
  endif else begin
    pzty=data->get_instr_y_vector(line)
    slittxt='Y: '
  endelse
  fmirrytext = widget_label(exposurebase, $
    value = strtrim(slittxt+ string(pzty[slitpos],format='(f10.3)'),2)+' arcsec', $
    /align_left)

  title = 'Slit Position'
  slitslider = widget_slider(sliderbase, xsize=90, $
    minimum=0, maximum=nslit-1, title=title, $
    value=slitpos, event_pro='spice_xwhisker_slitslider',/drag)

  maskfield = widget_base(lcol, /column, /frame, event_pro = 'spice_xwhisker_mask')
  maskbutton = cw_bgroup(maskfield, ['Mask regions outside slit'], $
    set_value=[1],/nonexclusive)


  ; control of gamma and histo_
  gammacol = widget_base(lcol, /row)
  gamma=1.0
  gamma_slider = cw_fslider(gammacol,/edit,format='(f6.2)',/frame, $
    maximum=3.0,minimum=0.1,value=gamma, $
    title='Gamma Correction', $
    event_func='spice_xwhisker_gamma',/drag)

  histo_lim=-3.0
  histoopt_slider = cw_fslider(gammacol,/edit,format='(f6.2)',/frame, $
    maximum=-1.0,minimum=-6.0,value=histo_lim, $
    title='log(HistoOpt Value)', $
    event_func='spice_xwhisker_histoopt',/drag)
  histo_lim=10.^histo_lim

  lineplotbase = widget_base(lcol, /column)
  names = ['Not active', 'Row plot', 'Column plot']
  lineplot = widget_droplist(lineplotbase, value = names, $
    title = 'Plot pixel values', $
    event_pro = 'spice_xwhisker_lineplot')

  closefield = widget_base(lcol, /column)
  closebutton = widget_button(closefield, value = 'Close', $
    event_pro = 'spice_xwhisker_destroy')

  ; realize main window:
  wp = widget_positioner(tlb, parent=group_leader)
  wp->position
  widget_control, tlb, tlb_get_size = tlb_sz

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
  ;
  imagepos = [0.15, 0.10, 0.77, 0.95]
  ; set up default display mode:
  info = {drawimage:ptr_new(), $
    wd:ptr_new(wd,/no_copy), $
    image:image, $
    xdim:xdim, $
    ydim:ydim, $
    xscale:ptr_new(), $
    yscale:ptr_new(), $
    ypscale:ptr_new(), $
    data:ptr_new(), $
    sit_and_stare:sit_and_stare, $
    n_subplot:0, $
    xdim_unit:1, $
    ydim_unit:1, $
    exposuretext:exposuretext, $
    fmirrytext:fmirrytext, $
    xycentext:xycentext, $
    xticks:0, $
    nlam:nlam, $
    nraster:nraster, $
    expindx:expindx, $
    nexpprp:nexpprp, $
    exprp:1, $
    ndim:ndim, $
    line:line, $
    maskbutton:maskbutton, $
    screensize:screensize, $
    lcol_xsz:lcol_xsz, $
    d_xsz:d_xsz, $
    d_ysz:d_ysz, $
    tlb:tlb, $
    r:r, g:g, b:b, $
    imagepos:imagepos, $
    bottom:bottom, $
    ncolors:ncolors, $
    sx:0, $
    sy:0, $
    dwoption:0, $
    dwoption_menu:dwoption_menu, $
    drawid:drawid, $
    colorbar_title:colorbar_title,$
    lineplot:lineplot,  $
    slitpos:slitpos, $
    gamma:gamma, $
    histo_lim:histo_lim, $
    imin:imin, $
    imax:imax, $
    missing:data->get_missing_value(), $
    drawcolor:drawcolor, $
    mainpixid:pixid, $
    pixid:pixid, $
    xtitle:xtitle, $
    ytitle:ytitle, $
    wid:wid}
  info = ptr_new(info, /no_copy)
  (*info).data=ptr_new(data)
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to spice_xwhisker_draw,
  ; in order to draw the image
  pseudoevent={widget_button,id:0L, $
    top:tlb, handler:0l, select:1}
  spice_xwhisker_draw, pseudoevent

  xmanager, 'spice_xwhisker', tlb, /no_block, event_handler = 'spice_xwhisker_resize', $
    group_leader = group, cleanup = 'spice_xwhisker_cleanup'

end
