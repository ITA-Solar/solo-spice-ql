;+
; NAME:
;       SPICE_XRASTER
;
; PURPOSE:
;
;       SPICE_XRASTER is used to display 3-D spectroscopic data in the form
;       of a raster (i.e. intensity[lambda, slit pos, raster pos.].
;       One line is displayed as I[lambda, slit pos] with one display window
;       for each raster (using !p.multi). If more than one line, these
;       are added as extra rows of display windows.
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xraster, data_obj, windows, ncolors=ncolors, $
;                     group_leader = groupleader
;
; INPUTS:
;       data_obj: Data object. based on the superclass HW_DATA
;
; KEYWORD PARAMETERS:
;       windows : The index(es) of the line windows to be displayed
;       group_leader: Widget parent (if any).
;       ncolors: Number of colors for xraster. Default is
;                !d.n_colors<256.
;
; OUTPUTS:
;       None
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       SPICE_XRASTER defines the widgets and displays data. Display
;       can be output  to ps-file or jpeg. To illustrate the use
;       of windows, consider an observation consisting of
;       intensity in 5 spectral lines with 50 wavelength pixels,
;       512 pixels along the slit and 300 raster positions.
;       The to display line numbers 2 and 3 (of line 0-4),
;       the call to spice_xraster would be:
;       spice_xraster, data_obj, [2, 3], $
;                group_leader = group_leader, ncolors = ncolors
;       This would create a display with 2x300 windows of 50x512 pixels
;       (although the x-y size of the display windows are scaled)
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;          Jul 2002: �ivind Wikst�l - xraster.pro first version
;       20-Apr-2004: �ivind Wikst�l - Added funtions to change wavl. scale
;                                     [pix/Angstr.]
;       13-Nov-2006: Viggo Hansteen - Replaced call to tvimage with call to
;                                     plot_image, this simplifies the logic
;                                     quite a bit!
;       29-Sep-2007: A. Gardini     - Pointers freed by cleanup.
;       14-Feb-2008: A. Gardini     - Set the xvs maximum to 2^15-1.
;       22-Apr-2008: A. Gardini     - Set margin parameters in panel.
;       17-Jan-2013: V. Hansteen    - rewritten as iris_xraster
;       19-May-2020: M. Wiesmann    - rewritten as spice_xraster
;
; $Id: 16.10.2020 11:46 CEST $
;-
;
; save as postscript file
pro spice_xraster_ps,event
  thisfile=dialog_pickfile(/write,file='spice_xraster.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xraster_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro spice_xraster_jpeg,event
  thisfile=dialog_pickfile(/write,file='spice_xraster.jpg')
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
pro spice_xraster_draw, event
  widget_control, event.top, get_uvalue = info
  old_charsize=!p.charsize
  if !d.name ne 'PS' then begin
    wset, (*info).wid
    !p.charsize=2.0
    tcharsize=1.0
    erase
  endif else begin
    !p.charsize=0.85
    tcharsize=0.5
  endelse
  nr=(*info).nexp
  nwin = (*info).nwin
  xsz = 0
  ysz = 0
  ; this loop is for determining max size of spectral line windows
  ; (since they can have varying size)
  for i = 0, nwin-1 do begin
    j=(*info).windows[i]
    ;pos=*(*info).data->getpos(j)
    ;sz = size(wd)
    xsz = xsz > *(*info).data->get_header_info('naxis3', j)
    ysz = ysz > *(*info).data->get_header_info('naxis2', j)
  endfor
  ; determine size of each window. The scale factors
  ; (xpixels*2)x(ypixels/2), with a minimum of
  ; 50x100 and max of 100x250 pixels.
  xpix = (xsz*2) > 75 < 250
  ypix = (ysz/2) > 100 < 250
  nxticks = 3
  ; determine if draw window needs to be larger (by
  ; applying scroll bars.)
  xfac = fix((xpix*nr*2)/(*info).x_scroll_size)
  yfac = fix((ypix*nwin*1.2)/(*info).y_scroll_size)
  xvs = (*info).x_scroll_size*xfac  > (*info).x_scroll_size < 2L^15-1
  yvs = (*info).y_scroll_size*yfac  > (*info).y_scroll_size
  ;
  widget_control, (*info).drawid, draw_xsize = xvs, draw_ysize = yvs
  if *(*info).data->get_missing_value() ne *(*info).data->get_missing_value() then missing=-99999L $
  else missing=*(*info).data->get_missing_value()
  wdmin=fltarr((*info).nwin)-missing
  wdmax=fltarr((*info).nwin)+missing
  ; set up plot scale:
  for i = 0, (*info).nwin-1 do begin
    j = (*info).windows[i]
    for it=0,min([5,nr-1]) do begin
      var=*(*info).data->get_one_image(j,it)
      wdmin[i]=min([min(iris_histo_opt(var,0.01,/bot_only,missing=missing),/nan),wdmin[i]],/nan)
      wdmax[i]=max([max(iris_histo_opt(var,0.001,/top_only,missing=missing),/nan),wdmax[i]],/nan)
    endfor
    if wdmin[i] gt wdmax[i] then begin
      wdmin[i]=min(var, max=maxtemp)
      wdmax[i]=maxtemp
    endif
    sz = size(var)

    ; wavelength scale of NUV/FUV1/FUV2
    if ~(*info).xdim_unit then begin
      lambda=indgen(sz[1])
    endif else begin
      lambda=*(*info).data->get_lambda_vector(j)
    endelse
    if ~(*info).ydim_unit then begin
      ypos=indgen(sz[2])
    endif else begin
      ypos=*(*info).data->get_instr_y_vector(j)
    endelse
    xscale = interpol(lambda, xpix)
    yscale = interpol(ypos, ypix)
    origin=[min(xscale),min(yscale)]
    timepos=[min(xscale)+(max(xscale)-min(xscale))*0.05,max(yscale)-(max(yscale)-min(yscale))*0.1]

    ; draw images
    for it = 0, nr-1 do begin
      drawimage = congrid(*(*info).data->get_one_image(j,it),xpix,ypix)
      sz=size(drawimage)
      scale=[(max(xscale)-min(xscale))/sz[1],(max(yscale)-min(yscale))/sz[2]]
      ymin = wdmin[i]
      ymax = wdmax[i]
      if it eq 0 then ytitle=*(*info).data->get_window_id(j)+' '+(*info).ytitle else ytitle=''
      br_panel,it,i,nx=nr,ny=(*info).nwin,order=0,ydist=3,/xlabel,ytop=3,xright=5
      plot_image,drawimage,origin=origin,scale=scale,/nosquare, $
        xtitle = (*info).xtitle, xticks = nxticks, ytitle=ytitle,min=ymin,max=ymax
      if i eq 0 then xyouts,timepos[0],timepos[1],'t = '+ $
        strtrim(string((*(*info).data->get_time_vector(j))[it],format='(f6.1)'),2) +' [s]', $
        alignment=0.0,chars=tcharsize,color=255
    endfor
  endfor
  br_panel,/reset
  !p.multi = 0
  !p.charsize = 1.0
  ;create colorbar (if plot to 'PS' then skip colorbar:
  if !d.name ne 'PS' then begin
    widget_control, (*info).colorbarid, get_value = wid
    wset, wid
    erase
    widget_control, (*info).colorbarid, draw_ysize = yvs
    nwin=(*info).nwin
    for i = 0,nwin-1 do begin
      ; find max and min values of drawimage to produce color bar y-scale
      ymin = wdmin[i]
      ymax = wdmax[i]
      if ymax-ymin eq 0.0 then ymax=ymin+1
      format='(i6)'
      if ymax-ymin lt 10 then format='(f7.4)'
      position = [0.75, 0.04+float(nwin-i-1)/nwin*0.95, $
        0.95, 0.01+float(nwin-i)/nwin*0.95]
      hw_colorbar, position=position, range = [ymin, ymax], $
        /vertical , format=format, title=(*info).colorbar_title, $
        /keep_pos
    endfor
  endif
end

; Popup window for line selection
pro spice_xraster_pickline, event
  widget_control, event.top ,get_uvalue = info
  ; open window for line selection
  lineselect_widget = widget_base(title = 'Select line', $
    group_leader = (*info).tlb,/row)
  closefield = widget_base(lineselect_widget,/column)
  closebutton = widget_button(closefield, value = 'OK', $
    event_pro = 'spice_xraster_pickline_destroy')
  line_base = widget_base(lineselect_widget,/column,/frame)
  linelist = cw_bgroup(line_base, (*info).linelist, /return_index, $
    /exclusive, event_func = 'spice_xraster_pickline_pick')
  widget_control, lineselect_widget, set_uvalue = info
  widget_control, lineselect_widget, /realize
  xmanager, 'Select line', lineselect_widget, $
    /no_block, group_leader = (*info).tlb
end

; get the value of the selected line from the line list:
function spice_xraster_pickline_pick, event
  widget_control, event.top, get_uvalue = info
  (*info).line = event.value+(*info).windows[0]
  return, 0
end

; close Line selection widget
pro spice_xraster_pickline_destroy, event
  widget_control, event.top, get_uvalue = info
  if (*info).messenger eq (*info).animenu then begin
    spice_xraster_anim, event
  endif
  widget_control, event.top,/destroy
end

; Controls the animation event: If only one line,
; start animation, if several line, then pop up
; line selection window first.
pro spice_xraster_control_anim,event
  widget_control, event.top, get_uvalue = info
  if (*info).nwin lt 2 then begin
    spice_xraster_anim, event
  endif else begin
    (*info).messenger = (*info).animenu
    spice_xraster_pickline, event
  endelse
end

; create animation widget and launch animation
pro spice_xraster_anim, event
  print,'does NOT work yet'
  return
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
    offset=*(*info).data->getposition((*info).line),/float,swap=swap, $
    magnification=magnification,missing=*(*info).data->missing()

  ;;   *(*info).data-> getwin,(*info).line,wd,pos
  ;; ;
  ;;   sz = size(wd)
  ;;   ndim = sz[0]
  ;;   xsize = sz[1]
  ;;   ysize = sz[2]
  ;; ;
  ;;   if ndim lt 3 then begin
  ;;     ok = dialog_message('Data array must be 3-D to make animation!',/center)
  ;;     return
  ;;   endif
  ;; ; bytscale data to save time in animation tool
  ;; ;  wdb = bytscl(iris_histo_opt(wd,1.e-2,missing=*(*info).data->missing()))
  ;; ; write data to assoc file if not already existing:
  ;;   ct=0
  ;;   repeat begin
  ;;     ct=ct+1
  ;;     assoc_file = IRISxfiles_appReadme()+'/iris_xraster_ximovie_'+strtrim(string(ct),2)+'.tmp'
  ;;   endrep until ((findfile(assoc_file))[0] eq '')
  ;;   if ct gt 99 then begin
  ;;     message,'more than 100 temporary assoc files stored in',/info
  ;;     message,IRISxfiles_appReadme()+'/iris_xdetector_ximovie_XX.tmp. Consider purge!',/info
  ;;   endif
  ;;   openw, lu, assoc_file, /get_lun
  ;;   rec = assoc(lu, wd)
  ;;   rec[0] = wd
  ;;   close, lu & free_lun, lu
  ;; ; start iris_ximovie, with the delete keyword (afile is removed from disc
  ;; ; when iris_ximovie is closed
  ;;   iris_ximovie, assoc_file, xsize, ysize, group_leader = (*info).tlb, $
  ;;      /fdelete,/float,missing=*(*info).data->missing()
  return
end

; change spatial scale to pixels
pro spice_xraster_spix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data->get_axis_title((*info).ydim, /pixels)
  (*info).ydim_unit = 0
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xraster_draw, pseudoevent
end

; change spatial scale to arcsec
pro spice_xraster_sarcsec, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data->get_axis_title((*info).ydim)
  (*info).ydim_unit = 1
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xraster_draw, pseudoevent
end

; change wavelength scale to pixels
pro spice_xraster_wpix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data->get_axis_title((*info).xdim, /pixels)
  (*info).xdim_unit = 0
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xraster_draw, pseudoevent
end

; change wavelength scale to Angstrom
pro spice_xraster_wangstr, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data->get_axis_title((*info).xdim)
  (*info).xdim_unit = 1
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  spice_xraster_draw, pseudoevent
end




;; change wavelength scale to pixels
;pro spice_xraster_wpix, event
;  widget_control, event.top, get_uvalue = info
;  ; change titles in aux object
;  (*(*info).data->getaux())->setwscale,'pixels'
;  (*(*info).data->getaux())->setxytitle,wscale='pixels'
;  ; set titles for image plots
;  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
;  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
;  pseudoevent={widget_button,id:0L, $
;    top:event.top, handler:0l, select:1}
;  spice_xraster_draw, pseudoevent
;end
;
;; change spatial scale to pixels
;pro spice_xraster_spix, event
;  widget_control, event.top, get_uvalue = info
;  ; change titles in aux object
;  (*(*info).data->getaux())->setsscale,'pixels'
;  (*(*info).data->getaux())->setxytitle,sscale='pixels'
;  ; set titles for image plots
;  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
;  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
;  pseudoevent={widget_button,id:0L, $
;    top:event.top, handler:0l, select:1}
;  spice_xraster_draw, pseudoevent
;end
;
;; change spatial scale to arcsec
;pro spice_xraster_sarcsec, event
;  widget_control, event.top, get_uvalue = info
;  ; change titles in aux object
;  (*(*info).data->getaux())->setsscale,'arcsec'
;  (*(*info).data->getaux())->setxytitle,sscale='arcsec'
;  ; set titles for image plots
;  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
;  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
;  pseudoevent={widget_button,id:0L, $
;    top:event.top, handler:0l, select:1}
;  spice_xraster_draw, pseudoevent
;end
;
;; change wavelength scale to Angstrom
;pro spice_xraster_wangstr, event
;  widget_control, event.top, get_uvalue = info
;  ; set titles for image plots
;  (*(*info).data->getaux())->setwscale,string(197b)
;  (*(*info).data->getaux())->setxytitle,wscale=string(197b)
;  ; set titles for image plots
;  (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
;  (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
;  pseudoevent={widget_button,id:0L, $
;    top:event.top, handler:0l, select:1}
;  spice_xraster_draw, pseudoevent
;end





; select color table
pro spice_xraster_colors, event
  widget_control, event.top, get_uvalue=info
  thisevent = tag_names(event, /structure_name)
  case thisevent of
    'WIDGET_BUTTON': begin
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'spice_xraster colors (' + strtrim((*info).wid, 2) + ')', $
        group_leader = event.top, notifyid = [event.id, event.top]
    endcase
    'XCOLORS_LOAD': begin
      (*info).r = event.r((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).g = event.g((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).b = event.b((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      if !d.n_colors gt 256 then begin
        pseudoevent={widget_button,id:0l, $
          top:event.top, handler:0l, select:1}
        spice_xraster_draw, pseudoevent
      endif
    endcase
  endcase
  widget_control, event.top, set_uvalue = info
end

; protect colors
pro spice_xraster_protect_colors,event
  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
end

; resize main window
pro spice_xraster_resize, event
  widget_control, event.top ,get_uvalue = info
  if (*info).timer eq 'off' then begin
    widget_control,event.top,timer=0.1
    (*info).timer='on'
  endif
  case tag_names(event,/structure_name) of
    'WIDGET_TIMER': if (*info).oldx eq (*info).xs and (*info).oldy eq (*info).ys then (*info).redraw=1 $
    else widget_control,event.top,timer=0.25
    'WIDGET_BASE': begin
      (*info).xs=event.x
      (*info).ys=event.y
    end
    else:
  endcase
  if (*info).redraw then begin
    (*info).d_xsz = ((*info).xs - (*info).lcol_xsz-(*info).cb_xsz) > 0
    (*info).d_ysz = (*info).ys
    (*info).x_scroll_size = (*info).d_xsz
    (*info).y_scroll_size = (*info).d_ysz
    widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
      draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $
      ysize = (*info).d_ysz
    widget_control, (*info).colorbarid, draw_xsize = (*info).cb_xsz, $
      draw_ysize = (*info).d_ysz, xsize = (*info).cb_xsz, $
      ysize = (*info).d_ysz

    pseudoevent={widget_button,id:0L, $
      top:event.top,handler:0l,select:1}
    spice_xraster_draw, pseudoevent
    (*info).redraw=0
    (*info).timer='off'
  endif
  (*info).oldx=(*info).xs
  (*info).oldy=(*info).ys
end

; close spice_xraster
pro spice_xraster_destroy, event
  widget_control, event.top,/destroy
end

pro spice_xraster_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, (*info).data
  ptr_free, info
end

pro spice_xraster, data, windows, ncolors=ncolors, group_leader = group_leader
  ;
  if n_params() lt 2 then begin
    message,'spice_xraster,data,windows, ncolors=ncolors,group_leader = group',/cont
    return
  endif

  if n_elements(ncolors) eq 0 then ncolors = (!d.n_colors < 256)
  maxexp=200
  ; drawing window size in relation to screen
  screensize=get_screen_size()
  if n_elements(scfac) eq 0 then scfac=0.6
  sz=screensize*scfac
  d_xsz = sz[0]
  d_ysz = sz[1]
  nwin=n_elements(windows)       ; number of line windows selected by user
  line = windows[0]
  ;nraster = max(data->getnraster())   ; number of raster positions in data set
  nexp =  max(data->get_number_exposures())        ; number of exposures
  if nexp gt maxexp then begin
    warning=['Raster/Time series contains more than '+string(strtrim(maxexp,2))+' exposures', $
      'spice_xraster will be quite slow. Continue?']
    continue=dialog_message(warning,/cancel,/default_cancel,dialog_parent=group)
    if continue eq 'Cancel' then return
  endif
  xdim = 2   ; wavelength
  ydim = 1   ; slit pos
  xtitle = data->get_axis_title(xdim) ; wavelength
  ytitle = data->get_axis_title(ydim) ; slit position

  ;create linelist
  linelist = strarr(nwin)
  for i = 0, nwin-1 do linelist[i] = 'Line '+strtrim(data->get_window_id(windows[i]),2)

  ; base widget:
  xwt = 'SPICE_Xraster -' +data->get_filename()   ; spice_xraster window title
  tlb = widget_base(/row, title=xwt, tlb_size_events = 1, $
    mbar=menubar, xoffset=100, yoffset=100, group_leader=group_leader) ;
  lcol = widget_base(tlb, /frame, /column)      ;left column.
  rcol = widget_base(tlb, /column)              ;right column.

  ; create pulldown menus on the base widget menubar
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', uvalue='save', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'spice_xraster_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'spice_xraster_jpeg')
  exitmenu=widget_button(filemenu, value='Close', event_pro='spice_xraster_destroy')
  optmenu=widget_button(menubar,value='Options', uvalue='options')
  colmenu=widget_button(optmenu, value='Colour table', $
    event_pro='spice_xraster_colors')
  animenu = widget_button(optmenu, value = 'Create Animation', uvalue='anim', $
    event_pro = 'spice_xraster_control_anim')
  wscalemenu=widget_button(optmenu, value='Change wavelength scale',/menu)
  angstr = string("305B)+'ngstr'+string("370B)+'m'
  pixmenu=widget_button(wscalemenu, value='Pixels',event_pro='spice_xraster_wpix')
  angstrmenu=widget_button(wscalemenu, value=angstr,event_pro='spice_xraster_wangstr')
  sscalemenu=widget_button(optmenu, value='Change spatial scale',/menu)
  pixmenu=widget_button(sscalemenu, value='Pixels',event_pro='spice_xraster_spix')
  angstrmenu=widget_button(sscalemenu, value='arcsec',event_pro='spice_xraster_sarcsec')

  ; display window:
  displaybase = widget_base(rcol, /row)
  drawid=widget_draw(displaybase, retain = 2, $
    xsize = d_xsz, x_scroll_size = d_xsz ,$
    ysize = d_ysz, y_scroll_size = d_ysz ,$
    event_pro='spice_xraster_draw')
  cb_xsz = 84  ; xsize of color bar draw widget
  ; create color bar to the right of display window:
  colorbarid = widget_draw(displaybase, retain = 2, $
    xsize = cb_xsz, x_scroll_size = cb_xsz, $$
    ysize = d_ysz, y_scroll_size= d_ysz)
  colorbar_title=data->get_title()+' '+(data->get_variable_unit())
  ; close button
  closefield = widget_base(lcol, /column)
  closebutton = widget_button(closefield, value = 'Close', $
    event_pro = 'spice_xraster_destroy')
  ; realize main window:

  widget_control, tlb, /realize, tlb_get_size = tlb_sz
  ; define size of widget and the menu column
  tlb_xsz = tlb_sz[0]  ; xsize of whole widget in pixels
  tlb_ysz = tlb_sz[1]  ; ysize of whole widget in pixels
  lcol_xsz = tlb_xsz - d_xsz - cb_xsz
  ; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid

  ; get and save color table
  tvlct, r, g, b, /get
  bottom = 0
  if (!d.n_colors le 256) then begin
    r = r[bottom:ncolors-1+bottom]
    g = g[bottom:ncolors-1+bottom]
    b = b[bottom:ncolors-1+bottom]
  endif

  ; define the info structure, used send information around
  info = {xscale:ptr_new(), $
    yscale:ptr_new(), $
    data:ptr_new(), $
    xdim:xdim, $
    ydim:ydim, $
    xdim_unit:1, $
    ydim_unit:1, $
    nwin:nwin, $
    ;nraster:nraster, $
    nexp:nexp, $
    line:line, $
    windows:windows, $
    linelist:linelist, $
    lcol_xsz:lcol_xsz, $
    d_xsz:d_xsz, $
    d_ysz:d_ysz, $
    oldx:0, $
    oldy:0, $
    xs:0, $
    ys:0, $
    redraw:0, $
    timer:'off', $
    x_scroll_size:d_xsz, $
    y_scroll_size:d_ysz, $
    cb_xsz:cb_xsz, $
    tlb:tlb, $
    lcol:lcol, $
    rcol:rcol,  $
    animenu:animenu, $
    messenger:0, $
    r:r, g:g, b:b, $
    bottom:bottom, $
    ncolors:ncolors, $
    drawid:drawid, $
    colorbarid:colorbarid, $
    colorbar_title:colorbar_title,$ ;
    xtitle:xtitle, $
    ytitle:ytitle, $
    wid:wid}
  info = ptr_new(info, /no_copy)
  (*info).data=ptr_new(data)
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to spice_xraster_draw,
  ; in order to draw the image

  pseudoevent={widget_button,id:0L, top:tlb, handler:0l, select:1}
  spice_xraster_draw,pseudoevent
  xmanager,'spice_xraster',tlb,/no_block,event_handler='spice_xraster_resize', $
    group_leader=group,cleanup = 'spice_xraster_cleanup'
end
