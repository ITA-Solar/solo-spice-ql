; $Id: 2023-05-16 15:03 CEST $

; save as postscript file
pro spice_xlineplot_ps,event
  thisfile=dialog_pickfile(/write,file='pixelplot.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
;  keywords=pswindow()
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro spice_xlineplot_jpeg,event
  thisfile=dialog_pickfile(/write,file='pixelplot.jpg')
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

pro spice_xlineplot_cslider, event
  widget_control, event.top,get_uvalue=info
  (*info).cpos=event.value
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

pro spice_xlineplot_startslider, event
  widget_control, event.top,get_uvalue=info
  (*info).start=event.value

  if (*info).start ge (*info).stop then begin
    if (*info).start gt n_elements((*info).xscale)-2 then begin
      (*info).start = n_elements((*info).xscale)-2
      widget_control,(*info).startslider,set_value=(*info).start
    endif 
    (*info).stop = (*info).start +1
    widget_control,(*info).stopslider,set_value=(*info).stop
  endif

  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

pro spice_xlineplot_stopslider, event
  widget_control, event.top,get_uvalue=info
  (*info).stop=event.value

  if (*info).stop le (*info).start then begin
    if (*info).stop lt 1 then begin
      (*info).stop = 1
      widget_control,(*info).stopslider,set_value=(*info).stop
    endif
    (*info).start = (*info).stop -1
    widget_control,(*info).startslider,set_value=(*info).start
  endif

  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

pro spice_xlineplot_yminslider, event
  widget_control, event.top,get_uvalue=info
  (*info).ymin=event.value

  if (*info).ymin ge (*info).ymax then begin
    if (*info).ymin gt n_elements((*info).yscale)-2 then begin
      (*info).ymin = n_elements((*info).yscale)-2
      widget_control,(*info).yminslider,set_value=(*info).ymin
    endif 
    (*info).ymax = (*info).ymin +1
    widget_control,(*info).ymaxslider,set_value=(*info).ymax
  endif

  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

pro spice_xlineplot_ymaxslider, event
  widget_control, event.top,get_uvalue=info
  (*info).ymax=event.value

  if (*info).ymax le (*info).ymin then begin
    if (*info).ymax lt 1 then begin
      (*info).ymax = 1
      widget_control,(*info).ymaxslider,set_value=(*info).ymax
    endif
    (*info).ymin = (*info).ymax -1
    widget_control,(*info).yminslider,set_value=(*info).ymin
  endif

  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

pro spice_xlineplot_psym, event
  widget_control, event.top, get_uvalue = info
  (*info).psym=event.select
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

; set automatic y-scale or not
pro spice_xlineplot_yauto, event
  widget_control, event.top, get_uvalue = info
  (*info).yauto=event.select
  if (*info).yauto eq 1 then begin
    widget_control, (*info).yminslider, sensitive = 0
    widget_control, (*info).ymaxslider, sensitive = 0
  endif else begin
    widget_control, (*info).yminslider, sensitive = 1
    widget_control, (*info).ymaxslider, sensitive = 1
  endelse

  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  spice_xlineplot_draw,pseudoevent
end

pro spice_xlineplot_movie, event
  widget_control, event.top, get_uvalue = info

  sz = size((*info).data)
  if sz[0] lt 2 then begin
    ok = dialog_message('Spice_Xlineplot: Data must be 2D to play movie')
    return
  endif

  dt=0.01
  xtitle = (*info).xtitle
  ytitle = (*info).ytitle
  xscale = (*info).xscale
  yscale = (*info).yscale
  bg = 0
  fg = !d.n_colors-1

; yp=reform((*info).data[(*info).cpos,*,*])
  yp = (*info).data
  sz=size(yp)
  yrange=[yscale[(*info).ymin],yscale[(*info).ymax]]
  plot, xscale,yp[*, (*info).cpos], $
        xrange = [xscale[(*info).start],xscale[(*info).stop]], $
        yrange=yrange, $
        xtitle = xtitle, ytitle = ytitle, xstyle = 1, $
        background=bg,color=fg
  for i = (*info).cpos+1, sz[2]-1 do begin
    wait,dt
    oplot,xscale,yp[*, i-1],color = bg
    oplot,xscale,yp[*, i],color = fg
    widget_control, (*info).cslider, set_value = i
  endfor
end

pro spice_xlineplot_draw, event
  widget_control, event.top, get_uvalue = info
  if !d.name ne 'PS' then wset, (*info).wid
  xtitle = (*info).xtitle
  ytitle = (*info).ytitle
  xscale = (*info).xscale
  yscale = (*info).yscale
  if (*info).psym eq 1 then psym = -2 else psym = 0
; if (*info).stop le (*info).start then begin
;   message_text = $
;     'Xrange: Stop value less than start. Setting Xstop to Xstart + 2'
;   (*info).stop = (*info).start+2
;   ok = dialog_message(message_text, dialog_parent = (*info).tlb)
;   widget_control, (*info).stopslider, set_value = (*info).stop
; endif
  line_data = (*info).data[*,(*info).cpos]
  ind = where(line_data eq line_data, count)
  if count eq 0 then line_data[*]=0
  if (*info).yauto eq 1 then begin
    plot, xscale,  line_data, $
          xrange = [xscale[(*info).start], xscale[(*info).stop]], $
          xtitle = xtitle, ytitle = ytitle, xstyle = 1, psym = psym, $
          title = (*info).title, /yn
  endif else begin
    plot, xscale, line_data, $
          xrange = [xscale[(*info).start], xscale[(*info).stop]], $
;         xrange = [(*info).start, (*info).stop], $
          yrange = [yscale[(*info).ymin], yscale[(*info).ymax]], $
          xtitle = xtitle, ytitle = ytitle, xstyle = 1, psym = psym, $
          title = (*info).title, ystyle = 1
  endelse
end

;resize main window
pro spice_xlineplot_resize, event
  widget_control, event.top ,get_uvalue = info
  (*info).d_xsz = event.x  > 0
  (*info).d_ysz = event.y - (*info).menu_ysz
  widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
                   draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $
                   ysize = (*info).d_ysz
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top,handler:0l,select:1}
  widget_control,(*info).action,send_event=pseudoevent
;  widget_control, pseudoevent
end

pro spice_xlineplot_destroy, event
    widget_control, event.top, /destroy
end

pro spice_xlineplot_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, info
end

pro spice_xlineplot, data, xscale = xscale, xtitle = xtitle, ytitle = ytitle, $
              cscale = cscale, cslider_title = cslider_title, title = title, $
              groupl = groupl, n_subplot=n_subplot, $
              widget_xsz = widget_xsz, widget_ysz = widget_ysz

  sz = size(data)
  if sz[0] eq 0 then begin
    ok = dialog_message('Data must be an array')
    return
  endif

  if sz[0] gt 3 then begin
    ok = dialog_message('Data cannot be more than 3D')
    return
  endif

  if n_elements(xtitle) eq 0 then xtitle='Position'
  if n_elements(ytitle) eq 0 then ytitle='Pixel value'
  if n_elements(title) eq 0 then title = ''
  if n_elements(cslider_title) eq 0 then cslider_title = 'Column nummber'
  if n_elements(xscale) eq 0 then xscale = indgen(sz[1])
  if n_elements(widget_xsz) eq 0 then d_xsz = 600 else $
    d_xsz = widget_xsz
  if n_elements(widget_ysz) eq 0 then d_ysz = 500 else $
    d_ysz = widget_ysz

  tlb = widget_base(title = 'Line plot tool', mbar = menubar, $
                    tlb_size_events = 1, $
                          group_leader = groupl,/row)
  lcol = widget_base(tlb, /frame, /column)
  rcol = widget_base(tlb, /column)

  displaybase = widget_base(rcol, /row)

  d_xsz = 600
  d_ysz = 500

  xmin = min(xscale)
  xmax = max(xscale) > (xmin+1.)
  ymin = min(data)
  ymax = max(data) > (ymin+1.)
; new version AG 8/7/08
  xmin = 0
  xmax = n_elements(xscale)-1
  ymax = max(data) ;> (ymin+1.)
  yscale = dblarr(n_elements(xscale))
  yscale = ymin - 0.1*(ymax-ymin) + indgen(n_elements(yscale)) * $
           (1.2*(ymax-ymin)) / (n_elements(yscale)-1)
  ymin = 0
  ymax = n_elements(yscale)-1

  drawid=widget_draw(displaybase, retain = 2, xsize = d_xsz, ysize = d_ysz, $
                     /button_events, event_pro = 'spice_xlineplot_draw')

  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'spice_xlineplot_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'spice_xlineplot_jpeg')

  exitmenu=widget_button(filemenu, value='Close', event_pro='spice_xlineplot_destroy')

  cslider = -1
  if sz[0] gt 1 then begin
    if n_elements(cscale) ne 0 then begin
      cmin = min(cscale)
      cmax = max(cscale)
    endif else begin
      cmin = 0
      cmax = (sz[2]-1) > 1
    endelse
    cslider = widget_slider(lcol, xsize=120, $
                          minimum=0, maximum=cmax, $
                          title=cslider_title, $
                          value=0, event_pro = 'spice_xlineplot_cslider', /drag)
  endif

  starttitle = 'Start [X]'
  stoptitle = 'Stop [X]'
  ymintitle = 'Set Yaxis min'
  ymaxtitle = 'Set Yaxis max'

  startslider = widget_slider(lcol, xsize=120, $
                          minimum=xmin, maximum=xmax, $
                          title=starttitle, $
                          value=xmin, event_pro = 'spice_xlineplot_startslider', /drag)
  stopslider = widget_slider(lcol, xsize=120, $
                          minimum=xmin, maximum=xmax, $
                          title=stoptitle, $
                          value=xmax, $
                          event_pro = 'spice_xlineplot_stopslider', /drag)

  yminslider = widget_slider(lcol, xsize=120, $
                          minimum=ymin, maximum=ymax, $
                          title=ymintitle, $
                          value=ymin, /sensitive,$
                          event_pro = 'spice_xlineplot_yminslider', /drag)

  ymaxslider = widget_slider(lcol, xsize=120, $
                          minimum=ymin, maximum=ymax, $
                          title=ymaxtitle, $
                          value=ymax, /sensitive, $
                          event_pro = 'spice_xlineplot_ymaxslider', /drag)

  moviebutton = widget_button(lcol, value = 'Movie', $
                              event_pro = 'spice_xlineplot_movie')

  ; button for switching psym-plot on/off:
  psymbase = widget_base(lcol, /column, /nonexclusive)
  psymbutton = widget_button(psymbase, value='Mark each data point', $
                              event_pro = 'spice_xlineplot_psym')
  yautobutton = widget_button(psymbase, value='Automatic y-scale', $
                              event_pro = 'spice_xlineplot_yauto')

  closefield = widget_base(lcol,/column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'spice_xlineplot_destroy')

    ; realize main window:
  wp = widget_positioner(tlb, parent=groupl)
  wp->position, /left_align, n_subplot=n_subplot
  widget_control, tlb, tlb_get_size = tlb_sz

  ; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid

  ; set automatic y-scaling as default:
  widget_control, yautobutton, set_button=1
  tlb_xsz = tlb_sz[0]  ; xsize of whole widget in pixels
  tlb_ysz = tlb_sz[1]  ; ysize of whole widget in pixels
  menu_ysz = tlb_ysz - d_ysz

  ; define the info structure, used send information around
  info = {data:data, $
          tlb:tlb, $
          drawid:drawid, $
          d_xsz:d_xsz           ,$
          d_ysz:d_ysz           ,$
          tlb_xsz:tlb_xsz       ,$
          tlb_ysz:tlb_ysz       ,$
          menu_ysz:menu_ysz     ,$
          action:drawid, $
          wid:wid, $
          cpos:0, $
          start:xmin, $
          stop:xmax, $
          ymin:ymin, $
          ymax:ymax, $
          psym:0, $
          yauto:1, $
          cslider:cslider, $
          startslider:startslider, $
          stopslider:stopslider, $
          yminslider:yminslider, $
          ymaxslider:ymaxslider, $
          xscale:xscale, $
          yscale:yscale, $
          xtitle:xtitle, $
          ytitle:ytitle, $
          title:title}
  info = ptr_new(info, /no_copy)

; set automatic y-scaling as default:
  widget_control, yautobutton, set_button=1
  widget_control, yminslider, sensitive=0
  widget_control, ymaxslider, sensitive=0
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to xdisplay_draw,
  ; in order to draw the image
  pseudoevent={widget_button,id:(*info).action, $
               top:tlb, handler:0l, select:1}
  widget_control, (*info).action, send_event = pseudoevent

  xmanager, 'spice_xlineplot', tlb, /no_block, group_leader = groupl, $
            event_handler = 'spice_xlineplot_resize', cleanup = 'spice_xlineplot_cleanup'
end

