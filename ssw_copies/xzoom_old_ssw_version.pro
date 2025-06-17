;+
; NAME:
;       XZOOM
;
; PURPOSE:
;	The purpose of xzoom is to display a zoomed image of some
;	portion of an original image. The zoomed in area is selected
;	using the mouse to draw a box on the original image. The area
;	inside the box is displayed in a new window, using a zoom factor
;	of two.
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;       xzoom, image, xscale, yscale, xtitle = xtitle, ytitle = ytitle, $
;       xrange = xrange, yrange = yrange, group_leader = groupleader
;
; INPUTS:
;       image: The selected area to zoom in on
;       xscale: the x coordinates of pixels of the zoomed image
;       yscale: the y coordinates of pixels of the zoomed image
;
; KEYWORD PARAMETERS:
;	xtitle: The xtitle of the zoomed image display
;	ytitle: The ytitle of the zoomed image display
;	xrange: The range of the x axis
;	yrange: The range of the y axis
;       group_leader: Widget parent (if any).
;
; OUTPUTS:
;       None
;
; CALLS:
;
;
; COMMON BLOCKS:
;
;
; PROCEDURE:
;	Opens a new widget with a draw area where the zoomed image is
;	displayed. It is possible also to zoom in on the zoomed image.
;	The zoomed image may be output to postscript or jpeg files.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;        March 2001: Oivind Wikstol - Based on method by David Fanning/
;	26-Nov-2002: Oivind Wikstol - Added documentation.
;        3-Dec-2007: A. Gardini     - Pointers freed.
;        1-Dec-2008: A. Gardini     - Addition of x/yrange to preserve axes'
;                                     scales.
;-
; save as postscript file
PRO xzoom_ps, event
  thisfile = dialog_pickfile(/write, file = 'xzoom.ps')
  IF thisfile EQ '' THEN return
  widget_control, event.top, get_uvalue = info
  ; keywords=pswindow()
  thisdevice = !d.name
  set_plot, 'ps', /copy
  device, file = thisfile, _extra = keywords, /inches, bits_per_pixel = 8, /color
  ; draw image and axes
  imagepos = [0.10, 0.15, 0.95, 0.9]
  tvimage, bytscl(*(*info).image, top = (*info).ncolors), $
    position = imagepos, /erase, /nointerp
  plot, xscale, xrange = [min(xscale), max(xscale)], $
    xtitle = xtitle, /nodata, xstyle = 1, ystyle = 4, $
    position = imagepos, /noerase
  axis, yaxis = 0, yrange = [min(yscale), max(yscale)], ystyle = 1, ytitle = ytitle, $
    ytick_get = ytick
  axis, yaxis = 1, yrange = [min(yscale), max(yscale)], ystyle = 1, $
    ytickname = strarr(n_elements(ytick)) + ' '

  device, /close_file
  set_plot, thisdevice
END

; save as jpeg file
PRO xzoom_jpeg, event
  thisfile = dialog_pickfile(/write, file = 'xzoom.jpg')
  IF thisfile EQ '' THEN return
  widget_control, event.top, get_uvalue = info
  wset, (*info).wid
  snapshot = tvrd()
  tvlct, r, g, b, /get
  s = size(snapshot)
  image24 = bytarr(3, s[1], s[2])
  image24(0, *, *) = r(snapshot)
  image24(1, *, *) = g(snapshot)
  image24(2, *, *) = b(snapshot)
  write_jpeg, thisfile, image24, true = 1, quality = 75
END

PRO xzoom_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  wdelete, (*info).pixid
  ptr_free, (*info).image
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, info
END

PRO xzoom_draw_events, event
  widget_control, event.top, get_uvalue = info
  events = ['down', 'up', 'motion']
  thisevent = events[event.type]
  CASE thisevent OF
    'down': BEGIN
      ; turn motion events on2
      ; set static corner
      widget_control, (*info).drawid, draw_motion_events = 1
      (*info).sx = event.x
      (*info).sy = event.y
    ENDCASE
    'up': BEGIN
      ; erase last box
      ; turn motion events off
      device, copy = [0, 0, (*info).xsize, (*info).ysize, 0, 0, $
        (*info).pixid]
      widget_control, (*info).drawid, draw_motion_events = 0
      ; sx=(*info).sx
      ; sy=(*info).sy
      imagepos = (*info).imagepos
      dxfac = 1. / (imagepos[2] - imagepos[0])
      dyfac = 1. / (imagepos[3] - imagepos[1])
      sx = ((*info).sx - imagepos[0] * (*info).xsize) * dxfac
      sy = ((*info).sy - imagepos[1] * (*info).ysize) * dyfac
      dx = (event.x - imagepos[0] * (*info).xsize) * dxfac > 0
      dy = (event.y - imagepos[1] * (*info).ysize) * dyfac > 0
      sx = (sx < (*info).xsize - 1) > 0
      sy = (sy < (*info).ysize - 1) > 0
      dx = (dx < (*info).xsize - 1) > 0
      dy = (dy < (*info).ysize - 1) > 0
      image = (*((*info).image))[sx < dx : sx > dx, sy < dy : sy > dy]
      xscale = *(*info).xscale
      yscale = *(*info).yscale
      ddx = ((*info).xrange[1] - (*info).xrange[0]) / n_elements(xscale)
      ddy = ((*info).yrange[1] - (*info).yrange[0]) / n_elements(yscale)
      xscale = xscale[sx < dx : sx > dx]
      yscale = yscale[sy < dy : sy > dy]
      xrange = [min(xscale) - ddx / 2., max(xscale) + ddx / 2.]
      yrange = [min(yscale) - ddy / 2., max(yscale) + ddy / 2.]
      s = size(image)
      mind = min(s[0 : 2])
      IF mind GE 2 THEN BEGIN
        xmax = (*info).screensize[0]
        ymax = (*info).screensize[1]
        image = congrid(image, s[1] * 2 < xmax, s[2] * 2 < ymax)
        xscale = interpol(xscale, s[1] * 2 < xmax)
        yscale = interpol(yscale, s[2] * 2 < ymax)
        xzoom, image, xscale, yscale, $
          xtitle = (*info).xtitle, ytitle = (*info).ytitle, $
          xrange = xrange, yrange = yrange, $
          group_leader = event.top
      ENDIF
    ENDCASE
    'motion': BEGIN
      ; erase previous box
      ; draw new box
      dx = event.x
      dy = event.y
      sx = (*info).sx
      sy = (*info).sy
      wset, (*info).wid
      device, copy = [0, 0, (*info).xsize, (*info).ysize, 0, 0, (*info).pixid]
      plots, [sx, sx, dx, dx, sx], [sy, dy, dy, sy, sy], /device, $
        color = (*info).drawcolor
    ENDCASE
  ENDCASE
END

PRO xzoom_resize, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).drawid, draw_xsize = event.x, draw_ysize = event.y
  (*info).xsize = event.x
  (*info).ysize = event.y
  ; xscale = interpol(*(*info).xscale, event.x)
  ; yscale = interpol(*(*info).yscale, event.y)
  xrange = (*info).xrange
  yrange = (*info).yrange
  dx = (xrange[1] - xrange[0]) / event.x
  xscale = indgen(event.x) * dx + xrange[0] + dx / 2.
  dy = (yrange[1] - yrange[0]) / event.y
  yscale = indgen(event.y) * dy + yrange[0] + dy / 2.
  image = *(*info).image
  image = congrid(image, event.x, event.y)

  window, /pixmap, /free, xsize = (*info).xsize, ysize = (*info).ysize

  imagepos = [0.10, 0.15, 0.95, 0.9]
  tvimage, bytscl(image, top = ncolors), position = imagepos, /erase, /nointerp
  plot, xrange, yrange, /nodata, xstyle = 1, ystyle = 1, $
    xtitle = xtitle, ytitle = ytitle, $
    position = imagepos, /noerase
  (*info).pixid = !d.window
  wset, (*info).wid

  imagepos = [0.10, 0.15, 0.95, 0.9]
  tvimage, bytscl(image, top = ncolors), position = imagepos, /erase, /nointerp
  plot, xrange, yrange, /nodata, xstyle = 1, ystyle = 1, $
    xtitle = xtitle, ytitle = ytitle, $
    position = imagepos, /noerase
  sz = size(xscale)
  (*info).xscale = ptr_new(sz(1))
  *(*info).xscale = xscale
  sz = size(yscale)
  (*info).yscale = ptr_new(sz(1))
  *(*info).yscale = yscale

  *(*info).image = image
END

PRO xzoom_destroy, event
  widget_control, event.top, /destroy
END

PRO xzoom, image, xscale, yscale, xtitle = xtitle, ytitle = ytitle, $
  xrange = xrange, yrange = yrange, group_leader = group
  IF n_params() EQ 0 THEN BEGIN
    print, 'xzoom: No image specified!'
  ENDIF

  ; xrange, yrange are passed by xzoom and not passed by xdetector, xmap, etc.
  IF n_elements(xrange) EQ 0 THEN BEGIN
    dx = (max(xscale) - min(xscale)) / (n_elements(xscale) - 2)
    xrange = [min(xscale) - dx, max(xscale) + dx]
  ENDIF
  IF n_elements(yrange) EQ 0 THEN BEGIN
    dy = (max(yscale) - min(yscale)) / (n_elements(yscale) - 2)
    yrange = [min(yscale) - dx, max(yscale) + dx]
  ENDIF

  IF n_elements(ncolors) EQ 0 THEN ncolors = (!d.n_colors < 256)
  IF n_elements(bottom) EQ 0 THEN bottom = 0
  IF n_elements(drawcolor) EQ 0 THEN drawcolor = !p.color
  s = size(image)
  xsize = s[1]
  ysize = s[2]

  tlb = widget_base(title = 'xzoom', /column, mbar = menubar, $
    tlb_size_events = 1)
  lcol = widget_base(tlb) ; left column.
  rcol = widget_base(tlb) ; medium column.
  filemenu = widget_button(menubar, value = 'File', /menu, uvalue = 'file')
  savemenu = widget_button(filemenu, value = 'Save as', uvalue = 'save', /menu)
  psmenu = widget_button(savemenu, value = 'Postscript', event_pro = 'xzoom_ps')
  jpgmenu = widget_button(savemenu, value = 'JPG', event_pro = 'xzoom_jpeg')
  exitmenu = widget_button(filemenu, value = 'Close', event_pro = 'xzoom_destroy')

  window, /pixmap, /free, xsize = xsize, ysize = ysize
  imagepos = [0.10, 0.15, 0.95, 0.9]
  tvimage, bytscl(image, top = ncolors), position = imagepos, /erase, /nointerp
  plot, xrange, yrange, /nodata, xstyle = 1, ystyle = 1, $
    xtitle = xtitle, ytitle = ytitle, $
    position = imagepos, /noerase
  pixid = !d.window
  drawid = widget_draw(rcol, xsize = xsize, ysize = ysize, /button_events, $
    retain = 2, event_pro = 'xzoom_draw_events')

  closearea = widget_base(lcol, /row)
  closebutton = widget_button(closearea, value = 'Close', $
    event_pro = 'xzoom_destroy')
  widget_control, tlb, /realize
  widget_control, drawid, get_value = wid
  wset, wid
  device, copy = [0, 0, xsize, ysize, 0, 0, pixid]
  ; get screen size
  screensize = get_screen_size()

  info = {drawid: drawid, $
    image: ptr_new(image), $
    imagepos: imagepos, $
    screensize: screensize, $
    xsize: xsize, $
    ysize: ysize, $
    xscale: ptr_new(), $
    yscale: ptr_new(), $
    xrange: xrange, $
    yrange: yrange, $
    xtitle: xtitle, $
    ytitle: ytitle, $
    drawcolor: drawcolor, $
    ncolors: ncolors, $
    pixid: pixid, $
    wid: wid, $
    sx: 0, $
    sy: 0}
  info = ptr_new(info, /no_copy)

  sz = size(xscale)
  (*info).xscale = ptr_new(sz(1))
  *(*info).xscale = xscale
  sz = size(yscale)
  (*info).yscale = ptr_new(sz(1))
  *(*info).yscale = yscale

  widget_control, tlb, set_uvalue = info

  xmanager, 'xzoom', tlb, /no_block, $
    event_handler = 'xzoom_resize', cleanup = 'xzoom_cleanup', $
    group_leader = group
END
