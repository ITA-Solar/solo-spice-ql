;+
; NAME:
;       SPICE_XMAP
;
; PURPOSE:
;
;       SPICE_XMAP is used to display 2-D (or higher) data. It is a
;       widget based program with several options and functions that
;       allows data to be displayed in a nukmber of modes.
;
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;       spice_xmap, data [, linelist = linelist, group_leader = groupleader, ncolors=ncolors]
;
; INPUTS:
;       data: Can be either the name and path of a SPICE data file,
;             or a SPICE data object.
;
; KEYWORD PARAMETERS:
;       linelist: list of line indexes, default 0.
;       group_leader: Widget parent (if any).
;       ncolors: Number of colors for spice_xmap. Default is !d.n_colors<256.
;
; Outputs:
;       None
;
; CALLS:
;
; COMMON BLOCKS:
;
;
; PROCEDURE:
;       SPICE_XMAP defines the widgets and displays data. It has several
;       options for displaying data in different modes, zooming,
;       selecting colors, file output etc. It is a QL-tool for
;       displaying data of 2 dimensions or higher. SPICE_XMAP consists
;       of many functions that is caled whenever the user does
;       something in the widget program.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       12-Sep-2002: Oivind Wikstol. First version.
;       15-Feb-2007: Viggo Hansteen - reverse x-axis if raster.
;       29-Sep-2007: A. Gardini - Pointers freed by cleanup. Other changes
;                                 already made in Jun-2007
;        3-Dec-2007: A. Gardini - Freed pointers.
;       17-Dec-2007: A. Gardini - Fixed "Plot pixel values" problem:
;                                 realsize=1, ypscale removed, ptrs corrected
;       19-Dec-2007: A. Gardini - Drawimage used in xmap_pixplot.
;        3-Jan-2008: A. Gardini - Initialization of the continuum in wd_def.
;                                 Ghosted "Define line" and "Save" buttons.
;        8-Feb-2008: A. Gardini - Cleaning. "Define line" button erased in
;                                 the plot of moments.
;       17-Feb-2008: A. Gardini - Added sliders in the plot of moments.
;       20-Feb-2008: A. Gardini - Error corrected in the slider definition.
;       18-Mar-2008: A. Gardini - Changed the column order in moment objects.
;       19-Mar-2008: A. Gardini - Set the iwin keyword in xmap_save_moment.
;       10-Oct-2008: A. Gardini - Added groupl calling xmoment, cleanup check
;       11-Nov-2008: A. Gardini - Corrected dx and dy in xmap_warcsec.
;       30-Apr-2009: A. Gardini - Fixed 1 pixel shift error.
;       16-Jun-2009: A. Gardini - Set getypos(iwin) in xmap_warcsec.
;       19-Aug-2009: A. Gardini - Fixed bug in the xscale in
;                                 xmap_warcsec.
;       22-Jan-2013: V. Hansteen - First IRIS modified version.
;       28-May-2020: M. Wiesmann - First SPICE modified version.
;
; $Id: 2024-12-19 13:56 CET $
;-
;
; save as postscript file
PRO spice_xmap_ps, event
  thisfile = dialog_pickfile(/write, file = 'spice_xmap.ps')
  IF thisfile EQ '' THEN return
  widget_control, event.top, get_uvalue = info
  thisdevice = !d.name
  charsize = !p.charsize
  set_plot, 'ps', /copy
  !p.charsize = 0.8
  ysz = 15
  xsz = (*info).aspect * ysz
  calc_xysize, xsz, ysz, ps_xsz, ps_ysz, nxchar = total(!x.margin), nychar = total(!y.margin)
  device, xsize = ps_xsz, ysize = ps_ysz, file = thisfile, bits_per_pixel = 8, /color
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xmap_draw, pseudoevent
  device, /close_file
  set_plot, thisdevice
  !p.charsize = charsize
END

; save as jpeg file
PRO spice_xmap_jpeg, event
  thisfile = dialog_pickfile(/write, file = 'spice_xmap.jpg')
  IF thisfile EQ '' THEN return
  widget_control, event.top, get_uvalue = info
  wset, (*info).wid
  snapshot = tvrd()
  tvlct, r, g, b, /get
  s = size(snapshot)
  image24 = bytarr(3, s[1], s[2])
  image24[0, *, *] = r[snapshot]
  image24[1, *, *] = g[snapshot]
  image24[2, *, *] = b[snapshot]
  write_jpeg, thisfile, image24, true = 1, quality = 75
END

FUNCTION spice_xmap_gamma, event
  widget_control, event.top, get_uvalue = info
  (*info).gamma = event.value
  im_min = 0.0
  (*info).imin = min(iris_histo_opt(*(*info).drawimage, $
    (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  (*info).imax = max(iris_histo_opt(*(*info).drawimage, $
    (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  IF (*info).imax EQ im_min THEN BEGIN
    (*info).imin = min(iris_histo_opt((*info).drawimage, (*info).histo_lim, missing = (*info).missing))
    (*info).imax = max(iris_histo_opt(*(*info).drawimage, (*info).histo_lim, missing = (*info).missing))
    (*info).gamma = 1.0
    text = 'All data < im_min ' + strtrim(string(im_min, format = '(f4.2)'), 2) + ' gamma reset to 1.0'
    message, text, /info
    !NULL = dialog_message(text, dialog_parent = (*info).tlb)
  ENDIF
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xmap_draw, pseudoevent
  return, 0
END

FUNCTION spice_xmap_histoopt, event
  widget_control, event.top, get_uvalue = info
  (*info).histo_lim = 10.0 ^ (event.value)
  im_min = 0.0
  (*info).imin = min(iris_histo_opt(*(*info).drawimage, $
    (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  (*info).imax = max(iris_histo_opt(*(*info).drawimage, $
    (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  IF (*info).imax EQ im_min THEN BEGIN
    (*info).imin = min(iris_histo_opt(*(*info).drawimage, (*info).histo_lim, missing = (*info).missing))
    (*info).imax = max(iris_histo_opt(*(*info).drawimage, (*info).histo_lim, missing = (*info).missing))
  ENDIF
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xmap_draw, pseudoevent
  return, 0
END

; display image in the draw window:
PRO spice_xmap_draw, event
  widget_control, event.top, get_uvalue = info
  moment_name = *(*info).data.get_variable_type()
  imin = (*info).imin + (*info).min / float(255) * ((*info).imax - (*info).imin)
  imax = (*info).imax * (*info).max / float(255)
  gamma = (*info).gamma
  CASE moment_name OF
    'Velocity': BEGIN
      imax = max([abs(imax), abs(imin)])
      imin = -imax
    END
    ELSE:
  ENDCASE
  IF (*info).defcol THEN BEGIN
    ; case moment_name of
    ; 'Intensity': (*(*info).data->getaux())->loadct,'int'
    ; 'Velocity': begin
    ; (*(*info).data->getaux())->loadct,'vel'
    ; (*info).color=127
    ; end
    ; 'Line width': (*(*info).data->getaux())->loadct,'wid'
    ; else: loadct,0
    ; endcase
    loadct, 0
  ENDIF
  IF !d.name NE 'PS' THEN BEGIN
    wset, (*info).wid
    bgblack = 1
    ticklen = 0.02
  ENDIF ELSE BEGIN
    bgblack = 0
    ticklen = -0.02
    (*info).color = 0
  ENDELSE
  ; xytitle = *(*info).data->get_axis_title()
  ; (*info).xtitle = xytitle[(*info).xdim]
  ; (*info).ytitle = xytitle[(*info).ydim]
  xscale = [min(*(*info).xscale), max(*(*info).xscale)]
  dx = xscale[1] - xscale[0]
  IF (*info).aspect LT 0.05 THEN xtickinterval = (dx / 2.) ELSE xtickinterval = (dx / 3.)
  mplot_image, (*(*info).drawimage) ^ gamma, *(*info).xscale, *(*info).yscale, $
    bgblack = bgblack, xtickformat = '(f7.1)', xtickinterval = xtickinterval, ticklen = ticklen, $
    xtitle = (*info).xtitle, ytitle = (*info).ytitle, color = (*info).color, min = imin, max = imax
  IF imax - imin EQ 0.0 THEN imax = imin + 1
  IF imax - imin LT 10 THEN format = '(f7.3)' ELSE format = '(f10.1)'
  IF (!p.charsize NE 0) THEN pcharsize = !p.charsize ELSE pcharsize = 1.0
  xpos_cb_0 = 1.0 - (!x.margin[1] - 2) * !d.x_ch_size / !d.x_size * pcharsize
  xpos_cb_1 = 1.0 - (!x.margin[1] - 4) * !d.x_ch_size / !d.x_size * pcharsize
  ypos_cb_0 = !y.margin[0] * !d.y_ch_size / !d.y_size * pcharsize
  ypos_cb_1 = 1.0 - !y.margin[1] * !d.y_ch_size / !d.y_size * pcharsize
  hw_colorbar, position = [xpos_cb_0, ypos_cb_0, xpos_cb_1, ypos_cb_1], $
    /vertical, /right, format = format, title = (*info).colorbar_title, $
    range = [imin, imax], color = (*info).color, charsize = pcharsize
  IF (*info).defcol THEN loadct, 0
END

; get the value of the draw window option menu:
FUNCTION spice_xmap_dwoption, event
  widget_control, event.top, get_uvalue = info
  (*info).dwoption = event.value
  return, 0
END

; set screen size to preset value
FUNCTION spice_xmap_drawsizeoption, event
  IF event.select EQ 0 THEN return, 0
  widget_control, event.top, get_uvalue = info
  w_ysz = (*info).tlb_ysz - (*info).d_ysz
  CASE event.value OF
    0: xysz = (*info).standard_size
    1: xysz = (*info).big_size
  ENDCASE
  ; if (*info).keep_aspect then begin
  ; xysz=(*(*info).data->getaux())->getdrawsize(sizemode,aspect=(*info).aspect)
  ; endif else begin
  ; xysz=(*(*info).data->getaux())->getdrawsize(sizemode)
  ; endelse
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, top: (*info).tlb, handler: 0l, $
    x: xysz[0] + (*info).lcol_xsz, y: xysz[1] + w_ysz}
  spice_xmap_resize, pseudoevent
  return, 0
END

; wavelength selection buttons
FUNCTION spice_xmap_dsoption, event
  IF event.select EQ 0 THEN return, 0
  CASE event.value OF
    0: spice_xmap_wpix, event
    1: spice_xmap_warcsec, event
  ENDCASE
  return, 0
END

; change x/y axes scale to pixels
PRO spice_xmap_wpix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data.get_axis_title((*info).xdim, /pixels)
  (*info).ytitle = *(*info).data.get_axis_title((*info).ydim, /pixels)
  ; set scale for images
  *(*info).xscale = (*info).xscale_pixels
  *(*info).yscale = (*info).yscale_pixels
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: event.top, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; change x/y scale to arcsec
PRO spice_xmap_warcsec, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data.get_axis_title((*info).xdim)
  (*info).ytitle = *(*info).data.get_axis_title((*info).ydim)
  ; set scale for images
  *(*info).xscale = (*info).xscale_physical
  *(*info).yscale = (*info).yscale_physical
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: event.top, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

PRO spice_xmap_aspect, event
  widget_control, event.top, get_uvalue = info
  (*info).keep_aspect = event.select
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: event.top, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

PRO spice_xmap_default_colors, event
  widget_control, event.top, get_uvalue = info
  (*info).defcol = event.select
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: event.top, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; slider to select exposure within a raster pos (if multiple)
PRO spice_xmap_expprp_slider, event
  widget_control, event.top, get_uvalue = info
  (*info).exprp = event.value
  nr = (*info).exprp - 1
  (*info).expindx = indgen((*info).nraster) * (*info).nexpprp + (*info).exprp - 1

  widget_control, (*info).exposuretext, $
    set_value = strtrim(string(*(*info).data.getexp(nr), $
      format = '(a,f5.2,a)'), 2) + ' s'

  ; create resize pseudoevent to draw selected line
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: (*info).tlb, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; select line from linelist
PRO spice_xmap_lineselect, event
  widget_control, event.top, get_uvalue = info
  (*info).line = (*info).linelist[event.index]

  ; create resize pseudoevent to draw selected line
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: (*info).tlb, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; select data product to display (in case of moments)
PRO spice_xmap_dpselect, event
  widget_control, event.top, get_uvalue = info
  (*info).dpnr = event.index
  moment_name = *(*info).data.getmomentnames((*info).dpnr)
  ;
  IF moment_name EQ 'Velocity' THEN BEGIN
    gamma = 1.0
    ; idl-disable-next-line unknown-structure
    pseudoevent = {widget_slider, id: 0l, $
      top: event.top, handler: 0l, value: gamma, drag: 1}
    !NULL = spice_xmap_gamma(pseudoevent)
    widget_control, (*info).gamma_slider, sensitive = 0
  ENDIF ELSE widget_control, (*info).gamma_slider, sensitive = 1
  (*info).color = 255
  IF (*info).defcol THEN BEGIN
    CASE moment_name OF
      'Intensity': (*(*info).data.getaux()).loadct, 'int'
      'Velocity': BEGIN
        (*(*info).data.getaux()).loadct, 'vel'
        (*info).color = 127
      END
      'Line width': (*(*info).data.getaux()).loadct, 'wid'
      ELSE: loadct, 0
    ENDCASE
  ENDIF
  ; create resize pseudoevent to draw selected data
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: (*info).tlb, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; zoom in draw window:
PRO spice_xmap_zoom, event
  widget_control, event.top, get_uvalue = info
  IF event.type GT 2 THEN return

  ; set up axis titles for line plots (options 1 or 2 below)
  ; varname = *(*info).data->getvariablename()
  ; varname = varname[0] +': column average'
  ; xytitle = *(*info).data->getxytitle()

  events = ['down', 'up', 'motion']
  thisevent = events[event.type]
  window, /pixmap, /free, xsize = (*info).d_xsz, ysize = (*info).d_ysz
  dx = max(*(*info).xscale) - min(*(*info).xscale)
  IF (*info).aspect LT 0.05 THEN xtickinterval = (dx / 2.) ELSE xtickinterval = (dx / 3.)
  imin = (*info).imin
  imax = (*info).imax
  mplot_image, (*(*info).drawimage) ^ (*info).gamma, *(*info).xscale, *(*info).yscale, $
    min = imin, max = imax, $
    xtickformat = '(f6.1)', xtickinterval = xtickinterval, $
    xtitle = (*info).xtitle, ytitle = (*info).ytitle, /bgblack, color = (*info).color
  imageposx = [!x.margin[0] * !d.x_ch_size, (*info).d_xsz - !x.margin[1] * !d.x_ch_size]
  imageposy = [!y.margin[0] * !d.y_ch_size, (*info).d_ysz - !y.margin[1] * !d.y_ch_size]
  imagepos = [imageposx[0], imageposy[0], imageposx[1], imageposy[1]]
  ;
  IF imax - imin EQ 0.0 THEN imax = imin + 1
  IF imax - imin LT 10 THEN format = '(f7.3)' ELSE format = '(f10.1)'
  IF (!p.charsize NE 0) THEN pcharsize = !p.charsize ELSE pcharsize = 1.0
  xpos_cb_0 = 1.0 - (!x.margin[1] - 2) * !d.x_ch_size / !d.x_size * pcharsize
  xpos_cb_1 = 1.0 - (!x.margin[1] - 4) * !d.x_ch_size / !d.x_size * pcharsize
  ypos_cb_0 = !y.margin[0] * !d.y_ch_size / !d.y_size * pcharsize
  ypos_cb_1 = 1.0 - !y.margin[1] * !d.y_ch_size / !d.y_size * pcharsize
  hw_colorbar, position = [xpos_cb_0, ypos_cb_0, xpos_cb_1, ypos_cb_1], $
    /vertical, /right, format = format, title = (*info).colorbar_title, $
    range = [imin, imax], color = (*info).color, charsize = pcharsize
  (*info).pixid = !d.window

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
      device, copy = [0, 0, (*info).d_xsz, (*info).d_ysz, 0, 0, $
        (*info).pixid]
      widget_control, (*info).drawid, draw_motion_events = 0
      image = (*(*info).drawimage) ^ (*info).gamma
      sz = size(image)
      dxfac = 1. / (imagepos[2] - imagepos[0])
      dyfac = 1. / (imagepos[3] - imagepos[1])
      sx = ((*info).sx - imagepos[0]) * dxfac * sz[1]
      sy = ((*info).sy - imagepos[1]) * dyfac * sz[2]
      dx = (event.x - imagepos[0]) * dxfac * sz[1]
      dy = (event.y - imagepos[1]) * dyfac * sz[2]
      sx = (sx < sz[1] - 1) > 0
      sy = (sy < sz[2] - 1) > 0
      dx = (dx < sz[1] - 1) > 0
      dy = (dy < sz[2] - 1) > 0
      image = image[sx < dx : sx > dx, sy < dy : sy > dy]
      xscale = *(*info).xscale
      yscale = *(*info).yscale
      xscale = xscale[sx < dx : sx > dx]
      yscale = yscale[sy < dy : sy > dy]
      sz = size(image)
      mind = min(sz[0 : 2])
      CASE (*info).dwoption OF
        0: BEGIN
          IF mind GE 2 THEN BEGIN
            xmax = (*info).screensize[0]
            ymax = (*info).screensize[1]
            image = congrid(image, sz[1] * 2 < xmax, sz[2] * 2 < ymax)
            xscale = interpol(xscale, sz[1] * 2 < xmax)
            yscale = interpol(yscale, sz[2] * 2 < ymax)
            spice_xzoom, image, xscale, yscale, xtitle = (*info).xtitle, $
              ytitle = (*info).ytitle, group_leader = event.top, n_subplot = (*info).n_subplot
          ENDIF
        END
        1: BEGIN
          ; set up axis titles for line plots (options 1 or 2 below)
          varname = *(*info).data.get_variable_unit()
          varname = varname[0]
          xytitle = *(*info).data.get_axis_title()
          dmean = total(image, 2) / sz[2]
          IF sz[0] GE 2 THEN BEGIN
            spice_xlineplot, dmean, xscale = xscale, $
              title = varname, $
              xtitle = xytitle[(*info).xdim], $
              ytitle = varname, $
              groupl = event.top, n_subplot = (*info).n_subplot
          ENDIF
        END
        2: BEGIN
          ; set up axis titles for line plots (options 1 or 2 below)
          varname = *(*info).data.get_variable_unit()
          varname = varname[0]
          xytitle = *(*info).data.get_axis_title()
          dmean = total(image, 1) / sz[1]
          IF sz[0] GE 2 THEN BEGIN
            spice_xlineplot, dmean, xscale = yscale, $
              title = varname, $
              xtitle = xytitle[(*info).ydim], $
              ytitle = varname, $
              groupl = event.top, n_subplot = (*info).n_subplot
          ENDIF
        END
      ENDCASE
      (*info).n_subplot = (*info).n_subplot + 1
    ENDCASE
    'motion': BEGIN
      ; erase previous box
      ; draw new box
      dx = event.x
      dy = event.y
      sx = (*info).sx
      sy = (*info).sy
      wset, (*info).wid
      device, copy = [0, 0, (*info).d_xsz, (*info).d_ysz, 0, 0, (*info).pixid]
      plots, [sx, sx, dx, dx, sx], [sy, dy, dy, sy, sy], /device, $
        color = (*info).drawcolor
    ENDCASE
  ENDCASE
  wdelete, (*info).pixid
  NOACTION:
END

; select color table
PRO spice_xmap_colors, event
  widget_control, event.top, get_uvalue = info
  thisevent = tag_names(event, /structure_name)
  CASE thisevent OF
    'WIDGET_BUTTON': BEGIN
      widget_control, event.top, TLB_GET_OFFSET = offset_parent
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'spice_xmap colors (' + strtrim((*info).wid, 2) + ')', $
        group_leader = event.top, notifyid = [event.id, event.top], $
        xoffset = offset_parent[0] + 50, yoffset = offset_parent[1] + 50
    ENDCASE
    'XCOLORS_LOAD': BEGIN
      (*info).r = event.r[(*info).bottom : (*info).ncolors - 1 + (*info).bottom]
      (*info).g = event.g[(*info).bottom : (*info).ncolors - 1 + (*info).bottom]
      (*info).b = event.b[(*info).bottom : (*info).ncolors - 1 + (*info).bottom]
      IF !d.n_colors GT 256 THEN BEGIN
        ; idl-disable-next-line unknown-structure
        pseudoevent = {widget_button, id: 0l, $
          top: event.top, handler: 0l, select: 1}
        spice_xmap_draw, pseudoevent
      ENDIF
    ENDCASE
  ENDCASE
  widget_control, event.top, set_uvalue = info
END

; select color table
PRO spice_xmap_bgr, event
  widget_control, event.top, get_uvalue = info
  (*(*info).data.getaux()).loadct, 'vel'
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xmap_draw, pseudoevent
  widget_control, event.top, set_uvalue = info
END

; protect colors
PRO spice_xmap_protect_colors, event
  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
END

PRO spice_xmap_momminslider, event
  widget_control, event.top, get_uvalue = info
  (*info).min = event.value
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xmap_draw, pseudoevent
END

PRO spice_xmap_mommaxslider, event
  widget_control, event.top, get_uvalue = info
  (*info).max = event.value
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xmap_draw, pseudoevent
END

; resize main window and set up drawimage
PRO spice_xmap_resize, event
  widget_control, event.top, get_uvalue = info
  ; set up image in it`s original size:
  ; if (*info).xdim eq 2 or (*info).xdim eq 1 or (*info).xdim eq 3 then rotate=4 else rotate=1
  ; if (*info).comment eq 'IRIS_moment' then begin
  ; moment_name=*(*info).data->getmomentnames((*info).dpnr)
  ; wd=(*(*info).data)->getvar((*info).line)
  ; drawimage = rotate(reform(wd[(*info).dpnr, *, *]),rotate)
  ; (*info).colorbar_title=(*(*info).data->getmomentnames((*info).dpnr))+$
  ; ' '+(*(*info).data->getmomentunits((*info).dpnr))
  ; endif else begin
  wd = *(*info).wd
  ; lpx=(*(*info).data-> getwd_def())[(*info).line].line_px
  ; cpx=[0,0];(*(*info).data-> getwd_def())[(*info).line].cont_px
  sz = size(wd)
  ; csz = size(wd[cpx[0]:cpx[1],*,*])
  ; if (*info).nexpprp eq 1 then begin
  ; if csz[3] gt 1 then begin
  ; var=wd[cpx[0]:cpx[1],*,*]
  ; bad=where(var eq *(*info).data->get_missing_value(),nbad)
  ; if nbad ne 0 then var[bad]=!values.f_nan
  ; cont = total(var,3,/nan)/csz[3]
  ; endif else cont=0.0
  cont = 0
  var = wd
  bad = where(var EQ *(*info).data.get_missing_value(), nbad)
  IF nbad NE 0 THEN var[bad] = !values.f_nan
  IF sz[0] GE 3 THEN drawimage = total(var, 3, /nan) / sz[3] - cont $
  ELSE drawimage = var - cont
  IF *(*info).data.get_sit_and_stare() THEN BEGIN
    drawimage = reform(drawimage)
    drawimage = rotate(drawimage, 1)
  ENDIF
  ; endif else begin
  ; if csz[1] gt 1 then begin
  ; var=wd[cpx[0]:cpx[1],*,(*info).expindx]
  ; bad=where(var eq *(*info).data->missing(),nbad)
  ; if nbad ne 0 then var[bad]=!values.f_nan
  ; cont = total(var,1,/nan)/csz[1]
  ; endif else cont=0.0
  ; var=wd[lpx[0]:lpx[1],*,(*info).expindx]
  ; bad=where(var eq *(*info).data->missing(),nbad)
  ; if nbad ne 0 then var[bad]=!values.f_nan
  ; drawimage = rotate(total(var,1,/nan)/sz[1]-cont,rotate)
  ; endelse
  ; if *(*info).data->getwscale() eq 'arcsec' then
  ; drawimage=spice_xwarp(drawimage,*(*info).data)
  (*info).ct = 0
  ; endelse
  (*info).max = 255
  (*info).min = 0
  widget_control, (*info).momminslider, set_slider_min = 0, $
    set_slider_max = 255, $
    set_value = 0
  widget_control, (*info).mommaxslider, set_slider_min = 0, $
    set_slider_max = 255, $
    set_value = 255
  ; resize if necessary:
  w_ysz = (*info).tlb_ysz - (*info).d_ysz
  m_xsz = total(!x.margin) * !d.x_ch_size
  m_ysz = total(!y.margin) * !d.y_ch_size
  widget_control, (*info).tlb, tlb_get_offset = tlb_offset
  IF event.x NE (*info).tlb_xsz OR event.y NE (*info).tlb_ysz THEN BEGIN
    IF (*info).keep_aspect NE 0 THEN BEGIN
      s_fac = 0.97
      (*info).d_xsz = (((event.x - (*info).lcol_xsz) > 1) < $
        ((((*info).screensize[1] - tlb_offset[1]) * s_fac - w_ysz - m_ysz) * (*info).aspect)) + m_xsz
      (*info).d_ysz = ((*info).d_xsz - m_xsz) / (*info).aspect + m_ysz
    ENDIF ELSE BEGIN
      (*info).d_xsz = (event.x - (*info).lcol_xsz) > 1
      (*info).d_ysz = event.y - w_ysz
    ENDELSE
    IF (*info).d_ysz + tlb_offset[1] GT (*info).screensize[1] THEN yoff = 10
    widget_control, (*info).tlb, yoffset = yoff
    widget_control, (*info).drawid, xsize = (*info).d_xsz, ysize = (*info).d_ysz
    widget_control, (*info).tlb, tlb_get_size = tlb_sz
    (*info).tlb_xsz = tlb_sz[0]
    (*info).tlb_ysz = tlb_sz[1]
  ENDIF
  ; resize image and axis if not original image size
  ; if *(*info).data->get_sit_and_stare() then begin
  ; xscale=*(*info).data->get_time((*info).line)
  ; angle=round(*(*info).data->get_satellite_rotation())
  ; if angle lt 0 then angle=360+angle
  ; if angle eq 90 or angle eq 270 then begin
  ; yscale=*(*info).data->get_instr_x_vector()
  ; endif else yscale=*(*info).data->get_instr_y_vector()
  ; sx={xtitle:'Time [s]',rot:0}
  ; endif else begin
  ; xscale=*(*info).data->get_instr_x_vector((*info).line)
  ; yscale=*(*info).data->get_instr_y_vector((*info).line)
  ; endelse
  ; xscale_pixels = findgen(N_ELEMENTS(xscale))
  ; xscale_physical = xscale
  ; yscale_pixels = findgen(N_ELEMENTS(yscale))
  ; yscale_physical = yscale
  ; drawimage=rotate(drawimage,sx.rot)
  sz = size(drawimage)
  nx = sz[1]
  ny = sz[2]
  ; TODO:
  ; if *(*info).data->getwscale() eq 'pixels' then begin
  ; xscale=findgen(nx)
  ; yscale=findgen(ny)
  ; endif
  ; dx=(max(xscale)-min(xscale))/(nx)
  ; dy=(max(yscale)-min(yscale))/(ny)
  ; if (*info).realsize then begin
  ; ptr_free,(*info).drawimage
  ; (*info).drawimage = ptr_new(uintarr(nx, ny))
  ; *(*info).drawimage = drawimage
  ; ptr_free,(*info).xscale
  ; (*info).xscale = ptr_new(nx)
  ; *(*info).xscale = interpol(xscale,nx)
  ; ptr_free,(*info).yscale
  ; (*info).yscale = ptr_new(ny)
  ; *(*info).yscale = yscale
  ; endif else begin
  ; ptr_free,(*info).xscale
  ; (*info).xscale = ptr_new(nx)
  ; *(*info).xscale = interpol(xscale,nx)
  ; ptr_free,(*info).yscale
  ; (*info).yscale = ptr_new(ny)
  ; *(*info).yscale = interpol(yscale,ny)
  ptr_free, (*info).drawimage
  (*info).drawimage = ptr_new(uintarr(nx, ny))
  *(*info).drawimage = drawimage
  ; endelse
  (*info).tlb_xsz = event.x
  (*info).tlb_ysz = event.y
  ; draw image
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xmap_draw, pseudoevent
END

PRO spice_xmap_pixplot, event
  widget_control, event.top, get_uvalue = info
  thisevent = tag_names(event, /structure_name)
  CASE thisevent OF
    'WIDGET_DROPLIST': BEGIN
      mode = event.index
    END
    ELSE:
  ENDCASE
  ; set up titles for plot
  xytitle = *(*info).data.get_axis_title()
  varname = (*info).colorbar_title
  CASE mode OF
    0: BEGIN
      return
    END
    1: BEGIN
      spice_xlineplot, *(*info).drawimage, $
        xscale = *(*info).xscale, $
        xtitle = xytitle[(*info).xdim], $
        ytitle = varname, $
        cslider_title = xytitle[(*info).ydim], $
        groupl = (*info).tlb
    END
    2: BEGIN
      spice_xlineplot, transpose(*(*info).drawimage), $
        xtitle = xytitle[(*info).ydim], $
        ytitle = varname, $
        cslider_title = xytitle[(*info).xdim], $
        groupl = (*info).tlb
    END
    ELSE: print, 'spice_xmap_lineplot: Impossible mode!!'
  ENDCASE
END

; Toggle masking on/off
PRO spice_xmap_mask, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).maskbutton, get_value = masking
  wd = *(*info).data.get_window_data((*info).line, no_masking = masking EQ 0)
  *(*info).wd = wd
  ; draw image
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: (*info).tlb, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; save data
PRO spice_xmap_save_moments, event
  widget_control, event.top, get_uvalue = info

  IF obj_isa(*(*info).data, 'eis_moment') THEN $
    (*(*info).data).save, iwin = (*info).linelist
END

PRO spice_xmap_linedef, event
  widget_control, event.top, get_uvalue = info
  widget_control, /hourglass
  ; if *(*info).data->getcomment() eq 'IRIS_moment' then begin
  ; lambda=*(*info).data->getwavelength((*info).line)
  ; mspec=*(*info).data->getmspec((*info).line)
  ; wlref=1
  ; endif else begin
  wd = *(*info).wd
  lambda = *(*info).data.get_lambda_vector((*info).line)
  mspec = iris_mean_spec(wd, missing = *(*info).data.get_missing_value())
  wlref = 0
  ; endelse
  xmoment_moment, *(*info).data, mspec, (*info).line, lambda, wlref = wlref, groupl = event.top
  IF NOT (ptr_valid(info)) THEN return ; if spice_xmap window closes
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: (*info).tlb, handler: 0l, x: (*info).tlb_xsz, y: (*info).tlb_ysz}
  spice_xmap_resize, pseudoevent
END

; close spice_xmap
PRO spice_xmap_destroy, event
  widget_control, event.top, /destroy
END

PRO spice_xmap_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  wdelete, (*info).mainpixid
  IF (*info).object_created THEN obj_destroy, *(*info).data
  ptr_free, (*info).data
  ptr_free, (*info).drawimage
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, info
END

PRO spice_xmap, input_data, linelist = linelist, group_leader = group_leader, $
  ncolors = ncolors
  IF n_params() LT 1 THEN BEGIN
    message, 'spice_xmap,data,linelist=linelist, group_leader=group,ncolors=ncolors', /cont
    return
  ENDIF

  data = spice_object(input_data, is_spice = is_spice, object_created = object_created)
  IF ~is_spice THEN return

  ; drawing window size in relation to screen
  IF n_elements(scfac) EQ 0 THEN scfac = 0.8
  IF n_elements(standard_size) EQ 0 THEN standard_size = 700
  IF n_elements(ncolors) EQ 0 THEN ncolors = (!d.n_colors < 256)
  IF n_elements(drawcolor) EQ 0 THEN drawcolor = !p.color
  IF n_elements(linelist) EQ 0 THEN linelist = 0
  line = linelist[0]
  ; get axis titles:
  sit_and_stare = data.get_sit_and_stare()
  ;
  nwin = data.get_number_windows()
  nraster = data.get_number_exposures(line) ; number of raster positions
  nexp = data.get_number_exposures(line)
  nexpprp = 1 ; number of exp pr. raster pos.
  ydim = 1
  ;
  ; so far QL can not handle sit-and-stare with different exposure times
  ; (when it is run as "multiple exp pr rast. pos.
  ; Will have to deal with that...
  ; OW 14-april 2005.
  ;
  expindx = 0
  exprp = 0
  ; check if more than one exposure/raster pos. If not return
  IF (sit_and_stare) THEN BEGIN
    nexpprp = 1
    IF nexp LE 1 THEN BEGIN
      warning = 'Less than 2 exposures in raster. Can not build map. Returning'
      ok = dialog_message(warning, /center)
      return
    ENDIF
    ; (data->getaux())->setwscale,'arcsec'
    ; (data->getaux())->setxytitle,sscale='arcsec',tscale='sec'
    xscale = data.get_time_vector(line) / 1000.
    aspect = 1.0
    xdim = 3
  ENDIF ELSE BEGIN
    IF nraster LE 1 THEN BEGIN
      warning = 'Less than 2 raster pos. in raster. Can not build map. Returning'
      ok = dialog_message(warning, /center)
      return
    ENDIF
    ; (data->getaux())->setwscale,'arcsec'
    ; (data->getaux())->setxytitle,sscale='arcsec'
    xscale = data.get_instr_x_vector(line)
    yscale = data.get_instr_y_vector(line)
    aspect = (max(xscale) - min(xscale)) / (max(yscale) - min(yscale))
    ; if sx.rot eq 3 or sx.rot eq 1 then begin
    xdim = 0
    ; ydim=2
    ; endif else begin
    ; xdim=2
    ; ydim=1
    ; endelse
  ENDELSE
  ; drawing window size in relation to screen
  screensize = spice_get_screen_size()
  xsz = standard_size
  ysz = standard_size
  IF aspect GT 1.0 THEN ysz = fix(ysz / aspect) $
  ELSE xsz = fix(xsz * aspect)
  ; xysz=(data->getaux())->getdrawsize('standard',aspect=aspect)
  ; xsz = xysz[0]
  ; ysz = xysz[1]

  ; initialize window size for big-option
  screensize = screensize * scfac
  big_size = fix(max(screensize))
  big_size = [big_size, big_size]
  IF aspect GT 1.0 THEN big_size[1] = fix(big_size[1] / aspect) $
  ELSE big_size[0] = fix(big_size[0] * aspect)
  IF big_size[0] GT screensize[0] THEN BEGIN
    big_size[1] = fix(float(big_size[1]) * screensize[0] / float(big_size[0]))
    big_size[0] = fix(screensize[0])
  ENDIF
  IF big_size[1] GT screensize[1] THEN BEGIN
    big_size[0] = fix(float(big_size[0]) * screensize[1] / float(big_size[1]))
    big_size[1] = fix(screensize[1])
  ENDIF

  IF data.get_sit_and_stare() THEN BEGIN
    xscale = data.get_time_vector(line)
    angle = round(data.get_satellite_rotation())
    IF angle LT 0 THEN angle = 360 + angle
    IF angle EQ 90 OR angle EQ 270 THEN BEGIN
      yscale = data.get_instr_x_vector(line)
    ENDIF ELSE yscale = data.get_instr_y_vector(line)
  ENDIF ELSE BEGIN
    xscale = data.get_instr_x_vector(line)
    yscale = data.get_instr_y_vector(line)
  ENDELSE
  xscale_pixels = findgen(n_elements(xscale))
  xscale_physical = xscale
  yscale_pixels = findgen(n_elements(yscale))
  yscale_physical = yscale

  ;
  IF nexpprp GT 1 THEN BEGIN
    exprp = 1 ; intitialize first exp at each raster pos.
    expindx = indgen(nraster) * nexpprp + exprp - 1
  ENDIF

  message = ['Loading data into memory...', '...this may take some time']
  xmessage, message, wbase = wbase, font = 'helvetica'
  widget_control, /hourglass
  ; comment = data->getcomment()
  ; if comment eq 'IRIS_moment' then begin
  ; data->getwin,line,wd,pos,/load,/noscale
  ; line/cont limits defined in xmoments
  ; endif else begin
  ; data->getwin,line,wd,pos,/load
  wd = data.get_window_data(line)
  ; line/cont limits reset
  ; data->setline_px,line,[0,(size(wd))[1]-1]
  ; data->setcont_px,line,[0,0]
  ; endelse
  xkill, wbase

  !x.margin[1] = 15 ; space for colorbar
  calc_xysize, xsz, ysz, d_xsz, d_ysz, nxchar = total(!x.margin)
  standard_size = [d_xsz, d_ysz]
  calc_xysize, big_size[0], big_size[1], d_xsz_big, d_ysz_big, nxchar = total(!x.margin)
  big_size = [d_xsz_big, d_ysz_big]
  ; initialize size of draw window:
  window, /pixmap, /free, xsize = d_xsz, ysize = d_ysz
  pixid = !d.window

  ; base widget:
  xwt = 'SPICE_Xmap - ' + data.get_title() + '   ' ; spice_xmap window title
  tlb = widget_base(/row, title = xwt, tlb_size_events = 1, $
    mbar = menubar, xoffset = 100, yoffset = 100, group_leader = group_leader)
  lcol = widget_base(tlb, /frame, /column) ; left column.
  rcol = widget_base(tlb, /column) ; right column.

  ; create pulldown menus on the base widget menubar
  filemenu = widget_button(menubar, value = 'File', /menu, uvalue = 'file')
  savemenu = widget_button(filemenu, value = 'Save as', uvalue = 'save', /menu)
  psmenu = widget_button(savemenu, value = 'Postscript', $ ; idl-disable-line unused-var
    event_pro = 'spice_xmap_ps')
  jpgmenu = widget_button(savemenu, value = 'JPG', event_pro = 'spice_xmap_jpeg') ; idl-disable-line unused-var
  exitmenu = widget_button(filemenu, value = 'Close', $ ; idl-disable-line unused-var
    event_pro = 'spice_xmap_destroy')

  optmenu = widget_button(menubar, value = 'Options', uvalue = 'options')
  colmenu = widget_button(optmenu, value = 'Colour table', $ ; idl-disable-line unused-var
    event_pro = 'spice_xmap_colors')
  ; colmenu=widget_button(optmenu, value='Color table BGR', $
  ; event_pro='spice_xmap_bgr')
  ; display window:
  displaybase = widget_base(rcol, /row)
  drawid = widget_draw(displaybase, retain = 2, $
    xsize = d_xsz, ysize = d_ysz, $
    /button_events, event_pro = 'spice_xmap_zoom')
  ; if comment eq 'IRIS_moment' then begin
  ; colorbar_title=(data->gettitle())+' '+data->getmomentunits(0)
  ; endif else begin
  colorbar_title = data.get_title() + ' ' + data.get_variable_unit()
  ; endelse
  ; information on raster
  titletext = widget_label(lcol, value = data.get_start_time() + ' ' + data.get_obs_id(), /align_center) ; idl-disable-line unused-var
  lsubcol = widget_base(lcol, /row)
  id = data.get_window_id(line)
  idtext = widget_label(lcol, value = strtrim(id, 2), /align_left) ; idl-disable-line unused-var
  exposuretext = widget_label(lcol, $ ; idl-disable-line unused-var
    value = 'Exp = ' + strtrim(string(data.get_exposure_time(line), format = '(a,f5.2)'), 2) + ' s', $
    /align_left)
  ; TODO:
  ; xycentext = widget_label(lcol, $
  ; value = 'Xcen: '+ string((data->getxcen(line)),format='(f8.2)')+ $
  ; ' Ycen: '+ string((data->getycen(line)),format='(f8.2)'), $
  ; /align_left)
  ; menu for controlling action in draw window
  dwoption = widget_base(lcol, /column, /frame)
  dwoption_title = widget_label(dwoption, value = 'Apply chosen action to', /align_left)
  dwoption_title = widget_label(dwoption, value = 'user-defined rectangle', /align_left)
  dwoption_title = widget_label(dwoption, value = '(click and hold to draw rectangle)', /align_left)
  menu = ['Zoom', 'Average along wavelength', 'Average along slit', $
    'Average along raster posistion', 'Average in Time']
  IF sit_and_stare THEN BEGIN
    dwoption_names = [menu[0], menu[2], menu[4]]
  ENDIF ELSE BEGIN
    dwoption_names = [menu[0], menu[2], menu[3]]
  ENDELSE
  dwoption_menu = cw_bgroup(dwoption, dwoption_names, /return_index, $
    /exclusive, set_value = 0, $
    event_func = 'spice_xmap_dwoption')
  ; slider for case of seveal exposure per raster position
  IF nexpprp GT 1 THEN BEGIN
    sliderbase = widget_base(lsubcol, /column)
    exprp = 1
    expprpslider = widget_slider(sliderbase, xsize = 90, $ ; idl-disable-line unused-var
      minimum = 1, maximum = nexpprp, $
      title = 'Exp.# at rast. pos.', $
      value = 1, $
      event_pro = 'spice_xmap_expprp_slider')
  ENDIF
  ; menu for line plots of pixel values
  pixplotfield = widget_base(lcol, /column, /frame)
  pixnames = ['Not active', 'Row plot', 'Column plot']
  pixelplot = widget_droplist(pixplotfield, value = pixnames, $
    title = 'Plot pixel values', $
    event_pro = 'spice_xmap_pixplot')

  maskfield = widget_base(lcol, /column, /frame, event_pro = 'spice_xmap_mask')
  maskbutton = cw_bgroup(maskfield, ['Mask regions outside slit'], $
    set_value = [1], /nonexclusive)

  ; menu for preset widget sizes
  drawsizeoption = widget_base(lcol, /column, /frame)
  drawsizeoption_title = widget_label(drawsizeoption, value = 'Resize widget') ; idl-disable-line unused-var
  menu = ['Standard', 'Big']
  drawsizeoption_names = menu
  drawsizeoption_menu = cw_bgroup(drawsizeoption, drawsizeoption_names, $ ; idl-disable-line unused-var
    /return_index, $
    /exclusive, set_value = 0, $
    event_func = 'spice_xmap_drawsizeoption')
  ; keep true aspect option on/off
  aspectfield = widget_base(lcol, /row, /nonexclusive)
  aspectbutton = widget_button(aspectfield, $ ; idl-disable-line unused-var
    value = 'Keep aspect', $
    event_pro = 'spice_xmap_aspect')
  ; default colors
  defcolfield = widget_base(lcol, /column, /nonexclusive) ; idl-disable-line unused-var
  defcolbutton = widget_button(aspectfield, $ ; idl-disable-line unused-var
    value = 'Default colors', $
    event_pro = 'spice_xmap_default_colors')

  ; create droplist menu for selecting line
  IF n_elements(linelist) GT 1 THEN BEGIN
    ll = widget_base(lcol, /column, /frame)
    ll_title = 'Select line'
    ll_names = data.get_window_id(linelist)
    ll_menu = widget_droplist(ll, value = ll_names, title = ll_title, $ ; idl-disable-line unused-var
      event_pro = 'spice_xmap_lineselect')
  ENDIF

  IF NOT sit_and_stare THEN BEGIN
    dsbase = widget_base(lcol, /row, /frame)
    ds_names = ['Pixels/step', 'Arcsec']
    dsbutton = cw_bgroup(dsbase, ds_names, /return_index, $ ; idl-disable-line unused-var
      /exclusive, set_value = 1, $
      event_func = 'spice_xmap_dsoption')
  ENDIF

  ; missing=data->get_missing_value()
  gammacol = widget_base(lcol, /row)
  gamma = 1.0
  gamma_slider = cw_fslider(gammacol, /edit, format = '(f6.2)', /frame, $
    maximum = 3.0, minimum = 0.1, value = gamma, $
    title = 'Gamma Correction', $
    event_func = 'spice_xmap_gamma', /drag)

  histo_lim = -3.0
  histoopt_slider = cw_fslider(gammacol, /edit, format = '(f6.2)', /frame, $ ; idl-disable-line unused-var
    maximum = -1.0, minimum = -6.0, value = histo_lim, $
    title = 'log(HistoOpt Value)', $
    event_func = 'spice_xmap_histoopt', /drag)
  histo_lim = 10. ^ histo_lim

  ; create menu for selecting data product (if moments/gauss_fit):
  ; comment = data->getcomment()
  dp_base = widget_base(lcol, /column, /frame) ; idl-disable-line unused-var
  ; if comment eq 'IRIS_moment' then begin
  ; dp_title = 'Select data'
  ; dp_names = data->getvariablename()
  ; dp_names = dp_names[where (dp_names ne '')]
  ; dp_menu = widget_droplist(dp_base, value = dp_names, title = dp_title, $
  ; event_pro = 'spice_xmap_dpselect')
  ; imin = min(iris_histo_opt(wd[0, *, *],missing=missing),/nan)
  ; imax = max(iris_histo_opt(wd[0, *, *],missing=missing),/nan)
  ; endif else begin
  ; lpx=(data->getwd_def())[line].line_px
  cpx = [0, 0] ; (data->getwd_def())[line].cont_px
  sz = size(wd)
  csz = size(wd[cpx[0] : cpx[1], *, *])
  ;
  ; if nexpprp eq 1 then begin
  IF csz[1] GT 1 THEN BEGIN
    var = wd[cpx[0] : cpx[1], *, *]
    bad = where(var EQ data.get_missing_value(), nbad)
    IF nbad NE 0 THEN var[bad] = !values.f_nan
    cont = total(var, 1, /nan) / csz[1]
  ENDIF ELSE cont = 0.0
  var = wd
  bad = where(var EQ data.get_missing_value(), nbad)
  IF nbad NE 0 THEN var[bad] = !values.f_nan
  ; endif else begin
  ; if csz[1] gt 1 then begin
  ; var=wd[cpx[0]:cpx[1],*,expindx]
  ; bad=where(var eq data->get_missing_value(),nbad)
  ; if nbad ne 0 then var[bad]=!values.f_nan
  ; cont = total(var,1,/nan)/csz[1]
  ; endif else cont=0.0
  ; var=wd[lpx[0]:lpx[1],*,expindx]
  ; bad=where(var eq data->get_missing_value(),nbad)
  ; if nbad ne 0 then var[bad]=!values.f_nan
  ; endelse
  drawimage = rotate(total(var, 1, /nan) / sz[1] - cont, 0)
  imin = min(iris_histo_opt(drawimage, missing = data.get_missing_value()), /nan)
  imax = max(iris_histo_opt(drawimage, missing = data.get_missing_value()), /nan)
  ; endelse
  mommbase = widget_base(lcol, /row, /frame)
  mommintitle = 'Minimum plot value'
  mommaxtitle = 'Maximum plot value'
  momminslider = widget_slider(mommbase, xsize = 142, $
    maximum = 255, minimum = 0, value = 0, $
    title = mommintitle, $
    event_pro = 'spice_xmap_momminslider', /drag, /frame)
  mommaxslider = widget_slider(mommbase, xsize = 142, $
    maximum = 255, minimum = 0, value = 255, $
    title = mommaxtitle, $
    event_pro = 'spice_xmap_mommaxslider', /drag, /frame)
  ;
  buttonrow1 = widget_base(lcol, /row)
  ; if comment eq 'moments' then begin
  ; savefield = widget_base(buttonrow1, /column)
  ; savebutton = widget_button(savefield, value = 'Save Moments', $
  ; event_pro = 'spice_xmap_save_moments')
  ; endif else begin
  linedeffield = widget_base(buttonrow1, /column)
  linedefbutton = widget_button(linedeffield, value = 'Define Line', $ ; idl-disable-line unused-var
    event_pro = 'spice_xmap_linedef')
  ; endelse
  closefield = widget_base(buttonrow1, /column)
  closebutton = widget_button(closefield, value = 'Close     ', $ ; idl-disable-line unused-var
    event_pro = 'spice_xmap_destroy')

  ; ; eis_icon_base=widget_base(lcol,/col)
  ; ; eis_icon                   = widget_draw(eis_icon_base  ,             $
  ; ;                               retain      = 2,                        $
  ; ;                               XSize       = 210,                      $
  ; ;                               YSize       = 105,                      $
  ; ;                               frame       = 1)

  ; realize main window:
  wp = widget_positioner(tlb, parent = group_leader)
  wp.position
  widget_control, tlb, tlb_get_size = tlb_sz

  ; define size of widget and the menu column
  tlb_xsz = tlb_sz[0] ; xsize of whole widget in pixels
  tlb_ysz = tlb_sz[1] ; ysize of whole widget in pixels
  lcol_xsz = tlb_xsz - d_xsz

  ; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid

  ; get and save color table
  tvlct, r, g, b, /get
  bottom = 0
  IF (!d.n_colors LE 256) THEN BEGIN
    r = r[bottom : ncolors - 1 + bottom]
    g = g[bottom : ncolors - 1 + bottom]
    b = b[bottom : ncolors - 1 + bottom]
  ENDIF
  ;
  imagepos = [0.10, 0.10, 0.77, 0.95]
  ; define the info structure, used send information around
  info_struct = {drawimage: ptr_new(), $
    wd: ptr_new(wd, /no_copy), $
    ; pos:pos, $
    n_subplot: 0, $
    xscale: ptr_new(xscale_physical), $
    yscale: ptr_new(yscale_physical), $
    xscale_pixels: xscale_pixels, $
    xscale_physical: xscale_physical, $
    yscale_pixels: yscale_pixels, $
    yscale_physical: yscale_physical, $
    max: 0.0, $
    min: 0.0, $
    ct: 0, $
    data: ptr_new(data), $
    object_created: object_created, $
    xdim: xdim, $
    ydim: ydim, $
    nwin: nwin, $
    nraster: nraster, $
    nexp: nexp, $
    nexpprp: nexpprp, $
    exprp: exprp, $
    expindx: expindx, $
    rpos: 0, $
    line: line, $
    linelist: linelist, $
    dpnr: 0, $
    maskbutton: maskbutton, $
    ; comment:comment, $
    screensize: screensize, $
    keep_aspect: 0, $
    standard_size: standard_size, $
    big_size: big_size, $
    imagepos: imagepos, $
    lcol_xsz: lcol_xsz, $
    d_xsz: d_xsz, $
    d_ysz: d_ysz, $
    aspect: aspect, $
    defcol: 0, $
    x_scroll_size: d_xsz, $
    y_scroll_size: d_ysz, $
    tlb: tlb, $
    tlb_xsz: tlb_xsz, $
    tlb_ysz: tlb_ysz, $
    realsize: 1, $
    lcol: lcol, $
    rcol: rcol, $
    r: r, g: g, b: b, $
    bottom: bottom, $
    color: 255, $
    ncolors: ncolors, $
    sx: 0, $
    sy: 0, $
    messenger: 0, $
    dwoption: 0, $
    dwoption_menu: dwoption_menu, $
    drawid: drawid, $
    colorbar_title: colorbar_title, $
    pixelplot: pixelplot, $
    drawcolor: drawcolor, $
    mainpixid: pixid, $
    pixid: pixid, $
    xtitle: data.get_axis_title(xdim), $
    ytitle: data.get_axis_title(ydim), $
    gamma_slider: gamma_slider, $
    gamma: gamma, $
    histo_lim: histo_lim, $
    imin: imin, $
    imax: imax, $
    ; missing:missing,$
    momminslider: momminslider, $
    mommaxslider: mommaxslider, $
    wid: wid}

  info = ptr_new(info_struct, /no_copy)
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info

  ; ; widget_control, eis_icon , get_value = drawID
  ; ; wset,drawID
  ; ; fileName = concat_dir(GETENV('ancillary') , 'eis_logo_sarah_small.jpg')
  ; ; read_jpeg , filename , icon
  ; ; icon_resized = CONGRID(icon,3,210,105)
  ; ; tvscl,icon_resized , true = 1

  ; create pseudoevent and send this event to spice_xmap_draw,
  ; in order to draw the image
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_base, id: 0l, $
    top: tlb, handler: 0l, x: tlb_xsz, y: tlb_ysz}
  spice_xmap_resize, pseudoevent

  xmanager, 'ql', tlb, /no_block, event_handler = 'spice_xmap_resize', cleanup = 'spice_xmap_cleanup'
END
