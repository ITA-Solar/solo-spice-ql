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
;       28-Jan-2020: M. Wiesmann    - Rewritten for SPICE as spice_xwhisker (prits-group@astro.uio.no)
;
;-
; $Id: 2025-07-31 13:25 CEST $

; save as postscript file
PRO spice_xwhisker_ps, event
  thisfile = dialog_pickfile(/write, file = 'spice_xwhisker.ps')
  IF thisfile EQ '' THEN return
  widget_control, event.top, get_uvalue = info
  thisdevice = !d.name
  set_plot, 'ps', /copy
  device, file = thisfile, /inches, bits_per_pixel = 8, /color
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xwhisker_draw, pseudoevent
  device, /close_file
  set_plot, thisdevice
END

; save as jpeg file
PRO spice_xwhisker_jpeg, event
  thisfile = dialog_pickfile(/write, file = 'spice_xwhisker.jpg')
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

; display image in the draw window:
PRO spice_xwhisker_draw, event
  widget_control, event.top, get_uvalue = info
  IF !d.name NE 'PS' THEN BEGIN
    wset, (*info).wid
    bgblack = 1
    ticklen = 0.02
  ENDIF ELSE BEGIN
    bgblack = 0
    ticklen = -0.02
  ENDELSE
  widget_control, (*info).drawid, xsize = (*info).d_xsz, $
    ysize = (*info).d_ysz
  ; make new drawimage and axes
  sz = size((*info).image)
  ; region=*(*info).data->getregion((*info).line,/full)
  ; px=*(*info).data->getlambda(region,wscale='pixels')
  ; xscale=*(*info).data->getlambda(region)
  ; pos=(*info).pos
  ; pos[0]=pos[0]-(*(*info).data->getccd(region))[0]
  ; xscale=xscale[pos[0]-px[0]:pos[0]-px[0]+pos[1]-1]
  sit_and_stare = (*info).sit_and_stare
  IF ~(*info).xdim_unit THEN BEGIN
    IF sz[0] EQ 1 THEN xpos = [-0.5, 0.5] $
    ELSE xpos = indgen(sz[1])
  ENDIF ELSE BEGIN
    xpos = *(*info).data.get_lambda_vector((*info).line)
    IF n_elements(xpos) EQ 1 THEN BEGIN
      cdelt = *(*info).data.get_resolution((*info).line, /lambda) / 2.0
      xpos = [xpos - cdelt, xpos + cdelt]
    ENDIF
  ENDELSE
  xscale = xpos
  IF ~(*info).ydim_unit THEN BEGIN
    ypos = indgen(sz[2])
  ENDIF ELSE BEGIN
    IF sit_and_stare THEN ypos = *(*info).data.get_time_vector((*info).line) $
    ELSE ypos = *(*info).data.get_instr_x_vector((*info).line, /auto_diff_rot)
  ENDELSE
  yscale = ypos
  IF sz[0] EQ 1 THEN BEGIN
    drawimage = [[(*info).image], [(*info).image]]
    drawimage = transpose(drawimage, [1, 0])
  ENDIF ELSE drawimage = (*info).image
  drawimage = congrid(drawimage, (*info).d_xsz, (*info).d_ysz)
  (*info).xticks = fix((*info).d_xsz / 100)
  sz = size(drawimage)
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, (*info).ypscale
  ptr_free, (*info).drawimage
  (*info).drawimage = ptr_new(uintarr(sz[1], sz[2]))
  *(*info).drawimage = drawimage ^ (*info).gamma
  (*info).xscale = ptr_new(sz[1])
  (*info).ypscale = ptr_new(sz[1])
  (*info).yscale = ptr_new(sz[2])
  *(*info).xscale = interpol(xscale, sz[1])
  *(*info).yscale = interpol(yscale, sz[2])
  *(*info).ypscale = interpol(yscale, sz[1])
  mplot_image, *(*info).drawimage, min = (*info).imin, max = (*info).imax, $
    * (*info).xscale, *(*info).yscale, $
    xstyle = 1, ystyle = 1, position = (*info).imagepos, $
    xtitle = (*info).xtitle, ytitle = (*info).ytitle, $
    xticks = (*info).xticks, xminor = (*info).xticks * 2, bgblack = bgblack, ticklen = ticklen, /old
  ; create colorbar:
  ymin = (*info).imin
  ymax = (*info).imax
  IF ymax - ymin EQ 0.0 THEN ymax = ymin + 1
  format = '(f10.1)'
  IF ymax - ymin LT 10 THEN format = '(f7.4)'
  hw_colorbar, position = [((*info).imagepos)[2] + 0.02, $
    ((*info).imagepos)[1], $
    ((*info).imagepos)[2] + 0.05, $
    ((*info).imagepos)[3]], range = [ymin, ymax], $
    /vertical, /right, format = format, title = (*info).colorbar_title
END

FUNCTION spice_xwhisker_gamma, event
  widget_control, event.top, get_uvalue = info
  (*info).gamma = event.value
  im_min = 0.0
  (*info).imin = min(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  (*info).imax = max(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  IF (*info).imax EQ im_min THEN BEGIN
    (*info).imin = min(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing))
    (*info).imax = max(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing))
    (*info).gamma = 1.0
    text = 'All data < im_min ' + strtrim(string(im_min, format = '(f4.2)'), 2) + ' gamma reset to 1.0'
    message, text, /info
    !NULL = dialog_message(text, dialog_parent = (*info).tlb)
  ENDIF
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xwhisker_draw, pseudoevent
  return, 0
END

FUNCTION spice_xwhisker_histoopt, event
  widget_control, event.top, get_uvalue = info
  (*info).histo_lim = 10.0 ^ (event.value)
  im_min = 0.0
  (*info).imin = min(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  (*info).imax = max(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing) > im_min) ^ (*info).gamma
  IF (*info).imax EQ im_min THEN BEGIN
    (*info).imin = min(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing))
    (*info).imax = max(spice_histo_opt((*info).image, (*info).histo_lim, missing = (*info).missing))
  ENDIF
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xwhisker_draw, pseudoevent
  return, 0
END

; get the value of the draw window option menu:
FUNCTION spice_xwhisker_dwoption, event
  widget_control, event.top, get_uvalue = info
  (*info).dwoption = event.value
  return, 0
END

; slider to select exposure within a raster pos (if multiple)
PRO spice_xwhisker_expprp_slider, event
  widget_control, event.top, get_uvalue = info
  (*info).exprp = event.value
  (*info).expindx = indgen((*info).nraster) * (*info).nexpprp + (*info).exprp - 1
  nr = (*info).exprp - 1
  wd = *(*info).wd
  (*info).image = reform(wd[*, (*info).slitpos, (*info).expindx])
  good = finite((*info).image)
  IF (where(good))[0] EQ -1 THEN BEGIN
    message, 'All data is NaN for expprp ' + string(nr), /info
  ENDIF
  widget_control, (*info).exposuretext, $
    set_value = strtrim('Exp time: ' + string((*(*info).data.getexp())[nr], $
      format = '(f7.1)') + ' s', 2)
  rot = round(*(*info).data.getinfo('SAT_ROT'))
  IF rot < 0 THEN rot = 360 + rot
  IF rot EQ 90 OR rot EQ 270 THEN BEGIN
    pzty = (*(*info).data.getxpos((*info).line))[(*info).slitpos]
  ENDIF ELSE BEGIN
    pzty = (*(*info).data.getypos((*info).line))[(*info).slitpos]
  ENDELSE
  widget_control, (*info).fmirrytext, $
    set_value = 'Y: ' + string(pzty, format = '(f10.3)') + ' arcsec'
  ; display new exposure nr
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xwhisker_draw, pseudoevent
END

; slider to select raster position
PRO spice_xwhisker_slitslider, event
  widget_control, event.top, get_uvalue = info
  (*info).slitpos = event.value
  nr = (*info).exprp - 1
  wd = *(*info).wd
  IF (*info).nexpprp LE 1 THEN BEGIN
    (*info).image = reform(wd[*, (*info).slitpos, *])
  ENDIF ELSE BEGIN
    (*info).image = reform(wd[*, (*info).slitpos, (*info).expindx])
  ENDELSE
  good = finite((*info).image)
  IF (where(good))[0] EQ -1 THEN BEGIN
    message, 'All data is NaN for expprp ' + string(nr), /info
  ENDIF
  rot = round(*(*info).data.get_satellite_rotation())
  IF rot < 0 THEN rot = 360 + rot
  IF rot EQ 90 OR rot EQ 270 THEN BEGIN
    pzty = *(*info).data.get_instr_x_vector((*info).line, /auto_diff_rot)
    slittxt = 'X: '
  ENDIF ELSE BEGIN
    pzty = *(*info).data.get_instr_y_vector((*info).line, /auto_diff_rot)
    slittxt = 'Y: '
  ENDELSE
  widget_control, (*info).fmirrytext, $
    set_value = slittxt + string(pzty[(*info).slitpos], format = '(f10.3)') + ' arcsec'
  ; display new raster position
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xwhisker_draw, pseudoevent
END

; zoom in draw window:
PRO spice_xwhisker_zoom, event
  widget_control, event.top, get_uvalue = info
  IF event.type GT 2 THEN return
  events = ['down', 'up', 'motion']
  thisevent = events[event.type]
  window, /pixmap, /free, xsize = (*info).d_xsz, ysize = (*info).d_ysz
  mplot_image, *(*info).drawimage, min = (*info).imin, max = (*info).imax, $
    * (*info).xscale, *(*info).yscale, $
    xstyle = 1, ystyle = 1, position = (*info).imagepos, $
    xtitle = (*info).xtitle, ytitle = (*info).ytitle, $
    xticks = (*info).xticks, xminor = (*info).xticks * 2, /bgblack
  ;
  ymin = (*info).imin
  ymax = (*info).imax
  IF ymax - ymin EQ 0.0 THEN ymax = ymin + 1
  format = '(f10.1)'
  IF ymax - ymin LT 10 THEN format = '(f7.4)'
  hw_colorbar, position = [((*info).imagepos)[2] + 0.02, $
    ((*info).imagepos)[1], $
    ((*info).imagepos)[2] + 0.05, $
    ((*info).imagepos)[3]], range = [ymin, ymax], $
    /vertical, /right, format = format, title = (*info).colorbar_title
  ;
  imagepos = (*info).imagepos
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
      image = *(*info).drawimage
      sz = size(image)
      dxfac = float(sz[1]) / (imagepos[2] - imagepos[0]) / float((*info).d_xsz)
      dyfac = float(sz[2]) / (imagepos[3] - imagepos[1]) / float((*info).d_ysz)
      sx = ((*info).sx - imagepos[0] * (*info).d_xsz) * dxfac
      sy = ((*info).sy - imagepos[1] * (*info).d_ysz) * dyfac
      dx = (event.x - imagepos[0] * (*info).d_xsz) * dxfac
      dy = (event.y - imagepos[1] * (*info).d_ysz) * dyfac
      sx = (sx < (*info).d_xsz - 1) > 0
      sy = (sy < (*info).d_ysz - 1) > 0
      dx = (dx < (*info).d_xsz - 1) > 0
      dy = (dy < (*info).d_ysz - 1) > 0
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
          varname = *(*info).data.get_variable_type()
          varname = varname[0] + ': column average'
          dmean = total(image, 1) / sz[1]
          IF sz[0] GE 2 THEN BEGIN
            spice_xlineplot, dmean, xscale = yscale, $
              title = varname, $
              xtitle = (*info).xtitle, $
              ytitle = varname, $
              groupl = event.top, n_subplot = (*info).n_subplot
          ENDIF
        END
        2: BEGIN
          ; set up axis titles for line plots (options 1 or 2 below)
          varname = *(*info).data.get_variable_type()
          varname = varname[0] + ': row average'
          dmean = total(image, 2) / sz[2]
          IF sz[0] GE 2 THEN BEGIN
            spice_xlineplot, dmean, xscale = xscale, $
              title = varname, $
              xtitle = (*info).xtitle, $
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

; create animation widget and launch animation
PRO spice_xwhisker_anim, event
  print, 'does NOT work yet'
  return
  widget_control, event.top, get_uvalue = info
  IF 1.0 EQ swap_endian(1.0, /swap_if_big_endian) THEN swap = 1
  iris_ximovie, *(*info).data.get_filename(), group_leader = (*info).tlb, $
    * (*info).data.getxw((*info).line), *(*info).data.getyw((*info).line), $
    nframes = *(*info).data.getnraster((*info).line), $
    offset = *(*info).data.getposition((*info).line), /float, swap = swap, $
    magnification = 0.9, missing = *(*info).data.missing()

  ; ;   stop
  ; ;   wd=(*(*info).data)->getvar((*info).line,/load)

  ; ;   sz = size(wd)
  ; ;   ndim = sz[0]
  ; ;   xsize = sz[1]
  ; ;   ysize = sz[2]

  ; ;   if ndim lt 3 then begin
  ; ;     ok = dialog_message('Data array must be 3-D to make animation!')
  ; ;     return
  ; ;   endif
  ; ; ; bytscale data to save time in animation tool
  ; ; ;  wdb = bytscl(spice_histo_opt(wd,1.e-2,missing=*(*info).data->missing()))
  ; ; ; write data to assoc file:
  ; ;   ct=0
  ; ;   repeat begin
  ; ;     ct=ct+1
  ; ;     assoc_file = IRISxfiles_appReadme()+'/spice_xwhisker_ximovie_'+strtrim(string(ct),2)+'.tmp'
  ; ;   endrep until ((findfile(assoc_file))[0] eq '')
  ; ;   if ct gt 99 then begin
  ; ;     message,'more than 100 temporary assoc files stored in',/info
  ; ;     message,IRISxfiles_appReadme()+'/spice_xdetector_ximovie_XX.tmp. Consider purge!',/info
  ; ;   endif
  ; ;   openw, lu, assoc_file, /get_lun
  ; ;   rec = assoc(lu, wd)
  ; ;   rec[0] = wd
  ; ;   close, lu & free_lun, lu
  ; ; ; start iris_ximovie, with the delete keyword (afile is removed from disc
  ; ; ; when iris_ximovie is closed
  ; ;   iris_ximovie, assoc_file, xsize, ysize, group_leader = (*info).tlb, $
  ; ;     /fdelete,magnification=0.9,missing=*(*info).data->missing(),/float
  return
END

; change spatial scale to pixels
PRO spice_xwhisker_spix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data.get_axis_title((*info).ydim, /pixels)
  (*info).ydim_unit = 0
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent
END

; change spatial scale to arcsec
PRO spice_xwhisker_sarcsec, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data.get_axis_title((*info).ydim)
  (*info).ydim_unit = 1
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent
END

; change wavelength scale to pixels
PRO spice_xwhisker_wpix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data.get_axis_title((*info).xdim, /pixels)
  (*info).xdim_unit = 0
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent
END

; change wavelength scale to Angstrom
PRO spice_xwhisker_wangstr, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data.get_axis_title((*info).xdim)
  (*info).xdim_unit = 1
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent
END

; select color table
PRO spice_xwhisker_colors, event
  widget_control, event.top, get_uvalue = info
  thisevent = tag_names(event, /structure_name)
  CASE thisevent OF
    'WIDGET_BUTTON': BEGIN
      widget_control, event.top, TLB_GET_OFFSET = offset_parent
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'spice_xwhisker colors (' + strtrim((*info).wid, 2) + ')', $
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
        spice_xwhisker_draw, pseudoevent
      ENDIF
    ENDCASE
  ENDCASE
  widget_control, event.top, set_uvalue = info
END

; protect colors
PRO spice_xwhisker_protect_colors, event
  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
END

; resize main window
PRO spice_xwhisker_resize, event
  widget_control, event.top, get_uvalue = info
  (*info).d_xsz = (event.x - (*info).lcol_xsz) > 0
  (*info).d_ysz = event.y
  widget_control, (*info).drawid, xsize = (*info).d_xsz, $
    ysize = (*info).d_ysz
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent
END

PRO spice_xwhisker_lineplot, event
  widget_control, event.top, get_uvalue = info
  thisevent = tag_names(event, /structure_name)
  CASE thisevent OF
    'WIDGET_DROPLIST': BEGIN
      mode = event.index
    END
    ELSE:
  ENDCASE
  ; set up titles for plot
  varname = *(*info).data.get_variable_type()
  varname = varname[0]
  CASE mode OF
    0: BEGIN
      return
    END
    1: BEGIN
      data = (*info).image[*, *]
      spice_xlineplot, data, xscale = *(*info).xscale, $
        xtitle = (*info).xtitle, $
        cslider_title = (*info).ytitle, $
        ytitle = varname, groupl = (*info).tlb
    END
    2: BEGIN
      data = transpose((*info).image[*, *])
      spice_xlineplot, data, xtitle = (*info).ytitle, $
        cslider_title = (*info).xtitle, ytitle = varname, $
        groupl = (*info).tlb
    END
  ENDCASE
END

PRO spice_xwhisker_mask, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).maskbutton, get_value = masking
  print, masking
  wd = *(*info).data.get_window_data((*info).line, no_masking = masking EQ 0)
  image = reform(wd[*, (*info).slitpos, *, *])
  IF *(*info).data.get_missing_value() NE *(*info).data.get_missing_value() THEN missing = -99999l $
  ELSE missing = *(*info).data.get_missing_value()
  wd = spice_histo_opt(wd, missing = missing)
  image = spice_histo_opt(image)
  *(*info).wd = wd
  (*info).image = image
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent
END

; close spice_xwhisker
PRO spice_xwhisker_destroy, event
  widget_control, event.top, /destroy
END

PRO spice_xwhisker_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  wdelete, (*info).mainpixid
  ; free_lun, (*info).alu
  IF (*info).object_created THEN obj_destroy, *(*info).data
  ptr_free, (*info).data
  ptr_free, (*info).wd
  ptr_free, (*info).drawimage
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, (*info).ypscale
  ptr_free, info
END

PRO spice_xwhisker, input_data, line, group_leader = group_leader, $
  ncolors = ncolors
  IF n_params() LT 2 THEN BEGIN
    message, $
      'spice_xwhisker,data,line, group_leader = group,ncolors = ncolors', /cont
    return
  ENDIF

  data = spice_object(input_data, is_spice = is_spice, object_created = object_created)
  IF ~is_spice THEN return

  IF n_elements(ncolors) EQ 0 THEN ncolors = (!d.n_colors < 256)
  IF n_elements(drawcolor) EQ 0 THEN drawcolor = !p.color
  ; drawing window size in relation to screen
  IF n_elements(scfac) EQ 0 THEN scfac = 0.6
  screensize = get_screen_size()
  sz = screensize * scfac
  d_xsz = sz[1] / 1.5
  d_ysz = sz[0] / 1.4
  ;
  sit_and_stare = data.get_sit_and_stare()
  nslit = data.get_header_keyword('NAXIS2', line)
  nraster = data.get_number_exposures(line)
  nexpprp = 1 ; data->getnexp_prp(line)  ; number of exp pr. raster pos.
  slitpos = nslit / 2
  ; so far QL can not handle sit-and-stare with different exposure times
  ; (when it is run as "multiple exp pr rast. pos.)
  ; Will have to deal with that...
  ; OW 14-april 2005.
  IF sit_and_stare THEN nexpprp = 1
  message = ['Loading data into memory...', '...this may take some time']
  xmessage, message, wbase = wbase, font = 'helvetica'
  widget_control, /hourglass
  wd = data.get_window_data(line)
  xkill, wbase
  IF nexpprp LE 1 THEN BEGIN
    image = reform(wd[*, slitpos, *, *])
    expindx = 0
  ENDIF ELSE BEGIN
    expnr_at_rp = 1 ; intitialize first exp at each raster pos.
    expindx = indgen(nraster) * nexpprp + expnr_at_rp - 1
    image = reform(wd[*, slitpos, expindx])
  ENDELSE
  IF data.get_missing_value() NE data.get_missing_value() THEN missing = -99999l $
  ELSE missing = data.get_missing_value()
  wd = spice_histo_opt(wd, missing = missing)
  imin = min(wd)
  imax = max(wd)
  image = spice_histo_opt(image)
  ; initialize size of draw window
  sz = size(wd)
  ndim = sz[0]
  xsz = sz[1]
  nlam = xsz
  ysz = sz[3]
  ;
  xdim = 2
  IF sit_and_stare THEN ydim = 3 ELSE ydim = 0
  xtitle = data.get_axis_title(xdim)
  ytitle = data.get_axis_title(ydim)
  window, /pixmap, /free, xsize = xsz, ysize = ysz
  tv, bytscl(image, top = ncolors)
  pixid = !d.window
  ; base widget:
  xwt = 'SPICE_Xwhisker - ' + data.get_filename() ; spice_xwhisker window title
  tlb = widget_base(/row, title = xwt, tlb_size_events = 1, $
    mbar = menubar, xoffset = 100, yoffset = 100, group_leader = group_leader)
  lcol = widget_base(tlb, /frame, /column) ; left column.
  rcol = widget_base(tlb, /column) ; right column.

  ; create pulldown menus on the base widget menubar
  filemenu = widget_button(menubar, value = 'File', /menu, uvalue = 'file')
  savemenu = widget_button(filemenu, value = 'Save as', uvalue = 'save', /menu)
  psmenu = widget_button(savemenu, value = 'Postscript', event_pro = 'spice_xwhisker_ps') ; idl-disable-line unused-var
  jpgmenu = widget_button(savemenu, value = 'JPG', event_pro = 'spice_xwhisker_jpeg') ; idl-disable-line unused-var
  exitmenu = widget_button(filemenu, value = 'Close', event_pro = 'spice_xwhisker_destroy') ; idl-disable-line unused-var

  optmenu = widget_button(menubar, value = 'Options', uvalue = 'options')
  colmenu = widget_button(optmenu, value = 'Colour table', $ ; idl-disable-line unused-var
    event_pro = 'spice_xwhisker_colors')
  ; animenu=widget_button(optmenu, value='Create Animation', $
  ; event_pro='spice_xwhisker_anim')
  wscalemenu = widget_button(optmenu, value = 'Change wavelength scale', /menu)
  angstr = string("305b) + 'ngstr' + string("370b) + 'm'
  pixmenu = widget_button(wscalemenu, value = 'Pixels', event_pro = 'spice_xwhisker_wpix')
  angstrmenu = widget_button(wscalemenu, value = angstr, event_pro = 'spice_xwhisker_wangstr')
  sscalemenu = widget_button(optmenu, value = 'Change spatial scale', /menu)
  pixmenu = widget_button(sscalemenu, value = 'Pixels', event_pro = 'spice_xwhisker_spix')
  angstrmenu = widget_button(sscalemenu, value = 'arcsec', event_pro = 'spice_xwhisker_sarcsec')
  ; display window:
  displaybase = widget_base(rcol, /row)
  drawid = widget_draw(displaybase, retain = 2, $
    xsize = d_xsz, ysize = d_ysz, $
    /button_events, event_pro = 'spice_xwhisker_zoom')
  ;
  colorbar_title = data.get_title() + ' ' + (data.get_variable_unit())
  ; create menu for controlling action in draw window
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
    event_func = 'spice_xwhisker_dwoption')

  titletext = widget_label(lcol, value = data.get_start_time() + ' ' + data.get_obs_id(), /align_center) ; idl-disable-line unused-var

  lsubcol = widget_base(lcol, /row)
  sliderbase = widget_base(lsubcol, /col)

  IF nexpprp GT 1 THEN BEGIN
    expprpslider = widget_slider(sliderbase, xsize = 90, $ ; idl-disable-line unused-var
      minimum = 1, maximum = nexpprp, $
      title = 'Exp.# at rast. pos.', $
      value = 1, $
      event_pro = 'spice_xwhisker_expprp_slider')
  ENDIF

  id = data.get_window_id(line)
  idbase = widget_base(lsubcol, /col)
  idtext = widget_label(idbase, value = strtrim(id, 2), /align_left) ; idl-disable-line unused-var

  exposurebase = widget_base(idbase, /col)
  exposuretext = widget_label(exposurebase, $
    value = strtrim('Exp time: ' + string((data.get_exposure_time(line)), format = '(f7.1)') + ' s', 2), $
    /align_left)

  xycenbase = widget_base(exposurebase, /col)
  xycentext = widget_label(xycenbase, $
    value = 'Xcen: ' + string((data.get_header_keyword('crval1', line)), format = '(f10.3)') + $
      ' Ycen: ' + string((data.get_header_keyword('crval2', line)), format = '(f10.3)'), $
    /align_left)

  rot = data.get_satellite_rotation()
  IF n_elements(rot) EQ 0 THEN rot = 0
  rot = round(rot)
  IF rot LT 0 THEN rot = 360 + rot
  IF rot EQ 90 OR rot EQ 270 THEN BEGIN
    pzty = data.get_instr_x_vector(line, /auto_diff_rot)
    slittxt = 'X: '
  ENDIF ELSE BEGIN
    pzty = data.get_instr_y_vector(line, /auto_diff_rot)
    slittxt = 'Y: '
  ENDELSE
  fmirrytext = widget_label(exposurebase, $
    value = strtrim(slittxt + string(pzty[slitpos], format = '(f10.3)'), 2) + ' arcsec', $
    /align_left)

  title = 'Slit Position'
  slitslider = widget_slider(sliderbase, xsize = 90, $ ; idl-disable-line unused-var
    minimum = 0, maximum = nslit - 1, title = title, $
    value = slitpos, event_pro = 'spice_xwhisker_slitslider', /drag)

  maskfield = widget_base(lcol, /column, /frame, event_pro = 'spice_xwhisker_mask')
  maskbutton = cw_bgroup(maskfield, ['Mask regions outside slit'], $
    set_value = [1], /nonexclusive)

  ; control of gamma and histo_
  gammacol = widget_base(lcol, /row)
  gamma = 1.0
  gamma_slider = cw_fslider(gammacol, /edit, format = '(f6.2)', /frame, $ ; idl-disable-line unused-var
    maximum = 3.0, minimum = 0.1, value = gamma, $
    title = 'Gamma Correction', $
    event_func = 'spice_xwhisker_gamma', /drag)

  histo_lim = -3.0
  histoopt_slider = cw_fslider(gammacol, /edit, format = '(f6.2)', /frame, $ ; idl-disable-line unused-var
    maximum = -1.0, minimum = -6.0, value = histo_lim, $
    title = 'log(HistoOpt Value)', $
    event_func = 'spice_xwhisker_histoopt', /drag)
  histo_lim = 10. ^ histo_lim

  lineplotbase = widget_base(lcol, /column)
  names = ['Not active', 'Row plot', 'Column plot']
  lineplot = widget_droplist(lineplotbase, value = names, $
    title = 'Plot pixel values', $
    event_pro = 'spice_xwhisker_lineplot')

  closefield = widget_base(lcol, /column)
  closebutton = widget_button(closefield, value = 'Close', $ ; idl-disable-line unused-var
    event_pro = 'spice_xwhisker_destroy')

  ; realize main window:
  widget_position, tlb, parent = group_leader
  widget_control, tlb, tlb_get_size = tlb_sz

  ; define size of widget and the menu column
  tlb_xsz = tlb_sz[0] ; xsize of whole widget in pixels
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
  imagepos = [0.15, 0.10, 0.77, 0.95]
  ; set up default display mode:
  info_struct = {drawimage: ptr_new(), $
    wd: ptr_new(wd, /no_copy), $
    image: image, $
    xdim: xdim, $
    ydim: ydim, $
    xscale: ptr_new(), $
    yscale: ptr_new(), $
    ypscale: ptr_new(), $
    data: ptr_new(data), $
    object_created: object_created, $
    sit_and_stare: sit_and_stare, $
    n_subplot: 0, $
    xdim_unit: 1, $
    ydim_unit: 1, $
    exposuretext: exposuretext, $
    fmirrytext: fmirrytext, $
    xycentext: xycentext, $
    xticks: 0, $
    nlam: nlam, $
    nraster: nraster, $
    expindx: expindx, $
    nexpprp: nexpprp, $
    exprp: 1, $
    ndim: ndim, $
    line: line, $
    maskbutton: maskbutton, $
    screensize: screensize, $
    lcol_xsz: lcol_xsz, $
    d_xsz: d_xsz, $
    d_ysz: d_ysz, $
    tlb: tlb, $
    r: r, g: g, b: b, $
    imagepos: imagepos, $
    bottom: bottom, $
    ncolors: ncolors, $
    sx: 0, $
    sy: 0, $
    dwoption: 0, $
    dwoption_menu: dwoption_menu, $
    drawid: drawid, $
    colorbar_title: colorbar_title, $
    lineplot: lineplot, $
    slitpos: slitpos, $
    gamma: gamma, $
    histo_lim: histo_lim, $
    imin: imin, $
    imax: imax, $
    missing: data.get_missing_value(), $
    drawcolor: drawcolor, $
    mainpixid: pixid, $
    pixid: pixid, $
    xtitle: xtitle, $
    ytitle: ytitle, $
    wid: wid}
  info = ptr_new(info_struct, /no_copy)
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to spice_xwhisker_draw,
  ; in order to draw the image
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: tlb, handler: 0l, select: 1}
  spice_xwhisker_draw, pseudoevent

  xmanager, 'spice_xwhisker', tlb, /no_block, event_handler = 'spice_xwhisker_resize', $
    cleanup = 'spice_xwhisker_cleanup'
END
