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
;       spice_xraster, data, windows [, ncolors=ncolors, $
;                     group_leader = groupleader]
;
; INPUTS:
;       data: Can be either the name and path of a SPICE data file,
;             or a SPICE data object.
;       windows : The index(es) of the line windows to be displayed
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
; $Id: 2025-06-23 13:07 CEST $
;-
;
; save as postscript file
PRO spice_xraster_ps, event
  thisfile = dialog_pickfile(/write, file = 'spice_xraster.ps', dialog_parent = event.top)
  IF thisfile EQ '' THEN return
  widget_control, event.top, get_uvalue = info
  thisdevice = !d.name
  set_plot, 'ps', /copy
  device, file = thisfile, /inches, bits_per_pixel = 8, /color
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  widget_control, event.top, set_uvalue = info
  spice_xraster_draw, pseudoevent
  device, /close_file
  set_plot, thisdevice
END

; save as jpeg file
PRO spice_xraster_jpeg, event
  thisfile = dialog_pickfile(/write, file = 'spice_xraster.jpg', dialog_parent = event.top)
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
PRO spice_xraster_draw, event
  widget_control, event.top, get_uvalue = info
  IF !d.name NE 'PS' THEN BEGIN
    wset, (*info).wid
    !p.charsize = 2.0
    tcharsize = 1.0
    erase
  ENDIF ELSE BEGIN
    !p.charsize = 0.85
    tcharsize = 0.5
  ENDELSE
  nr = (*info).nexp
  nwin = (*info).nwin
  xsz = 0
  ysz = 0
  ; this loop is for determining max size of spectral line windows
  ; (since they can have varying size)
  FOR i = 0, nwin - 1 DO BEGIN
    j = (*info).windows[i]
    ; pos=*(*info).data->getpos(j)
    ; sz = size(wd)
    xsz = xsz > *(*info).data.get_header_keyword('naxis3', j)
    ysz = ysz > *(*info).data.get_header_keyword('naxis2', j)
  ENDFOR
  ; determine size of each window. The scale factors
  ; (xpixels*2)x(ypixels/2), with a minimum of
  ; 50x100 and max of 100x250 pixels.
  xpix = (xsz * 2) > 75 < 250
  ypix = (ysz / 2) > 100 < 250
  nxticks = 3
  ; determine if draw window needs to be larger (by
  ; applying scroll bars.)
  xfac = fix((xpix * nr * 2) / (*info).x_scroll_size)
  yfac = fix((ypix * nwin * 1.2) / (*info).y_scroll_size)
  xvs = (*info).x_scroll_size * xfac > (*info).x_scroll_size < 2l ^ 15 - 1
  yvs = (*info).y_scroll_size * yfac > (*info).y_scroll_size
  ;
  widget_control, (*info).drawid, draw_xsize = xvs, draw_ysize = yvs
  IF *(*info).data.get_missing_value() NE *(*info).data.get_missing_value() THEN missing = -99999l $
  ELSE missing = *(*info).data.get_missing_value()
  wdmin = fltarr((*info).nwin) - missing
  wdmax = fltarr((*info).nwin) + missing
  ; set up plot scale:
  FOR i = 0, (*info).nwin - 1 DO BEGIN
    j = (*info).windows[i]
    FOR it = 0, min([5, nr - 1]) DO BEGIN
      var = *(*info).data.get_one_image(j, it, no_masking = (*info).no_masking)
      wdmin[i] = min([min(spice_histo_opt(var, 0.01, /bot_only, missing = missing), /nan), wdmin[i]], /nan)
      wdmax[i] = max([max(spice_histo_opt(var, 0.001, /top_only, missing = missing), /nan), wdmax[i]], /nan)
    ENDFOR
    IF wdmin[i] GT wdmax[i] THEN BEGIN
      wdmin[i] = min(var, max = maxtemp)
      wdmax[i] = maxtemp
    ENDIF
    sz = size(var)

    ; wavelength scale of NUV/FUV1/FUV2
    IF ~(*info).xdim_unit THEN BEGIN
      lambda = indgen(sz[1])
      IF n_elements(lambda) EQ 1 THEN lambda = [lambda - 0.5, lambda + 0.5]
    ENDIF ELSE BEGIN
      lambda = *(*info).data.get_lambda_vector(j)
      IF n_elements(lambda) EQ 1 THEN BEGIN
        cdelt = *(*info).data.get_resolution(j, /lambda) / 2.0
        lambda = [lambda - cdelt, lambda + cdelt]
      ENDIF
    ENDELSE
    IF ~(*info).ydim_unit THEN BEGIN
      ypos = indgen(sz[2])
    ENDIF ELSE BEGIN
      ypos = *(*info).data.get_instr_y_vector(j, /auto_diff_rot)
    ENDELSE
    xscale = interpol(lambda, xpix)
    yscale = interpol(ypos, ypix)
    origin = [min(xscale), min(yscale)]
    timepos = [min(xscale) + (max(xscale) - min(xscale)) * 0.05, max(yscale) - (max(yscale) - min(yscale)) * 0.1]

    ; draw images
    FOR it = 0, nr - 1 DO BEGIN
      drawimage = congrid(*(*info).data.get_one_image(j, it, no_masking = (*info).no_masking), xpix, ypix)
      sz = size(drawimage)
      scale = [(max(xscale) - min(xscale)) / sz[1], (max(yscale) - min(yscale)) / sz[2]]
      ymin = wdmin[i]
      ymax = wdmax[i]
      IF it EQ 0 THEN ytitle = *(*info).data.get_window_id(j) + ' ' + (*info).ytitle ELSE ytitle = ''
      spice_br_panel, it, i, nx = nr, ny = (*info).nwin, order = 0, ydist = 3, /xlabel, ytop = 3, xright = 5, xleft = 12
      plot_image, drawimage, origin = origin, scale = scale, /nosquare, $
        xtitle = (*info).xtitle, xticks = nxticks, ytitle = ytitle, min = ymin, max = ymax
      IF i EQ 0 THEN xyouts, timepos[0], timepos[1], 't = ' + $
        strtrim(string((*(*info).data.get_time_vector(j))[it], format = '(f6.1)'), 2) + ' [s]', $
        alignment = 0.0, chars = tcharsize, color = 255
    ENDFOR
  ENDFOR
  spice_br_panel, /reset
  !p.multi = 0
  !p.charsize = 1.0
  ; create colorbar (if plot to 'PS' then skip colorbar:
  IF !d.name NE 'PS' THEN BEGIN
    widget_control, (*info).colorbarid, get_value = wid
    wset, wid
    erase
    widget_control, (*info).colorbarid, draw_ysize = yvs
    nwin = (*info).nwin
    FOR i = 0, nwin - 1 DO BEGIN
      ; find max and min values of drawimage to produce color bar y-scale
      ymin = wdmin[i]
      ymax = wdmax[i]
      IF ymax - ymin EQ 0.0 THEN ymax = ymin + 1
      format = '(i6)'
      IF ymax - ymin LT 10 THEN format = '(f7.4)'
      position = [0.75, 0.04 + float(nwin - i - 1) / nwin * 0.95, $
        0.95, 0.01 + float(nwin - i) / nwin * 0.95]
      hw_colorbar, position = position, range = [ymin, ymax], $
        /vertical, format = format, title = (*info).colorbar_title, $
        /keep_pos
    ENDFOR
  ENDIF
END

; Popup window for line selection
PRO spice_xraster_pickline, event
  widget_control, event.top, get_uvalue = info
  ; open window for line selection
  lineselect_widget = widget_base(title = 'Select line', $
    group_leader = (*info).tlb, /row)
  closefield = widget_base(lineselect_widget, /column)
  closebutton = widget_button(closefield, value = 'OK', $ ; idl-disable-line unused-var
    event_pro = 'spice_xraster_pickline_destroy')
  line_base = widget_base(lineselect_widget, /column, /frame)
  linelist = cw_bgroup(line_base, (*info).linelist, /return_index, $ ; idl-disable-line unused-var
    /exclusive, event_func = 'spice_xraster_pickline_pick')
  widget_control, lineselect_widget, set_uvalue = info
  widget_control, lineselect_widget, /realize
  xmanager, 'Select line', lineselect_widget, $
    /no_block, group_leader = (*info).tlb
END

; get the value of the selected line from the line list:
FUNCTION spice_xraster_pickline_pick, event
  widget_control, event.top, get_uvalue = info
  (*info).line = event.value + (*info).windows[0]
  return, 0
END

; close Line selection widget
PRO spice_xraster_pickline_destroy, event
  widget_control, event.top, get_uvalue = info
  IF (*info).messenger EQ (*info).animenu THEN BEGIN
    spice_xraster_anim, event
  ENDIF
  widget_control, event.top, /destroy
END

; Controls the animation event: If only one line,
; start animation, if several line, then pop up
; line selection window first.
PRO spice_xraster_control_anim, event
  widget_control, event.top, get_uvalue = info
  IF (*info).nwin LT 2 THEN BEGIN
    spice_xraster_anim, event
  ENDIF ELSE BEGIN
    (*info).messenger = (*info).animenu
    spice_xraster_pickline, event
  ENDELSE
END

; create animation widget and launch animation
PRO spice_xraster_anim, event
  print, 'does NOT work yet'
  return
  widget_control, event.top, get_uvalue = info
  magnification = 0.95
  minsize = 400.0
  maxsize = 800.0
  xsize = *(*info).data.getxw((*info).line)
  ysize = *(*info).data.getyw((*info).line)
  IF xsize LT minsize THEN magnification = minsize / xsize
  IF xsize GT maxsize THEN magnification = maxsize / xsize
  IF ysize * magnification LT minsize THEN magnification = minsize / ysize
  IF ysize * magnification GT maxsize THEN magnification = maxsize / ysize
  IF 1.0 EQ swap_endian(1.0, /swap_if_big_endian) THEN swap = 1
  iris_ximovie, *(*info).data.getfilename(), group_leader = (*info).tlb, $
    * (*info).data.getxw((*info).line), *(*info).data.getyw((*info).line), $
    nframes = *(*info).data.getnraster((*info).line), $
    offset = *(*info).data.getposition((*info).line), /float, swap = swap, $
    magnification = magnification, missing = *(*info).data.missing()

  ; ;   *(*info).data-> getwin,(*info).line,wd,pos
  ; ; ;
  ; ;   sz = size(wd)
  ; ;   ndim = sz[0]
  ; ;   xsize = sz[1]
  ; ;   ysize = sz[2]
  ; ; ;
  ; ;   if ndim lt 3 then begin
  ; ;     ok = dialog_message('Data array must be 3-D to make animation!',/center)
  ; ;     return
  ; ;   endif
  ; ; ; bytscale data to save time in animation tool
  ; ; ;  wdb = bytscl(spice_histo_opt(wd,1.e-2,missing=*(*info).data->missing()))
  ; ; ; write data to assoc file if not already existing:
  ; ;   ct=0
  ; ;   repeat begin
  ; ;     ct=ct+1
  ; ;     assoc_file = IRISxfiles_appReadme()+'/iris_xraster_ximovie_'+strtrim(string(ct),2)+'.tmp'
  ; ;   endrep until ((findfile(assoc_file))[0] eq '')
  ; ;   if ct gt 99 then begin
  ; ;     message,'more than 100 temporary assoc files stored in',/info
  ; ;     message,IRISxfiles_appReadme()+'/iris_xdetector_ximovie_XX.tmp. Consider purge!',/info
  ; ;   endif
  ; ;   openw, lu, assoc_file, /get_lun
  ; ;   rec = assoc(lu, wd)
  ; ;   rec[0] = wd
  ; ;   close, lu & free_lun, lu
  ; ; ; start iris_ximovie, with the delete keyword (afile is removed from disc
  ; ; ; when iris_ximovie is closed
  ; ;   iris_ximovie, assoc_file, xsize, ysize, group_leader = (*info).tlb, $
  ; ;      /fdelete,/float,missing=*(*info).data->missing()
  return
END

; change spatial scale to pixels
PRO spice_xraster_spix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data.get_axis_title((*info).ydim, /pixels)
  (*info).ydim_unit = 0
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
END

; change spatial scale to arcsec
PRO spice_xraster_sarcsec, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).ytitle = *(*info).data.get_axis_title((*info).ydim)
  (*info).ydim_unit = 1
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
END

; change wavelength scale to pixels
PRO spice_xraster_wpix, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data.get_axis_title((*info).xdim, /pixels)
  (*info).xdim_unit = 0
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
END

; change wavelength scale to Angstrom
PRO spice_xraster_wangstr, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).xtitle = *(*info).data.get_axis_title((*info).xdim)
  (*info).xdim_unit = 1
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
END

; Toggle masking of pixels outside slit ON
PRO spice_xraster_mask_on, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).no_masking = 0
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
END

; Toggle masking of pixels outside slit OFF
PRO spice_xraster_mask_off, event
  widget_control, event.top, get_uvalue = info
  ; set titles for image plots
  (*info).no_masking = 1
  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, $
    top: event.top, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
END

; ; change wavelength scale to pixels
; pro spice_xraster_wpix, event
; widget_control, event.top, get_uvalue = info
; ; change titles in aux object
; (*(*info).data->getaux())->setwscale,'pixels'
; (*(*info).data->getaux())->setxytitle,wscale='pixels'
; ; set titles for image plots
; (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
; (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
; pseudoevent={widget_button,id:0L, $
; top:event.top, handler:0l, select:1}
; spice_xraster_draw, pseudoevent
; end
;
; ; change spatial scale to pixels
; pro spice_xraster_spix, event
; widget_control, event.top, get_uvalue = info
; ; change titles in aux object
; (*(*info).data->getaux())->setsscale,'pixels'
; (*(*info).data->getaux())->setxytitle,sscale='pixels'
; ; set titles for image plots
; (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
; (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
; pseudoevent={widget_button,id:0L, $
; top:event.top, handler:0l, select:1}
; spice_xraster_draw, pseudoevent
; end
;
; ; change spatial scale to arcsec
; pro spice_xraster_sarcsec, event
; widget_control, event.top, get_uvalue = info
; ; change titles in aux object
; (*(*info).data->getaux())->setsscale,'arcsec'
; (*(*info).data->getaux())->setxytitle,sscale='arcsec'
; ; set titles for image plots
; (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
; (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
; pseudoevent={widget_button,id:0L, $
; top:event.top, handler:0l, select:1}
; spice_xraster_draw, pseudoevent
; end
;
; ; change wavelength scale to Angstrom
; pro spice_xraster_wangstr, event
; widget_control, event.top, get_uvalue = info
; ; set titles for image plots
; (*(*info).data->getaux())->setwscale,string(197b)
; (*(*info).data->getaux())->setxytitle,wscale=string(197b)
; ; set titles for image plots
; (*info).xtitle = (*(*info).data->getxytitle())[(*info).xdim]
; (*info).ytitle = (*(*info).data->getxytitle())[(*info).ydim]
;
; pseudoevent={widget_button,id:0L, $
; top:event.top, handler:0l, select:1}
; spice_xraster_draw, pseudoevent
; end

; select color table
PRO spice_xraster_colors, event
  widget_control, event.top, get_uvalue = info
  thisevent = tag_names(event, /structure_name)
  CASE thisevent OF
    'WIDGET_BUTTON': BEGIN
      widget_control, event.top, TLB_GET_OFFSET = offset_parent
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'spice_xraster colors (' + strtrim((*info).wid, 2) + ')', $
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
        spice_xraster_draw, pseudoevent
      ENDIF
    ENDCASE
  ENDCASE
  widget_control, event.top, set_uvalue = info
END

; protect colors
PRO spice_xraster_protect_colors, event
  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
END

; resize main window
PRO spice_xraster_resize, event
  widget_control, event.top, get_uvalue = info
  IF (*info).timer EQ 'off' THEN BEGIN
    widget_control, event.top, timer = 0.1
    (*info).timer = 'on'
  ENDIF
  CASE tag_names(event, /structure_name) OF
    'WIDGET_TIMER': IF (*info).oldx EQ (*info).xs AND (*info).oldy EQ (*info).ys THEN (*info).redraw = 1 $
    ELSE widget_control, event.top, timer = 0.25
    'WIDGET_BASE': BEGIN
      (*info).xs = event.x
      (*info).ys = event.y
    END
    ELSE:
  ENDCASE
  IF (*info).redraw THEN BEGIN
    (*info).d_xsz = ((*info).xs - (*info).lcol_xsz - (*info).cb_xsz) > 0
    (*info).d_ysz = (*info).ys
    (*info).x_scroll_size = (*info).d_xsz
    (*info).y_scroll_size = (*info).d_ysz
    widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
      draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $
      ysize = (*info).d_ysz
    widget_control, (*info).colorbarid, draw_xsize = (*info).cb_xsz, $
      draw_ysize = (*info).d_ysz, xsize = (*info).cb_xsz, $
      ysize = (*info).d_ysz

    ; idl-disable-next-line unknown-structure
    pseudoevent = {widget_button, id: 0l, $
      top: event.top, handler: 0l, select: 1}
    spice_xraster_draw, pseudoevent
    (*info).redraw = 0
    (*info).timer = 'off'
  ENDIF
  (*info).oldx = (*info).xs
  (*info).oldy = (*info).ys
END

; close spice_xraster
PRO spice_xraster_destroy, event
  widget_control, event.top, /destroy
END

PRO spice_xraster_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  IF (*info).object_created THEN obj_destroy, *(*info).data
  ptr_free, (*info).data
  ptr_free, info
END

PRO spice_xraster, input_data, windows, ncolors = ncolors, group_leader = group_leader
  ;
  IF n_params() LT 2 THEN BEGIN
    message, 'spice_xraster,data,windows, ncolors=ncolors,group_leader = group', /cont
    return
  ENDIF

  data = spice_object(input_data, is_spice = is_spice, object_created = object_created)
  IF ~is_spice THEN return

  IF n_elements(ncolors) EQ 0 THEN ncolors = (!d.n_colors < 256)
  maxexp = 200
  ; drawing window size in relation to screen
  screensize = get_screen_size()
  IF n_elements(scfac) EQ 0 THEN scfac = 0.6
  sz = screensize * scfac
  d_xsz = sz[0]
  d_ysz = sz[1]
  nwin = n_elements(windows) ; number of line windows selected by user
  line = windows[0]
  ; nraster = max(data->getnraster())   ; number of raster positions in data set
  nexp = max(data.get_number_exposures()) ; number of exposures
  IF nexp GT maxexp THEN BEGIN
    warning = ['Raster/Time series contains more than ' + string(strtrim(maxexp, 2)) + ' exposures', $
      'spice_xraster will be quite slow. Continue?']
    answer = dialog_message(warning, /cancel, /default_cancel, dialog_parent = group)
    IF answer EQ 'Cancel' THEN return
  ENDIF
  xdim = 2 ; wavelength
  ydim = 1 ; slit pos
  xtitle = data.get_axis_title(xdim) ; wavelength
  ytitle = data.get_axis_title(ydim) ; slit position

  ; create linelist
  linelist = strarr(nwin)
  FOR i = 0, nwin - 1 DO linelist[i] = 'Line ' + strtrim(data.get_window_id(windows[i]), 2)

  ; base widget:
  xwt = 'SPICE_Xraster -' + data.get_filename() ; spice_xraster window title
  tlb = widget_base(/row, title = xwt, tlb_size_events = 1, $
    mbar = menubar, xoffset = 100, yoffset = 100, group_leader = group_leader) ;
  lcol = widget_base(tlb, /frame, /column) ; left column.
  rcol = widget_base(tlb, /column) ; right column.

  ; create pulldown menus on the base widget menubar
  filemenu = widget_button(menubar, value = 'File', /menu, uvalue = 'file')
  savemenu = widget_button(filemenu, value = 'Save as', uvalue = 'save', /menu)
  psmenu = widget_button(savemenu, value = 'Postscript', event_pro = 'spice_xraster_ps') ; idl-disable-line unused-var
  jpgmenu = widget_button(savemenu, value = 'JPG', event_pro = 'spice_xraster_jpeg') ; idl-disable-line unused-var
  exitmenu = widget_button(filemenu, value = 'Close', event_pro = 'spice_xraster_destroy') ; idl-disable-line unused-var
  optmenu = widget_button(menubar, value = 'Options', uvalue = 'options')
  colmenu = widget_button(optmenu, value = 'Colour table', $ ; idl-disable-line unused-var
    event_pro = 'spice_xraster_colors')
  ; animenu = widget_button(optmenu, value = 'Create Animation', uvalue='anim', $
  ; event_pro = 'spice_xraster_control_anim')
  wscalemenu = widget_button(optmenu, value = 'Change wavelength scale', /menu)
  ; angstr = string("305"ob) + 'ngstr' + string("370"ob) + 'm'
  pixmenu = widget_button(wscalemenu, value = 'Pixels', event_pro = 'spice_xraster_wpix')
  angstrmenu = widget_button(wscalemenu, value = 'nm', event_pro = 'spice_xraster_wangstr')
  sscalemenu = widget_button(optmenu, value = 'Change spatial scale', /menu)
  pixmenu = widget_button(sscalemenu, value = 'Pixels', event_pro = 'spice_xraster_spix')
  angstrmenu = widget_button(sscalemenu, value = 'arcsec', event_pro = 'spice_xraster_sarcsec')
  maskmenu = widget_button(optmenu, value = 'Toggle masking', /menu)
  maskonmenu = widget_button(maskmenu, value = 'On', event_pro = 'spice_xraster_mask_on') ; idl-disable-line unused-var
  maskoffmenu = widget_button(maskmenu, value = 'Off', event_pro = 'spice_xraster_mask_off') ; idl-disable-line unused-var

  ; display window:
  displaybase = widget_base(rcol, /row)
  drawid = widget_draw(displaybase, retain = 2, $
    xsize = d_xsz, x_scroll_size = d_xsz, $
    ysize = d_ysz, y_scroll_size = d_ysz, $
    event_pro = 'spice_xraster_draw')
  cb_xsz = 84 ; xsize of color bar draw widget
  ; create color bar to the right of display window:
  colorbarid = widget_draw(displaybase, retain = 2, $
    xsize = cb_xsz, x_scroll_size = cb_xsz, $
    ysize = d_ysz, y_scroll_size = d_ysz)
  colorbar_title = data.get_title() + ' ' + (data.get_variable_unit())
  ; close button
  closefield = widget_base(lcol, /column)
  closebutton = widget_button(closefield, value = 'Close', $ ; idl-disable-line unused-var
    event_pro = 'spice_xraster_destroy')
  ; realize main window:

  widget_position, tlb, parent = group_leader
  widget_control, tlb, tlb_get_size = tlb_sz
  ; define size of widget and the menu column
  tlb_xsz = tlb_sz[0] ; xsize of whole widget in pixels
  ; tlb_ysz = tlb_sz[1] ; ysize of whole widget in pixels
  lcol_xsz = tlb_xsz - d_xsz - cb_xsz
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

  ; define the info structure, used send information around
  info_struct = {xscale: ptr_new(), $
    yscale: ptr_new(), $
    data: ptr_new(data), $
    object_created: object_created, $
    xdim: xdim, $
    ydim: ydim, $
    xdim_unit: 1, $
    ydim_unit: 1, $
    nwin: nwin, $
    ; nraster:nraster, $
    nexp: nexp, $
    line: line, $
    windows: windows, $
    linelist: linelist, $
    lcol_xsz: lcol_xsz, $
    d_xsz: d_xsz, $
    d_ysz: d_ysz, $
    oldx: 0, $
    oldy: 0, $
    xs: 0, $
    ys: 0, $
    redraw: 0, $
    no_masking: 0, $
    timer: 'off', $
    x_scroll_size: d_xsz, $
    y_scroll_size: d_ysz, $
    cb_xsz: cb_xsz, $
    tlb: tlb, $
    lcol: lcol, $
    rcol: rcol, $
    ; animenu:animenu, $
    messenger: 0, $
    r: r, g: g, b: b, $
    bottom: bottom, $
    ncolors: ncolors, $
    drawid: drawid, $
    colorbarid: colorbarid, $
    colorbar_title: colorbar_title, $ ;
    xtitle: xtitle, $
    ytitle: ytitle, $
    wid: wid}
  info = ptr_new(info_struct, /no_copy)
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to spice_xraster_draw,
  ; in order to draw the image

  ; idl-disable-next-line unknown-structure
  pseudoevent = {widget_button, id: 0l, top: tlb, handler: 0l, select: 1}
  spice_xraster_draw, pseudoevent
  xmanager, 'spice_xraster', tlb, /no_block, event_handler = 'spice_xraster_resize', $
    group_leader = group, cleanup = 'spice_xraster_cleanup'
END
