;+
; NAME:
;       SPICE_XCONTROL
;
; PURPOSE:
;	SPICE_XCONTROL is the main QL control window. After selecting a
;	file this windows opens with information about the data, and control
;	buttons etc. for the various display methods and data processing
;	options available for a particular set of data.
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xcontrol, data, group_leader = group
;
; INPUTS:
;	data: data object of type 'spice_data'
;
; KEYWORD PARAMETERS:
;	group_leader: Widget ID of parent widget
;
; OUTPUTS:
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      1-Jan-2013: First version started by Viggo Hansteen
;     16-Sep-2020: First version for SPICE started by Martin Wiesmann
;
; $Id: 2020-11-25 13:57 CET $
;-
;
;
pro spice_xcontrol_destroy, event
  pseudoevent = {WIDGET_KILL_REQUEST, $
    ID:event.id, $
    TOP:event.top, $
    HANDLER:event.handler}
  spice_xcontrol_event, pseudoevent
end

pro spice_xcontrol_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free,(*info).d
  ptr_free,info
end

pro spice_xcontrol_event, event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top,/destroy
  endif
end

; put information about data set in string array
pro spice_xcontrol_get_data_info, info
  ;nwin=*(*info).d->getnwin()
  ;nraster=*(*info).d->getnraster()
  line=strarr(99)
  line[0] = 'SPIOBSID: '+*(*info).d->get_obs_id()
  line[1] = 'SEQ_BEG : '+*(*info).d->get_header_info('SEQ_BEG', 0, '')
  line[2] = 'DATE-BEG: '+*(*info).d->get_start_time()
  line[3] = 'STUDYTYP: '+*(*info).d->get_header_info('STUDYTYP', 0, '')
  line[4] = 'STUDYDES: '+*(*info).d->get_header_info('STUDYDES', 0, '')
  line[5] = 'AUTHOR  : '+*(*info).d->get_header_info('AUTHOR', 0, '')
  line[6] = 'PURPOSE : '+*(*info).d->get_header_info('PURPOSE', 0, '')
  line[7] = '========================================================'
  line[8] = 'CROTA   : '+string(*(*info).d->get_satellite_rotation(), format='(F9.2)')
  line[9] = 'XCEN    : '+string(*(*info).d->get_xcen(0), format='(F8.1)') + '   ' + $
    'YCEN    : '+string(*(*info).d->get_ycen(0), format='(F8.1)')
  line[10] = 'FOVX    : '+string(*(*info).d->get_fovx(0), format='(F8.1)') + '   ' + $
    'FOVY    : '+string(*(*info).d->get_fovx(0), format='(F8.1)')
  line[11] = '========================================================'
  line[12] = 'Number of windows         : '+strtrim(string(*(*info).d->get_number_windows()),2)
  line[13] = 'Number of raster positions: '+strtrim(string(*(*info).d->get_number_exposures()),2)
  ;line[14] = 'SPIOBSID: '+*(*info).d->get_obs_id()



  ;    line[1] = '========================================================'
  ;    line[2] = 'Number of raster positions: '+strtrim(string(nraster),2)
  ;    line[3] = 'Number of line windows    : '+strtrim(string(nwin), 2)
  ;    line[4] = 'Line ID            Wavelength   Line window width/height     '
  ;    line[5] = '                    ('+string(197b)+'/pixel)              (pixels)          '
  ;    line[6] = '========================================================'
  ;  j = 7
  ;  for i = 0,nwin-1 do begin
  ;    lineid = strtrim(*(*info).d->getline_id(i), 2)
  ;    wvl = strtrim(string(*(*info).d->getline_wvl(i,wscale='pixels'),format='(f6.1)'), 2)
  ;    wvl_aa=strtrim(string(*(*info).d->getline_wvl(i,wscale='AA')),2)
  ;    npx = strtrim(string(*(*info).d->getxw(i)),2)
  ;    npy = strtrim(string(*(*info).d->getyw(i)),2)
  ;    newline='                                                           '
  ;    strput, newline, lineid, 0
  ;    strput, newline, wvl_aa+'/'+wvl, 18
  ;    strput, newline, npx+'/'+npy, 40
  ;    line[i+j] = newline
  ;  endfor
  ;
  case !version.os of
    'MacOS': cr=string(13b)
    'Win32': cr=string(13b)+string(10b)
    else: cr=string(10b)
  endcase
  ;
  (*info).data_textdump = strjoin(line[0:13],cr)
end

; print filename to console
pro spice_xcontrol_printfilename, event
  widget_control, event.top, get_uvalue = info
  print,(*(*info).d)->get_filename()
end

; Display Header in text window
pro spice_xcontrol_hdrdisp,event
  widget_control, event.top ,get_uvalue = info
  ; open text widget window to dump bytes in
  hdr_widget = widget_base(title = 'Header Contents',  $
    group_leader = (*info).tlb,/row)
  closefield = widget_base(hdr_widget,/column)
  closebutton = widget_button(closefield, value = 'Close', $
    event_pro = 'spice_xcontrol_hdrdisp_destroy')

  case !version.os of
    'MacOS': cr=string(13b)
    'Win32': cr=string(13b)+string(10b)
    else: cr=string(10b)
  endcase

  tab = widget_tab(hdr_widget, multiline=4)
  for itab=0,*(*info).d->get_number_windows()-1 do begin
    disp_base = widget_base(tab,/column, title=*(*info).d->get_window_id(itab))
    disp_field = widget_text(disp_base, group_leader = (*info).tlb, /scroll, $
      xs=100, ysize = 50)

    textall=''
    hdr=*(*info).d->get_header(itab, /string)
    for i=0, n_elements(hdr)-1 do begin
      textall=textall+hdr[i]+cr
    endfor
    widget_control, disp_field, set_value = textall, set_text_top_line=0
  endfor ; itab=0,*(*info).d->get_number_windows()-1
  widget_control, hdr_widget, /realize

  xmanager, 'DisplayHeaderContents', hdr_widget, /no_block, $
    group_leader = (*info).tlb
end

; close Header Display
pro spice_xcontrol_hdrdisp_destroy, event
  widget_control, event.top,/destroy
end

; event procedure for header display,
pro DisplayHeaderContents_event, event
end

function spice_xcontrol_lineselect, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).lineselect, get_value = lineselect
  widget_control, (*info).lineall_clear, get_value = lineall_clear
  case event.id of
    (*info).lineall_clear: begin
      if event.select eq 0 then return,0
      if event.value eq 0 then begin
        lineselect[*]=1
        lineall_clear=[1,0]
      endif else begin
        lineselect[*]=0
        lineall_clear=[0,1]
      endelse
      widget_control, (*info).lineselect, set_value = lineselect
    end
    (*info).lineselect: begin
      lineall_clear[*]=0
    end
    else: return,0
  endcase
  (*info).lines = lineselect
  widget_control, (*info).lineall_clear, set_value = lineall_clear
  return,0
end

; Display Header in text window
pro spice_xcontrol_leve3file,event
  widget_control, event.top ,get_uvalue = info
  if (*info).level3_dir eq '' then cd,current=wdir else wdir=(*info).level3_dir
  file=dialog_pickfile(dialog_parent=(*info).tlb,/directory,/write,path=wdir, $
    title='Select level 3 directory',get_path=level3_dir)
  if N_ELEMENTS(level3_dir) gt 0 && level3_dir ne '' then begin
    (*info).level3_dir=level3_dir
    save,level3_dir,file=IRISxfiles_appReadme()+'/spice_xcontrol_lev3dir.tmp'
    widget_control,(*info).lev3filemenu,set_value='Change level 3 dir: '+level3_dir+' '
  endif
end

function spice_xcontrol_dispselect, event
  widget_control, event.top, get_uvalue = info
  displaymode = event.value
  ; displaymode=0:  "Detector view". This displays the detector by showing
  ;                  all lines as a function of wavelength an slit position.
  ;                  Raster position or time is set, and can be changed in
  ;                  the display widget.
  ;
  ; displaymode=1: "Raster Browser" Peter Young's browser
  ;
  ; displaymode=2: "Line raster display".
  ;                 This is line profiles as a function of slit position
  ;                 at all raster positions or time (for "sit and stare"
  ;                 studies. Several lines may be displayed in the same
  ;                 window.
  ;
  ; displaymode=2:  "Line profile evolution". This is line profiles as a
  ;                  function of raster position (or time for "sit and stare"
  ;                  studies) at one slit position. Only one line window in
  ;                  one display window. Slit position can be set and changed
  ;                  in the display widget.
  ;
  ; displaymode=3:  "Intensity map". This is counts/intensity as a function
  ;                  of solar y (slit position) and solar x (for rasters) or
  ;                  time (for "sit and stare".) Intensities are obtained by
  ;                  calculating 0th order intensity moment of the line
  ;                  profile. Only one line map in each window, but lines
  ;                  can be set and changed also within the widget.
  ;
  warning = 'At least one line must be selected'
  case displaymode of
    0: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      spice_xdetector,*(*info).d,lindx, group_leader = (*info).tlb
    end
    1: begin
      spice_raster_browser,*(*info).d
    end
    2: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      spice_xraster,*(*info).d,lindx, group_leader = (*info).tlb
    end
    3: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      for i = 0, n_elements(lindx)-1 do $
        spice_xwhisker,*(*info).d,lindx[i], group_leader = (*info).tlb
    end
    4: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      for i = 0, n_elements(lindx)-1 do $
        spice_xmap,*(*info).d,linelist=lindx[i],group_leader=(*info).tlb
    end
  endcase
  return,0
end

pro spice_xcontrol_listmoments, event
  widget_control, event.top, get_uvalue = info
  thisevent=tag_names(event,/structure_name)
  case thisevent of
    'WIDGET_DROPLIST': begin
      mode = event.index
    end
    else:
  endcase
  case mode of
    0: begin
      return
    end
    1: begin
      spice_xcontrol_moments, 'moment', event
    end
    2: begin
      spice_xcontrol_moments, 'gauss', event
    end
    3: begin
      spice_xcontrol_moments, 'dgf', event
    end
    else: message,'spice_xcontrol_listmoments: Impossible mode!!',/info
  endcase
end

pro spice_xcontrol_detnuv, event
  widget_control, event.top, get_uvalue = info
  case event.release of
    0: begin
      bin_sp=*(*info).d->binning_spectral()
      ccd=['NUV']
      for i = 0,n_elements(ccd)-1 do begin
        lindx=where(*(*info).d->getregion() eq strmid(ccd[i],0,3))
        case strmid(ccd[i],3,1) of
          '1': lindx=lindx[where((*(*info).d->getxs(lindx)-1)*bin_sp[lindx]+1 lt 2072)]
          '2': lindx=lindx[where((*(*info).d->getxs(lindx)-1)*bin_sp[lindx]+1 ge 2072)]
          else:
        endcase
        if lindx[0] lt 0 then goto, continue
        iris_xdetector,*(*info).d,lindx, group_leader = (*info).tlb
        continue:
      endfor
    end
    else:
  endcase
end

pro spice_xcontrol_detfuv, event
  widget_control, event.top, get_uvalue = info
  case event.release of
    0: begin
      bin_sp=*(*info).d->binning_spectral()
      ccd=['FUV1','FUV2']
      for i = 0,n_elements(ccd)-1 do begin
        lindx=where(*(*info).d->getregion() eq strmid(ccd[i],0,3))
        case strmid(ccd[i],3,1) of
          '1': lindx=lindx[where((*(*info).d->getxs(lindx)-1)*bin_sp[lindx]+1 lt 2072)]
          '2': lindx=lindx[where((*(*info).d->getxs(lindx)-1)*bin_sp[lindx]+1 ge 2072)]
          else:
        endcase
        if lindx[0] lt 0 then goto, continue
        iris_xdetector,*(*info).d,lindx, group_leader = (*info).tlb
        continue:
      endfor
    end
    else:
  endcase
end

pro spice_xcontrol_shownuv1, event
  widget_control, event.top, get_uvalue = info
  if ((*(*info).d->getsji(2,/noload))[0])[0] eq -1 then return
  case event.release of
    0: xsji_image,*(*info).d,2
    1:
    else:
  endcase
end

pro spice_xcontrol_shownuv2, event
  widget_control, event.top, get_uvalue = info
  if ((*(*info).d->getsji(3,/noload))[0])[0] eq -1 then return
  case event.release of
    0: xsji_image,*(*info).d,3
    1:
    else:
  endcase
end

pro spice_xcontrol_showfuv1, event
  widget_control, event.top, get_uvalue = info
  if ((*(*info).d->getsji(0,/noload))[0])[0] eq -1 then return
  case event.release of
    0: xsji_image,*(*info).d,0
    1:
    else:
  endcase
end

pro spice_xcontrol_showfuv2, event
  widget_control, event.top, get_uvalue = info
  if ((*(*info).d->getsji(1,/noload))[0])[0] eq -1 then return
  case event.release of
    0: xsji_image,*(*info).d,1
    1:
    else:
  endcase
end

pro spice_xcontrol_pointing, event
  widget_control, event.top, get_uvalue = info
  wset,(*info).pointing_wid
  xcen=(*(*info).d->getxcen())[0]
  ycen=(*(*info).d->getycen())[0]
  dx=max(*(*info).d->getfovx())
  dy=max(*(*info).d->getfovy())
  theta=*(*info).d->getinfo('SAT_ROT')
  date_obs=*(*info).d->getdate_obs()
  case event.release of
    0:
    1: begin
      widget_control,/hourglass
      *(*info).sdo->read,date_obs,quality='high'
      if (*(*info).sdo->getim())[0] eq -1 then begin
        *(*info).sdo->read,*(*info).d->getdate_obs(),quality='low'
      endif
      *(*info).sdo->rotate,date_obs,/keep_limb
      coord=*(*info).sdo->raster_coords(xcen,ycen,dx,dy,theta)
      xpointing_image,*(*info).sdo->getdata(),coord
    end
    4: begin
      case (*info).pointing_phase of
        1: plot_image,*(*info).sdo->getim_4500(),true=1,pos=[0,0,1,1],xstyle=5,ystyle=5
        -1: plot_image,*(*info).sdo->getim(),true=1,pos=[0,0,1,1],xstyle=5,ystyle=5
        else:
      endcase
      (*info).pointing_phase=(*info).pointing_phase*(-1)
      coord=*(*info).sdo->raster_coords(xcen,ycen,dx,dy,theta)
      plots,[coord.x1,coord.x2,coord.x3,coord.x4,coord.x1],$
        [coord.y1,coord.y2,coord.y3,coord.y4,coord.y1],/data
    end
    else:
  endcase
end

; calculate moments
pro spice_xcontrol_moments, fit, event
  widget_control, event.top, get_uvalue = info

  lindx = where((*info).lines eq 1)     ; index of selected lines
  if lindx[0] lt 0 then begin
    ok = dialog_message('Please select at least 1 line', /information,/center)
    return
  endif
  nindx = n_elements(lindx)  ; number of lines selected
  nwin = *(*info).d->getnwin()        ; number of lines in data object (total)
  ; all lines or list of lines
  widget_control,/hourglass
  if nindx eq nwin then $
    moment = obj_new('iris_moment',*(*info).d,fit=fit,group_leader=(*info).tlb ) $
  else $
    moment = obj_new('iris_moment',*(*info).d,iwin=lindx,fit=fit,group_leader=(*info).tlb)
  if not(ptr_valid(info)) then return ; if the xcontrol window closes
  (*info).moment = ptr_new(moment)
  ; launch xmap to display moments:
  iris_xmap,*(*info).moment,linelist = lindx,group_leader = (*info).tlb
  return
end

;-----------------------------------------------------------------------------
; start main program
;-----------------------------------------------------------------------------

pro spice_xcontrol, input_data, group_leader = group_leader

  if n_params() lt 1 then begin
    message,'spice_xcontrol, data [, group_leader = group]',/cont
    return
  endif

  data = spice_get_object(input_data, is_spice=is_spice, object_created=object_created)
  if ~is_spice then return

  ;  ; information about data set
  nwin = data->get_number_windows()         ; number of line windows
  nraster = (data->get_number_exposures(0))   ; number of raster positions
  line_id = data->get_window_id()
  ;
  ;
  ; top level base widget:
  tlb = widget_base(/row, mbar=menubar, $
    title='SPICE_Xcontrol - '+data->get_filename(), $
    xoffset=50, yoffset=50,group_leader=group_leader, /tlb_kill_request_events)

  lcol = widget_base(tlb, /frame, /column)      ;left column.
  mcol = widget_base(tlb, /frame, /column)      ;middle column.
  rcol = widget_base(tlb, /frame, /column)      ;right column.

  ; create menu bar:
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  closemenu=widget_button(filemenu, value='Close', $
    event_pro='spice_xcontrol_destroy')

  ;  hdrdispmenu=widget_button(optmenu, value='Display header', $
  ;    event_pro='spice_xcontrol_hdrdisp')

  ; Select Line window(s)
  lineselect_nuvase = widget_base(lcol, /column, /frame)
  lineselect_row = widget_base(lineselect_nuvase, /column)

  ; data source label field
  lineselect_label = widget_base(lineselect_nuvase, /row)
  title = 'Select Line window(s)'    ; line select label string
  lslabel = widget_label(lineselect_label, value=title)

  if N_ELEMENTS(line_id) gt 3 then column=2 else column=1
  lineselect = cw_bgroup(lineselect_row, /nonexclusive, line_id, column=column, event_func = 'spice_xcontrol_lineselect')
  lineall_clear = cw_bgroup(lineselect_row, /nonexclusive, ['all','clear'], column=2, event_func = 'spice_xcontrol_lineselect')

  ; Select display tool
  dispselect_base = widget_base(lcol, /column, /frame)

  ; Select display tool label field
  dispselect_label = widget_base(dispselect_base, /row)
  title = 'Tool'    ; display tool select label string
  dslabel = widget_label(dispselect_label, value=title)

  ;  tools=['Detector        ', $
  ;    'Browser         ', $
  ;    'Spectroheliogram', $
  ;    'Whisker         ', $
  ;    'Intensity map   ']
  tools=['Detector        ', $
    'Raster Browser      ', $
    'Raster', $
    'Whisker         ', $
    'Intensity map (XMap)']
  if max(nraster) le 1 then tools = tools[0:2]
  dispselect = cw_bgroup(dispselect_label, row = 5, $
    tools,  event_func = 'spice_xcontrol_dispselect')

  ; Text field to display data information
  disp_base = widget_base(mcol, /row)
  data_info = widget_text(disp_base, xs=60, ys=20,/scroll)

  ; Icons to show layout of windows on detector
  xsize=250
  detector1 = spice_xcontrol_detector(data, detector2=detector2, xsize=xsize, ysize=ysize)
  detector_base=widget_base(mcol,/row)
  detector1_icon                   = widget_draw(detector_base  ,              $
    retain = 2,                              $
    XSize       = xsize,                 $
    YSize       = ysize,     $
    frame       = 1, event_pro='spice_xcontrol_detnuv',/button_events)

  detector2_icon                   = widget_draw(detector_base  ,              $
    retain = 2,                              $
    XSize       = xsize,                 $
    YSize       = ysize,     $
    frame       = 1, event_pro='spice_xcontrol_detfuv',/button_events)

  ; Solar map showing pointing of raster
  pointing_size=200
  calc_xysize,pointing_size,pointing_size,xs,ys,nxchar=4,nychar=4
  pointing_base=widget_base(lcol,/row)
  pointing_icon= widget_draw(pointing_base  ,             $
    retain = 2, $
    XSize       = pointing_size,                      $
    YSize       = pointing_size,                      $
    frame       = 1, event_pro='spice_xcontrol_pointing',/button_events)

  info_base=widget_base(pointing_base,/col)

  ;  lsubcol2 = widget_base(lcol, /row,/frame)
  ;  momfield = widget_base(lsubcol2, /col)
  ;  momnames = ['   Not Selected  ',$
  ;    ' Profile Moments ',$
  ;    'Single Gauss. Fit',$
  ;    'Double Gauss. Fit']
  ;  momtitle = 'Line fit '
  ;  momplot  = widget_droplist(momfield, value = momnames, title=momtitle, $
  ;    event_pro = 'spice_xcontrol_listmoments')

  ; Close and header display buttons
  lsubcol3 = widget_base(lcol, /column,/frame)
  hdrdispbutton= widget_button(lsubcol3, value='  Display header  ', $
    event_pro = 'spice_xcontrol_hdrdisp')
  closefield  = widget_base(lsubcol3, /column)
  printfilesbutton = widget_button(lsubcol3, value = 'Print filename to console', $
    event_pro = 'spice_xcontrol_printfilename')
  closebutton = widget_button(lsubcol3, value = '  Close   ', $
    event_pro = 'spice_xcontrol_destroy')

  ; SPICE icon
  spice_icon_base=widget_base(rcol,/col)
  spice_icon = widget_draw(spice_icon_base, retain=2, XSize=120, YSize=120, frame=1)

  ; Define the info structure, used to send information around
  info= { d:ptr_new(), $
    sdo:ptr_new(), $
    moment:ptr_new(), $
    lineselect:lineselect, $
    lineall_clear:lineall_clear, $
    data_textdump:'', $
    lines:intarr(nwin), $
    sjiref:-1, $
    pointing_size:pointing_size,$
    pointing_wid:-1,$
    pointing_phase:1,$
    tlb:tlb }
  info=ptr_new(info,/no_copy)
  (*info).d=ptr_new(data)

  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  widget_control, tlb, /realize
  ;
  spice_xcontrol_get_data_info, info
  widget_control, data_info, set_value = (*info).data_textdump, /append

  ; get the initial color table
  TVLCT, red, green, blue, /get

  widget_control, detector1_icon , get_value = drawID1
  wset,drawID1
  bad=where(finite(detector1) eq 0,nbad)
  if nbad ne 0 then detector1[bad]=-999
  deticon_min = min(iris_histo_opt(detector1,0.005,missing=-999)>1.e-4, max=deticon_max, /nan)
  loadct,9
  tvscl,alog10(detector1 > deticon_min < deticon_max),/nan

  widget_control, detector2_icon , get_value = drawID2
  wset,drawID2
  bad=where(finite(detector2) eq 0,nbad)
  if nbad ne 0 then detector2[bad]=-999
  deticon_min = min(iris_histo_opt(detector2,0.005,missing=-999)>1.e-4, max=deticon_max, /nan)
  loadct,3
  tvscl,alog10(detector2 > deticon_min < deticon_max),/nan

  ; set the inital color table again
  TVLCT, red, green, blue

  chars=2.0
  widget_control, spice_icon , get_value = drawID1
  wset,drawID1
  have_con=have_proc('spice_xfiles',out=fname)
  if have_con then begin
    fileName = concat_dir(file_dirname(file_dirname(fname)), 'ancillary/spice-logo---colour.jpg')
    if (file_info(fileName)).exists then begin
      read_jpeg,filename,icon
      icon_resized = congrid(icon,3,120,120)
      tvscl,icon_resized,true=1
    endif else begin
      xyouts,0.5,0.5,'SPICE',chars=chars,/normal,alignment=0.5
    endelse
  endif else begin
    xyouts,0.5,0.5,'SPICE',chars=chars,/normal,alignment=0.5
  endelse


  ; pointing icon

  npt=100
  phi=indgen(npt)/(npt-1.)*2.*!pi
  widget_control, pointing_icon , get_value = drawID2
  wset,drawID2
  ; check if we have a valid date, no SDO icon if not
  date_obs=data->get_start_time()
  ;  help,date_obs
  ;  date_obs='2013-10-25T05:05:30.761'
  ;  help,date_obs
  if datatype(date_obs) eq 'STR' then begin
    if strtrim(date_obs,2) eq '' then showim=0 else showim=1
  endif else begin
    if date_obs eq 0 then showim=0 else showim=1
  endelse
  ; check if we have a true color display, no SDO icon if not
  if !d.n_colors gt 256 then showim=1*showim else showim=0
  if showim then begin
    sdo=obj_new('sdo_icon',date_obs)
    if (sdo->getim_4500())[0] ne -1 and (sdo->getim())[0] ne -1 then begin
      sdo->rotate,date_obs,/keep_limb
      xcen=data->get_xcen()
      ycen=data->get_ycen()
      ;
      limb=sdo->getlimb()
      x=limb.r0*sin(phi)
      y=limb.r0*cos(phi)
      ; compute corners of raster
      dx=data->get_fovx()
      dy=data->get_fovy()
      theta=data->get_satellite_rotation()
      coord=sdo->raster_coords(xcen,ycen,dx,dy,theta)
      plot_image,sdo->getim(),true=1,pos=[0,0,1,1],xstyle=5,ystyle=5
      plots,[coord.x1,coord.x2,coord.x3,coord.x4,coord.x1],$
        [coord.y1,coord.y2,coord.y3,coord.y4,coord.y1],/data
      (*info).pointing_phase=1
      (*info).sdo=ptr_new(sdo)
    endif else showim=0
  endif
  if not showim then begin
    widget_control, pointing_icon , sensitive=0
    xcen=data->get_xcen()
    ycen=data->get_ycen()
    solar_radius=960.
    dx=data->get_fovx()
    dy=data->get_fovy()
    xraster=xcen[0]
    yraster=ycen[0]
    theta=data->get_satellite_rotation()/360.0*!pi
    coord={x1:xraster-dx*cos(theta)/2.-dy*sin(theta)/2., $
      x2:xraster+dx*cos(theta)/2.-dy*sin(theta)/2., $
      x3:xraster+dx*cos(theta)/2.+dy*sin(theta)/2., $
      x4:xraster-dx*cos(theta)/2.+dy*sin(theta)/2., $
      y1:yraster-dy*cos(theta)/2.-dx*sin(theta)/2., $
      y2:yraster-dy*cos(theta)/2.+dx*sin(theta)/2., $
      y3:yraster+dy*cos(theta)/2.+dx*sin(theta)/2., $
      y4:yraster+dy*cos(theta)/2.-dx*sin(theta)/2.}
    x=solar_radius*sin(phi)
    y=solar_radius*cos(phi)
    plot,x,y,xstyle=4,ystyle=4,xmargin=[2,2],ymargin=[2,2]
    plots,[coord.x1,coord.x2,coord.x3,coord.x4,coord.x1],$
      [coord.y1,coord.y2,coord.y3,coord.y4,coord.y1],/data
  endif
  (*info).pointing_wid=drawID2

  xmanager, 'spice_xcontrol', tlb, /no_block, $
    group_leader = group, cleanup = 'spice_xcontrol_cleanup'
end
