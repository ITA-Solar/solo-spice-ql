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
; $Id: 16.10.2020 11:45 CEST $
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
  nwin=*(*info).d->getnwin()
  nraster=*(*info).d->getnraster()
  line=strarr(256)
  line[0] = 'OBSID: '+strtrim(*(*info).d->getobsid(), 2)
  line[1] = '========================================================'
  line[2] = 'Number of raster positions: '+strtrim(string(nraster),2)
  line[3] = 'Number of line windows    : '+strtrim(string(nwin), 2)
  line[4] = 'Line ID            Wavelength   Line window width/height     '
  line[5] = '                    ('+string(197b)+'/pixel)              (pixels)          '
  line[6] = '========================================================'
  j = 7
  for i = 0,nwin-1 do begin
    lineid = strtrim(*(*info).d->getline_id(i), 2)
    wvl = strtrim(string(*(*info).d->getline_wvl(i,wscale='pixels'),format='(f6.1)'), 2)
    wvl_aa=strtrim(string(*(*info).d->getline_wvl(i,wscale='AA')),2)
    npx = strtrim(string(*(*info).d->getxw(i)),2)
    npy = strtrim(string(*(*info).d->getyw(i)),2)
    newline='                                                           '
    strput, newline, lineid, 0
    strput, newline, wvl_aa+'/'+wvl, 18
    strput, newline, npx+'/'+npy, 40
    line[i+j] = newline
  endfor
  ;
  case !version.os of
    'MacOS': cr=string(13b)
    'Win32': cr=string(13b)+string(10b)
    else: cr=string(10b)
  endcase
  ;
  (*info).data_textdump = strjoin(line[0:j+i],cr)
end

; print filename to console
pro spice_xcontrol_printfilename, event
  widget_control, event.top, get_uvalue = info
  print,(*(*info).d)->getfilename()
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

  ysz = 60.*n_elements(*(*info).d->gethdr())
  disp_base = widget_base(hdr_widget,/column)
  disp_field = widget_text(disp_base, group_leader = (*info).tlb, /scroll, $
    xs=100, ysize = 50)

  case !version.os of
    'MacOS': cr=string(13b)
    'Win32': cr=string(13b)+string(10b)
    else: cr=string(10b)
  endcase

  textall=''
  hdr=*(*info).d->gethdr(0)

  for i=0, n_elements(hdr)-1 do begin
    textall=textall+hdr[i]+cr
  endfor
  for iext=1,*(*info).d->getnwin() do begin
    hdr=*(*info).d->gethdr(iext)
    textall=textall+'======= Header for extension '+ $
      string(iext,format='(i2)')+' ================================'+cr
    for i=0, n_elements(hdr)-1 do begin
      textall=textall+hdr[i]+cr
    endfor
  endfor

  widget_control, disp_field, set_value = textall, set_text_top_line=0
  widget_control, hdr_widget, /realize

  xmanager, 'Display Header Contents', hdr_widget, /no_block, $
    group_leader = (*info).tlb
end

; close Header Display
pro spice_xcontrol_hdrdisp_destroy, event
  widget_control, event.top,/destroy
end

function spice_xcontrol_lineselect_fuv, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).lineselect_fuv, get_value = lineselect
  (*info).lines[where((*info).line_mask)] = lineselect
  return,0
end

function spice_xcontrol_lineselect_nuv, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).lineselect_nuv, get_value = lineselect
  (*info).lines[where((*info).line_mask eq 0)] = lineselect
  return, 0
end

pro spice_xcontrol_lineselect_sji, event
  widget_control, event.top, get_uvalue = info
  thisevent=tag_names(event,/structure_name)
  case thisevent of
    'WIDGET_DROPLIST': begin
      mode = event.index
    end
    else: mode=-1
  endcase
  (*info).sjiref = mode
  return
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

function spice_xcontrol_replace_level3,event
  widget_control, event.top ,get_uvalue = info
  widget_control, (*info).replace_level3, get_value = replace_l3
  (*info).replace_l3=replace_l3
  return,0
end

pro spice_xcontrol_level3, event
  widget_control, event.top, get_uvalue = info
  if (*info).level3_dir eq '' then cd,current=wdir else wdir=(*info).level3_dir
  search=strsplit(*(*info).d->getfilename(),'_',/extract)
  search=strjoin((search)[0:n_elements(search)-2],'_')
  obsdirl2 = file_dirname(search)
  obsdir = file_basename(obsdirl2)
  search=file_search(search+'*.fits')
  dirsep = path_sep()
  if strmid(wdir, 0,1, /reverse_offset) ne dirsep then wdir = wdir+dirsep
  if strmid(obsdir, 0,1, /reverse_offset) ne dirsep then obsdir = obsdir+dirsep
  if strmid(obsdirl2, 0,1, /reverse_offset) ne dirsep then obsdirl2 = obsdirl2+dirsep
  if (*info).sjiref eq -1 then sji_ref=*(*info).d->getdefault_sjiwin() $
  else sjiref=(*info).sjiref
  sjifile=*(*info).d->getfilename_sji(sjiref)
  if (where((*info).lines eq 1))[0] eq -1 then lines='all' $
  else lines=*(*info).d->getline_id(where((*info).lines eq 1))
  message = ['iris_make_fits_level3 on '+strjoin(lines,'/')+' line(s),', $
    'to be stored in directory ',$
    wdir, $
    'Save directory can be reset in "Options" menu', ' ']
  desc = [ $
    '0, LABEL, '+message+', CENTER', $
    '0, BUTTON, copy reference SJI file|copy all SJI files|add "'+obsdir+'" to save directory, COLUMN, TAG=bg', $
    '1, BASE,, ROW', $
    '0, BUTTON, OK, QUIT, TAG=OK', $
    '2, BUTTON, CANCEL, QUIT']
  answer = CW_FORM(desc, /COLUMN, title='Calling iris_make_fits_level3')

  if answer.ok then begin
    widget_control,/hourglass
    if answer.bg[2] then wdir=wdir+obsdir
    file_mkdir, wdir
    if ~file_test(sjifile) then sjifile=!NULL
    if max((*info).lines) eq 0 or total((*info).lines) eq *(*info).d->getnwin() then begin
      iris_make_fits_level3,search,[0],/sp,sjifile=sjifile,/all,wdir=wdir,replace=(*info).replace_l3
    endif else begin
      iwin=where((*info).lines eq 1)
      iris_make_fits_level3,search,iwin,/sp,sjifile=sjifile,wdir=wdir,replace=(*info).replace_l3
    endelse
    if answer.bg[1] then begin
      sjifileall=file_search(obsdirl2+'*SJI*.fits')
      if N_ELEMENTS(sjifileall) gt 0 && sjifileall[0] ne '' then file_copy, sjifileall, wdir
    endif else if answer.bg[0] && file_test(sjifile) then begin
      file_copy, sjifile, wdir
    endif
  endif
end

function spice_xcontrol_dispselect, event
  widget_control, event.top, get_uvalue = info
  displaymode = event.value
  ; displaymode=0: "Line raster display".
  ;                 This is line profiles as a function of slit position
  ;                 at all raster positions or time (for "sit and stare"
  ;                 studies. Several lines may be displayed in the same
  ;                 window.
  ;
  ; displaymode=1: "Raster Browser" Peter Young's browser
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
  ; displaymode=4:  "Detector view". This displays the detector by showing
  ;                  all lines as a function of wavelength an slit position.
  ;                  Raster position or time is set, and can be changed in
  ;                  the display widget.
  ;
  warning = 'At least one line must be selected'
  case displaymode of
    0: begin
      bin_sp=*(*info).d->binning_spectral()
      ccd=['FUV1','FUV2','NUV']
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
    1: begin
      iris_raster_browser,*(*info).d
    end
    2: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      iris_xraster,*(*info).d,lindx, group_leader = (*info).tlb
    end
    3: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      for i = 0, n_elements(lindx)-1 do $
        iris_xwhisker,*(*info).d,lindx[i], group_leader = (*info).tlb
    end
    4: begin
      lindx = where((*info).lines eq 1)
      if lindx[0] eq -1 then begin
        ok = dialog_message(warning,/center)
        return, 0
      endif
      for i = 0, n_elements(lindx)-1 do $
        iris_xmap,*(*info).d,linelist=lindx[i],group_leader=(*info).tlb
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

pro spice_xcontrol, data, group_leader = group_leader

  if n_params() lt 1 then begin
    message,'spice_xcontrol, data [, group_leader = group]',/cont
    return
  endif

  ;  dirsep=path_sep()
  ;  ; information about data set
  ;  nwin = data->getnwin()         ; number of line windows
  ;  nraster = (data->getnraster())   ; number of raster positions
  ;  nslitw=(data-> getyw())
  ;  line_npx = (data-> getxw())
  ;  line_id = data->getline_id()
  ;  line_mask = intarr(nwin)
  ;
  ;  ; construct an image of the detectors
  ;  bin_sp=data->binning_region('FUV')
  ;  fuvicon=fltarr(((data->getccd_sz('FUV'))[0])/bin_sp,max(data->getyw()))
  ;  bin_sp=data->binning_region('NUV')
  ;  nuvicon=fltarr(((data->getccd_sz('NUV'))[0])/bin_sp,max(data->getyw()))
  ;  yw=data->getyw()
  ;  ywmax=max(data->getyw(),ic)
  ;  ysmax=data->getys(ic)
  ;  xw=data->getxw()
  ;  xs=data->getxs()
  ;  ys=data->getys()-ysmax
  ;  for i=0,nwin-1 do begin
  ;    wd=data->getvar(i)
  ;    imax=data->getinfo('TDP90_'+strtrim(string(i+1),2))
  ;    if data->getregion(i) eq 'FUV' then begin
  ;      xss=0
  ;      if data->getline_id(i) eq 'FULL CCD FUV2' then xss=xs[i]-xs[i-1]-1
  ;      key_frame=-1
  ;      repeat key_frame=key_frame+1 until $
  ;        max(data->descale_array(wd[xss:xss+xw[i]-1,*,key_frame]),/nan) gt imax/2. or key_frame eq nraster[i]-1
  ;      fuvicon[xs[i]:xs[i]+xw[i]-1,0:yw[i]-1] = data->descale_array(wd[xss:xss+xw[i]-1,*, key_frame])
  ;    endif else begin
  ;      xs[i]=xs[i] ;-(data->getccd('NUV'))[0]
  ;      key_frame=-1
  ;      repeat key_frame=key_frame+1 until $
  ;        max(data->descale_array(wd[*,*,key_frame]),/nan) gt imax/2. or key_frame eq nraster[i]-1
  ;      nuvicon[xs[i]:xs[i]+xw[i]-1,0:yw[i]-1] = data->descale_array(wd[*,*,key_frame])
  ;    endelse
  ;  endfor
  ;
  ;  index=where((data->getregion()) eq 'FUV',ct)
  ;  if ct gt 0 then line_mask[where((data->getregion()) eq 'FUV')]=1
  ;
  ; top level base widget:
  tlb = widget_base(/row, mbar=menubar, $
    title='SPICE_Xcontrol - '+data->get_filename(), $
    xoffset=50, yoffset=50,group_leader=group_leader, /tlb_kill_request_events)

  lcol = widget_base(tlb, /frame, /column)      ;left column.
  mcol = widget_base(tlb, /frame, /column)      ;middle column.
  rcol = widget_base(tlb, /frame, /column)      ;right column.
  ;
  level3dirfile = IRISxfiles_appReadme()+'/spice_xcontrol_lev3dir.tmp'
  if (file_info(level3dirfile)).exists then restore,level3dirfile else cd,current=level3_dir
  ; create menu bar:
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  closemenu=widget_button(filemenu, value='Close', $
    event_pro='spice_xcontrol_destroy')

  optmenu=widget_button(menubar, value='Options',/menu, uvalue='Options')
  lev3filemenu=widget_button(optmenu, value='Change level 3 dir: '+level3_dir+' ', $
    event_pro='spice_xcontrol_leve3file')
  hdrdispmenu=widget_button(optmenu, value='Display header', $
    event_pro='spice_xcontrol_hdrdisp')

  ; Select Line window(s)
  lineselect_nuvase = widget_base(lcol, /column, /frame)
  lineselect_row = widget_base(lineselect_nuvase, /row)
  lineselect_rcol = widget_base(lineselect_nuvase, /col)

  ; data source label field
  ;  lineselect_label = widget_base(lineselect_nuvase, /row)
  ;  title = 'Select Line window(s)'    ; line select label string
  ;  lslabel = widget_label(lineselect_label, value=title)

  ;  index = where(line_mask,ct)
  ;  if ct gt 0 then begin
  ;    line_id_fuv=line_id[index]
  ;    lineselect_fuv = cw_bgroup(lineselect_row, /nonexclusive, $
  ;      line_id_fuv, event_func = 'spice_xcontrol_lineselect_fuv')
  ;  endif else lineselect_fuv = strarr(1)
  ;  index = where(line_mask eq 0,ct)
  ;  if ct gt 0 then begin
  ;    line_id_nuv=line_id[index]
  ;    lineselect_nuv = cw_bgroup(lineselect_row, /nonexclusive, $
  ;      line_id_nuv, event_func = 'spice_xcontrol_lineselect_nuv')
  ;  endif else lineselect_nuv = strarr(1)
  ;
  ;  index = where(data->getread_sji() eq 1,ct)
  ;  if ct gt 0 then begin
  ;    line_id_sji=data->getsji_id(index)
  ;    lineselect_sji = widget_droplist(lineselect_rcol, value = line_id_sji, title='Level 3 ref cube', $
  ;      event_pro = 'spice_xcontrol_lineselect_sji')
  ;  endif else lineselect_sji = strarr(1)
  ;
  ;  level3button = widget_button(lineselect_rcol, value='Generate level3 files ', $
  ;    event_pro = 'spice_xcontrol_level3')
  ;
  ;  replace_l3=0
  ;  replace_level3 = cw_bgroup(lineselect_rcol,/nonexclusive,'Replace existing level3 file', $
  ;    event_func = 'spice_xcontrol_replace_level3',set_value=replace_l3)
  ;
  ;  ; Select display mode
  ;  dispselect_base = widget_base(lcol, /column, /frame)
  ;
  ;  ; Select display mode label field
  ;  dispselect_label = widget_base(dispselect_base, /row)
  ;  title = 'Mode'    ; display mode select label string
  ;  dslabel = widget_label(dispselect_label, value=title)
  ;
  ;  modes=['Detector        ', $
  ;    'Browser         ', $
  ;    'Spectroheliogram', $
  ;    'Whisker         ', $
  ;    'Intensity map   ']
  ;  if max(nraster) le 1 then modes = modes[0:2]
  ;  dispselect = cw_bgroup(dispselect_label, row = 5, $
  ;    modes,  event_func = 'spice_xcontrol_dispselect')
  ;
  ;  ; Text field to display data information
  ;  disp_base = widget_base(mcol, /row)
  ;  data_info = widget_text(disp_base, xs=60, ys=20,/scroll)

  ; Icons to show layout of windows on detector
  ;  icon_size=400
  ;  icon_aspect=1./2.66
  ;  nuv_icon_base=widget_base(mcol,/col)
  ;  nuv_icon                   = widget_draw(nuv_icon_base  ,              $
  ;    retain = 2,                              $
  ;    XSize       = icon_size,                 $
  ;    YSize       = icon_size*icon_aspect,     $
  ;    frame       = 1, event_pro='spice_xcontrol_detnuv',/button_events)
  ;  line_nuv=min(where(line_mask eq 0,ct))
  ;
  ;  fuv_icon_base=widget_base(mcol,/col)
  ;  fuv_icon                   = widget_draw(fuv_icon_base  ,              $
  ;    retain = 2,                              $
  ;    XSize       = icon_size,                 $
  ;    YSize       = icon_size*icon_aspect,     $
  ;    frame       = 1, event_pro='spice_xcontrol_detfuv',/button_events)
  ;  line_fuv=min(where(line_mask,ct))
  ;  info_base = widget_base(mcol,/col)
  ;  label='DATE_OBS: '+data->getinfo('DATE_OBS')
  ;  info_fuv_label = widget_label(info_base, value=label,/align_left)
  ;  label='XCEN: '+strtrim(string(data->getxcen(),format='(f7.2)'),2)+' ' + $
  ;    'YCEN: '+strtrim(string(data->getycen(),format='(f7.2)'),2)+' ' + $
  ;    'FOVX: '+strtrim(string(data->getfovx(),format='(f7.2)'),2)+' ' + $
  ;    'max(FOVY): '+strtrim(string(max(data->getfovy()),format='(f7.2)'),2)
  ;  info_fuv_label = widget_label(info_base, value=label,/align_left)
  ;  label='SAT_ROT: '+strtrim(string(data->getinfo('SAT_ROT'),format='(f6.2)'),2)
  ;  info_fuv_label = widget_label(info_base, value=label,/align_left)
  ;  label='OBS_DESC: '+data->getinfo('OBS_DESC')
  ;  info_fuv_label = widget_label(info_base, value=label,/align_left)
  ;  label='OBSLABEL: '+data->getinfo('OBSLABEL')+' OBSTITLE: '+data->getinfo('OBSTITLE')
  ;  info_fuv_label = widget_label(info_base, value=label,/align_left)
  ;
  ;  ; SJI images
  ;
  ;  sji_size=150
  ;  sji_nuv_base=widget_base(rcol,/col)
  ;  sji_nuv1_icon                   = widget_draw(sji_nuv_base  ,             $
  ;    retain = 2, $
  ;    XSize       = sji_size,                      $
  ;    YSize       = sji_size,                      $
  ;    frame       = 1, event_pro='spice_xcontrol_shownuv1',/button_events)
  ;  sji_nuv2_icon                   = widget_draw(sji_nuv_base  ,             $
  ;    retain = 2, $
  ;    XSize       = sji_size,                      $
  ;    YSize       = sji_size,                      $
  ;    frame       = 1, event_pro='spice_xcontrol_shownuv2',/button_events)
  ;  sji_fuv1_icon                   = widget_draw(sji_nuv_base  ,             $
  ;    retain = 2, $
  ;    XSize       = sji_size,                      $
  ;    YSize       = sji_size,                      $
  ;    frame       = 1, event_pro='spice_xcontrol_showfuv1',/button_events)
  ;  sji_fuv2_icon                   = widget_draw(sji_nuv_base  ,             $
  ;    retain = 2, $
  ;    XSize       = sji_size,                      $
  ;    YSize       = sji_size,                      $
  ;    frame       = 1, event_pro='spice_xcontrol_showfuv2',/button_events)

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

  lsubcol2 = widget_base(lcol, /row,/frame)
  momfield = widget_base(lsubcol2, /col)
  momnames = ['   Not Selected  ',$
    ' Profile Moments ',$
    'Single Gauss. Fit',$
    'Double Gauss. Fit']
  momtitle = 'Line fit '
  momplot  = widget_droplist(momfield, value = momnames, title=momtitle, $
    event_pro = 'spice_xcontrol_listmoments')
  ; Close and header display buttons
  lsubcol3 = widget_base(lcol, /row,/frame)
  llsubcol3= widget_base(lsubcol3, /column)      ;left column.
  rlsubcol3= widget_base(lsubcol3, /column)      ;right column.
  hdrdispfield = widget_base(llsubcol3, /col)
  hdrdispbutton= widget_button(hdrdispfield, value='Display header  ', $
    event_pro = 'spice_xcontrol_hdrdisp')
  closefield  = widget_base(rlsubcol3, /column)
  closebutton = widget_button(closefield, value = 'Close   ', $
    event_pro = 'spice_xcontrol_destroy')
  printfilesbutton = widget_button(lcol, value = 'Print filename to console', $
    event_pro = 'spice_xcontrol_printfilename')

  ; SPICE icon
  spice_icon_base=widget_base(rcol,/col)
  spice_icon = widget_draw(spice_icon_base, retain=2, XSize=120, YSize=120, frame=1)

  nwin=1
  ; Define the info structure, used to send information around
  info= { d:ptr_new(), $
    sdo:ptr_new(), $
    moment:ptr_new(), $
    lev3filemenu:lev3filemenu, $
    ;    lineselect_sji:lineselect_sji, $
    ;    lineselect_nuv:lineselect_nuv, $
    ;    lineselect_fuv:lineselect_fuv, $
    data_textdump:'', $
    level3_dir:level3_dir,$
    ;    replace_l3:replace_l3,$
    ;    replace_level3:replace_level3,$
    lines:intarr(nwin), $
    sjiref:-1, $
    ;    line_mask:line_mask, $
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
  ;  spice_xcontrol_get_data_info, info
  ;  widget_control, data_info, set_value = (*info).data_textdump, /append

  ;  widget_control, nuv_icon , get_value = drawID1
  ;  wset,drawID1
  ;  deticon_resized = congrid(nuvicon,icon_size,icon_size*icon_aspect)
  ;  bad=where(finite(deticon_resized) eq 0,nbad)
  ;  if nbad ne 0 then deticon_resized[bad]=data->get_missing_value()
  ;  ;  nbad=n_elements(where(deticon_resized eq data->missing()))
  ;  ;  fbad=float(nbad)/float(n_elements(deticon_resized))
  ;  deticon_min = min(iris_histo_opt(deticon_resized,0.005,missing=data->get_missing_value())>1.e-4)
  ;  deticon_max = max(iris_histo_opt(deticon_resized,0.005,missing=data->get_missing_value()))
  ;  deticon_mean=mean(deticon_resized)-deticon_min
  ;  loadct,9
  ;  tvscl,alog10(deticon_resized > deticon_min < deticon_max),/nan
  ;  loadct,0
  ;  widget_control, fuv_icon , get_value = drawID2
  ;  wset,drawID2
  ;  deticon_resized = congrid(fuvicon,icon_size,icon_size*icon_aspect)
  ;  bad=where(finite(deticon_resized) eq 0,nbad)
  ;  if nbad ne 0 then deticon_resized[bad]=data->get_missing_value()
  ;  ;  nbad=n_elements(where(deticon_resized eq data->missing()))
  ;  ;  fbad=float(nbad)/float(n_elements(deticon_resized))
  ;  deticon_min = min(iris_histo_opt(deticon_resized,0.01,/bot_only,missing=data->get_missing_value()))
  ;  deticon_max = max(iris_histo_opt(deticon_resized,0.001,/top_only,missing=data->get_missing_value()))
  ;  deticon_mean=mean(deticon_resized)-deticon_min
  ;  loadct,3
  ;  tvscl,(deticon_resized > deticon_min < deticon_max),/nan
  loadct,0
  ;
  chars=2.0
  chars_small=1.0
  ;  widget_control, sji_nuv1_icon , get_value = drawID
  ;  wset,drawID
  ;  lwin=2
  ;  sji_im=data->getsji(lwin,/noload)
  ;  if (sji_im[0])[0] eq -1 then begin
  ;    xyouts,0.5,0.5,'SJI_2796',chars=chars,/normal,alignment=0.5
  ;  endif else begin
  ;    imax=data->getinfo('DATAP90',lwin,/sji)
  ;    key_frame=-1
  ;    repeat key_frame=key_frame+1 until $
  ;      max(data->descale_array(sji_im[*,*,key_frame]),/nan) gt imax/2. $
  ;      or key_frame eq data->getnexp_sji(lwin)-1
  ;    sji_im=data->descale_array(sji_im[*,*,key_frame])
  ;    sumsptrl = data->getinfo('SUMSPTRL',lwin,/sji)
  ;    sumsptrl = sumsptrl eq 0 ? 1:sumsptrl
  ;    sumspat=data->getinfo('SUMSPAT',lwin,/sji)
  ;    sumspat = sumspat eq 0 ? 1:sumspat
  ;    sji_sz=[data->getinfo('NAXIS1',lwin,/sji),data->getinfo('NAXIS2',lwin,/sji)]
  ;    sji_im=congrid(sji_im,sji_sz[0],sji_sz[1]*sumspat/sumsptrl)
  ;    sji_im=iris_histo_opt(sji_im[*,*],0.01,missing=data->missing())
  ;    sji_im_lim=max(sji_im)/1000.
  ;    xw=data->getfovx_sji(lwin)
  ;    yw=data->getfovy_sji(lwin)
  ;    if xw ge yw then begin
  ;      sji_im_resized = congrid(sji_im,sji_size,sji_size*yw/xw)
  ;      tvscl,(sji_im_resized>sji_im_lim),0,(1.-yw/xw)/2.*sji_size
  ;    endif else begin
  ;      sji_im_resized = congrid(sji_im,sji_size*xw/yw,sji_size)
  ;      tvscl,(sji_im_resized>sji_im_lim),(1.-xw/yw)/2.*sji_size,0
  ;    endelse
  ;    xyouts,0.1,0.1,data->getsji_id(lwin),chars=chars_small,/normal,alignment=0.
  ;  endelse

  ;  widget_control, sji_nuv2_icon , get_value = drawID
  ;  wset,drawID
  ;  lwin=3
  ;  sji_im=data->getsji(lwin,/noload)
  ;  if (sji_im[0])[0] eq -1 then begin
  ;    xyouts,0.5,0.5,'SJI_2832',chars=chars,/normal,alignment=0.5
  ;  endif else begin
  ;    imax=data->getinfo('DATAP90',lwin,/sji)
  ;    key_frame=-1
  ;    repeat key_frame=key_frame+1 until $
  ;      max(data->descale_array(sji_im[*,*,key_frame]),/nan) gt imax/2. $
  ;      or key_frame eq data->getnexp_sji(lwin)-1
  ;    sji_im=data->descale_array(sji_im[*,*,key_frame])
  ;    sumsptrl = data->getinfo('SUMSPTRL',lwin,/sji)
  ;    sumsptrl = sumsptrl eq 0 ? 1:sumsptrl
  ;    sumspat=data->getinfo('SUMSPAT',lwin,/sji)
  ;    sumspat = sumspat eq 0 ? 1:sumspat
  ;    sji_sz=[data->getinfo('NAXIS1',lwin,/sji),data->getinfo('NAXIS2',lwin,/sji)]
  ;    sji_im=congrid(sji_im,sji_sz[0],sji_sz[1]*sumspat/sumsptrl)
  ;    sji_im=iris_histo_opt(sji_im[*,*],0.01,missing=data->missing())
  ;    sji_im_lim=max(sji_im)/1000.
  ;    xw=data->getfovx_sji(lwin)
  ;    yw=data->getfovy_sji(lwin)
  ;    if xw ge yw then begin
  ;      sji_im_resized = congrid(sji_im,sji_size,sji_size*yw/xw)
  ;      tvscl,(sji_im_resized>sji_im_lim),0,(1.-yw/xw)/2.*sji_size
  ;    endif else begin
  ;      sji_im_resized = congrid(sji_im,sji_size*xw/yw,sji_size)
  ;      tvscl,(sji_im_resized>sji_im_lim),(1.-xw/yw)/2.*sji_size,0
  ;    endelse
  ;    xyouts,0.1,0.1,data->getsji_id(lwin),chars=chars_small,/normal,alignment=0.
  ;  endelse

  ;  widget_control, sji_fuv1_icon , get_value = drawID
  ;  wset,drawID
  ;  lwin=0
  ;  sji_im=data->getsji(lwin,/noload)
  ;  if (sji_im[0])[0] eq -1 then begin
  ;    xyouts,0.5,0.5,'SJI_1330',chars=chars,/normal,alignment=0.5
  ;  endif else begin
  ;    imax=data->getinfo('DATAP90',lwin,/sji)
  ;    key_frame=-1
  ;    repeat key_frame=key_frame+1 until $
  ;      max(data->descale_array(sji_im[*,*,key_frame]),/nan) gt imax/2. $
  ;      or key_frame eq data->getnexp_sji(lwin)-1
  ;    sji_im=data->descale_array(sji_im[*,*,key_frame])
  ;    sumsptrl = data->getinfo('SUMSPTRl',lwin,/sji)
  ;    sumsptrl = sumsptrl eq 0 ? 1:sumsptrl
  ;    sumspat=data->getinfo('SUMSPAT',lwin,/sji)
  ;    sumspat = sumspat eq 0 ? 1:sumspat
  ;    sji_sz=[data->getinfo('NAXIS1',lwin,/sji),data->getinfo('NAXIS2',lwin,/sji)]
  ;    sji_im=congrid(sji_im,sji_sz[0],sji_sz[1]*sumspat/sumsptrl)
  ;    sji_im=iris_histo_opt(sji_im[*,*],0.01,missing=data->missing())
  ;    sji_im_lim=max(sji_im)/1000.
  ;    xw=data->getfovx_sji(lwin)
  ;    yw=data->getfovy_sji(lwin)
  ;    ;    for i=0,data->getnraster(0)-1 do begin
  ;    ;       sji_im[xys.xs[i]:xys.xs[i]+1,0:yw-1] = $
  ;    ;         (sji_im[xys.xs[i]:xys.xs[i]+1,0:yw-1]/10.)
  ;    ;    endfor
  ;    if xw ge yw then begin
  ;      sji_im_resized = congrid(sji_im,sji_size,sji_size*yw/xw)
  ;      tvscl,sji_im_resized>sji_im_lim,0,(1.-yw/xw)/2.*sji_size
  ;    endif else begin
  ;      sji_im_resized = congrid(sji_im,sji_size*xw/yw,sji_size)
  ;      tvscl,sji_im_resized>sji_im_lim,(1.-xw/yw)/2.*sji_size,0
  ;    endelse
  ;    xyouts,0.1,0.1,data->getsji_id(lwin),chars=chars_small,/normal,alignment=0.
  ;  endelse

  ;  widget_control, sji_fuv2_icon , get_value = drawID
  ;  wset,drawID
  ;  lwin=1
  ;  sji_im=data->getsji(lwin,/noload)
  ;  if (sji_im[0])[0] eq -1 then begin
  ;    xyouts,0.5,0.5,'SJI_1400',chars=chars,/normal,alignment=0.5
  ;  endif else begin
  ;    imax=data->getinfo('DATAP90',lwin,/sji)
  ;    key_frame=-1
  ;    repeat key_frame=key_frame+1 until $
  ;      max(data->descale_array(sji_im[*,*,key_frame]),/nan) gt imax/2. $
  ;      or key_frame eq data->getnexp_sji(lwin)-1
  ;    sji_im=data->descale_array(sji_im[*,*,key_frame])
  ;    sumsptrl = data->getinfo('SUMSPTRL',lwin,/sji)
  ;    ;sumsptrl = sumsptrf eq 0 ? 1:sumsptrl
  ;    sumspat=data->getinfo('SUMSPAT',lwin,/sji)
  ;    sumspat = sumspat eq 0 ? 1:sumspat
  ;    sji_sz=[data->getinfo('NAXIS1',lwin,/sji),data->getinfo('NAXIS2',lwin,/sji)]
  ;    sji_im=congrid(sji_im,sji_sz[0],sji_sz[1]*sumspat/sumsptrl)
  ;    sji_im=iris_histo_opt(sji_im[*,*],0.01,missing=data->missing())
  ;    sji_im_lim=max(sji_im)/1000.
  ;    xw=data->getfovx_sji(lwin)
  ;    yw=data->getfovy_sji(lwin)
  ;    if xw ge yw then begin
  ;      sji_im_resized = congrid(sji_im,sji_size,sji_size*yw/xw)
  ;      tvscl,sji_im_resized>sji_im_lim,0,(1.-yw/xw)/2.*sji_size
  ;    endif else begin
  ;      sji_im_resized = congrid(sji_im,sji_size*xw/yw,sji_size)
  ;      tvscl,sji_im_resized>sji_im_lim,(1.-xw/yw)/2.*sji_size,0
  ;    endelse
  ;    xyouts,0.1,0.1,data->getsji_id(lwin),chars=chars_small,/normal,alignment=0.
  ;  endelse


  ; SPICE icon

;  widget_control, iris_icon , get_value = drawID1
;  wset,drawID1
;  fileName = concat_dir(getenv('IRIS_ANCILLARY'),'iris_logo.jpg')
;  if (file_info(fileName)).exists then begin
;    read_jpeg,filename,icon
;    icon_resized = congrid(icon,3,iris_icon_size,iris_icon_size*iris_icon_aspect)
;    tvscl,icon_resized,true=1
;  endif else begin
;    xyouts,0.5,0.5,'IRIS',chars=chars,/normal,alignment=0.5
;  endelse
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
