;+
; NAME:
;     SPICE_BROWSER_WIDGET
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     This procedure creates the widget interface for spice_raster_browser.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_widget, data [, yoffsets=yoffsets, $
;       chunk_size=chunk_size, retina=retina, no_goes=no_goes, $
;       flare_data=flare_data]
;
; INPUTS:
;     DATA:  A spice_data object
;
; KEYWORDS:
;     QUIET: If set, then do not print messages to the IDL command window.
;
;     YOFFSETS: If set, then the three wavelength channels will be
;               adjusted for spatial offsets in Y. This should only be
;               used if you notice that there are offsets in the
;               images. Most data-sets should be
;               aligned. **There shouldn't be a need to use
;               this keyword any more.**
;
;     CHUNK_SIZE: Only applicable for sit-and-stare data. It defines the
;                 size of the chunks (in number of exposures) to be
;                 displayed.
;
;     RETINA: The widget was too big for my MacBook Pro retina screen,
;             so I've added this keyword to shrink the fonts.
;
;     NO_GOES: This disables the GOES plot.
;
;     FLARE_DATA: the structure returned by iris_hek_swpc_flares() or -1
;
; OUTPUT:
;     None
;
; DEPENDENCIES:
;     spice_browser_font
;     spice_browser_get_metadata
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-
; $Id: 2023-05-15 14:58 CEST $


PRO spice_browser_widget, data, yoffsets=yoffsets, quiet=quiet, $
  chunk_size=chunk_size, retina=retina, no_goes=no_goes, $
  flare_data=flare_data
  COMPILE_OPT IDL2

  ;
  ; Apply scale factors to the plot windows if the screen size is small
  ;
  ss=get_screen_size()
  CASE 1 OF
    ss[0] LT 1024: plot_scale=0.7
    ss[0] GE 1024 AND ss[0] LT 1280: plot_scale=1.0
    ss[0] GE 1280 AND ss[0] LE 1600: plot_scale=1.0
    ss[0] GT 1600 AND ss[0] LT 1900: plot_scale=1.3
    ss[0] GE 1900: plot_scale=1.5
  ENDCASE


  spice_browser_font, font, retina=retina
  spice_browser_font, bigfont, /big, retina=retina
  spice_browser_font, fixfont, /fixed, retina=retina

  ;
  ; Get metadata from the object.
  ;
  meta=spice_browser_get_metadata(data)

  ;
  ; This takes a value of 0 or 1 (1=sit-and-stare).
  ;
  sit_stare = meta.sit_stare

  obs_type = meta.obs_type

  ;
  ; Gets the mid-point time of each exposure in '12:00:00' format. This
  ; is stored in wid_data. Note that the array is reversed if
  ; rast_direct=0.
  ;
  midtime=meta.midtime
  midtime_earth=meta.midtime_earth
  tmid_min=meta.tmid_min
  utc=meta.utc

  xpos=meta.xpos
  ypos=meta.ypos

  nx=n_elements(xpos)
  ny=n_elements(ypos)

  ;
  ; Sit-and-stare "chunking"
  ; ------------------------
  ; Large sit-and-stare data-sets yield very large arrays that slow down
  ; raster_browser. I deal with this by breaking the sequence into
  ; smaller chunks. The different chunks are accessed through a slider
  ; (much like the slider used for accessing different raster files in a
  ; sequence).
  ;
  ; The size of a chunk is determined by nxpos. If chunking is not
  ; performed (or it's a raster sequence) then nxpos=nx.
  ;
  ; nxpos is computed to try and make the chunks fairly equal in
  ; length; the maximum possible size is nxpos=700.
  ;
  ; Xrange is now set to give the X-index range in the chunk, not the
  ; complete data-set. To convert Xrange to the index range within the
  ; entire data-set, do   newXrange=Xrange+ichunk*nxpos
  ;
  nxpos=nx
  ixpos=0
  jxpos=nx-1
  IF sit_stare EQ 1 THEN BEGIN
    IF n_elements(chunk_size) NE 0 THEN nxpos=fix(chunk_size) ELSE nxpos=700
    n=nx/nxpos
    IF (nx - n*nxpos) GT 0 THEN BEGIN
      nxpos=ceil(float(nx)/float(n+1))
    ENDIF
    ;
    ;
    IF data->get_number_exposures(0) GT nxpos*1.5 THEN BEGIN
      IF NOT keyword_set(quiet) THEN print,'% SPICE_RASTER_BROWSER: sit-and-stare chunking is switched on (chunk_size='+trim(nxpos)+').'
      ixpos=0
      jxpos=nxpos-1
    ENDIF ELSE BEGIN
      nxpos=nx
    ENDELSE
  ENDIF

  ;
  ; This is the number of chunks that sit-and-stare sequences are
  ; divided into
  ;
  nchunk=ceil(float(nx)/float(nxpos))

  xrange=[0,nxpos-1]


  ;
  ; Get raster direction by using the getdx value.
  ;   rast_direct=1  -> left-to-right (east-to-west)
  ;   rast_direct=0  -> right-to-left (west-to-east)
  ;
  rast_direct=meta.rast_direct


  scale=meta.scale
  origin=meta.origin

  ;
  ; yoffset is a holdover from EIS. I'm leaving it just in case.
  ;
  yoffset=0
  origin[1]=origin[1]-yoffset

  ;
  ; By default I pick up the line ID file from my webpage, but if
  ; this isn't found, then I use the file in SSW.
  ;
  line_ids=1
  sock_list,'http://files.pyoung.org/iris/iris_chianti_lookup_table.txt',page
  np=n_elements(page)
  IF np GT 10 THEN BEGIN
    ion=''
    str={wvl: 0., ion: ''}
    FOR i=0,np-1 DO BEGIN
      reads,page[i],format='(f10.0,a10)',wvl,ion
      str.wvl=wvl
      str.ion=trim(ion)
      IF n_tags(idstr) EQ 0 THEN idstr=str ELSE idstr=[idstr,str]
    ENDFOR
  ENDIF ELSE BEGIN
    chck=file_search('$SSW/iris/idl/nrl/iris_chianti_lookup_table.txt')
    IF chck[0] EQ '' THEN BEGIN
      chck=file_search('$SSW/iris/idl/uio/ancillary/iris_chianti_lookup_table.txt')
      file=chck[0]
    ENDIF ELSE BEGIN
      file=chck[0]
    ENDELSE
    ;
    IF file NE '' THEN BEGIN
      openr,lin,file,/get_lun
      ion=''
      str={wvl: 0., ion: ''}
      WHILE eof(lin) NE 1 DO BEGIN
        readf,lin,format='(f10.0,a10)',wvl,ion
        str.wvl=wvl
        str.ion=ion
        IF n_tags(idstr) EQ 0 THEN idstr=str ELSE idstr=[idstr,str]
      ENDWHILE
      free_lun,lin
    ENDIF ELSE BEGIN
      line_ids=0
      idstr=0
    ENDELSE
  ENDELSE

  ;
  ; Get slitjaw information
  ;
  ;  sji_file=filestr.sji_file
  ;  IF sji_file[0] NE '' THEN BEGIN
  ;    sji=1
  ;    sji_d=iris_sji(sji_file)
  ;    sji_ti=sji_d->gettime()
  ;    sji_nexp=sji_d->getnexp(0)
  ;    sji_exp=sji_d->getexp(indgen(sji_nexp))
  ;    sji_ti=sji_ti+sji_exp/2.    ; get mid-time of exposure
  ;    sji_id=sji_d->getsji_id()
  ;    sji_start=sji_d->getinfo('DATE_OBS')
  ;    sji_end=sji_d->getinfo('DATE_END')
  ;    duration=anytim2tai(sji_end)-anytim2tai(sji_start)
  ;    sji_cadence=duration/float(sji_nexp)
  ;  ENDIF ELSE BEGIN
  ;  sji=0
  sji_file=''
  ;  sji_id=''
  sji_d=0
  sji_cadence=0.
  ;  ENDELSE


  IF ~data->has_dumbbells() THEN BEGIN
    IF data->get_number_windows() LT 4 THEN n_plot_window=data->get_number_windows() $
    ELSE n_plot_window=4
    sji=0
    sji_id=''
  ENDIF ELSE BEGIN
    IF data->get_number_windows() LT 3 THEN n_plot_window=data->get_number_windows() $
    ELSE n_plot_window=3
    sji=1
    IF data->get_dumbbells_index(/lower) GE 0 THEN sji_id='Lower'
    IF data->get_dumbbells_index(/upper) GE 0 THEN BEGIN
      IF N_ELEMENTS(sji_id) EQ 0 THEN sji_id='Upper' $
      ELSE sji_id = [sji_id, 'Upper']
    ENDIF
  ENDELSE


  wid_data={iwin: intarr(n_plot_window)-1, $     ; index of wavelength window
    im_zoom: 0, $  ; image zoom factor (min=1, max=10)
    spec_zoom: intarr(n_plot_window), $  ; spectrum zoom factor (min=1, max=10)
    im_plot_id: intarr(n_plot_window), $ ; window ID for image
    spec_plot_id: intarr(n_plot_window),  $  ; window ID for spectrum
    despike: 0, $     ; De-spike info
    offset: 0,  $     ; allow for offset between CCDs
    autoint: bytarr(n_plot_window)+1,   $    ; Auto-scale intensity? (1=yes)
    nexp_prp: -1, $
    xpix: 0, $
    ypix: 0,  $
    nx: nx, $
    ny: ny, $
    ccd: bytarr(data->get_number_windows()), $
    lbin: 5, $
    origin: origin, $
    scale: scale, $
    shifts: fltarr(data->get_number_exposures(0)), $
    shift_set: 1, $
    lambda: fltarr(n_plot_window), $
    ilambda: intarr(n_plot_window), $
    goes_plot_id: 0, $
    line_ids: line_ids, $      ; 0 or 1 if line IDs available
    idstr: idstr, $            ; structure of line IDs
    midtime: midtime, $
    midtime_earth: midtime_earth, $
    utc: utc, $
    velocity: 0, $
    linlog: 0, $
    lrange: intarr(2,n_plot_window), $
    xrange: xrange, $
    yrange: [0,ny-1], $
    yoffsets: keyword_set(yoffsets), $
    sit_stare: meta.sit_stare, $
    im_type: 0, $
    xpos: xpos, $
    ixpos: ixpos, $
    jxpos: jxpos, $
    nxpos: nxpos, $
    nchunk: nchunk, $
    ichunk: 0, $
    ypos: ypos, $
    tmid: midtime, $
    tmid_min: tmid_min, $
    rast_direct: rast_direct, $
    roll: data->get_header_keyword('CROTA', 0, 0), $
    l1p5_ver: meta.l1p5_ver, $
    sji: sji, $
    sji_file: sji_file, $     ; array of all SJI filenames
    sji_id: sji_id, $         ; array of IDs (e.g., 'SJI_1400')
    sji_plot_id: [-1,-1], $
    sji_index: -1, $
    sji_frame: 0, $           ; computed by plot_sji_image
    sji_mov_frames: 0, $
    sji_nframe: 0, $
    sji_xrange: [0,0], $
    sji_yrange: [0,0], $
    sji_cadence: sji_cadence, $
    autoint_sji: 1, $
    ;filestr: filestr, $
    coltable: 0, $
    retina: keyword_set(retina), $
    exptime: fltarr(n_plot_window), $
    n_plot_window: n_plot_window, $
    ;hcr: hcr, $
    flare_data: flare_data $
  }


  ;
  ; Work out default IWIN values for each of the four plot windows.
  ; 23-Feb-2014 - I've added a "backup" check in order to pick
  ;               the weaker doublet line if the strong one isn't
  ;               available.
  ;
  ;  rlamb=    [2796.352,1335.708,1393.757,1401.158,1349.403,1334.532]
  ;  rl_backup=[2803.530,1334.532,1402.770,-1,-1,-1]
  ;  nwin=data->get_number_windows()
  ;  count=0
  ; This look up table doesn't work, because spice uses very differen wavelengths
  ; So we just use the sfirst 3 or 4 windows
  FOR i_plot_window=0,n_plot_window-1 DO BEGIN
    wid_data.iwin[i_plot_window] = i_plot_window
    wid_data.lambda[i_plot_window] = data->get_header_keyword('CRVAL3', i_plot_window)
    lam = data->get_lambda_vector(i_plot_window)
    getmin = min(abs(lam), imin)
    wid_data.ilambda[i_plot_window] = imin
    nl = n_elements(lam)
    wid_data.lrange[*,i_plot_window] = [0,nl-1]
  ENDFOR
  ;  FOR j=0,n_elements(rlamb)-1 DO BEGIN
  ;    swtch=0
  ;    stop
  ;    FOR i=0,nwin-1 DO BEGIN
  ;      lam=data->get_lambda_vector(i)
  ;      print,lam
  ;      IF rlamb[j] GE min(lam) AND rlamb[j] LE max(lam) THEN BEGIN
  ;        wid_data.iwin[count]=i
  ;        wid_data.lambda[count]=rlamb[j]
  ;        getmin=min(abs(rlamb[j]-lam),imin)
  ;        wid_data.ilambda[count]=imin
  ;        nl=n_elements(lam)
  ;        wid_data.lrange[*,count]=[0,nl-1]
  ;        count=count+1
  ;        swtch=1
  ;        break
  ;      ENDIF
  ;    ENDFOR
  ;    ;
  ;    IF swtch EQ 0 THEN BEGIN
  ;      IF rl_backup[j] NE -1 THEN BEGIN
  ;        FOR i=0,nwin-1 DO BEGIN
  ;          lam=data->get_lambda_vector(i)
  ;          print,lam
  ;          IF rl_backup[j] GE min(lam) AND rl_backup[j] LE max(lam) THEN BEGIN
  ;            wid_data.iwin[count]=i
  ;            wid_data.lambda[count]=rl_backup[j]
  ;            getmin=min(abs(rl_backup[j]-lam),imin)
  ;            wid_data.ilambda[count]=imin
  ;            nl=n_elements(lam)
  ;            wid_data.lrange[*,count]=[0,nl-1]
  ;            count=count+1
  ;            swtch=1
  ;            break
  ;          ENDIF
  ;        ENDFOR
  ;      ENDIF
  ;    ENDIF
  ;    ;
  ;    IF count EQ n_plot_window THEN break
  ;  ENDFOR

  k=where(wid_data.iwin EQ -1,nk)
  IF nk GT 0 THEN BEGIN
    FOR i=0,nk-1 DO BEGIN
      wid_data.iwin[k[i]]=0
      lam=data->get_lambda_vector(i)
      wid_data.lambda[k[i]]=mean(lam)
    ENDFOR
  ENDIF

  extra_title=''
  IF n_tags(hcr) NE 0 THEN BEGIN
    IF trim(hcr.obstitle) NE '' THEN BEGIN
      extra_title=' -- '+trim(hcr.obstitle)
    ENDIF
  ENDIF
  spice_browser_base=widget_base(/row,map=1,title='SPICE_RASTER_BROWSER'+extra_title)

  subbase1=widget_base(spice_browser_base,/col,map=1)
  ;; exit=cw_bgroup(subbase1,/row,['EXIT','HELP'], $
  ;;                font=bigfont)

  spice_browser_coltable,desc=desc
  desc=['0\EXIT','0\HELP',desc]
  exit=cw_pdmenu(subbase1,desc,font=bigfont)


  tab=widget_tab(subbase1)

  opt_base=widget_base(tab,title='Options',/column)
  meta_base=widget_base(tab,title='Metadata',/column)

  ;
  ; PIXEL NUMBER & EXPOSURE TIME
  ; ----------------------------
  textbase=widget_base(opt_base,/col,frame=1)
  pixtext=widget_label(textbase,value='Selected image pixel',font=font)
  xt='X-pixel: '+trim(0)
  xtext=widget_label(textbase,value=xt,font=font,xsiz=100)
  yt='Y-pixel: '+trim(0)
  ytext=widget_label(textbase,value=yt,font=font,xsiz=100)
  tt='S/C Time: '
  ttext=widget_label(textbase,value=tt,font=font,xsiz=150)
  ett='Earth Time: '
  ettext=widget_label(textbase,value=ett,font=font,xsiz=150)


  ;
  ; The following adds a slider if multiple files have been input.
  ;
  ;  IF filestr.nfiles GT 1 THEN BEGIN
  ;    nf=filestr.nfiles
  ;    file_base=widget_base(opt_base,/row,frame=1)
  ;    title='File no. (0-'+trim(nf-1)+')'
  ;    file_slider=widget_slider(file_base,min=0,max=nf-1,font=font,title=title)
  ;    file_butt1=widget_button(file_base,value='-',font=bigfont)
  ;    file_butt2=widget_button(file_base,value='+',font=bigfont)
  ;  ENDIF ELSE BEGIN
  file_slider=0
  file_butt1=0
  file_butt2=0
  ;  ENDELSE


  ;
  ; SLIDER FOR SIT-AND-STARE CHUNK DATA
  ; -----------------------------------
  IF nx NE nxpos THEN BEGIN
    chunk_base=widget_base(opt_base,/row,frame=1)
    title='Sit-stare seq. (0-'+trim(nchunk-1)+')'
    chunk_slider=widget_slider(chunk_base,min=0,max=nchunk-1,font=font,title=title)
    chunk_butt1=widget_button(chunk_base,value='-',font=bigfont)
    chunk_butt2=widget_button(chunk_base,value='+',font=bigfont)
  ENDIF ELSE BEGIN
    chunk_slider=0
    chunk_butt1=0
    chunk_butt2=0
  ENDELSE


  ;
  ; NUMBER OF PIXELS FOR WAVELENGTH SUMMATION
  ; -----------------------------------------
  wpix_base=widget_base(opt_base,/col,frame=1)
  wpix_text=widget_label(wpix_base,val='Wavelength pixels to sum:', $
    font=font,/align_left)
  wpix=[1,5,9,15]
  i=where(wid_data.lbin EQ wpix)
  wpix_sum=cw_bgroup(wpix_base,/exclusive,trim(wpix), $
    set_value=i,/row,font=font)

  ;
  ; SWITCH LINE IDS ON OR OFF
  ; -------------------------
  IF n_tags(wid_data.idstr) NE 0 THEN BEGIN
    lids_base=widget_base(opt_base,/col,frame=1)
    lids_text=widget_label(lids_base,val='Show line IDS?', $
      font=font,/align_left)
    lids_butts=cw_bgroup(lids_base,['No','Yes'], $
      set_value=wid_data.line_ids,/exclusive,font=font,/row)
  ENDIF ELSE BEGIN
    lids_butts=0
  ENDELSE


  ;
  ; WAVELENGTH OR VELOCITY?
  ; -----------------------
  vel_base=widget_base(opt_base,/col,frame=1)
  vel_text=widget_label(vel_base,val='Spectrum axis:', $
    font=font,/align_left)
  vel_butts=cw_bgroup(vel_base,['Wavelength','Velocity'], $
    set_value=wid_data.velocity,/exclusive,font=font,/row)


  ;
  ; INTENSITY SCALING: LOG OR LINEAR?
  ; ---------------------------------
  log_base=widget_base(opt_base,/col,frame=1)
  log_text=widget_label(log_base,val='Image scaling:', $
    font=font,/align_left)
  log_butts=cw_bgroup(log_base,['Linear','Log'], $
    set_value=wid_data.linlog,/exclusive,font=font,/row)

  ;
  ; Images: X-Y, or lambda-Y
  ; ---------------------------------
  im_type_base=widget_base(opt_base,/col,frame=1)
  im_type_text=widget_label(im_type_base,val='Image type:', $
    font=font,/align_left)
  IF sit_stare EQ 1 THEN options=['time-Y','lambda-Y'] ELSE options=['X-Y','lambda-Y']
  temp = where(data->get_number_exposures() EQ 1, counttemp)
  IF counttemp GT 0 THEN wid_data.im_type=1
  im_type_butts=cw_bgroup(im_type_base,options, $
    set_value=wid_data.im_type,/exclusive,font=font,/row)
  IF counttemp GT 0 THEN widget_control, im_type_butts, sensitive=0

  exp_base=widget_base(im_type_base,/row,frame=1,sens=0)
  title='Exp no. (0-'+trim(nxpos-1)+')'
  if nxpos gt 1 then begin
    exp_slider=widget_slider(exp_base,min=0,max=nxpos-1,font=font,title=title)
    exp_butt1=widget_button(exp_base,value='-',font=bigfont)
    exp_butt2=widget_button(exp_base,value='+',font=bigfont)
  endif else begin
    exp_slider=0
    exp_butt1=0
    exp_butt2=0
  endelse

  ;
  ; MASKING option, mask regions outside of slit?
  ; ---------------------------------
  mask_base=widget_base(opt_base,/col,frame=1)
  mask_butt=cw_bgroup(mask_base,['Mask regions outside slit'], $
    set_value=[1],/nonexclusive,font=font,/row)

  ;
  ; NEXP_PRP > 1 BUTTONS
  ; --------------------
  ;  IF nexp_prp GT 1 THEN BEGIN
  ;    nexp_prp_base=widget_base(opt_base,/col,frame=1)
  ;    wid_data.nexp_prp=0
  ;    text=trim(indgen(nexp_prp))
  ;    exp=data->getexp()
  ;    exp=exp[0:nexp_prp-1]
  ;    expstr=trim(string(format='(f12.1)',exp))+' s'
  ;    nexp_prp_text=widget_label(nexp_prp_base,val='Choose exposure', $
  ;      font=font,/align_left)
  ;    nexp_prp_butts=cw_bgroup(nexp_prp_base,expstr,/exclusive, $
  ;      set_value=wid_data.nexp_prp,/col,font=font)
  ;
  ;  ENDIF ELSE BEGIN
  nexp_prp_butts=0
  ;  ENDELSE

  ;
  ; The following contains meta-data that goes on the Metadata tab.
  ;
  stud_acr=data->get_header_keyword('OBS_ID', 0, '')
  ;  IF n_tags(filestr) NE 0 THEN BEGIN
  ;    date_obs=filestr.t0
  ;    date_end=filestr.t1
  ;  ENDIF ELSE BEGIN
  date_obs=data->get_header_keyword('DATE-BEG', 0, '')
  date_end=data->get_header_keyword('DATE-END', 0, '')
  ;  ENDELSE
  ;
  text1=widget_label(meta_base,val='OBSID: '+stud_acr,font=font, $
    /align_left)
  text1a=widget_label(meta_base,val='TYPE: '+obs_type,font=font, $
    /align_left)
  if date_obs ne '' then begin
    ex=anytim(/ex,date_obs)
    date=trim(ex[4])+'-'+trim(get_month(ex[5]-1,/trunc))+'-'+trim(ex[6])
    time=strpad(trim(ex[0]),2,fill='0')+':'+strpad(trim(ex[1]),2,fill='0')
  endif else begin
    date='N/A'
    time='N/A'
  endelse
  text2=widget_label(meta_base,val='DATE: '+date,font=font, $
    /align_left)
  text3=widget_label(meta_base,val='START TIME: '+time,font=font, $
    /align_left)
  ;
  value='XCEN: '+trim(string(format='(f10.1)',meta.xcen))
  text6=widget_label(meta_base,val=value,font=font, $
    /align_left)
  ;
  value='YCEN: '+trim(string(format='(f10.1)',meta.ycen))
  text7=widget_label(meta_base,val=value,font=font, $
    /align_left)
  ;  IF nexp_prp EQ 1 THEN BEGIN
  cadence=meta.cadence
  cadstr=trim(string(format='(f10.1)',cadence))+' s'
  text4=widget_label(meta_base,val='CADENCE: '+cadstr,font=font, $
    /align_left)
  ;  ENDIF
  nuvbin=data->get_header_keyword('NBIN3', 0)
  fuvbin=data->get_header_keyword('NBIN3', 0)
  spatbin=data->get_header_keyword('NBIN2', 0)
  text5a=widget_label(meta_base,val='FUV SPEC BIN: '+trim(fuvbin),font=font, $
    /align_left)
  text5b=widget_label(meta_base,val='NUV SPEC BIN: '+trim(nuvbin),font=font, $
    /align_left)
  text5c=widget_label(meta_base,val='SPATIAL BIN: '+trim(nuvbin),font=font, $
    /align_left)
  ;
  rollstr=trim(string(format='(f10.1)',wid_data.roll))
  text5=widget_label(meta_base,val='ROLL: '+rollstr,font=font, $
    /align_left)
  ;
  text5=widget_label(meta_base,val='L1.5 version: '+wid_data.l1p5_ver,font=font, $
    /align_left)
  IF n_tags(hcr) NE 0 THEN BEGIN
    IF trim(hcr.planners) NE '' THEN BEGIN
      text8=widget_label(meta_base,val='PLANNER: '+trim(hcr.planners),font=font, $
        /align_left)
    ENDIF
    IF trim(hcr.target) NE '' THEN BEGIN
      text9=widget_label(meta_base,val='TARGET: '+trim(hcr.target),font=font, $
        /align_left)
    ENDIF
    IF trim(hcr.noaanum) NE '' THEN BEGIN
      text10=widget_label(meta_base,val='NOAA NUM: '+trim(hcr.noaanum),font=font, $
        /align_left)
    ENDIF
  ENDIF
  ;
  ; This is a button that pops up a widget showing what EIS was doing at
  ; the time of the observation.
  ;
  IF have_proc('eis_obs_structure') THEN eis_butt=widget_button(meta_base,value='SHOW Hinode/EIS observations',font=font) ELSE eis_butt=''


  goes_plot=widget_draw(subbase1,xsiz=150*plot_scale,ysiz=120*plot_scale)
  IF n_tags(flare_data) EQ 0 THEN BEGIN
    goes_butt=widget_label(subbase1,value='No SWPC flares for this period',font=font)
  ENDIF ELSE BEGIN
    goes_butt=widget_button(subbase1,value='SWPC flare list',font=font)
  ENDELSE


  xsiz=fix(260*plot_scale)
  ysiz=fix(250*plot_scale)

  choices=trim(data->get_window_id())
  choices=['Choose a wavelength window',choices]
  nc=n_elements(choices)

  plot_base=lonarr(n_plot_window)
  int_butt_base=lonarr(n_plot_window)
  min_lbl=lonarr(n_plot_window)
  min_text=lonarr(n_plot_window)
  max_lbl=lonarr(n_plot_window)
  max_text=lonarr(n_plot_window)
  auto_int=lonarr(n_plot_window)
  ;pd_window_base=lonarr(n_plot_window)
  ;pd_window=lonarr(n_plot_window)
  cw_pd_window=lonarr(n_plot_window)
  im_plot=lonarr(n_plot_window)
  spec_plot=lonarr(n_plot_window)
  window_lbl=lonarr(n_plot_window)
  ;whisk_butt=lonarr(n_plot_window)
  ;
  FOR i=0,n_plot_window-1 DO BEGIN
    plot_base[i]=widget_base(spice_browser_base,/col)
    ;
    int_butt_base[i]=widget_base(plot_base[i],/row)
    min_lbl[i]=widget_label(int_butt_base[i],value='Min:',font=font)
    min_text[i]=widget_text(int_butt_base[i],value=trim(0.), $
      font=font,xsiz=7,/editable)
    max_lbl[i]=widget_label(int_butt_base[i],value='Max:',font=font)
    max_text[i]=widget_text(int_butt_base[i],value=trim(0.), $
      font=font,xsiz=7,/editable)
    auto_int[i]=widget_button(int_butt_base[i],value='Auto',font=font)
    ;
    lbl_text='Current window: '+trim(data->get_window_id(wid_data.iwin[i]))
    window_lbl[i]=widget_label(plot_base[i], $
      value=lbl_text,/align_left,font=font, $
      /frame)
    ;
    choices=spice_browser_wvl_list(data,wid_data)
    cw_pd_window[i]=cw_pdmenu(plot_base[i],choices,font=font)
    ;
    ;  pd_window_base[i]=widget_base(/row,plot_base[i])
    ;  pd_window[i]=widget_droplist(pd_window_base[i],value=choices,font=font)
    ;
    ;  IF wid_data.sit_stare EQ 1 THEN BEGIN
    ;    whisk_butt[i]=widget_button(pd_window_base[i],value='Whisker (t-Y) plot',font=font)
    ;  ENDIF
    ;
    im_plot[i]=widget_draw(plot_base[i],xsiz=xsiz,ysiz=ysiz,/sens, $
      /button_events)
    ;
    spec_plot[i]=widget_draw(plot_base[i],xsiz=xsiz,ysiz=ysiz,/sens, $
      /button_events)
  ENDFOR


  ;
  ; The following sets up the widgets for displaying the SJI images
  ;
  sji_plot=lonarr(2)
  IF wid_data.sji EQ 1 THEN BEGIN
    plot_base_sji=widget_base(spice_browser_base,/col)
    ;
    int_butt_base_sji=widget_base(plot_base_sji,/row)
    min_lbl_sji=widget_label(int_butt_base_sji,value='Min:',font=font)
    min_text_sji=widget_text(int_butt_base_sji,value=trim(0.), $
      font=font,xsiz=7,/editable)
    max_lbl_sji=widget_label(int_butt_base_sji,value='Max:',font=font)
    max_text_sji=widget_text(int_butt_base_sji,value=trim(0.), $
      font=font,xsiz=7,/editable)
    auto_int_sji=widget_button(int_butt_base_sji,value='Auto',font=font)
    ;
    wid_data.sji_index=0
    lbl_text='Current window: '+trim(sji_id[wid_data.sji_index])
    sji_window_lbl=widget_label(plot_base_sji, $
      value=lbl_text,/align_left,font=font, $
      /frame)
    sji_pd_window=widget_droplist(plot_base_sji,value=sji_id,font=font)
    ;
    sji_plot[0]=widget_draw(plot_base_sji,xsiz=xsiz,ysiz=ysiz)
    sji_plot[1]=widget_draw(plot_base_sji,xsiz=xsiz,ysiz=ysiz)
    ;
    ;    sji_movie_butt=widget_button(plot_base_sji,value='SHOW MOVIE',font=bigfont)
    ;    ;
    ;    sji_frames_base=widget_base(plot_base_sji,/row)
    ;
    ;    ;
    ;    ; These are the options for the number of frames to display. Note
    ;    ; that the max no. of frames is appended (although minus 1).
    ;    ;
    ;    value=[11,21,31,51,101,201,301,501,1001,2001,3001,5001]
    ;    k=where(value LT sji_nexp)
    ;    value=[value[k],sji_nexp]
    ;    value_string=string(value)
    ;    ;
    ;    ; Choose default value of no. of frames by choosing closest to 10mins
    ;    ;
    ;    getmin=min(abs(float(value)*sji_cadence-600.),imin)
    ;    set_droplist_select=imin
    ;    mov_duration=value[imin]*sji_cadence/60.   ; minutes
    ;    ;
    ;    ; Create the drop-list for no. of frames.
    ;    ;
    ;    sji_frames_lbl=widget_label(sji_frames_base,value='No. of frames:',font=font)
    ;    sji_frames_droplist=widget_droplist(sji_frames_base,value=value_string,font=font)
    ;    wid_data.sji_mov_frames=fix(value_string[set_droplist_select])
    ;    ;
    ;    ; Create label showing movie duration
    ;    ;
    ;    sji_dur_str=trim(string(mov_duration,format='(f7.1)'))
    ;    dur_t='Movie duration: '+sji_dur_str+' mins (approx)'
    ;    sji_dur_text=widget_label(plot_base_sji,value=dur_t,font=font,/align_left,xsiz=250)
    ;    ;
    ;    sji_xrange_text=widget_label(plot_base_sji,value='X-range (pixels):', $
    ;      font=font,/align_left,xsiz=250)
    ;    sji_yrange_text=widget_label(plot_base_sji,value='Y-range (pixels):', $
    ;      font=font,/align_left,xsiz=250)
  ENDIF ELSE BEGIN
    min_text_sji=0
    max_text_sji=0
    auto_int_sji=0
    sji_window_lbl=0
    sji_pd_window=0
    sji_plot=[-1,-1]
    ;    sji_movie_butt=0
    ;    sji_frames_droplist=0
    ;    sji_dur_text=0
    ;    sji_xrange_text=0
    ;    sji_yrange_text=0
  ENDELSE


  ;
  ; Work out the time range for the GOES plot. For short rasters force
  ; the time range to be 10 mins.
  ;
  ; Note that in the sample data that I'm working with DATE_END
  ; is empty, hence the if statement below.
  ;
  date_obs_tai=anytim2tai(date_obs)
  IF trim(date_end) EQ '' THEN BEGIN
    date_end_tai=date_obs_tai+600.
    date_end=anytim2utc(/ccsds,date_end_tai)
  ENDIF ELSE BEGIN
    date_end_tai=anytim2tai(date_end)
  ENDELSE
  tai_diff=date_end_tai-date_obs_tai
  ;
  IF tai_diff/60. LT 10 THEN BEGIN
    g_start_tai=date_obs_tai-(600-tai_diff)/2.
    g_end_tai=date_end_tai+(600-tai_diff)/2.
    g_start=anytim2utc(/ccsds,g_start_tai)
    g_end=anytim2utc(/ccsds,g_end_tai)
  ENDIF ELSE BEGIN
    g_start=date_obs
    g_end=date_end
  ENDELSE

  ;
  ; Get GOES data
  ;
  net_chck=have_network()
  IF net_chck EQ 1 AND NOT keyword_set(no_goes) THEN BEGIN
    g=ogoes()
    g->set,tstart=g_start,tend=g_end,/sdac,mode=1
  ENDIF ELSE BEGIN
    g=0
  ENDELSE


  ;
  ; 'images' stores the images for each of the four display windows.
  ; I add an additional 30 pixels in Y in order to handle Y-offsets
  ; between the FUV1, FUV2 and NUV channels.
  ;
  images=fltarr(wid_data.nxpos,wid_data.ny+1100,n_plot_window)


  ;
  ; 'spectra' contains the 1D spectrum for each window. Note that I pad
  ; each spectrum to be the maximum size of all of the spectrum windows
  ;
  nl = intarr(data->get_number_windows())
  FOR i=0,data->get_number_windows()-1 DO BEGIN
    nl[i] = data->get_header_keyword('NAXIS3', i)
  END
  spectra=fltarr(max(nl),n_plot_window)

  ;
  ; I use this for storing exposure images
  ;
  expimages=fltarr(max(nl),wid_data.ny+1100,n_plot_window)

  ;
  ; This loads the array of SJI images.
  ;
  ;; IF sji_file NE '' THEN BEGIN
  ;;   widget_control,/hourglass
  ;;   sji_array=sji_d->getvar()
  ;; ENDIF


  ;
  ; Make an associated file for the SJI images
  ;
  ;; IF wid_data.sji EQ 1 THEN BEGIN
  ;;   t1=systime(1)
  ;;   assoc_file = IRISxfiles_appReadme()+'/iris_sji_file.tmp'
  ;;   chck=file_search(assoc_file)
  ;;   IF chck[0] NE '' THEN file_delete,assoc_file
  ;;   widget_control,/hourglass
  ;;   sji_array=sji_d->getvar()
  ;;  ;
  ;;  ; The following writes the image array to the associated file
  ;;   openw,lu,assoc_file,/get_lun
  ;;   rec=assoc(lu,sji_array)
  ;;   rec[0]=sji_array
  ;;   close,lu
  ;;   free_lun,lu
  ;;  ;
  ;;   wid_data.sji_assoc=assoc_file
  ;;   sji_array=0
  ;;   t2=systime(1)
  ;;   print,'SJI time: ',t2-t1
  ;; ENDIF



  state={data: data, $
    sji_d:  sji_d, $   ; --> this is object for SJI
    ;       sji_array: sji_array, $
    goes: g, $
    exit: exit, $
    wid_data: wid_data, $
    spice_browser_base: spice_browser_base, $
    xtext: xtext, $
    ytext: ytext, $
    ttext: ttext, $
    ettext: ettext, $
    nexp_prp_butts: nexp_prp_butts, $
    wpix_sum: wpix_sum, $
    im_plot: im_plot, $
    spec_plot: spec_plot, $
    min_text: min_text, $
    max_text: max_text, $
    ;       pd_window: pd_window, $
    window_lbl: window_lbl, $
    lids_butts: lids_butts, $
    auto_int: auto_int, $
    expimages: expimages, $
    images: images, $
    vel_butts: vel_butts, $
    log_butts: log_butts, $
    im_type_butts: im_type_butts, $
    file_slider: file_slider, $
    file_butt1: file_butt1, $
    file_butt2: file_butt2, $
    chunk_slider: chunk_slider, $
    chunk_butt1: chunk_butt1, $
    chunk_butt2: chunk_butt2, $
    exp_base: exp_base, $
    exp_slider: exp_slider, $
    exp_butt1: exp_butt1, $
    exp_butt2: exp_butt2, $
    mask_butt:mask_butt, $
    text3: text3, $
    min_text_sji: min_text_sji, $
    max_text_sji: max_text_sji, $
    auto_int_sji: auto_int_sji, $
    sji_window_lbl: sji_window_lbl, $
    sji_pd_window: sji_pd_window, $
    sji_plot: sji_plot, $
    ;    sji_movie_butt: sji_movie_butt, $
    ;    sji_frames_droplist: sji_frames_droplist, $
    ;    sji_dur_text: sji_dur_text, $
    ;    sji_xrange_text: sji_xrange_text, $
    ;    sji_yrange_text: sji_yrange_text, $
    spectra: spectra, $
    cw_pd_window: cw_pd_window, $
    ;       whisk_butt: whisk_butt, $
    eis_butt: eis_butt, $
    goes_butt: goes_butt}

  wp = widget_positioner(spice_browser_base, parent=group_leader)
  wp->position
  WIDGET_CONTROL, spice_browser_base, set_uvalue=state


  ;
  ; The following sets the initial xpix and ypix values to be the center
  ; of the raster image.
  ;
  xsiz=n_elements(xpos)
  ysiz=n_elements(ypos)
  ;
  ; Modified xpix to deal with sit-and-stare 'chunk' data. PRY, 20-Feb-2015
  ;
  xpix=ixpos+nxpos/2
  ypix=fix((ysiz)/2)
  state.wid_data.xpix=xpix
  state.wid_data.ypix=ypix

  xt='X-pixel: '+trim(state.wid_data.xpix)
  widget_control,state.xtext,set_value=xt
  ;
  yt='Y-pixel: '+trim(state.wid_data.ypix)
  widget_control,state.ytext,set_value=yt
  ;

  if nxpos gt 1 then widget_control,state.exp_slider,set_value=state.wid_data.xpix

  ;
  ; Get window IDs for graphic windows
  ;
  FOR i=0,n_plot_window-1 DO BEGIN
    WIDGET_CONTROL, im_plot[i], GET_VALUE=val
    state.wid_data.im_plot_id[i]=val
    ;
    WIDGET_CONTROL, spec_plot[i], GET_VALUE=val
    state.wid_data.spec_plot_id[i]=val
  ENDFOR
  ;
  IF wid_data.sji EQ 1 THEN BEGIN
    widget_control,sji_plot[0],get_value=val
    state.wid_data.sji_plot_id[0]=val
    widget_control,sji_plot[1],get_value=val
    state.wid_data.sji_plot_id[1]=val
  ENDIF
  ;
  widget_control,goes_plot,get_value=val
  state.wid_data.goes_plot_id=val
  ;
  widget_control,spice_browser_base,set_uvalue=state

  ;
  ; Set initial value for SJI movie frames
  ;
  ;  IF wid_data.sji EQ 1 THEN widget_control,sji_frames_droplist,set_droplist_select=set_droplist_select

  ;
  ; Make initial plots
  ;
  widget_control,/hourglass
  FOR i=0,n_plot_window-1 DO BEGIN
    spice_browser_update_image,state,i
    spice_browser_update_spectrum,state,i
    spice_browser_plot_image, state, i
    spice_browser_plot_spectrum, state, i
  ENDFOR
  ;
  spice_browser_goes_plot, state

  IF wid_data.sji EQ 1 THEN BEGIN
    spice_browser_plot_sji, state
  ENDIF

  tt='S/C Time: '+trim(midtime[state.wid_data.xpix-1])
  widget_control,state.ttext,set_value=tt
  ett='Earth Time: '+trim(midtime_earth[state.wid_data.xpix-1])
  widget_control,state.ettext,set_value=ett


  XMANAGER, 'spice_browser_base', spice_browser_base, group=group

  IF datatype(g) EQ 'OBJ' THEN obj_destroy,g
  IF sji_file[0] NE '' THEN obj_destroy,sji_d

END
