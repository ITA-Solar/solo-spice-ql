;+
; NAME:
;     spice_browser_plot_sji
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_plot_sji, state
;
; INPUTS:
;     input:  XXX
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     None.
;
; OUTPUT:
;     XXX
;
; EXAMPLE:
;
; INTERNAL ROUTINES:
;
; PROGRAMMING NOTES:
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-
; $Id: 2022-06-02 11:47 CEST $


PRO spice_browser_plot_sji, state
  ;
  ; This routine plots the SJI image.
  ;
  FOR i_dumbbell=0,1 DO BEGIN
    ;
    ; Set the graphics window.
    ;
    wset,state.wid_data.sji_plot_id[i_dumbbell]

    ;
    ; This is for the case of chunked sit-and-stare data, where xpix can
    ; potentially be larger than nx. (Not a problem for other types of
    ; data.)
    ;
    IF state.wid_data.xpix GE state.wid_data.nx THEN BEGIN
      plot,[0,1],[0,1],/nodata, $
        xticklen=1e-6,yticklen=1e-6,charsiz=1e-6
      xyouts,/normal,0.5,0.5,align=0.5, $
        'No data',charsiz=2
      return
    ENDIF

    sji_file=state.wid_data.sji_file
    xpix=state.wid_data.xpix
    utc=state.wid_data.utc[xpix]


    ;
    ; Get the SJI file index from the drop-down list
    ;
    ;sji_ind=state.wid_data.sji_index


    ;  ;
    ;  ; Load the SJI data object.
    ;  ;
    ;  d=iris_sji(sji_file[sji_ind])
    ;
    ;  ;
    ;  ; Get time information from the SJI object. Note that sji_tai is the
    ;  ; time of the mid-point of the exposure.
    ;  ;
    ;  sji_ti=d->get_time_vector(0)
    ;  sji_nexp=d->get_number_exposures(0)
    ;  sji_exp=d->get_exposure_time(indgen(sji_nexp))
    ;  sji_ti=sji_ti+sji_exp/2.        ; get mid-time of exposure
    ;  sji_utc=d->ti2utc(sji_ti)
    ;  sji_tai=anytim2tai(sji_utc)
    ;  sji_start=d->getinfo('DATE_OBS')
    ;  sji_end=d->getinfo('DATE_END')
    ;
    ;  obj_destroy,d
    ;
    ;  ;
    ;  ; Update the SJI cadence stored in wid_data.sji_cadence
    ;  ;
    ;  t0_tai=anytim2tai(sji_start)
    ;  t1_tai=anytim2tai(sji_end)
    ;  state.wid_data.sji_cadence=(t1_tai-t0_tai)/float(sji_nexp)
    ;
    ;  ;
    ;  ; Update the SJI movie duration widget
    ;  ;
    ;  sji_dur=state.wid_data.sji_cadence*state.wid_data.sji_mov_frames/60.
    ;  sji_dur_str=trim(string(sji_dur,format='(f7.1)'))
    ;  dur_t='Movie duration: '+sji_dur_str+' mins (approx)'
    ;  widget_control,state.sji_dur_text,set_value=dur_t



    ;
    ; Find the image index that is closest to the raster exposure.
    ;
    ;  utc_tai=anytim2tai(utc)   ; TAI for the raster exposure
    ;  getmin=min(abs(utc_tai-sji_tai),imin)

    ;
    ; sji_frame is the index of the image frame that will be displayed.
    ; sji_nframe is the total no. of frames
    ;
    ;  state.wid_data.sji_frame=imin
    ;  state.wid_data.sji_nframe=sji_nexp

    ;
    ; The following reads the SJI file. The input imagen requires an
    ; array so I have to give two indices.
    ;
    ;  IF imin EQ 0 THEN BEGIN
    ;    read_iris_l2,sji_file[sji_ind],index,data,imagen=[imin,imin+1], $
    ;      /keep_null,/silent
    ;    image=reform(data[*,*,0])
    ;    index=index[0]
    ;  ENDIF ELSE BEGIN
    ;    read_iris_l2,sji_file[sji_ind],index,data,imagen=[imin-1,imin], $
    ;      /keep_null,/silent
    ;    image=reform(data[*,*,1])
    ;    index=index[1]
    ;  ENDELSE
    IF i_dumbbell EQ 0 THEN BEGIN
      window_index = state.data->get_dumbbells_index(/upper)
      title = 'Upper Dumbbell'
    ENDIF ELSE BEGIN
      window_index = state.data->get_dumbbells_index(/lower)
      title = 'Lower Dumbbell'
    ENDELSE
    IF window_index LT 0 THEN continue
    ;data = state.data->get_window_data(window_index)
    ;help, data
    ;stop
    image = state.data->get_one_image(window_index, 0)

    ;
    ; Get SJI image coordinate information.
    ;
    ; When roll is +90 or -90, then xcen and ycen are the wrong way round
    ; and so I need to swap them. However, crpix1, crpix2, fovx and fovy are not
    ; reversed.
    ;
    ; For roll=90, I need to reverse cdelt1.
    ;
    xcen=state.data->get_header_info('CRVAL1', window_index)
    ycen=state.data->get_header_info('CRVAL2', window_index)
    cdelt1=state.data->get_resolution(window_index, /x)
    cdelt2=state.data->get_resolution(window_index, /y)
    crpix1=state.data->get_header_info('CRPIX1', window_index)
    crpix2=state.data->get_header_info('CRPIX2', window_index)
    fovx=cdelt1 * state.data->get_header_info('NAXIS1', window_index)
    fovy=cdelt2 * state.data->get_header_info('NAXIS2', window_index)
    ;
    IF abs(state.wid_data.roll) GE 85 THEN BEGIN
      xc_temp=xcen
      xcen=ycen
      ycen=xc_temp
      ;
      IF state.wid_data.roll GE 85 THEN cdelt1=-cdelt1
      IF state.wid_data.roll LE -85 THEN cdelt2=-cdelt2
    ENDIF

    ;
    ; Get image scale (used for plot_image call); also get aspect_ratio
    ; (y-scale/x-scale) [updated, 13-Sep-2017]
    ;
    scale=[cdelt1,cdelt2]
    aspect_ratio=cdelt2/cdelt1

    ;
    ; The following lines of code set up the field-of-view to be displayed
    ; for the SJI images. The X-range and Y-range of the FOV will be
    ; stored in XR and YR.
    ;
    ; Since the SJI and raster are registered with each other then we can
    ; take the raster yrange and apply it to the SJI images.
    ;
    yr=state.wid_data.yrange
    ;
    ; Get the (X,Y) position of the selected raster pixel.
    ;
    ;xpos=state.data->getxpos()
    ;ypos=state.data->getypos()
    xpos=state.wid_data.xpos
    ypos=state.wid_data.ypos
    xpix=state.wid_data.xpix
    ypix=state.wid_data.ypix
    xp=xpos[xpix]
    yp=ypos[ypix]
    ;
    ; The X-direction is more tricky. First of all I need to do a sanity
    ; check to make sure the raster X-position lies within the SJI
    ; field-of-view (I've found one file for which this isn't the case,
    ; 11:26 on 6-Sep-2014).
    ;
    sji_xr=xcen+[-fovx,fovx]/2.
    IF xp LT sji_xr[0] OR xp GT sji_xr[1] THEN badfile=1 ELSE badfile=0
    ;
    ; If we have a bad file, then print a message to the SJI window, and
    ; exit.
    ;
    IF badfile EQ 1 THEN BEGIN
      plot,[0,1],[0,1],/nodata, $
        xticklen=1e-6,yticklen=1e-6,charsiz=1e-6
      xyouts,/normal,0.5,0.5,align=0.5, $
        'SJI and slit coordinates!cnot consistent',charsiz=2
      widget_control,state.spice_browser_base,set_uvalue=state
      return
    ENDIF
    ;
    ;
    ; Use XP to determine the X-pixel index for the selected pixel within
    ; the SJI image.
    ;  (note: crpix1 is pixel x-position of image center; cdelt1 is size of
    ;         pixel in x-direction.)
    ;
    mid_x=round(crpix1-(xcen-xp)/cdelt1)
    ;
    ; Now derive the X-range (XR).
    ;  13-Sep-2017: I've added aspect_ratio as some images have 1x2 binning.
    ;
    ny=yr[1]-yr[0]+1
    s=size(image,/dim)
    nx=s[0]
    nx=round(min([nx,ny*aspect_ratio]))
    x0=max([mid_x-nx/2,0])
    x1=min([mid_x+nx/2,s[0]-1])
    xr=[x0,x1]


    ;
    ; Create 'origin' for plot_image.
    ;
    ;origin=[ xcen - ( (crpix1-xr[0])*cdelt1), $
    ;  ycen - ( (crpix2-yr[0])*cdelt2) ]
    origin=[ state.data->get_wcs_coord(window_index,[0,0,0,0], /x), $
      ycen - ( (crpix2-yr[0])*cdelt2) ]

    ;
    ; Extract the sub-image to be plotted.
    ;
    ;image=image[xr[0]:xr[1],yr[0]:yr[1]]

    state.wid_data.sji_xrange=xr
    state.wid_data.sji_yrange=yr

    ;
    ; Update the X-range and Y-range pixel labels.
    ;
    ;  sji_xrange_txt='X-range (pixels): '+trim(xr[0])+' to '+trim(xr[1])
    ;  widget_control,state.sji_xrange_text,set_value=sji_xrange_txt
    ;  ;
    ;  sji_yrange_txt='Y-range (pixels): '+trim(yr[0])+' to '+trim(yr[1])
    ;  widget_control,state.sji_yrange_text,set_value=sji_yrange_txt

    ;
    ; This picks out the intensity min and max values for the
    ; display. Note that the intmin value for the lambda-Y plots will be
    ; modified later.
    ;
    IF state.wid_data.autoint_sji EQ 0 THEN BEGIN
      widget_control,state.min_text_sji,get_value=intmin
      widget_control,state.max_text_sji,get_value=intmax
      intmin=float(intmin)
      intmax=float(intmax)
    ENDIF ELSE BEGIN
      k=where(image GE 0,nk)
      IF nk EQ 0 THEN BEGIN
        intmin=0
        intmax=10
      ENDIF ELSE BEGIN
        chck=sigrange(image[k],range=range,fraction=0.99,missing=-200)
        intmin=range[0]
        intmax=range[1]
      ENDELSE
      widget_control,state.min_text_sji,set_value=trim(string(format='(f10.1)',intmin))
      widget_control,state.max_text_sji,set_value=trim(string(format='(f10.1)',intmax))
      widget_control,state.spice_browser_base,set_uvalue=state
    ENDELSE


    ;
    ; Now deal with the case when the logarithm of the intensity is
    ; selected.
    ;
    missing=-200
    k=where(image NE missing)
    IF state.wid_data.linlog EQ 1 THEN BEGIN
      IF state.wid_data.autoint_sji EQ 0 THEN BEGIN
        intmin=alog10(max([intmin,1]))
        intmax=alog10(intmax)
      ENDIF ELSE BEGIN
        intmin=alog10(max([intmin,1]))
        intmax=alog10(max(image))
        widget_control,state.min_text_sji,set_value=trim(string(format='(f10.1)',10.^intmin))
        widget_control,state.max_text_sji,set_value=trim(string(format='(f10.1)',10.^intmax))
        widget_control,state.spice_browser_base,set_uvalue=state
      ENDELSE
      image=alog10(image>1)
    ENDIF

    ;
    ; Set the X and Y titles
    ;
    roll=state.wid_data.roll
    CASE 1 OF
      roll GE -5 AND roll LE 5: BEGIN
        xtitle='X / arcsec'
        ytitle='Y / arcsec'
      END
      roll GE 85 OR roll LE -85: BEGIN
        ytitle='X / arcsec'
        xtitle='Y / arcsec'
      END
      ELSE: BEGIN
        xtitle='X-position / arcsec'
        ytitle='Y-position / arcsec'
      END
    ENDCASE


    ;
    ; Plot the SJI image.
    ;
    texp_string='xx';trim(string(sji_exp[state.wid_data.sji_frame],format='(f7.2)'))
    ;title='title';anytim2utc(/ccsds,/time,/trunc,sji_utc[imin])+' UT (exp: '+texp_string+'s)'
    plot_image,image,origin=origin,scale=scale,min=intmin,max=intmax, $
      title=title, $
      xtitle=xtitle, ytitle=ytitle
    ;
    ; Use a box to indicate the position of the raster pixel
    ;
    plots,xp,yp,psym=6,symsiz=2

  ENDFOR ; i_dumbbell=0,1

  widget_control,state.spice_browser_base,set_uvalue=state

END
