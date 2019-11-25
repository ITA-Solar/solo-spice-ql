;+
; NAME:
;     SPICE_RASTER_BROWSER
;
; PURPOSE:
;     This routine is used to browse 3D IRIS data-cubes. It has been
;     adapted from the EIS_RASTER_BROWSER routine available in the
;     Hinode/EIS Solarsoft distribution.
;
;     Within the image display window, the mouse buttons are used as:
;        LEFT   Zoom in to image (factor 2).
;        MIDDLE Choose a new spatial pixel for spectrum display.
;        RIGHT  Zoom out from image (factor 2).
;
;     Similarly, for the spectrum display windows, the mouse buttons are
;     used as:
;        LEFT   Zoom in to spectrum (factor 2).
;        MIDDLE Choose a new spectrum pixel for image display.
;        RIGHT  Zoom out from spectrum (factor 2).
;
;     If you are using the *Mac trackpad*, then you can still use
;     spice_raster_browser. Go to X11->Preferences...->Input and select
;     the 'Emulate three button mouse' option. The GUI now accepts the
;     following inputs:
;
;        CLICK  Zoom in to image (factor 2).
;        OPTION+CLICK  Select a new spatial pixel
;        COMMAND+CLICK Zoom out from image.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_raster_browser, File
;
; INPUTS:
;     File:  Can be either the name of an IRIS data file, or an IRIS data
;            object. Can also be an array of filenames if the files
;            belong to a raster sequence. In this case an additional
;            widget appears allowing the user to flick between
;            rasters.
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     QUIET:   If set, then do not print messages to the IDL command
;             window.
;
;     YOFFSETS: If set, then the three wavelength channels will be
;               adjusted for spatial offsets in Y. This should only be
;               used if you notice that there are offsets in the
;               images. Most data-sets should be
;               aligned. **There shouldn't be a need to use
;               this keyword any more.**
;
;     NO_SJI: By default spice_raster_browser looks for SJI files that
;             match FILE and displays the images in the 4th plot
;             column of the GUI. Setting /NO_SJI means that the 4th
;             column is used for displaying a fourth raster column
;             (this was the original behavior, before Feb-2015).
;
;     NO_HCR: The routine queries the Heliophysics Coverage Registry
;             (HCR) to retrieve some metadata, but sometimes this
;             service crashes. By setting this keyword you can bypass
;             the HCR.
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
; EXAMPLE:
;     Find a raster file sequence:
;     IDL> file=iris_find_file('29-Mar-2014 17:00')
;
;     Start the browser:
;     IDL> spice_raster_browser, file
;
; INTERNAL ROUTINES:
;     spice_browser_goes_plot, spice_browser_oplot_line_ids,
;     spice_browser_plot_image, IRIS_BROWSER_PLOT_SPEC,
;     spice_browser_font, IRIS_BROWSER_EVENT, IRIS_BROWSER_WIDGET,
;     spice_browser_update_spectrum, spice_browser_update_image,
;     spice_browser_plot_sji, spice_browser_coltable,
;     spice_browser_wvl_list, IRB_GET_FLARE_TEXT
;
; PROGRAMMING NOTES:
;     Incorporating SJI images
;     ------------------------
;     I decided to use the read_iris_l2 routine to access SJI images
;     as it was quicker than using the iris_sji object. The SJI images
;     are loaded using the subroutine spice_browser_plot_sji.
;
;     Sit-and-stare chunking
;     ----------------------
;     Sit-and-stare studies can run continuously for long periods
;     (e.g., 10 hours) and so loading them into raster_browser can be
;     very slow. To get around this, I decided to break such studies
;     into chunks, and a new slider is introduced to select different
;     chunks. To handle this there are extra parameters in wid_data:
;       nxpos   The size of a chunk in numbers of exposures.
;       ixpos   Index of the start of a chunk.
;       jxpos   Index of the end of a chunk.
;       nchunk  Number of chunks.
;       ichunk  Index of current chunk.
;     nxpos is computed automatically by the routine.
;
; TO-DO LIST:
;     - display SJI images correctly when the roll angle is not 0 or 90
;       degrees.
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-


PRO spice_browser_base_event, event
  ;
  ; Event handler.
  ;
  WIDGET_CONTROL,Event.top, get_uvalue=state

  k=where(event.id EQ state.im_plot,nk)
  IF nk GT 0 THEN BEGIN
    pwin=k[0]
    ;
    ; RESPOND TO MOUSE CLICKS ON IMAGE PLOT
    ; -------------------------------------
    CASE event.release OF
      1: BEGIN
        widget_control,state.spice_browser_base,get_uvalue=state
        im_zoom=state.wid_data.im_zoom
        IF im_zoom LT 6 THEN im_zoom=im_zoom+1
        state.wid_data.im_zoom=im_zoom
        widget_control,state.spice_browser_base,set_uvalue=state
        ;
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_calc_zoom_params,state,i
          spice_browser_plot_image,state,i
          ;        spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
      END
      ;
      2: BEGIN
        widget_control,state.spice_browser_base,get_uvalue=state
        ;
        ; make sure coordinate conversion takes place on image plot, so need
        ; to re-plot the image
        ;
        spice_browser_plot_image,state,pwin
        ;
        xy=convert_coord(round(event.x),round(event.y), $
          /device,/to_data)
        ;
        IF state.wid_data.im_type EQ 0 THEN scale=state.wid_data.scale ELSE scale=[1.,1.]
        ;
        xpix=(xy[0])/scale[0]+0.5
        ypix=(xy[1])/scale[1]+0.5
        ;
        IF state.wid_data.im_type EQ 0 THEN BEGIN
          xpix_chunk=fix(xpix) + state.wid_data.xrange[0]
          widget_control,state.exp_slider,set_value=xpix_chunk
          state.wid_data.xpix=xpix_chunk+state.wid_data.ichunk*state.wid_data.nxpos
        ENDIF ELSE BEGIN
          ;        state.wid_data.ilambda=fix(xpix)+state.wid_data.lrange[0]
        ENDELSE
        state.wid_data.ypix=fix(ypix)+state.wid_data.yrange[0]
        ;
        nx=state.wid_data.nx
        ny=state.wid_data.ny

        IF (state.wid_data.xpix GE 0) AND (state.wid_data.xpix LT nx) AND $
          (state.wid_data.ypix GE 0) AND (state.wid_data.ypix LT ny) THEN BEGIN
          ;
          xpix=state.wid_data.xpix
          xt='X-pixel: '+trim(state.wid_data.xpix)
          widget_control,state.xtext,set_value=xt
          ;
          yt='Y-pixel: '+trim(state.wid_data.ypix)
          widget_control,state.ytext,set_value=yt
          ;
          tt='Time: '+state.wid_data.midtime[xpix]
          widget_control,state.ttext,set_value=tt
          ;
          widget_control,state.spice_browser_base,set_uvalue=state
          ;
          widget_control,/hourglass
          n=state.wid_data.n_plot_window
          FOR i=0,n-1 DO BEGIN
            spice_browser_calc_zoom_params,state,i
            spice_browser_plot_image,state,i
            spice_browser_update_spectrum,state,i
            spice_browser_plot_spectrum,state,i
          ENDFOR
          IF state.wid_data.sji EQ 1 THEN BEGIN
            spice_browser_plot_sji, state
          ENDIF
          spice_browser_goes_plot,state
        ENDIF
      END
      ;
      4: BEGIN
        widget_control,state.spice_browser_base,get_uvalue=state
        im_zoom=state.wid_data.im_zoom
        IF im_zoom gt 0 THEN im_zoom=im_zoom-1
        state.wid_data.im_zoom=im_zoom
        widget_control,state.spice_browser_base,set_uvalue=state
        ;
        ;
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_calc_zoom_params,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
      END

      ELSE:
    ENDCASE
  ENDIF


  spec_plot=state.spec_plot
  k=where(event.id EQ spec_plot,nk)
  IF nk GT 0 THEN BEGIN
    pwin=k[0]
    ;
    ; RESPOND TO MOUSE CLICKS ON SPECTRUM PLOT
    ; ----------------------------------------
    CASE event.release OF
      1: BEGIN
        widget_control,state.spice_browser_base,get_uvalue=state
        spec_zoom=state.wid_data.spec_zoom[pwin]
        IF spec_zoom LT 6 THEN spec_zoom=spec_zoom+1
        state.wid_data.spec_zoom[pwin]=spec_zoom
        widget_control,state.spice_browser_base,set_uvalue=state
        ;
        spice_browser_calc_zoom_params,state,pwin
        spice_browser_plot_image,state,pwin
        spice_browser_plot_spectrum,state,pwin
      END
      ;
      2: BEGIN
        widget_control,state.spice_browser_base,get_uvalue=state
        ;
        ; make sure coordinate conversion takes place on spectrum plot, so need
        ; to re-plot the spectrum
        ;
        spice_browser_plot_spectrum,state,pwin
        ;
        ;      lpix=round(event.x+0.5)
        lpix=round(event.x)
        xy=convert_coord(lpix,0., $
          /device,/to_data)
        lambda=xy[0]
        IF state.wid_data.velocity EQ 1 THEN BEGIN
          lx=v2lamb(lambda,state.wid_data.lambda[pwin])
          lambda=lx+state.wid_data.lambda[pwin]
        ENDIF

        iwin=state.wid_data.iwin[pwin]
        wvl=state.data->getlam(iwin)
        ;
        getmin=min(abs(lambda-wvl),lpix)
        state.wid_data.lambda[pwin]=lambda
        state.wid_data.ilambda[pwin]=lpix
        ;
        widget_control,state.spice_browser_base,set_uvalue=state
        ;
        widget_control,/hourglass
        spice_browser_calc_zoom_params,state,pwin
        ;
        spice_browser_update_image,state,pwin
        spice_browser_plot_image,state,pwin
        ;
        spice_browser_update_spectrum,state,pwin
        spice_browser_plot_spectrum,state,pwin
      END
      ;
      4: BEGIN
        widget_control,state.spice_browser_base,get_uvalue=state
        spec_zoom=state.wid_data.spec_zoom[pwin]
        IF spec_zoom gt 0 THEN spec_zoom=spec_zoom-1
        state.wid_data.spec_zoom[pwin]=spec_zoom
        widget_control,state.spice_browser_base,set_uvalue=state
        ;
        spice_browser_calc_zoom_params,state,pwin
        spice_browser_plot_image,state,pwin
        spice_browser_plot_spectrum,state,pwin
      END

      ELSE:
    ENDCASE
  ENDIF


  ;
  ; PULL-DOWN MENU FOR CHOOSING WAVELENGTH WINDOW (AND WHISKER PLOT)
  ; ----------------------------------------------------------------
  cw_pd_window=state.cw_pd_window
  k=where(event.id EQ cw_pd_window,nk)
  IF nk GT 0 THEN BEGIN
    pwin=k[0]
    window_list=state.data->getline_id()
    nw=n_elements(window_list)
    iwin=event.value-1
    ;
    ; Select the new window
    ; ---------------------
    CASE 1 OF
      iwin LT nw: BEGIN
        state.wid_data.iwin[pwin]=iwin
        widget_control,state.window_lbl[pwin], $
          set_value='Current window: '+trim(window_list[iwin])
        ;
        ; Choose default wavelength pixel for new window
        ;
        ll=state.data->getlam(iwin)
        nl=n_elements(ll)
        state.wid_data.lambda[pwin]=ll[nl/2]
        state.wid_data.ilambda[pwin]=nl/2
        state.wid_data.lrange[*,pwin]=[0,nl-1]
        ;
        ; Reset zoom back to 1
        ;
        state.wid_data.spec_zoom[pwin]=0
        ;
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        ;
        spice_browser_update_image,state,pwin
        spice_browser_update_spectrum,state,pwin
        spice_browser_plot_image,state,pwin
        spice_browser_plot_spectrum,state,pwin
      END
      ;
      ; Button for whisker plot
      ; -----------------------
      ;  - this only works for sit-and-stare data.
      ;
      iwin EQ nw: BEGIN
        xrange=state.wid_data.xrange+state.wid_data.ichunk*state.wid_data.nxpos
        iris_xwhisker, state.data, state.wid_data.iwin[pwin], slitpos=state.wid_data.ypix, $
          wpix_range=reform(state.wid_data.lrange[*,pwin]), $
          tpix_range=xrange
      END
    ENDCASE
  ENDIF

  ;
  ; CHOOSE A NEW WAVELENGTH WINDOW
  ; ------------------------------
  ;; pd_window=state.pd_window
  ;; k=where(event.id EQ pd_window,nk)
  ;; IF nk GT 0 THEN BEGIN
  ;;   pwin=k[0]
  ;;   IF event.index GT 0 THEN BEGIN
  ;;     state.wid_data.iwin[pwin]=event.index-1
  ;;    ;
  ;;    ; change label for window
  ;;    ;
  ;;     id=state.data->getline_id()
  ;;     widget_control,state.window_lbl[pwin], $
  ;;          set_value='Current window: '+trim(id[event.index-1])
  ;;    ;
  ;;    ; Choose default wavelength pixel for new window
  ;;    ;
  ;;     iwin=event.index-1
  ;;     ll=state.data->getlam(iwin)
  ;;     nl=n_elements(ll)
  ;;     state.wid_data.lambda[pwin]=ll[nl/2]
  ;;     state.wid_data.ilambda[pwin]=nl/2
  ;;     state.wid_data.lrange[*,pwin]=[0,nl-1]
  ;;    ;
  ;;    ; Reset zoom back to 1
  ;;    ;
  ;;     state.wid_data.spec_zoom[pwin]=0
  ;;    ;
  ;;     widget_control,state.spice_browser_base,set_uvalue=state
  ;;     widget_control,/hourglass
  ;;    ;
  ;;     spice_browser_update_image,state,pwin
  ;;     spice_browser_update_spectrum,state,pwin
  ;;     spice_browser_plot_image,state,pwin
  ;;     spice_browser_plot_spectrum,state,pwin
  ;;   ENDIF
  ;; END

  ;
  ; MANUAL INTENSITY SCALING OF IMAGES - MINIMUM VALUE
  ; --------------------------------------------------
  mintext=state.min_text
  k=where(event.id EQ mintext,nk)
  IF nk GT 0 THEN BEGIN
    pwin=k[0]
    state.wid_data.autoint[pwin]=0
    widget_control,state.spice_browser_base,set_uvalue=state
    spice_browser_plot_image,state,pwin
  END


  ;
  ; MANUAL INTENSITY SCALING OF IMAGES - MAXIMUM VALUE
  ; --------------------------------------------------
  maxtext=state.max_text
  k=where(event.id EQ maxtext,nk)
  IF nk GT 0 THEN BEGIN
    pwin=k[0]
    state.wid_data.autoint[pwin]=0
    widget_control,state.spice_browser_base,set_uvalue=state
    spice_browser_plot_image,state,pwin
  END

  ;
  ; MANUAL INTENSITY SCALING OF *SJI* IMAGES - MINIMUM VALUE
  ; --------------------------------------------------------
  mintext=state.min_text_sji
  k=where(event.id EQ mintext,nk)
  IF nk GT 0 THEN BEGIN
    state.wid_data.autoint_sji=0
    widget_control,state.spice_browser_base,set_uvalue=state
    spice_browser_plot_sji,state
  ENDIF

  ;
  ; MANUAL INTENSITY SCALING OF *SJI* IMAGES - MAXIMUM VALUE
  ; --------------------------------------------------------
  maxtext=state.max_text_sji
  k=where(event.id EQ maxtext,nk)
  IF nk GT 0 THEN BEGIN
    state.wid_data.autoint_sji=0
    widget_control,state.spice_browser_base,set_uvalue=state
    spice_browser_plot_sji,state
  ENDIF


  ;
  ; AUTOMATIC INTENSITY SCALING OF SJI IMAGES
  ; -----------------------------------------
  autoint=state.auto_int_sji
  k=where(event.id EQ autoint,nk)
  IF nk GT 0 THEN BEGIN
    state.wid_data.autoint_sji=1
    widget_control,state.spice_browser_base,set_uvalue=state
    spice_browser_plot_sji,state
  ENDIF



  ;
  ; AUTOMATIC INTENSITY SCALING OF IMAGES
  ; -------------------------------------
  autoint=state.auto_int
  k=where(event.id EQ autoint,nk)
  IF nk GT 0 THEN BEGIN
    pwin=k[0]
    state.wid_data.autoint[pwin]=1
    widget_control,state.spice_browser_base,set_uvalue=state
    spice_browser_plot_image,state,pwin
  END

  ;
  ; LAUNCH IRIS_XWHISKER
  ; --------------------
  ;; whisk_butt=state.whisk_butt
  ;; k=where(event.id EQ whisk_butt,nk)
  ;; IF nk GT 0 THEN BEGIN
  ;;   pwin=k[0]
  ;;   xrange=state.wid_data.xrange+state.wid_data.ichunk*state.wid_data.nxpos
  ;;   iris_xwhisker, state.data, state.wid_data.iwin[pwin], slitpos=state.wid_data.ypix, $
  ;;                  wpix_range=reform(state.wid_data.lrange[*,pwin]), $
  ;;                  tpix_range=xrange
  ;; ENDIF


  ;
  ; SELECT A SJI WINDOW FROM PULL-DOWN LIST
  ; ---------------------------------------
  sji_pd_window=state.sji_pd_window
  k=where(event.id EQ sji_pd_window,nk)
  IF nk GT 0 THEN BEGIN
    state.wid_data.sji_index=event.index
    sji_id=state.wid_data.sji_id[event.index]
    widget_control,state.sji_window_lbl, $
      set_value='Current window: '+trim(sji_id)
    ;
    ; the following replaces the sji_d tag with the new sji object
    ;
    sji_file=state.wid_data.sji_file[event.index]
    state_temp=rem_tag(state,'sji_d')
    state=0
    state=add_tag(state_temp,iris_sji(sji_file),'sji_d')
    state_temp=0
    ;
    ; For the droplist with the number of frames options, I need to re-do
    ; the options since different channels may have different numbers of frames.
    sji_droplist_options=spice_browser_sji_frame_options(state, $
      default_option=default_option)
    state.wid_data.sji_mov_frames=fix(sji_droplist_options[default_option])

    ;; sji_nexp=state.sji_d->getnexp(0)
    ;; sji_droplist_value=[trim(1),trim(sji_nexp)]
    widget_control,state.sji_frames_droplist,set_value=sji_droplist_options
    ;
    ;
    widget_control,state.spice_browser_base,set_uval=state
    spice_browser_plot_sji, state
  ENDIF



  CASE event.id OF

    state.file_slider: BEGIN
      filestr=state.wid_data.filestr
      IF event.value NE filestr.current THEN BEGIN
        state.wid_data.filestr.current=event.value
        state.data=iris_obj(filestr.filelist[event.value])
        ;      state.data=d
        widget_control,state.spice_browser_base,set_uvalue=state
        meta=spice_browser_get_metadata(state.data)
        spice_browser_update_widdata,state,meta
        spice_browser_update_info,state
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN BEGIN
          spice_browser_plot_sji,state
        ENDIF
        spice_browser_goes_plot,state
      ENDIF
    END

    state.file_butt1: BEGIN
      filestr=state.wid_data.filestr
      val=max([filestr.current-1,0])
      IF val NE filestr.current THEN BEGIN
        state.wid_data.filestr.current=val
        d=iris_obj(filestr.filelist[val])
        state.data=d
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,state.file_slider,set_value=val
        meta=spice_browser_get_metadata(d)
        spice_browser_update_widdata,state,meta
        spice_browser_update_info,state
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN BEGIN
          spice_browser_plot_sji,state
        ENDIF
        spice_browser_goes_plot,state
      ENDIF
    END

    state.file_butt2: BEGIN
      filestr=state.wid_data.filestr
      val=min([filestr.current+1,filestr.nfiles-1])
      IF val NE filestr.current THEN BEGIN
        state.wid_data.filestr.current=val
        d=iris_obj(filestr.filelist[val])
        state.data=d
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,state.file_slider,set_value=val
        meta=spice_browser_get_metadata(d)
        spice_browser_update_widdata,state,meta
        spice_browser_update_info,state
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN BEGIN
          spice_browser_plot_sji,state
        ENDIF
        spice_browser_goes_plot,state
      ENDIF
    END

    ;
    ; This handles events from the sit-and-stare "chunk" slider.
    ;
    state.chunk_slider: BEGIN
      ichunk=state.wid_data.ichunk
      nchunk=state.wid_data.nchunk
      IF event.value NE ichunk THEN BEGIN
        xpix=state.wid_data.xpix
        ixpos=state.wid_data.ixpos
        xpix_chunk=xpix-(ixpos)
        ichunk=event.value
        nxpos=state.wid_data.nxpos
        state.wid_data.ichunk=event.value
        state.wid_data.ixpos=ichunk*nxpos
        state.wid_data.jxpos=(ichunk+1)*nxpos-1
        state.wid_data.xpix=xpix_chunk+state.wid_data.ixpos
        ;
        ; For the last chunk, the number of exposures may be different
        ; from the other chunks, so it's necessary to adjust the
        ; maximum value that the slider can take.
        ;
        IF ichunk EQ nchunk-1 THEN BEGIN
          slider_max=state.wid_data.nx-(nchunk-1)*nxpos-1
        ENDIF ELSE BEGIN
          slider_max=nxpos-1
        ENDELSE
        widget_control,state.exp_slider,set_slider_max=slider_max
        ;
        spice_browser_update_info, state
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN BEGIN
          spice_browser_plot_sji,state
        ENDIF
        spice_browser_goes_plot,state
      ENDIF
    END

    state.chunk_butt1: BEGIN
      ichunk=state.wid_data.ichunk
      nchunk=state.wid_data.nchunk
      IF ichunk NE 0 THEN BEGIN
        xpix=state.wid_data.xpix
        ixpos=state.wid_data.ixpos
        xpix_chunk=xpix-(ixpos)
        ichunk=ichunk-1
        nxpos=state.wid_data.nxpos
        state.wid_data.ichunk=ichunk
        state.wid_data.ixpos=ichunk*nxpos
        state.wid_data.jxpos=(ichunk+1)*nxpos-1
        state.wid_data.xpix=xpix_chunk+state.wid_data.ixpos
        widget_control,state.chunk_slider,set_value=ichunk
        spice_browser_update_info, state
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN BEGIN
          spice_browser_plot_sji,state
        ENDIF
        spice_browser_goes_plot,state
      ENDIF
    END

    state.chunk_butt2: BEGIN
      ichunk=state.wid_data.ichunk
      nchunk=state.wid_data.nchunk
      IF ichunk NE nchunk THEN BEGIN
        xpix=state.wid_data.xpix
        ixpos=state.wid_data.ixpos
        xpix_chunk=xpix-(ixpos)
        ichunk=ichunk+1
        nxpos=state.wid_data.nxpos
        state.wid_data.ichunk=ichunk
        state.wid_data.ixpos=ichunk*nxpos
        state.wid_data.jxpos=(ichunk+1)*nxpos-1
        state.wid_data.xpix=xpix_chunk+state.wid_data.ixpos
        widget_control,state.chunk_slider,set_value=ichunk
        spice_browser_update_info, state
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN BEGIN
          spice_browser_plot_sji,state
        ENDIF
        spice_browser_goes_plot,state
      ENDIF
    END

    state.wpix_sum: BEGIN
      wpix=[1,5,9,15]
      state.wid_data.lbin=wpix[event.value]
      widget_control,state.spice_browser_base,set_uvalue=state
      ;
      ; I need the if below since two 'events' are registered when a button
      ; is selected. The first has select=0
      IF event.select EQ 1 THEN BEGIN
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
      ENDIF

    END

    state.lids_butts: BEGIN
      widget_control,state.lids_butts,get_value=chck
      IF chck NE state.wid_data.line_ids THEN BEGIN
        state.wid_data.line_ids=event.value
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_plot_spectrum,state,i
        ENDFOR
      ENDIF
    END

    ;
    ; Convert between wavelength and velocity for spectrum plots.
    ;
    state.vel_butts: BEGIN
      widget_control,state.vel_butts,get_value=chck
      IF chck NE state.wid_data.velocity THEN BEGIN
        state.wid_data.velocity=event.value
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_plot_spectrum,state,i
          IF state.wid_data.im_type EQ 1 THEN spice_browser_plot_image,state,i
        ENDFOR
      ENDIF
    END

    ;
    ; Convert linear and log intensity scaling for images.
    ;
    state.log_butts: BEGIN
      widget_control,state.log_butts,get_value=chck
      IF chck NE state.wid_data.linlog THEN BEGIN
        state.wid_data.linlog=event.value
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_plot_image,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
      ENDIF
    END

    ;
    ; Switch between image types (X-Y or lambda-Y)
    ;
    state.im_type_butts: BEGIN
      widget_control,state.im_type_butts,get_value=chck
      IF chck NE state.wid_data.im_type THEN BEGIN
        state.wid_data.im_type=event.value
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,state.exp_base,sens=event.value
        widget_control,/hourglass
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_plot_image,state,i
        ENDFOR
      ENDIF
    END

    ;
    ; Use slider and/or buttons to change exposure number.
    ;
    state.exp_slider: BEGIN
      ;
      ; Have to be careful to convert the slider value to xpix by making
      ; use of nxpos and ichunk.
      nxpos=state.wid_data.nxpos
      ichunk=state.wid_data.ichunk
      xpix=event.value+ichunk*nxpos
      IF xpix GT state.wid_data.nx-1 THEN BEGIN
        xpix=state.wid_data.nx-1
        widget_control,state.exp_slider,set_value=xpix-ichunk*nxpos
      ENDIF
      state.wid_data.xpix=xpix
      ;
      widget_control,state.spice_browser_base,set_uvalue=state
      meta=spice_browser_get_metadata(state.data)
      spice_browser_update_widdata,state,meta
      spice_browser_update_info,state
      n=state.wid_data.n_plot_window
      FOR i=0,n-1 DO BEGIN
        spice_browser_update_image,state,i
        spice_browser_update_spectrum,state,i
        spice_browser_plot_image,state,i
        spice_browser_plot_spectrum,state,i
      ENDFOR
      IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
      spice_browser_goes_plot,state
    END
    ;
    state.exp_butt1: BEGIN
      xpix=state.wid_data.xpix
      IF xpix NE 0 THEN BEGIN
        state.wid_data.xpix=xpix-1
        xpix_chunk=state.wid_data.xpix - state.wid_data.ichunk*state.wid_data.nxpos
        meta=spice_browser_get_metadata(state.data)
        spice_browser_update_widdata,state,meta
        spice_browser_update_info,state
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,state.exp_slider,set_value=xpix_chunk
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
        spice_browser_goes_plot,state
      ENDIF
    END
    ;
    state.exp_butt2: BEGIN
      xpix=state.wid_data.xpix
      ;    print,xpix,state.wid_data.nxp-1
      IF xpix NE state.wid_data.nx-1 THEN BEGIN
        state.wid_data.xpix=xpix+1
        xpix_chunk=state.wid_data.xpix - state.wid_data.ichunk*state.wid_data.nxpos
        meta=spice_browser_get_metadata(state.data)
        spice_browser_update_widdata,state,meta
        spice_browser_update_info,state
        widget_control,state.spice_browser_base,set_uvalue=state
        widget_control,state.exp_slider,set_value=xpix_chunk
        n=state.wid_data.n_plot_window
        FOR i=0,n-1 DO BEGIN
          spice_browser_update_image,state,i
          spice_browser_update_spectrum,state,i
          spice_browser_plot_image,state,i
          spice_browser_plot_spectrum,state,i
        ENDFOR
        IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
        spice_browser_goes_plot,state
      ENDIF
    END

    ;
    ; Where nexp_prp>1, allows specific exp. time to be selected.
    ;
    state.nexp_prp_butts: BEGIN
      widget_control,state.spice_browser_base,get_uval=state
      state.wid_data.nexp_prp=event.value
      widget_control,state.spice_browser_base,set_uval=state
      ;
      wind=state.wind
      new_state=rem_tag(state,'wind')
      ;
      iwin=state.wid_data.iwin
      data=state.data
      make_wind_struc,data,state.hdrstr,iwin,wind, $
        offset=state.wid_data.offset,state=state
      ;
      state=0
      state=add_tag(new_state,wind,'wind')
      widget_control,state.spice_browser_base,set_uval=state
      ;
      spice_browser_plot_image,state
      spice_browser_plot_spectrum,state
    END


    state.sji_movie_butt: BEGIN
      ;
      ; This is the 'Show Movie' button and it sends the movie parameters to
      ; ximovie
      ;

      ; A quirk of ximovie is that it will crash if pos is set to the
      ; full size of the SJI images. I thus check the status of im_zoom:
      ; if no zoom (i.e., full image) then pos is not defined.
      ;
      im_zoom=state.wid_data.im_zoom
      IF im_zoom NE 0 THEN pos=[state.wid_data.sji_xrange,state.wid_data.sji_yrange]

      widget_control,state.min_text_sji,get_value=sji_min
      widget_control,state.max_text_sji,get_value=sji_max

      i=state.wid_data.sji_frame
      n=state.wid_data.sji_nframe
      d=state.wid_data.sji_mov_frames

      IF d EQ n THEN BEGIN
        first_im=0
        last_im=n-1
      ENDIF ELSE BEGIN
        d2=fix((fix(d)-1)/2.)
        first_im=max([0,i-d2])
        last_im=min([i+d2,n-1])
      ENDELSE

      state.sji_d->ximovie,pos=pos, log=state.wid_data.linlog, $
        first_im=first_im,last_im=last_im,start_im=i
    END

    state.sji_frames_droplist: BEGIN
      widget_control,state.sji_frames_droplist,get_value=value
      result=value[event.index]
      IF trim(result) EQ 'all' THEN sji_mov_frames=state.wid_data.sji_nframe $
      ELSE sji_mov_frames=fix(result)
      state.wid_data.sji_mov_frames=sji_mov_frames
      ;
      ; Update the movie duration widget
      ;
      sji_mov_dur=(sji_mov_frames*state.wid_data.sji_cadence)/60.
      sji_mov_dur_txt=trim(string(sji_mov_dur,format='(f7.1)'))
      sji_dur_txt='Movie duration: '+sji_mov_dur_txt+' mins (approx)'
      widget_control,state.sji_dur_text,set_val=sji_dur_txt
      ;
      widget_control,state.spice_browser_base,set_uval=state
    END

    state.eis_butt: BEGIN
      t0=state.wid_data.filestr.t0
      t1=state.wid_data.filestr.t1
      iris_eis_obs_check,t0,t1,out_string=out_string,margin=30.
      spice_browser_font,tfont,/fixed
      spice_browser_font,bfont
      len=strlen(out_string)
      xsiz=max(len)
      ysiz=min([n_elements(out_string),30])
      ysiz=ysiz>10
      xpopup,out_string,tfont=tfont,bfont=bfont,xsiz=xsiz,ysiz=ysiz
    END

    state.goes_butt: BEGIN
      out_string=irb_get_flare_text(state.wid_data.flare_data)
      spice_browser_font,tfont,/fixed
      spice_browser_font,bfont
      len=strlen(out_string)
      xsiz=max(len)
      ysiz=min([n_elements(out_string),30])
      ysiz=ysiz>10
      xpopup,out_string,tfont=tfont,bfont=bfont,xsiz=xsiz,ysiz=ysiz
    END

    state.exit: BEGIN

      CASE event.value OF

        0: widget_control, event.top, /destroy
        1: BEGIN
          spice_browser_font,font,retina=state.wid_data.retina
          str1=['HELP FOR SPICE_RASTER_BROWSER',$
            '',$
            'spice_raster_browser is used to browse the 3D data cubes produced by the',$
            'IRIS instrument from narrow slit rasters. ',$
            '',$
            'It is recommended that you order your IRIS data files into a standard hierarchy', $
            'see the', $
            '',$
            'You will see four pairs of images in the graphic user interface',$
            '(GUI). The top row of four shows images in four of the emission lines,',$
            'the bottom row of four show spectra containing the four emission lines.',$
            '',$
            'A star in the images and a cross in the spectra indicate the pixel',$
            'that is highlighted from the data cube. Thus the image represents a',$
            'slice through the data cube at the specified wavelength pixel',$
            '(actually it is an average of several wavelength pixels - see below).',$
            '',$
            'USING THE MOUSE BUTTONS',$
            '',$
            'You can change the chosen pixel by clicking with the MIDDLE mouse',$
            'button in either of the two graphic windows. Clicking in the image',$
            'changes the spatial pixel; clicking in the spectrum changes the',$
          'wavelength pixel.',$
            '',$
            'You can zoom into and out of the images and spectra by using the LEFT',$
            'and RIGHT mouse buttons.  The LEFT zooms in; the RIGHT button zooms',$
          'out.',$
            '',$
            'If you are using the Mac trackpad on a laptop, then you can use',$
            'keyboard commands to substitute for the extra mouse buttons. First go',$
            'to X11->Preferences...->Input and select the Emulate 3 button mouse',$
            'option. You can now do:',$
            '',$
            '     CLICK - Zoom in to image',$
            '     OPTION+CLICK - Select a new image pixel',$
            '     COMMAND+CLICK - Zoom out from image',$
            '',$
            'CHANGING THE WAVELENTH WINDOWS',$
            '',$
            'Above each of the four pairs of graphic windows is a drop-down list',$
            'menu which allows you to choose different wavelength windows.',$
            '',$
            'ADJUSTING THE INTENSITY SCALING',$
            '',$
            'Above each of the four pairs of graphic windows are two text boxes',$
            'which allow you to change the intensity scaling in the images. (This',$
            'is useful to bring out weak features in the images.)  By default the',$
            'routine uses the minimum and maximum of the image. You can reset the',$
            'values by clicking on the AUTO button.',$
            '',$
            'WAVELENGTH PIXELS TO SUM',$
            '',$
            'This allows the signal-to-noise in the image to be increased by binning', $
            'over multiple wavelength pixels. The default is to average over 5 pixels.', $
            'The buttons on the left-hand side of the widget can be used to change to 1, ', $
            '3, 5 or 7 pixels.',$
            '',$
            'SHOW LINE IDS?',$
            '',$
            'If this option is selected then suggested line identifications are',$
            'overplotted on the spectrum. These IDs are not complete and may not be',$
            'accurate in all conditions (e.g., Ca XVII will not be present in quiet',$
            'Sun conditions), but it is hoped they will be a useful guidance for',$
            'understanding the data. The wavelengths are from the CHIANTI',$
            'database. Note that you must have the CHIANTI database installed in',$
            'Solarsoft for this feature to work.',$
            '']

          xpopup,str1,tfont=font,bfont=font,xsiz=70,ysiz=30, $
            title='HELP file for spice_raster_browser'
        END

        ELSE: BEGIN
          ;
          ; Have to subtract 3 as exit=0, help=1, color=2, so coltables start at 3.
          ;
          new_coltable=event.value-3
          coltable=state.wid_data.coltable
          IF coltable NE event.value THEN BEGIN
            spice_browser_coltable,state=state,set_value=new_coltable
            ;
            n=state.wid_data.n_plot_window
            FOR i=0,n-1 DO BEGIN
              spice_browser_plot_image,state,i
            ENDFOR
            IF state.wid_data.sji EQ 1 THEN spice_browser_plot_sji, state
          ENDIF
        END

      ENDCASE

    END

    ELSE:

  ENDCASE

END


;-----------------
PRO spice_browser_widget, data, group=group, yoffsets=yoffsets, filestr=filestr, $
  chunk_size=chunk_size, retina=retina, hcr=hcr, no_goes=no_goes, $
  flare_data=flare_data


  IF n_tags(filestr) EQ 0 THEN filestr=0

  id=data->getline_id()
  i=where(id NE '')
  nwind=n_elements(id[i])

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


  spice_browser_font,font, retina=retina
  spice_browser_font,bigfont,/big, retina=retina
  spice_browser_font,fixfont,/fixed, retina=retina

  ;
  ; Get metadata from the object.
  ;
  meta=spice_browser_get_metadata(data)

  ;
  ; This takes a value of 0 or 1 (1=sit-and-stare).
  ;
  sit_stare=meta.sit_stare


  obs_type=meta.obs_type

  nexp=data->getnexp()    ; this is an array for IRIS
  nexp_prp=data->getnexp_prp()
  IF nexp_prp GT 1 THEN nexp=nexp/nexp_prp
  nexp=nexp[0]

  ;
  ; Gets the mid-point time of each exposure in '12:00:00' format. This
  ; is stored in wid_data. Note that the array is reversed if
  ; rast_direct=0.
  ;
  midtime=meta.midtime
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
    IF nexp GT nxpos*1.5 THEN BEGIN
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
  sji_file=filestr.sji_file
  IF sji_file[0] NE '' THEN BEGIN
    sji=1
    sji_d=iris_sji(sji_file)
    sji_ti=sji_d->gettime()
    sji_nexp=sji_d->getnexp(0)
    sji_exp=sji_d->getexp(indgen(sji_nexp))
    sji_ti=sji_ti+sji_exp/2.    ; get mid-time of exposure
    sji_id=sji_d->getsji_id()
    sji_start=sji_d->getinfo('DATE_OBS')
    sji_end=sji_d->getinfo('DATE_END')
    duration=anytim2tai(sji_end)-anytim2tai(sji_start)
    sji_cadence=duration/float(sji_nexp)
  ENDIF ELSE BEGIN
    sji=0
    sji_file=''
    sji_id=''
    sji_d=0
    sji_cadence=0.
  ENDELSE


  IF sji_file[0] EQ '' THEN BEGIN
    n_plot_window=4
  ENDIF ELSE BEGIN
    n_plot_window=3
    n=n_elements(sji_file)
    sji_id=strarr(n)
    FOR i=0,n-1 DO BEGIN
      basename=file_basename(sji_file[i])
      sji_id[i]=strmid(basename,35,8)
    ENDFOR
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
    ccd: bytarr(nwind), $
    lbin: 5, $
    origin: origin, $
    scale: scale, $
    shifts: fltarr(nexp[0]), $
    shift_set: 1, $
    lambda: fltarr(n_plot_window), $
    ilambda: intarr(n_plot_window), $
    goes_plot_id: 0, $
    line_ids: line_ids, $      ; 0 or 1 if line IDs available
    idstr: idstr, $            ; structure of line IDs
    midtime: midtime, $
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
    roll: data->getinfo('SAT_ROT'), $
    l1p5_ver: meta.l1p5_ver, $
    sji: sji, $
    sji_file: sji_file, $     ; array of all SJI filenames
    sji_id: sji_id, $         ; array of IDs (e.g., 'SJI_1400')
    sji_plot_id: -1, $
    sji_index: -1, $
    sji_frame: 0, $           ; computed by plot_sji_image
    sji_mov_frames: 0, $
    sji_nframe: 0, $
    sji_xrange: [0,0], $
    sji_yrange: [0,0], $
    sji_cadence: sji_cadence, $
    autoint_sji: 1, $
    filestr: filestr, $
    coltable: 0, $
    retina: keyword_set(retina), $
    exptime: fltarr(n_plot_window), $
    n_plot_window: n_plot_window, $
    hcr: hcr, $
    flare_data: flare_data $
  }


  ;
  ; Work out default IWIN values for each of the four plot windows.
  ; 23-Feb-2014 - I've added a "backup" check in order to pick
  ;               the weaker doublet line if the strong one isn't
  ;               available.
  ;
  rlamb=    [2796.352,1335.708,1393.757,1401.158,1349.403,1334.532]
  rl_backup=[2803.530,1334.532,1402.770,-1,-1,-1]
  nwin=data->getnwin()
  count=0

  FOR j=0,n_elements(rlamb)-1 DO BEGIN
    swtch=0
    FOR i=0,nwin-1 DO BEGIN
      lam=data->getlam(i)
      IF rlamb[j] GE min(lam) AND rlamb[j] LE max(lam) THEN BEGIN
        wid_data.iwin[count]=i
        wid_data.lambda[count]=rlamb[j]
        getmin=min(abs(rlamb[j]-lam),imin)
        wid_data.ilambda[count]=imin
        nl=n_elements(lam)
        wid_data.lrange[*,count]=[0,nl-1]
        count=count+1
        swtch=1
        break
      ENDIF
    ENDFOR
    ;
    IF swtch EQ 0 THEN BEGIN
      IF rl_backup[j] NE -1 THEN BEGIN
        FOR i=0,nwin-1 DO BEGIN
          lam=data->getlam(i)
          IF rl_backup[j] GE min(lam) AND rl_backup[j] LE max(lam) THEN BEGIN
            wid_data.iwin[count]=i
            wid_data.lambda[count]=rl_backup[j]
            getmin=min(abs(rl_backup[j]-lam),imin)
            wid_data.ilambda[count]=imin
            nl=n_elements(lam)
            wid_data.lrange[*,count]=[0,nl-1]
            count=count+1
            swtch=1
            break
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
    ;
    IF count EQ n_plot_window THEN break
  ENDFOR

  k=where(wid_data.iwin EQ -1,nk)
  IF nk GT 0 THEN BEGIN
    FOR i=0,nk-1 DO BEGIN
      wid_data.iwin[k[i]]=0
      lam=data->getlam(i)
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
  tt='Time: '
  ttext=widget_label(textbase,value=tt,font=font,xsiz=150)


  ;
  ; The following adds a slider if multiple files have been input.
  ;
  IF filestr.nfiles GT 1 THEN BEGIN
    nf=filestr.nfiles
    file_base=widget_base(opt_base,/row,frame=1)
    title='File no. (0-'+trim(nf-1)+')'
    file_slider=widget_slider(file_base,min=0,max=nf-1,font=font,title=title)
    file_butt1=widget_button(file_base,value='-',font=bigfont)
    file_butt2=widget_button(file_base,value='+',font=bigfont)
  ENDIF ELSE BEGIN
    file_slider=0
    file_butt1=0
    file_butt2=0
  ENDELSE


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
  im_type_butts=cw_bgroup(im_type_base,options, $
    set_value=wid_data.im_type,/exclusive,font=font,/row)

  exp_base=widget_base(im_type_base,/row,frame=1,sens=0)
  title='Exp no. (0-'+trim(nxpos-1)+')'
  exp_slider=widget_slider(exp_base,min=0,max=nxpos-1,font=font,title=title)
  exp_butt1=widget_button(exp_base,value='-',font=bigfont)
  exp_butt2=widget_button(exp_base,value='+',font=bigfont)

  ;
  ; NEXP_PRP > 1 BUTTONS
  ; --------------------
  IF nexp_prp GT 1 THEN BEGIN
    nexp_prp_base=widget_base(opt_base,/col,frame=1)
    wid_data.nexp_prp=0
    text=trim(indgen(nexp_prp))
    exp=data->getexp()
    exp=exp[0:nexp_prp-1]
    expstr=trim(string(format='(f12.1)',exp))+' s'
    nexp_prp_text=widget_label(nexp_prp_base,val='Choose exposure', $
      font=font,/align_left)
    nexp_prp_butts=cw_bgroup(nexp_prp_base,expstr,/exclusive, $
      set_value=wid_data.nexp_prp,/col,font=font)

  ENDIF ELSE BEGIN
    nexp_prp_butts=0
  ENDELSE

  ;
  ; The following contains meta-data that goes on the Metadata tab.
  ;
  stud_acr=data->getinfo('OBSID')
  IF n_tags(filestr) NE 0 THEN BEGIN
    date_obs=filestr.t0
    date_end=filestr.t1
  ENDIF ELSE BEGIN
    date_obs=data->getinfo('DATE_OBS')
    date_end=data->getinfo('DATE_END')
  ENDELSE
  ;
  text1=widget_label(meta_base,val='OBSID: '+stud_acr,font=font, $
    /align_left)
  text1a=widget_label(meta_base,val='TYPE: '+obs_type,font=font, $
    /align_left)
  ex=anytim(/ex,date_obs)
  date=trim(ex[4])+'-'+trim(get_month(ex[5]-1,/trunc))+'-'+trim(ex[6])
  text2=widget_label(meta_base,val='DATE: '+date,font=font, $
    /align_left)
  time=strpad(trim(ex[0]),2,fill='0')+':'+strpad(trim(ex[1]),2,fill='0')
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
  IF nexp_prp EQ 1 THEN BEGIN
    cadence=meta.cadence
    cadstr=trim(string(format='(f10.1)',cadence))+' s'
    text4=widget_label(meta_base,val='CADENCE: '+cadstr,font=font, $
      /align_left)
  ENDIF
  nuvbin=data->getinfo('SUMSPTRN')
  fuvbin=data->getinfo('SUMSPTRF')
  spatbin=data->getinfo('SUMSPAT')
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

  choices=trim(id[0:nwind-1])
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
    lbl_text='Current window: '+trim(id[wid_data.iwin[i]])
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
    sji_plot=widget_draw(plot_base_sji,xsiz=xsiz,ysiz=ysiz)
    ;
    sji_movie_butt=widget_button(plot_base_sji,value='SHOW MOVIE',font=bigfont)
    ;
    sji_frames_base=widget_base(plot_base_sji,/row)

    ;
    ; These are the options for the number of frames to display. Note
    ; that the max no. of frames is appended (although minus 1).
    ;
    value=[11,21,31,51,101,201,301,501,1001,2001,3001,5001]
    k=where(value LT sji_nexp)
    value=[value[k],sji_nexp]
    value_string=string(value)
    ;
    ; Choose default value of no. of frames by choosing closest to 10mins
    ;
    getmin=min(abs(float(value)*sji_cadence-600.),imin)
    set_droplist_select=imin
    mov_duration=value[imin]*sji_cadence/60.   ; minutes
    ;
    ; Create the drop-list for no. of frames.
    ;
    sji_frames_lbl=widget_label(sji_frames_base,value='No. of frames:',font=font)
    sji_frames_droplist=widget_droplist(sji_frames_base,value=value_string,font=font)
    wid_data.sji_mov_frames=fix(value_string[set_droplist_select])
    ;
    ; Create label showing movie duration
    ;
    sji_dur_str=trim(string(mov_duration,format='(f7.1)'))
    dur_t='Movie duration: '+sji_dur_str+' mins (approx)'
    sji_dur_text=widget_label(plot_base_sji,value=dur_t,font=font,/align_left,xsiz=250)
    ;
    sji_xrange_text=widget_label(plot_base_sji,value='X-range (pixels):', $
      font=font,/align_left,xsiz=250)
    sji_yrange_text=widget_label(plot_base_sji,value='Y-range (pixels):', $
      font=font,/align_left,xsiz=250)
  ENDIF ELSE BEGIN
    min_text_sji=0
    max_text_sji=0
    auto_int_sji=0
    sji_window_lbl=0
    sji_pd_window=0
    sji_plot=0
    sji_movie_butt=0
    sji_frames_droplist=0
    sji_dur_text=0
    sji_xrange_text=0
    sji_yrange_text=0
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
  images=fltarr(wid_data.nxpos,wid_data.ny+40,n_plot_window)


  ;
  ; 'spectra' contains the 1D spectrum for each window. Note that I pad
  ; each spectrum to be the maximum size of all of the spectrum windows
  ;
  nl=data->getxw()
  spectra=fltarr(max(nl),n_plot_window)

  ;
  ; I use this for storing exposure images
  ;
  expimages=fltarr(max(nl),wid_data.ny+40,n_plot_window)

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
    text3: text3, $
    min_text_sji: min_text_sji, $
    max_text_sji: max_text_sji, $
    auto_int_sji: auto_int_sji, $
    sji_window_lbl: sji_window_lbl, $
    sji_pd_window: sji_pd_window, $
    sji_plot: sji_plot, $
    sji_movie_butt: sji_movie_butt, $
    sji_frames_droplist: sji_frames_droplist, $
    sji_dur_text: sji_dur_text, $
    sji_xrange_text: sji_xrange_text, $
    sji_yrange_text: sji_yrange_text, $
    spectra: spectra, $
    cw_pd_window: cw_pd_window, $
    ;       whisk_butt: whisk_butt, $
    eis_butt: eis_butt, $
    goes_butt: goes_butt}

  WIDGET_CONTROL, spice_browser_base, /REALIZE, set_uvalue=state


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

  widget_control,state.exp_slider,set_value=state.wid_data.xpix

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
    widget_control,sji_plot,get_value=val
    state.wid_data.sji_plot_id=val
  ENDIF
  ;
  widget_control,goes_plot,get_value=val
  state.wid_data.goes_plot_id=val
  ;
  widget_control,spice_browser_base,set_uvalue=state

  ;
  ; Set initial value for SJI movie frames
  ;
  IF wid_data.sji EQ 1 THEN widget_control,sji_frames_droplist,set_droplist_select=set_droplist_select

  ;
  ; Make initial plots
  ;
  widget_control,/hourglass
  FOR i=0,n_plot_window-1 DO BEGIN
    spice_browser_update_image,state,i
    spice_browser_plot_image, state, i
    spice_browser_update_spectrum,state,i
    spice_browser_plot_spectrum, state, i
  ENDFOR
  ;
  spice_browser_goes_plot, state

  IF sji_file[0] NE '' THEN BEGIN
    spice_browser_plot_sji, state
  ENDIF

  tt='Time: '+trim(midtime[state.wid_data.xpix-1])
  widget_control,state.ttext,set_value=tt


  XMANAGER, 'spice_browser_base', spice_browser_base, group=group

  IF datatype(g) EQ 'OBJ' THEN obj_destroy,g
  IF sji_file[0] NE '' THEN obj_destroy,sji_d

END


;---------------------
PRO spice_raster_browser, input, quiet=quiet, yoffsets=yoffsets, no_sji=no_sji, $
  chunk_size=chunk_size, retina=retina, no_hcr=no_hcr, $
  no_goes=no_goes
  
  
  input = '/Users/mawiesma/data/iris/level2/20180706_155939_3600008058/iris_l2_20180706_155939_3600008058_raster_t000_r00001.fits'


;  IF n_params() EQ 0 THEN BEGIN
;    print,'Use:  IDL> spice_raster_browser, obj'
;    print,' or:  IDL> spice_raster_browser, filename'
;    print,'         multiple filenames can be specified if they belong to the same raster sequence.'
;    return
;  ENDIF

  ;
  ; Below I check if INPUT is a string (i.e., a filename) or an object.
  ;
  IF datatype(input) EQ 'STR' THEN BEGIN
    data=iris_obj(input[0])
    file=input
    swtch=1
  ENDIF ELSE BEGIN
    data=input
    swtch=0
  ENDELSE


  ;
  ; The routine iris_sji_match looks for SJI files that match the raster
  ; file. If an object has been input, then the routine makes uses of
  ; iris_find_filea nd iris_sji_match to find the SJI files.
  ;
  IF NOT keyword_set(no_sji) AND swtch EQ 1 THEN BEGIN
    sji_file=iris_sji_match(file[0])
  ENDIF ELSE BEGIN
    date_obs=data->getinfo('DATE_OBS')
    file=iris_find_file(date_obs,count=count)
    IF count NE 0 THEN sji_file=iris_sji_match(file[0]) ELSE sji_file=''
  ENDELSE


  ;
  ; If multiple filenames are specified, then the following checks to
  ; make sure they belong to the same raster sequence. I then create the
  ; structure 'filestr' containing information about the sequence.
  ;
  nf=n_elements(file)
  IF nf GT 1 THEN BEGIN
    bname=file_basename(file)
    strchck=strmid(bname[0],0,41)
    chck=strpos(bname,strchck)
    k=where(chck LT 0,nk)
    IF nk GT 0 THEN BEGIN
      print,'% SPICE_RASTER_BROWSER: multiple filenames have been specified, but they do not all belong to the'
      print,'                       same raster sequence. Please check your inputs.'
      print,'                       All files should have the same base-name: '+strchck
      return
    ENDIF
    ;
    ; The data object was loaded earlier, so extract info.
    ;
    t0=data->getinfo('DATE_OBS')
    t1=data->getinfo('ENDOBS')
    ;
    ;; d=iris_obj(file[nf-1])
    ;; t1=d->getinfo('DATE_END')
    ;; obj_destroy,d
    ;
    filestr={nfiles: nf, filelist: file, current: 0, t0: t0, t1: t1, sji_file: sji_file}
  ENDIF ELSE BEGIN
    t0=data->getinfo('DATE_OBS')
    t1=data->getinfo('DATE_END')
    filestr={nfiles: 1, filelist: file, current: 0, t0: t0, t1: t1, sji_file: sji_file}
  ENDELSE


  ;
  ; Check to make sure the data are not from the SJI.
  ;
  instrume=data->getinfo('INSTRUME')
  IF trim(instrume) EQ 'SJI' THEN BEGIN
    print,'% SPICE_RASTER_BROWSER: this routine is not compatible with slit-jaw image data. Returning...'
    IF swtch EQ 1 THEN obj_destroy,data
    return
  ENDIF

  IF NOT keyword_set(quiet) THEN BEGIN
    print,''
    print,' SPICE_RASTER_BROWSER was written by Peter Young (GMU/GSFC).'
    print,' Please report any errors to pyoung9@gmu.edu.'
    print,''
  ENDIF


  ;
  ; Check if we have an internet connection.
  ;
  net_chck=have_network()

  ;
  ; Use the HCR to get the obstitle and AR number. Note the latter comes
  ; from the metadata input by the IRIS planner.
  ;
  IF net_chck EQ 1 AND NOT keyword_set(no_hcr) THEN hcr=iris_obs2hcr(t0,t1)
  IF n_tags(hcr) NE 0 THEN BEGIN
    obstitle=hcr.obstitle
    obs_desc=hcr.goal
    noaanum=trim(hcr.noaanum)
    iris_fov=[hcr.xfov,hcr.yfov]
    iris_xcen=hcr.xcen
    iris_ycen=hcr.ycen
  ENDIF ELSE BEGIN
    obstitle=''
    obs_desc=data->getinfo('OBS_DESC')
    noaanum=''
    hcr=-1
  ENDELSE

  ;
  ; This retrieves a list of GOES flare for the observing period.
  ;
  IF net_chck EQ 1 THEN BEGIN
    flare_data=iris_hek_swpc_flares(starttime=t0,endtime=t1)
  ENDIF ELSE BEGIN
    flare_data=-1
  ENDELSE


  IF net_chck EQ 1 THEN BEGIN
    sock_list,'http://pyoung.org/iris/iris_raster_browser_check.html',page
  ENDIF


  ;
  ; I'm thinking of adding a check to see if EIS was running. TBD...
  ;
  ;; IF have_proc('eis_obs_structure') THEN BEGIN
  ;;   x0=iris_xcen-iris_fov[0]/2.
  ;;   x1=iris_xcen+iris_fov[0]/2.
  ;;   y0=iris_ycen-iris_fov[1]/2.
  ;;   y1=iris_ycen+iris_fov[1]/2.
  ;;   chck=eis_obs_structure(t0,t1,/quiet,count=count)
  ;;   IF count NE 0 THEN BEGIN
  ;;     FOR i=0,count-1 DO BEGIN
  ;;       k=where(chck[i].xcen
  ;;     ENDFOR
  ;;   ENDIF
  ;; ENDIF


  IF NOT keyword_set(quiet) THEN BEGIN
    print,'  OBSTITLE: '+obstitle
    print,'  OBS_DESC: ',data->getinfo('OBS_DESC')
    print,'  DATE_OBS: ',t0
    print,'  DATE_END: ',t1
    print,'  NOAA_NUM: ',noaanum
    IF n_elements(iris_xcen) NE 0 THEN print,'      XCEN: ',string(format='(f6.1)',iris_xcen)
    IF n_elements(iris_ycen) NE 0 THEN print,'      YCEN: ',string(format='(f6.1)',iris_ycen)
    print,format='("  Raster: ",i4," of ",i4)',data->getinfo('RASRPT'), $
      data->getinfo('RASNRPT')
    print,format='("  Roll angle: ",f6.1)',data->getinfo('SAT_ROT')
    print,''
  ENDIF



  spice_browser_widget,data,yoffsets=yoffsets, filestr=filestr, chunk_size=chunk_size, $
    retina=retina, hcr=hcr, no_goes=no_goes, flare_data=flare_data

  ;
  ; Tidy up before exiting.
  ;
  IF swtch EQ 1 THEN BEGIN
    obj_destroy,data
  ENDIF


END
