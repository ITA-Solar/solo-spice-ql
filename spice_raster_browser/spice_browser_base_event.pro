;+
; NAME:
;     spice_browser_base_event
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_base_event, event
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
        wvl=state.data->get_lambda_vector(iwin)
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
    window_list=state.data->get_window_id()
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
        ll=state.data->get_lambda_vector(iwin)
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
  ;;     id=state.data->get_window_id()
  ;;     widget_control,state.window_lbl[pwin], $
  ;;          set_value='Current window: '+trim(id[event.index-1])
  ;;    ;
  ;;    ; Choose default wavelength pixel for new window
  ;;    ;
  ;;     iwin=event.index-1
  ;;     ll=state.data->get_lambda_vector(iwin)
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
