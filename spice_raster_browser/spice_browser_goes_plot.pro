;+
; NAME:
;     spice_browser_goes_plot
;
; PURPOSE:
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_goes_plot, state
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


PRO spice_browser_goes_plot, state
  ;
  ; This plots the GOES light curve for the time period for which the
  ; EIS raster spans. It also overplots the time period for the
  ; particular exposure that the user has selected.
  ;
  g=state.goes

  IF datatype(g) EQ 'OBJ' THEN BEGIN
    data=g->getdata(/struct)
  ENDIF

  wset,state.wid_data.goes_plot_id

  IF n_tags(data) NE 0 THEN BEGIN
    pos=[0.14,0.12,0.92,0.89]
    g->plot,showclass=1,xsty=1,legend=0,position=pos,/low,/no_timestamp, $
      xtit=''

    IF state.wid_data.xpix LT state.wid_data.nx THEN BEGIN

      ti1=state.data->getti_1()
      ti2=state.data->getti_2()

      ti1=state.data->sec_from_obs_start(ti1)
      ti2=state.data->sec_from_obs_start(ti2)

      ti1_tai=state.data->ti2tai(ti1)
      ti2_tai=state.data->ti2tai(ti2)

      ti1=anytim2utc(ti1_tai)
      ti2=anytim2utc(ti2_tai)

      IF state.wid_data.rast_direct EQ 0 THEN BEGIN
        ti1=ti1[state.wid_data.nx-state.wid_data.xpix-1]
        ti2=ti2[state.wid_data.nx-state.wid_data.xpix-1]
      ENDIF ELSE BEGIN
        ti1=ti1[state.wid_data.xpix]
        ti2=ti2[state.wid_data.xpix]
      ENDELSE

      outplot,[ti1,ti1],[1e-10,1e10]
      outplot,[ti2,ti2],[1e-10,1e10]

    ENDIF

    clear_utplot
  ENDIF ELSE BEGIN
    plot,/nodata,[0,1],[0,1],xsty=4,ysty=4,xmarg=0,ymarg=0
    xyouts,0.5,0.5,align=0.5,'NO GOES DATA',charsiz=2
  ENDELSE


END
