;+
; NAME:
;     spice_browser_plot_image
;
; PURPOSE:
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_plot_image, state, pwin, ps=ps
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


PRO spice_browser_plot_image, state, pwin, ps=ps
  ;
  ; Plots the image in the upper window. Setting /ps sends the plot to a
  ; postscript file.
  ;

  iwin=state.wid_data.iwin[pwin]

  xs=state.wid_data.nx
  ys=state.wid_data.ny

  xpos=state.wid_data.xpos
  ypos=state.wid_data.ypos
  nxpos=state.wid_data.nxpos

  xpix=state.wid_data.xpix
  ypix=state.wid_data.ypix
  lpix=state.wid_data.ilambda[pwin]

  origin=state.wid_data.origin
  scale=state.wid_data.scale

  zoom=state.wid_data.im_zoom

  wset,state.wid_data.im_plot_id[pwin]

  ;
  ; This is the X-index offset used when "chunking" is being done on
  ; sit-and-stare data.
  ;
  xoffset=state.wid_data.ichunk*nxpos

  ;
  ; This loads up the image to be plotted, depending if a X-Y plot or a
  ; lambda-Y plot is to be shown.
  ;
  IF state.wid_data.im_type EQ 1 THEN BEGIN
    lam=state.data->getlam(iwin)
    nl=n_elements(lam)
    im=state.expimages[0:nl-1,*,pwin]
    i0=state.wid_data.lrange[0,pwin]
    i1=state.wid_data.lrange[1,pwin]
    j0=state.wid_data.yrange[0]
    j1=state.wid_data.yrange[1]
    im=im[i0:i1,j0:j1]
  ENDIF ELSE BEGIN
    im=state.images[*,*,pwin]
    i0=state.wid_data.xrange[0]
    i1=state.wid_data.xrange[1]
    j0=state.wid_data.yrange[0]
    j1=state.wid_data.yrange[1]
    im=im[i0:i1,j0:j1]
  ENDELSE


  ;
  ; This picks out the intensity min and max values for the
  ; display. Note that the intmin value for the lambda-Y plots will be
  ; modified later.
  ;
  IF state.wid_data.autoint[pwin] EQ 0 THEN BEGIN
    widget_control,state.min_text[pwin],get_value=intmin
    widget_control,state.max_text[pwin],get_value=intmax
    intmin=float(intmin)
    intmax=float(intmax)
  ENDIF ELSE BEGIN
    k=where(im GE 0,nk)
    IF nk EQ 0 THEN BEGIN
      intmin=0
      intmax=10
    ENDIF ELSE BEGIN
      chck=sigrange(im[k],range=range,fraction=0.99,missing=0)
      intmin=range[0]
      intmax=range[1]
    ENDELSE
    widget_control,state.min_text[pwin],set_value=trim(string(format='(f10.1)',intmin))
    widget_control,state.max_text[pwin],set_value=trim(string(format='(f10.1)',intmax))
    widget_control,state.iris_browser_base,set_uvalue=state
  ENDELSE


  IF state.wid_data.im_type EQ 1 THEN BEGIN
    ;
    ;  This is for the lambda-Y images plots
    ;
    IF keyword_set(state.wid_data.linlog) THEN BEGIN
      ;
      ; The following tries to set intmin in such a way that the color table
      ; is not wasted on noisy pixels in the background. If you need to
      ; adjust, change the number 10 in n_im/10
      ;
      k=where(im GE 0,n_im)
      IF n_im LT 100 THEN BEGIN
        intmin=1
      ENDIF ELSE BEGIN
        ks=sort(im[k])
        vals=im[k[ks[0:n_im/10]]]
        intmin=alog10(max([mean(vals),intmin,1]))
      ENDELSE
      intmax=alog10(intmax)
      ;
      im=alog10(im>0)
    ENDIF
    ;
    plot_image,im,xsty=5,ysty=5,min=intmin,max=intmax
    ;
    IF state.wid_data.velocity EQ 1 THEN BEGIN
      rlambda=state.wid_data.lambda[pwin]
      xra=[lam[i0],lam[i1]]
      xra=lamb2v(xra-rlambda,rlambda)
      axis,xaxis=0,xra=xra,xsty=1,xtitle='Velocity / km s!u-1!n'
      axis,xaxis=1,xra=xra,xsty=1,charsiz=0.0001
    ENDIF ELSE BEGIN
      xra=[lam[i0],lam[i1]]
      axis,xaxis=0,xra=xra,xsty=1,xtitle='Wavelength / A'
      axis,xaxis=1,xra=xra,xsty=1,charsiz=0.0001
    ENDELSE
    ;
    yra=ypos[[j0,j1]]
    axis,yaxis=0,yra=yra,ytit='Y / arcsec',ysty=1
    axis,yaxis=1,yra=yra,charsiz=0.00001,ysty=1
    ;
    usersym,[0,-1,1,0,1,-1,0],[0,-1,1,0,-1,1,0],th=2
    plots,(lpix-i0),(ypix-j0),psym=8,symsiz=2,col=1
    usersym,[0,0,0,0,1,-1,0],[0,-1,1,0,0,0,0],th=2
    plots,(lpix-i0),(ypix-j0),psym=8,symsiz=2
    ;
  ENDIF ELSE BEGIN
    ;
    ; This is for the X-Y images plots.
    ;
    origin=origin+[scale[0]*i0,scale[1]*j0]
    ;
    IF keyword_set(state.wid_data.linlog) THEN BEGIN
      ;
      ; The following tries to set intmin in such a way that the color table
      ; is not wasted on noisy pixels in the background. If you need to
      ; adjust, change the number 100 in n_im/100
      ;
      k=where(im GE 0)
      n_im=n_elements(im)
      ks=sort(im[k])
      vals=im[k[ks[0:n_im/100]]]
      intmin=alog10(max([mean(vals),intmin,1]))
      intmax=alog10(intmax)
      ;
      im=alog10(im>0)
    ENDIF

    IF abs(state.wid_data.roll) GE 85 THEN rswtch=1 ELSE rswtch=0
    ;
    IF state.wid_data.sit_stare EQ 1 THEN BEGIN
      IF rswtch EQ 1 THEN BEGIN
        ytitle='Time / mins'
        xtitle='Y / arcsec'
      ENDIF ELSE BEGIN
        xtitle='Time / mins'
        ytitle='Y / arcsec'
      ENDELSE
      xra=state.wid_data.tmid_min[[i0+xoffset,i1+xoffset]]
    ENDIF ELSE BEGIN
      IF rswtch EQ 1 THEN BEGIN
        ytitle='X / arcsec'
        xtitle='Y / arcsec'
      ENDIF ELSE BEGIN
        xtitle='X / arcsec'
        ytitle='Y / arcsec'
      ENDELSE
      xra=xpos[[i0,i1]]
    ENDELSE

    ;
    ; PLOT IMAGE
    ; ----------
    ;; plot_image,im,scale=scale,orig=origin, title='Zoom='+trim(2^zoom), $
    ;;            min=intmin,max=intmax, $
    ;;            xtitle=xtitle,ytitle=ytitle

    plot_image,im, title='Zoom='+trim(2^zoom), $
      min=intmin,max=intmax, xsty=5,ysty=5, $
      scale=scale
    axis,xaxis=0,xra=xra,xtit=xtitle,xsty=1
    axis,xaxis=1,xra=xra,charsiz=0.00001,xsty=1
    ;
    yra=ypos[[j0,j1]]
    axis,yaxis=0,yra=yra,ytit=ytitle,ysty=1
    axis,yaxis=1,yra=yra,charsiz=0.00001,ysty=1
    ;
    usersym,[0,-1,1,0,1,-1,0],[0,-1,1,0,-1,1,0],th=2
    plots,(xpix-i0-xoffset)*scale[0],(ypix-j0)*scale[1],psym=8,symsiz=2,col=1
    usersym,[0,0,0,0,1,-1,0],[0,-1,1,0,0,0,0],th=2
    plots,(xpix-i0-xoffset)*scale[0],(ypix-j0)*scale[1],psym=8,symsiz=2
  ENDELSE


END
