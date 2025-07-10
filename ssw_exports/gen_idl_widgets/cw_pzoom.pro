;+
; Project     : SOHO - CDS     
;                   
; Name        : CW_PZOOM()
;               
; Purpose     : One-window compound widget image zoom.
;               
; Explanation : CW_PZOOM is a general purpose image display widget that lets
;               the user zoom in and out on an (alterable) image.
;
;               The widget is crated in much the same way as a standard
;               widget_draw window, although a number of extra keywords are
;               used in order to control the appearance of the display (draw
;               window size, display window size, title, subtitle, etc.).
;
;               SETTING THE IMAGE TO BE DISPLAYED
;
;               can be done through the VALUE keyword of CW_PZOOM or through
;               the WIDGET_CONTROL SET_VALUE mechanism. The latter should only
;               be done after the widget hierarchy has been /realized. Setting
;               the value is done by:
;               
;               widget_control,CW_ID,set_value=image
;
;               where image is a two-dimensional array.
;
;               The display is automatically refreshed each time the image is
;               changed.
;
;               CONTROLLING THE COMPOUND WIDGET BEHAVIOUR 
;        
;               The appearance and behaviour of this compound widget is
;               controlled by numerous status variables. The status variables
;               can be set either through the use of keywords in the
;               CW_PZOOM() call, or by using e.g.:
;
;                    WIDGET_CONTROL,CW_ID,SET_VALUE = STRUCT
;
;               where CW_ID is the compound widget ID and STRUCT is a
;               structure with one or more tags corresponding to display
;               attributes to be altered. To set e.g., the plot TITLE to be
;               used:
;
;                   WIDGET_CONTROL,CW_ID,SET_VALUE = {TITLE:'Zoomable image'}
;
;               See the "KEYWORD DEFAULTS/EXPLANATIONS" section in the main
;               procedure for an updated list of all the status variables that
;               may be set through the use of keywords. If a keyword is not
;               marked with a "*" in the comment section, then it is also
;               possible to set this status variable through the SET_VALUE
;               mechanism.
;
;               In addition to the status variables which may be set through
;               keywords when creating the widget, there are a few special
;               status variables that can ONLY be specified through the
;               SET_VALUE mechanism:
;
;               XFOCUS  : The X index of the focus pixel
;               YFOCUS  : The Y index of the focus pixel
;               ZOOM    : The zoom factor. Zoom=1 means all data visible.
;               REPLOT  : Setting this to 1 causes the display to
;                         be refreshed, after which it will be cleared.
;               RESCALE : Setting this to 1 causes the color scaling to be
;                         performed and the display to be refreshed, after
;                         which it will be cleared.  Should be set when
;                         acknowledging an XTVSCALE event.
;                         
;               REPLOT_CROSS : Setting this to 1 causes the crosshair to
;                              be redrawn, without redisplaying the image.
;
;               NO REFRESH IS PERFORMED by set_value when altering
;               attributes WITHOUT setting THE REPLOT ATTRIBUTE
;
;               EVENT HANDLING
; 
;               CW_PZOOM can be used in a "dumb" mode through the use of the
;               /AUTONOMOUS keyword when creating the widget.  This causes the
;               widget to acknowledge and gobble up all zooming/refocusing
;               events, refreshing the display without passing on events to
;               the parent base.
;               
;               Unless the /AUTONOMOUS keyword is set, all WIDGET_DRAW event
;               are processed in the event handler into an "action string", by
;               default:
;
;               "ZOOM-" for ev.press eq 1 (left button)
;               "ZOOMP" for ev.press eq 2 (middle button)
;               "ZOOM+" for ev.press eq 4 (right button)
;
;               and
;               
;               "IGNORE" for any other widget_draw event.
;
;               The status variable IGNORE_ACTION contains a string with
;               a list of action texts to ignore, in the format:
;
;                   "(IGNORE)(MOTION)(RELEASE1)(RELEASE2)(RELEASE3)"
;
;               If an event is translated into an action string that appears
;               in the IGNORE_ACTION list, the event is gobbled up and
;               ignored.
;
;               If you'd like the user to be in control of what buttons to
;               use, you can use a CW_MOUSE widget (see keyword CW_MOUSE, and
;               the documentation of CW_MOUSE()).  The CW_MOUSE actions should
;               be:
;
;               "ZOOM+" for zooming in (no repointing)
;               "ZOOM-" for zooming out (no repointing)
;               "ZOOMP" for repointing (or anything else not on the
;                       IGNORE_ACTION list)
;
;               For ZOOM+/ZOOM- actions, the new ZOOM factor is calculated,
;               but the pixel coordinates of the button press is ignored.  For
;               any other action not to be ignored, the event coordinates are
;               converted into image pixel coordinates (new XFOCUS/ YFOCUS).
;
;               After this, a {CW_PZOOM_EVENT} or {CW_PZOOM_XTVSCALE_EVENT}
;               structure is created, and passed on to the owner of the
;               compound widget. Note that NO SCREEN OR STATUS UPDATES have
;               been done at this stage (see "acknowledging events" below).
;
;               The two possible event structures generated by CW_PZOOM
;               consist of the following tags:
;
;               ID       : Widget ID of cw_pzoom
;               TOP      : Top widget ID.
;               HANDLER  : Handler widget ID
;               XTVSCALE : Signals an XTVSCALE event when nonzero (i.e.,
;                          the event originates from XTVSCALE, not from the
;                          widget_draw window). 
;               SET      : A structure with new information generated
;                          by the user pressing a mouse button. The
;                          tags of this structure {CW_PZOOM_SET} are:
;
;                   XFOCUS : X index of new/current selected pixel.
;                   YFOCUS : Y index of new/current selected pixel.
;                   ZOOM   : The new/current zoom factor.
;                   RESCALE : Set to one when an XTVSCALE event occurred
;                   REPLOT : Always has a value of 1
;
;               OLD      : Contains the same tags as SET, but with
;                          the old (currently displayed) values.
;
;               PLOTREG : The Plot Region that has been used to
;                         display the image. Useful for overplotting
;                         after executing PRESTORE,EVENT.PLOTREG
;
;               EVENT   : The original WIDGET_DRAW event (for
;                         {CW_PZOOM_EVENT}) or an XTVSCALE_EVENT (for
;                         {CW_PZOOM_XTVSCALE_EVENT}.
;
; 
;               ACKNOWLEDGING EVENTS
;
;               In order to acknowledge the event to make the user changes
;               effective, all that has to be done is to use:
;
;                    WIDGET_CONTROL,EV.ID,SET_VALUE=EV.SET
;
;               this is in fact all that is done when the autonomous mode is
;               used.
;
;               OVERPLOTTING THE DISPLAY
;
;               Since the acknowledgement of a zoom/repointing event, or
;               changing the displayed data causes the display to be updated,
;               overplotting should be done AFTER setting the focus and/or
;               zoom values with REPLOT set to 1. Replotting changes the
;               current data coordinate system so overplotting may be done in
;               data coordinates.
;
;               Useful entities for overplotting can be retrieved through a
;               call to WIDGET_CONTROL,CW_ID,GET_VALUE=STATUS, where CW_ID is
;               the CW_PZOOM widget ID, and STATUS will be returned as a
;               structure with the following tags {CW_PZOOM_VALUE}:
;
;               VALUE  : HANDLE that points to the data (image) 
;                        Note that the data must not be removed or
;                        altered directly!
;               CLIP   : 4-element array with the indices of the lower
;                        left and upper right pixels that are
;                        displayed.  Note that the display may cover
;                        pixels outside the actual data array.
;               XFOCUS : The X index of the focus pixel
;               YFOCUS : The Y index of the focus pixel
;               PLOTREG: The plot region used to display the image.
;
;
;               A note on XTICKS:
;               
;               The use of XTICKS is somewhat tricky. The standard IDL way of
;               interpreting this keyword normally gives nonsensical results,
;               so CW_PZOOM tries to make the tickmarks fall on the center of
;               the pixels (which looks good for _some_ types of data). It is
;               also possible to use the function TICK_VEC by setting XTICKS
;               to a negative value. TICK_VEC tries to do a decent job of
;               placing UP TO the given number of tickmarks on the
;               display. Try it.
;               
;               
; Use         : PZOOM = CW_PZOOM(BASE [,KEYWORDS])
;    
; Inputs      : BASE : The base to put the draw window on.
;               
; Opt. Inputs : None.
;               
; Outputs     : Returns the widget ID of the compound widget.
;               
; Opt. Outputs: None.
;               
; Keywords    : Too many to justify updating a separate list. See the KEYWORD
;               DEFAULTS section inside the routine.
;
; Calls       : cdscongrid(), clipbox, copy_tag_values, cw_pzoom_plot,
;               cw_pzoom_scaleval, datatype(), default, handle_killer_hookup,
;               parcheck, pconvert(), prestore, pstore(), tick_vec(), typ(),
;               xtvscale()
;
; Common      : None.
;               
; Restrictions: Probably too many to specify.
;               
; Side effects: Updating the !P,!X,!Y system variables when refreshing the
;               display.
;               
; Category    : Utility, Image.
;               
; Prev. Hist. : Long.
;
; Written     : Stein Vidar Hagfors Haugan, UiO, 13 June 1996
;               
; Modified    : Version 2, 16 June 1996
;                       Clarified the problem with having two different
;                       event structures, and specified this explicitly
;                       in the explanation section.
;               Version 3, SVHH, 1 July 1996
;                       Added /CLEAN switch to PSTORE(), to delete any
;                       previous use of the same window number.
;               Version 4, SVHH, 29 May 1997
;                       Modified grabbing procedure to save time when zoom<1,
;                       and removed common block as cache. Using
;                       handle_killer_hookup instead of cleanup routine.
;               Version 5, SVHH, 15 September 1997
;                       Modified automatic handling of max-zoom limit to make
;                       more sense. 
;                       
; Version     : 5, 15 September 1997
;-            


;
; Scaling the image into a byte array
;

PRO cw_pzoom_scaleval,info
  
  handle_value,info.value,value,/no_copy
  
  use_xtvscale = info.ext.xtvscale GE 0L
  
  IF use_xtvscale THEN BEGIN
     scaledval = xtvscale(info.ext.xtvscale,value, $
                          missing=missing,color_missing=color_missing)
     
     info.ext.missing = missing
     info.ext.color_missing = color_missing
     
  END ELSE BEGIN
     
     good = value NE info.ext.missing
     
     ixgood = WHERE(good,count_good)
     ixbad = WHERE(good-1b,count_bad)
     
     IF count_bad EQ 0 THEN BEGIN
        ;; All good
        scaledval = byte(!D.TABLE_SIZE* $
                         (VALUE-MIN(VALUE))/(MAX(VALUE)-MIN(VALUE)))
     END ELSE IF count_good EQ 0 THEN BEGIN
        ;; All bad
        scaledval = REPLICATE(byte(info.ext.color_missing), $
                              info.int.asize(0),info.int.asize(1))
     END ELSE BEGIN
        ;; Some good, some bad.
        maxdata = MAX(value(ixgood))
        mindata = MIN(value(ixgood))
        scaledval = byte(!D.table_size* $
                         (value-mindata)/(maxdata-mindata))
        scaledval(ixbad) = info.ext.color_missing
     END
  END
  
  handle_value,info.value,value,/set,/no_copy
  
  handle_value,info.scaledv,scaledval,/set,/no_copy
  info.ext.rescale = 0
END


;
; Update display
;
PRO cw_pzoom_plot,info
  
  ;; KEEP !X, !Y, !P
  OLD_X = !X
  OLD_Y = !Y
  OLD_P = !P
  
  ;; Rescale colors if necessary 
  IF info.ext.rescale NE 0 THEN BEGIN
     cw_pzoom_scaleval,info
  END
  
  handle_value,info.scaledv,scaledval,/no_copy
  handle_value,info.im_h,im,/no_copy
  
  ;; Making the array for the display image
  
  IF NOT info.ext.squarepix THEN BEGIN
     ;; Relative physical size of pixels is given by SCALE
     x_bin = DOUBLE(info.ext.scale(0))/MIN(info.ext.scale)  ;; binfactors 
     y_bin = DOUBLE(info.ext.scale(1))/MIN(info.ext.scale)
  END ELSE BEGIN
     ;; Pixels to be treated as square
     x_bin = 1.0D
     y_bin = 1.0D
  END
  
  ;; Data size (could've used info.int.asize..)
  
  dsz = SIZE(scaledval)
  
  IF info.ext.stretch THEN BEGIN
     ;; The size of the grabbed box is calculated from the
     ;; (array) size of the supplied image, and then 
     ;; stretched to fit the display size.
     
     ;; ZOOM == 1 means displayed image should contain all pixels
     ;; in both directions (but not necessarily shown whith those
     ;; proportions -- depends on xdsize/ydsize).
     ;; 
     ;; The SCALE factors thus have no influence on the pixel sizes
     xnpix = round(dsz(1)/info.ext.zoom)
     ynpix = round(dsz(2)/info.ext.zoom)
  END ELSE BEGIN
     
     ;; If non-stretch mode is set, the grab box size is calculated as to
     ;; avoid deformation when rebinning to display size.
     
     ;; This enables the possibility of having a viewing window that does
     ;; *not* have the same proportional size as the data being viewed.
     
     ;; When zoom is equal to 1, all data should fit into the display.
     ;; The scaling factor must take into account the ratios of physical
     ;; image sizes to display sizes, and make sure that the dimension with
     ;; the larger ratio fits at zoom=1.
     ;; 
     
     ;; Relative physical sizes of the two data dimensions
     phys_sizes = dsz(1:2)*[x_bin,y_bin]
     
     ;; Ratios -- this is how much physical size we must grab for each
     ;; display pixel in each direction in order to make exact fits
     ;; for each separate dimension
     
     phys_to_disp = phys_sizes/info.int.dsize
     
     ;; The displayed pixels are assumed square, so to get a physical
     ;; area with height/width ratio equal to the height/width of
     ;; the display, the physical grab size is a mutiple of the
     ;; display size in each dimension. We multiply with the
     ;; maximum phys_to_disp ratio to get the most tight dimension
     ;; to fit at zoom=1.
     
     get_size = [info.int.dsize]*MAX(phys_to_disp)/info.ext.zoom
     
     ;; Conversion of physical grab sizes to pixel sizes:
     xnpix = round(get_size(0)/x_bin)
     ynpix = round(get_size(1)/y_bin)
  END
  
  IF info.ext.zoom GT 1 THEN dsz(1:2) = [xnpix,ynpix] $
  ELSE                       dsz(1:2) = [info.int.dsize]
  
  IF info.ext.replot THEN BEGIN
     
     ;; Since we're using scaled values we only need byte values
     
     dsz(3) = 1
     dsz(4) = dsz(1)*dsz(2)
     
     back = info.ext.background
     
     IF back EQ -1L THEN back = !P.background
     
     IF total(SIZE(im) EQ dsz) NE 5 THEN BEGIN
        ;; Free space
        im = 0
        im = make_array(SIZE = dsz,value=back)
     END ELSE BEGIN
        im(*,*) = back
     END
     
  END
  
  ;; Renormalizing focus coordinates (just checking)
  
  info.ext.xfocus = (info.ext.xfocus > 0) < (info.int.asize(0)-1)
  info.ext.yfocus = (info.ext.yfocus > 0) < (info.int.asize(1)-1)
  
  glued = info.ext.zoom le 1.0 
  
  ;; Finding the grab box
  
  startx = (info.ext.xfocus-xnpix/2)
  starty = (info.ext.yfocus-ynpix/2)
  
  ;; Centered view -- all or more than all data visible
  
  IF glued THEN BEGIN
     startx = -FIX((xnpix-info.int.asize(0))*0.5)
     starty = -FIX((ynpix-info.int.asize(1))*0.5)
  END
  
  
  IF info.ext.zoom EQ 2 THEN BEGIN
     ;; A compromize between glued/focus-centered position
     startx = round((2.0*startx - (xnpix-info.int.asize(0))*0.5)/3.0)
     starty = round((2.0*starty - (ynpix-info.int.asize(1))*0.5)/3.0)
  END
  
  
  offsetx = 0
  offsety = 0
  
  IF startx LT 0 THEN BEGIN
     offsetx = -startx
     startx = 0
  ENDIF
  
  IF starty LT 0 THEN BEGIN
     offsety = -starty
     starty = 0
  ENDIF
  
  IF info.ext.zoom LE 1 THEN BEGIN ;; All data visible...
     xsize = round(float(info.int.asize(0))*info.int.dsize(0)/xnpix + 0.25)
     ysize = round(float(info.int.asize(1))*info.int.dsize(1)/ynpix + 0.25)
     
     xstart = round(float(offsetx)*info.int.dsize(0)/xnpix)
     ystart = round(float(offsety)*info.int.dsize(1)/ynpix)
     im(xstart,ystart) = cdscongrid(scaledval,xsize,ysize)
  END
  
  endx = (startx+xnpix-1-offsetx) < (info.int.asize(0)-1)
  endy = (starty+ynpix-1-offsety) < (info.int.asize(1)-1)
  
  IF info.ext.zoom GT 1 THEN BEGIN 
     IF info.ext.replot THEN BEGIN
        im(offsetx,offsety) = scaledval(startx:endx,starty:endy) ;; grab
        plim = cdscongrid(im,info.int.dsize(0),info.int.dsize(1))
     END
  END ELSE BEGIN
     plim = temporary(im) ;; No memory copying
  END
  
  INFO.int.zero = [startx-offsetx,        starty-offsety,$
                   startx-offsetx+xnpix-1,starty-offsety+ynpix-1]
  
  IF info.ext.replot THEN BEGIN
     !X.range = (info.ext.origin(0) + $
                 info.ext.scale(0)*(info.int.zero(0)+[-0.5,xnpix-0.5]))
     !Y.range = (info.ext.origin(1) + $
                 info.ext.scale(1)*(info.int.zero(1)+[-0.5,ynpix-0.5]))
     
     position = [info.ext.origo,info.ext.origo + info.int.dsize-1]+0.00001
     origo = info.ext.origo
     
     IF !d.name EQ 'PS' THEN BEGIN
        !P.noerase = 1
        position = [!P.clip(0),!p.clip(1),!P.clip(2),!P.clip(3)]
        xsize = !P.clip(2)-!P.clip(0)
        ysize = !P.clip(3)-!P.clip(1)
        origo = !P.clip(0:1)
     END ELSE BEGIN
        xsize = 0
        ysize = 0
        wset,info.int.tv
     END
     
     blank = STRING(255b)

     ;; Shorthand
     e = info.ext
     
     IF e.background NE -1L THEN !P.background = e.background
     IF e.charsize NE -1.0 THEN !P.charsize = e.charsize
     IF e.xcharsize NE -1.0 THEN !X.charsize = e.xcharsize
     IF e.ycharsize NE -1.0 THEN !Y.charsize = e.ycharsize
     IF e.charthick NE -1.0 THEN !P.charthick = e.charthick
     IF e.color NE -1L THEN !P.color = e.color
     IF e.font NE -2L THEN !P.font = e.font
     IF e.xgridstyle NE -1L THEN !X.gridstyle = e.xgridstyle
     IF e.ygridstyle NE -1L THEN !Y.gridstyle = e.ygridstyle
     IF e.xstyle NE -1L THEN !X.style = e.xstyle
     IF e.ystyle NE -1L THEN !Y.style = e.ystyle
     IF e.subtitle NE blank THEN !P.subtitle = e.subtitle
     IF e.xthick NE -1.0 THEN !X.thick = e.xthick
     IF e.ythick NE -1.0 THEN !Y.thick = e.ythick
     IF e.xtickformat NE blank THEN !X.tickformat = e.xtickformat
     IF e.ytickformat NE blank THEN !Y.tickformat = e.ytickformat
     IF e.xticks NE 0 THEN !X.ticks = e.xticks
     IF e.yticks NE 0 THEN !Y.ticks = e.yticks
     IF e.ticklen NE 0.0 THEN !P.ticklen = e.ticklen
     IF e.xticklen NE 0.0 THEN !X.ticklen = e.xticklen
     IF e.yticklen NE 0.0 THEN !Y.ticklen = e.yticklen
     IF e.title NE blank THEN !P.title = e.title
     IF e.xtitle NE blank THEN !X.title = e.xtitle
     IF e.ytitle NE blank THEN !Y.title = e.ytitle
     
     ;; Xticks are special
     IF e.xticks LT 0 THEN BEGIN
        xtickv = tick_vec(-info.ext.xticks, $
                          !X.range(0),!X.range(1), $
                          subticks=xminor)
        !x.ticks = N_ELEMENTS(xtickv)-1
        !X.tickv = xtickv
        !X.minor = xminor
     END ELSE IF info.ext.xticks GT 0 THEN BEGIN
        ;; Try to put the xticks on the center of pixels
        nticks = (info.ext.xticks+1) < xnpix
        !X.ticks = nticks-1
        
        xticki = (LINDGEN(nticks)*xnpix)/nticks + xnpix/(2*nticks)
        !X.tickv = (info.ext.origin(0) + $
                    info.ext.scale(0)*(xticki+info.int.zero(0)))
        !X.minor = 0
     END
     
     
     !X.style = 1 OR !x.style
     !y.style = 1 OR !y.style
     
     ;; Plotting
     ;;
     plot,[0],/nodata,position=position,/DEVICE
     tv,plim,origo(0),origo(1),xsize=xsize,ysize=ysize,/DEVICE 
     clipbox
     
     
     ;; Store coordinate system etc.
     ;;
     OLD_P.clip = !P.clip
     OLD_P.position = !P.position
     
     OLD_X.window = !X.window
     OLD_X.type = !X.type
     OLD_X.crange = !X.crange
     OLD_X.s = !X.s
     
     OLD_Y.window = !Y.window
     OLD_Y.type = !Y.type
     OLD_Y.crange = !Y.crange
     OLD_Y.s = !Y.s
     
     !P = OLD_P
     !X = OLD_X
     !Y = OLD_Y
     
     dummy = pstore(1,xnpix,ynpix,/clean)
     
     IF !D.name NE 'PS' THEN info.int.preg = dummy

     info.ext.replot_cross = 1
  END ELSE BEGIN
     dummy = info.int.preg ;; Replot CROSS?
  END
  
  IF info.ext.zoom LE 1 THEN handle_value,info.im_h,plim,/set,/no_copy $
  ELSE                       handle_value,info.im_h,im,/set,/no_copy
  
  prestore,dummy
  
  ;;
  ;; Plot crosshair
  ;;
  
  ;;
  ;; Fractional pixel errors in the display
  ;;
  xfracp = round(0.49999d*xnpix/double(info.int.dsize(0)))
  yfracp = round(0.49999d*ynpix/double(info.int.dsize(1)))
  
  xx = pconvert(dummy,info.ext.xfocus-info.int.zero(0)+xfracp,$
                /pixel,/to_device)
  yy = pconvert(dummy,info.ext.yfocus-info.int.zero(1)+yfracp,$
                /pixel,/to_device,/Y)
  xx = round(xx)
  yy = round(yy)
  
  IF info.ext.replot_cross AND info.ext.cross GT 0.0 THEN BEGIN
     cross = info.ext.cross
     IF !D.name NE 'PS' THEN BEGIN
        DEVICE,get_graphics_function=oldgraph
        DEVICE,set_graphics_function=info.ext.cross_graph
     END ELSE BEGIN
        ;; Expand cross /DEVICE size corresponding to the change
        ;; in device clip size for POSTSCRIPT output
        cross = cross* $
           MIN([(!P.clip(2)-!P.clip(0))/FLOAT(info.int.dsize(0)), $
                (!P.clip(3)-!P.clip(1))/FLOAT(info.int.dsize(1))])
     END
     color = info.ext.cross_color
     IF color EQ -1L THEN color = info.ext.color
     IF color EQ -1L THEN color = !P.color
     PLOTS,[xx,xx],yy+cross*[-1,1],/DEVICE,color=color
     PLOTS,xx+cross*[-1,1],[yy,yy],/DEVICE,color=color
     IF !D.name NE 'PS' THEN BEGIN
        DEVICE,set_graphics_function=oldgraph
     END
  END
  
  handle_value,info.scaledv,scaledval,/set,/no_copy
  info.ext.replot = 0
  info.ext.replot_cross = 0
  
  ;; KEEP !X, !Y, !P
  !x = OLD_X
  !Y = OLD_Y
  !P = OLD_P
END


;
; Setting the compound widget value
;

PRO cw_pzoom_setv,ID,VALUE
  
  store = WIDGET_INFO(id,/child)
  WIDGET_CONTROL,store,get_uvalue=info,/no_copy
  
  IF datatype(value) NE 'STC' THEN BEGIN
     ;;
     ;; Setting the image
     ;;
     
     IF (SIZE(value))(0) NE 2 THEN  $
        MESSAGE,"CW_PZOOM value must be a two-dimensional array"
     
     ;; Remove old values
     handle_value,info.value,oldvalue,/no_copy
     handle_value,info.scaledv,oldscaled,/no_copy
     
     ;; Set the new value
     HANDLE_VALUE,info.value,value,/set
     
     ;; Update array size
     info.int.asize = (SIZE(value))(1:2)
     
     nold = N_ELEMENTS(oldvalue)
     IF nold EQ 4 OR nold EQ 0 THEN BEGIN
        ;; Previous image was the test value
        info.ext.xfocus = info.int.asize(0)/2
        info.ext.yfocus = info.int.asize(1)/2
     END
     
     info.ext.rescale = 1
     
  END ELSE BEGIN
     
     ext = info.ext
     copy_tag_values,ext,value
     info.ext = ext
     
  END
  
  ;; Is a rescale in order?
  
  IF info.ext.rescale NE 0 THEN BEGIN
     cw_pzoom_scaleval,info
     info.ext.replot = 1
  END
  
  ;; Is any replotting in order?
  
  IF info.ext.replot GT 0 OR info.ext.replot_cross GT 0 THEN  $
     cw_pzoom_plot,info
  
  ;; Store updated info structure 
  WIDGET_CONTROL,store,set_uvalue=info,/no_copy
END


;
; Returning some useful values.
;
FUNCTION cw_pzoom_getv,ID

  store = WIDGET_INFO(id,/child)
  WIDGET_CONTROL,store,get_uvalue=info,/no_copy
  
  value = {CW_PZOOM_VALUEz,$
           VALUE  : info.value,$
           CLIP   : info.int.zero,$
           XFOCUS : info.ext.xfocus,$
           YFOCUS : info.ext.yfocus,$
           ZOOM : info.ext.zoom,$
           PLOTREG: info.int.preg}
  
  WIDGET_CONTROL,store,set_uvalue=info,/no_copy
  
  RETURN,value
END
  
;
; EVENT handling.
;
; Most events will be draw events, but XTVSCALE events are also
; possible.
;
FUNCTION cw_pzoom_event,ev

  ;; Default place to find the info structure
  ;; 
  STORAGE = EV.ID
  
  WIDGET_CONTROL,STORAGE,get_uvalue=info,/no_copy
  
  ;; Assume a widget_draw event until proven otherwise
  ;; 
  xtvscale = 0
  auto = 0
  
  ;; But it might be an XTVSCALE event
  
  IF datatype(info) EQ 'STR' THEN BEGIN
     ;;
     ;; Put back 'XTVSCALE' string
     ;; 
     WIDGET_CONTROL,ev.id,set_uvalue=info
     ;;
     ;; Recheck it.
     ;; 
     IF info EQ 'XTVSCALE' THEN BEGIN
        ;;
        ;; We need to know the info-keeper
        ;; 
        STORAGE = WIDGET_INFO(ev.handler,/child)
        WIDGET_CONTROL,STORAGE,get_uvalue=info,/no_copy
        ;;
        ;; Ignore non-selected xtvscale events
        ;; 
        IF ev.xtvscale_id NE info.ext.xtvscale THEN BEGIN
           PRINT,"Ignoring alien XTVSCALE event"
           GOTO,NO_EVENT
        END
        ;;
        ;; These are needed to generate the "set" structure.
        ;; 
        xtvscale = 1
        xpix = info.ext.xfocus
        ypix = info.ext.yfocus
        zoom = info.ext.zoom
        
        ;; Return event to main program
        GOTO,MAKE_EVENT
     END
  END
  
  ;; If we get here, it's a draw-window event.
  ;; 
  
  IF info.int.cw_mouse NE -1 THEN BEGIN
     action = cw_tmouse(info.int.cw_mouse,ev)
  END ELSE $
     CASE ev.press OF 
     1: action = 'ZOOM-'
     2: action = 'ZOOMP'
     4: action = 'ZOOM+'
     ELSE: action = 'IGNORE'
  END

  ;; These are default/old values
  ;; 
  xpix = info.ext.xfocus
  ypix = info.ext.yfocus
  zoom = info.ext.zoom
  
  ;; No event unless proven otherwise
  ;; 
  evnt = 0
  
  ;; No autonomous mode until proven otherwise
  ;; 
  auto = 0
  
  ;;
  ;; If the action is on the ignore_action list we should (guess what)
  ;; ignore it.
  ;;
  
  IF STRPOS(info.ext.ignore_action,'('+action+')') GT -1 THEN GOTO,NO_EVENT
  
  CASE action OF 
     
     'ZOOM-': BEGIN
        IF zoom EQ info.ext.minzoom THEN GOTO,NO_EVENT
        zoom = (zoom/2) > info.ext.minzoom
        IF zoom GE 1 THEN zoom = round(zoom)
     END
     
     'ZOOM+': BEGIN
        maxzoom = info.ext.maxzoom
        IF maxzoom EQ 0 THEN BEGIN
           npixels_displayed = [info.int.zero(2)-info.int.zero(0)+1,$
                                info.int.zero(3)-info.int.zero(1)]
           IF min(npixels_displayed) LT 3 THEN maxzoom = info.ext.zoom $
           ELSE                                maxzoom = 2*info.ext.zoom
        END
        
           
        IF zoom EQ maxzoom THEN GOTO,NO_EVENT
        zoom = (zoom*2) < maxzoom 
        IF zoom GE 1 THEN zoom = round(zoom)
     END
     
     ELSE: BEGIN
        ;; *NOT* a zoom in/out event, we should report new coordinates
        
        ;; Find the plot region
        preg = pfind(ev,found)
        
        IF NOT found THEN BEGIN
           ;; Outsize clip region, incremental positioning.
           prestore,info.int.preg
           xpix = info.ext.xfocus
           ypix = info.ext.yfocus
           IF ev.x LT !P.clip(0) THEN xpix = xpix-1
           IF ev.x GT !P.clip(2) THEN xpix = xpix+1
           IF ev.y LT !P.clip(1) THEN ypix = ypix-1
           IF ev.y GT !P.clip(3) THEN ypix = ypix+1
        END ELSE BEGIN
           ;; Inside clip region, convert to pixels
           xpix = FIX(pconvert(preg,ev.x,/dev,/to_pix))
           ypix = FIX(pconvert(preg,ev.y,/dev,/to_pix,/Y))
           xpix = xpix+info.int.zero(0)
           ypix = ypix+info.int.zero(1)
        END
     END
     
  ENDCASE 
  
MAKE_EVENT:
  
  preg = info.int.preg
  
  set = {CW_PZOOM_SET,$
         XFOCUS : xpix,$
         YFOCUS : ypix,$
         ZOOM   : FLOAT(zoom),$
         REPLOT : 1,$
         RESCALE : (XTVSCALE NE 0)}
  
  old = {CW_PZOOM_SET,$
         XFOCUS : info.ext.xfocus,$
         YFOCUS : info.ext.yfocus,$
         ZOOM   : info.ext.zoom,$
         REPLOT : 0, $
         RESCALE : 0B}
  
  IF xtvscale THEN $
     evnt = {CW_PZOOM_XTVSCALE_EVENT,$
             ID:ev.handler,$
             TOP:ev.top,$
             HANDLER:0L,$
             XTVSCALE : XTVSCALE,$ ; Signal a rescaling event
             SET : set,$
             OLD : old,$
             PLOTREG:PREG,$
             EVENT:EV} $
  ELSE  $
     evnt = {CW_PZOOM_EVENT,$
             ID:ev.handler,$
             TOP:ev.top,$
             HANDLER:0L,$
             XTVSCALE : XTVSCALE,$ ; Signal a rescaling event
             SET : set,$
             OLD : old,$
             PLOTREG:PREG,$
             EVENT:EV}
     
  ;; Keeping it all inside the array
  
  evnt.set.xfocus = (evnt.set.xfocus > 0) < (info.int.asize(0)-1)
  evnt.set.yfocus = (evnt.set.yfocus > 0) < (info.int.asize(1)-1)
  
  ;; Autonomous mode?
  auto = info.int.auto
  
NO_EVENT:
  
  ;; NO replotting so far, just store info structure
  
  WIDGET_CONTROL,STORAGE,set_uvalue=info,/no_copy
  
  ;; Autonomous mode means always acknowledge any event and
  ;; gobble it up.
  ;; 
  IF auto AND datatype(evnt) EQ 'STC' THEN BEGIN
     WIDGET_CONTROL,ev.handler,set_value=evnt.set
     RETURN,0
  END
  
  RETURN,evnt
END


;; This routine is called once, upon realization of the widget
;; Display the value. Make one up if it's not set.
;;
PRO cw_pzoom_realize,ID
  
  STORAGE = WIDGET_INFO(ID,/CHILD) 
  WIDGET_CONTROL,STORAGE,get_uvalue = info,/no_copy
  
  ;; Get the tv window id.
  ;; 
  WIDGET_CONTROL,info.int.draw,get_value = tv
  info.int.tv = tv
  
  ;; Check the value
  ;; 
  handle_value,info.value,value,/no_copy
  
  ;; Make dummy value if none set.
  IF N_ELEMENTS(value) EQ 0 OR (SIZE(VALUE))(0) NE 2 OR $
     datatype(value) EQ 'COM' OR datatype(value) EQ 'STC' THEN BEGIN
     value = [ [1,0],$
               [0,1]]
  END
  
  ;; Store array size
  ;; 
  info.int.asize = (SIZE(value))(1:2)
  
  ;; Initial position
  ;; 
  info.ext.xfocus = info.int.asize(0)/2
  info.ext.yfocus = info.int.asize(1)/2

  ;; Store value and info structure
  ;; 
  HANDLE_VALUE,info.value,value,/set,/no_copy
  WIDGET_CONTROL,storage,set_uvalue=info,/no_copy
  
  ;; Force a redisplay
  ;; 
  WIDGET_CONTROL,id,set_value = {rescale:1}
END


;; 48 keywords ... !

FUNCTION cw_pzoom,on_base,  $
                  value=value, $
                  xwsize=xwsize, ywsize=ywsize,$
                  xdsize=xdsize, ydsize=ydsize, origo=origo,$
                  uvalue=uvalue, no_copy=no_copy, $
                  cw_mouse=cw_mouse,motion_events=motion_events,$
                  $
                  $;; PLOT  keywords:
                  $
                  background=background, charsize=charsize, $
                  xcharsize=xcharsize, ycharsize=ycharsize, $
                  charthick=charthick, color=color, font=font, $
                  xgridstyle=xgridstyle, ygridstyle=ygridstyle,  $
                  xstyle=xstyle, ystyle=ystyle, subtitle=subtitle,  $
                  xthick=xthick,ythick=ythick,  $
                  xtickformat=xtickformat, ytickformat=ytickformat, $
                  xticks=xticks, yticks=yticks, ticklen=ticklen,  $
                  xticklen=xticklen, yticklen=yticklen, title=title, $
                  xtitle=xtitle, ytitle=ytitle, $
                  $
                  $;; Data coordinate system:
                  $
                  origin=origin, scale=scale, $
                  squarepix=squarepix,stretch=stretch, $
                  $
                  $;; Color scaling
                  $
                  xtvscale=xtvscale,missing=missing, $
                  color_missing=color_missing,$
                  $
                  $;; Limitations
                  $
                  maxzoom=maxzoom,minzoom=minzoom,$
                  $
                  $;; Crosshair
                  $
                  cross=cross,cross_color=cross_color, $
                  cross_graph=cross_graph,$
                  $
                  $;; Miscellaneous
                  $
                  autonomous=autonomous,  $
                  ignore_action=ignore_action
;+  
;
; KEYWORD DEFAULTS/EXPLANATIONS
;
; All keywords not marked with * may be altered after widget creation
; through the
;   WIDGET_CONTROL,CW_ID,SET_VALUE={<KEYWORD_NAME>:<keyword_value>}
; mechanism.
  
; Widgety things
  
  default,xwsize,200            ;* Widget_draw xsize
  default,ywsize,200            ;* Widget_draw ysize
  default,xdsize,140            ;* Display area xsize
  default,ydsize,140            ;* Display area ysize
  default,origo,[50,40]         ; Origin of display area (pixels)
  default,uvalue,'CW_PZOOM'     ;*
  default,no_copy,0             ;* For setting uvalue of this Compound widget
  default,CW_MOUSE,-1L          ;* Compound widget mouse control box. 
  default,motion_events,0       ;* Make the WIDGET_DRAW return motion events
  
;  
; Standard PLOT keywords
;
  blank = STRING(255b)
  
  default,background,-1L        ; Use !P.background
  default,charsize,-1.0         ; Use !P.charsize
  default,xcharsize,-1.0        ; Use !X.charsize
  default,ycharsize,-1.0        ; Use !Y.charsize
  default,charthick,-1.0        ; Use !P.charthick
  default,color,-1L             ; Use !P.color
  default,font,-2L              ; Use !P.font
  default,xgridstyle,-1L        ; Use !X.gridstyle
  default,ygridstyle,-1L        ; Use !Y.gridstyle
  default,xstyle,-1L            ; Use !X.style
  default,ystyle,-1L            ; Use !Y.style
  default,subtitle,blank        ; Use !P.subtitle
  default,xthick,-1.0           ; Use !X.thick
  default,ythick,-1.0           ; Use !Y.thick
  default,xtickformat,blank     ; Use !X.tickformat
  default,ytickformat,blank     ; Use !Y.tickformat
  default,xticks,0              ; Use !X.ticks
  default,yticks,0              ; Use !Y.ticks
  default,ticklen,-0.05         ; 0.0 Means use !P.ticklen
  default,xticklen,0.0          ; Use !X.ticklen
  default,yticklen,0.0          ; Use !Y.ticklen
  default,title,blank           ; Use !P.title
  default,xtitle,blank          ; Use !X.title
  default,ytitle,blank          ; Use !Y.title
  
; Data coordinate system  
  default,origin,[0.0,0.0]      ; x,y coordinates of pixel [0,0]
  default,scale,[1.0,1.0]       ; x,y size of pixels, physical units
  default,squarepix,0           ; Ignore phys. pixel size (treat them square)
  default,stretch,0             ; Ign. phys. pix. size, stretch to fit display
  
; Color scaling
  default,xtvscale,-1L          ; Color scaling.
  default,missing,-1.0D         ; Missing value.
  default,color_missing,0B      ; Color used for missing valued pixels
  
; Limitations
  default,maxzoom,0             ; Max. zoom, 0 means the sensible limit.
  default,minzoom,0.5           ; Min. zoom -how much overwiew could you want!
  
; Crosshair
  default,cross,30              ; Crosshair size, pixels
  default,cross_color,!P.color  ; Crosshair color
  default,cross_graph,3         ; Crosshair graphics function

; Miscellaneous
  default,autonomous,0          ;* Auto-redisplay.
  
; Ignore these ACTIONs:
  default,ignore_action,'(IGNORE)(MOTION)(RELEASE1)(RELEASE2)(RELEASE3)'
;-  
  
;;
;; Once more: Check variable types
;;
  keyword = 0
  scalar = 0
  real = typ(/rea)
  natural = typ(/nat)
  strng = typ(/str)
  lng = typ(/lon)
  
; Widgety things
  parcheck,xwsize,       keyword, real,   scalar,'XWSIZE'
  parcheck,ywsize,       keyword, real,   scalar,'YWSIZE'
  parcheck,xdsize,       keyword, real,   scalar,'XDSIZE'
  parcheck,ydsize,       keyword, real,   scalar,'YDSIZE'
  parcheck,origo,        keyword, natural,     1,'ORIGO'
  no_copy = KEYWORD_SET(no_copy)
  parcheck,cw_mouse,     keyword, lng,    scalar,'CW_MOUSE'
  motion_events = KEYWORD_SET(motion_events)
  
; PLOT keywords:  
  parcheck,background,   keyword, natural,scalar,'BACKGROUND'
  parcheck,charsize,     keyword, real,   scalar,'CHARSIZE'
  parcheck,xcharsize,    keyword, real,   scalar,'XCHARSIZE'
  parcheck,ycharsize,    keyword, real,   scalar,'YCHARSIZE'
  parcheck,charthick,    keyword, real,   scalar,'CHARTHICK'
  parcheck,color,        keyword, natural,scalar,'COLOR'
  parcheck,font,         keyword, natural,scalar,'FONT'
  parcheck,xgridstyle,   keyword, natural,scalar,'XGRIDSTYLE'
  parcheck,ygridstyle,   keyword, natural,scalar,'YGRIDSTYLE'
  parcheck,xstyle,       keyword, natural,scalar,'XSTYLE'
  parcheck,ystyle,       keyword, natural,scalar,'YSTYLE'
  parcheck,subtitle,     keyword, strng,  scalar,'SUBTITLE'
  parcheck,xthick,       keyword, real,   scalar,'XTHICK'
  parcheck,ythick,       keyword, real,   scalar,'YTHICK'
  parcheck,xtickformat,  keyword, strng,  scalar,'XTICKFORMAT'
  parcheck,ytickformat,  keyword, strng,  scalar,'YTICKFORMAT'
  parcheck,xticks,       keyword, natural,scalar,'XTICKS'
  parcheck,yticks,       keyword, natural,scalar,'YTICKS'
  parcheck,ticklen,      keyword, real,   scalar,'TICKLEN'
  parcheck,xticklen,     keyword, real,   scalar,'XTICKLEN'
  parcheck,yticklen,     keyword, real,   scalar,'YTICKLEN'
  parcheck,title,        keyword, strng,  scalar,'TITLE'
  parcheck,xtitle,       keyword, strng,  scalar,'XTITLE'
  parcheck,ytitle,       keyword, strng,  scalar,'YTITLE'

; Data coordinate system 
  parcheck,origin,keyword,real,1,'ORIGIN'
  parcheck,scale,keyword,real,1,'SCALE'
  squarepix = KEYWORD_SET(squarepix)
  stretch = KEYWORD_SET(stretch)
  
; Color scaling:
  parcheck,xtvscale,     keyword,lng,     [0,1],'XTVSCALE'
  parcheck,missing,      keyword,real,   scalar,'MISSING'
  parcheck,color_missing,keyword,natural,scalar,'COLOR_MISSING'
  
; Limitations
  parcheck,maxzoom,      keyword,real,   scalar,'MAXZOOM'
  parcheck,minzoom,      keyword,real,   scalar,'MINZOOM'
  
; Crosshair
  parcheck,cross,        keyword,natural,scalar,'CROSS'
  parcheck,cross_color,  keyword,natural,scalar,'CROSS_COLOR'
  parcheck,cross_graph,  keyword,natural,scalar,'CROSS_GRAPH'
  
; Miscellaneous
  autonomous = KEYWORD_SET(autonomous)
  
; Ignore actions
  parcheck,ignore_action,keyword,strng,  scalar,'IGNORE_ACTION'
  
; N_ELEMENTS checks on origin, scale, and origo
  IF N_ELEMENTS(origin) NE 2 THEN $
     MESSAGE,"Keyword ORIGIN in CW_PZOOM must have two elements"
  IF N_ELEMENTS(scale) NE 2 THEN $
     MESSAGE,"Keyword SCALE in CW_PZOOM must have two elements"
  IF N_ELEMENTS(origo) NE 2 THEN $
     MESSAGE,"Keyword ORIGO in CW_PZOOM must have two elements"
;
; That's all!
; 
  
  wsize = LONG([xwsize,ywsize])
  dsize = LONG([xdsize,ydsize])
  
  base = WIDGET_BASE(on_base, space=0, xpad=0, ypad=0,$
                     uvalue=uvalue,no_copy=no_copy, $
                     event_func='cw_pzoom_event', $
                     func_get_value='cw_pzoom_getv', $
                     pro_set_value='cw_pzoom_setv', $
                     notify_realize='cw_pzoom_realize')
  
  draw = WIDGET_DRAW(base, $
                     xsize=wsize(0),ysize=wsize(1), $
                     /button_events,motion_events=motion_events)
  
  ;; Used for signalling from XTVSCALE
  signal = WIDGET_BASE(base,uvalue='XTVSCALE',map = 0)
  
  ;; Hook up for events from XTVSCALE
  FOR i=0,N_ELEMENTS(XTVSCALE)-1 DO BEGIN
     IF (XTVSCALE)(i) GE 0L THEN dummy = xtvscale((xtvscale)(i),signal=signal)
  END
  
  ;; External values
  ;; These can be set by the user "on the fly"
  
  ext = $
     { $ ;; cw_pzoom_disp,$
       origo            : origo,   $ ; Origin of viewing area
       $
       $;; PLOT keywords:
       $
       background       : LONG(BACKGROUND), $
       charsize         : FLOAT(CHARSIZE), $
       xcharsize        : FLOAT(XCHARSIZE), $
       ycharsize        : FLOAT(YCHARSIZE), $
       charthick        : FLOAT(CHARTHICK), $
       color            : LONG(COLOR), $
       font             : LONG(FONT), $
       xgridstyle       : LONG(XGRIDSTYLE), $
       ygridstyle       : LONG(YGRIDSTYLE), $
       xstyle           : LONG(XSTYLE), $
       ystyle           : LONG(YSTYLE), $
       subtitle         : SUBTITLE, $
       xthick           : FLOAT(XTHICK), $
       ythick           : FLOAT(YTHICK), $
       xtickformat      : XTICKFORMAT, $
       ytickformat      : YTICKFORMAT, $
       xticks           : LONG(XTICKS), $
       yticks           : LONG(YTICKS), $
       ticklen          : FLOAT(TICKLEN), $
       xticklen         : FLOAT(XTICKLEN), $
       yticklen         : FLOAT(YTICKLEN), $
       title            : TITLE, $
       xtitle           : XTITLE, $
       ytitle           : YTITLE, $
       $
       $;; Data coordinate system
       $
       origin           : origin,$
       scale            : scale,$
       squarepix        : squarepix,$
       stretch          : stretch,$ 
       $
       $;; Color scaling
       $
       missing          : DOUBLE(missing),$
       xtvscale         : (xtvscale)(0),$ ; Xtvscale_id
       color_missing    : color_missing,$
       $
       $;; Limitations
       $
       maxzoom          : FLOAT(maxzoom),$
       minzoom          : FLOAT(minzoom),$
       $
       $;; Crosshair
       $
       cross            : FLOAT(cross),$
       cross_color      : cross_color,$
       cross_graph      : cross_graph,$
       $
       $;; Miscellaneous 
       $
       ignore_action    : ignore_action,$
       $
       $;; State/action variables
       $
       XFOCUS           : 0,$   ; Cannot be set by keyword values.
       YFOCUS           : 0,$   ; 
       zoom             : 1.0,$ ;
       REPLOT           : 0,$   ;
       REPLOT_CROSS     : 0,$   ;
       RESCALE          : 0 $   ;
     }
     
  ;; Internal values
  ;; These are fixed for the lifetime of the compound widget.
  
  int = $
     { $ ;; cw_pzoom_int,$
       simple : 1,$
       ZERO   : [0L,0,0,0],$    ; Data Pixel number of *displayed* pixel (0,0)
       ASIZE  : [0,0],$         ; Data array size
       WSIZE  : wsize,$         ; Window size (total)
       DSIZE  : dsize,$         ; Display size (size of viewing area).
       auto   : autonomous,$    ; Block events?
       DRAW   : draw,$
       preg   : 0L,$
       CW_MOUSE : CW_MOUSE,$
       tv     : -1L}
  
  kill_handles = [handle_create(),handle_create(),handle_create()]
  
  ;; Handles will be automatically killed when the draw widget is destroyed.
  
  handle_killer_hookup,kill_handles,group_leader=draw
  
  info = $
     { $ ;;cw_pzoom_info,$
       value : kill_handles(0),$
       scaledv : kill_handles(1),$
       im_h : kill_handles(2),$
       kill_handles:kill_handles,$
       ext  : ext,$
       int: int}
  
  IF N_ELEMENTS(value) GT 0 THEN handle_value,info.value,value,/set
  
  WIDGET_CONTROL,draw,set_uvalue=info,/no_copy
  
  RETURN,base
END
