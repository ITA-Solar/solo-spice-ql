;+
; Project     : SOHO - CDS     
;                   
; Name        : CW_CUBEVIEW
;               
; Purpose     : Compound widget for visualizing data n-cubes (0 < n < 8)
;               
; Explanation : Creates a column display on the supplied BASE, containing an
;               image section and a plot section, allowing the user to
;               determine which data dimensions should be displayed, and to
;               move the focus point around inside the data.
;
;               When the focus point changes, an event is returned. The event
;               structure has the standard tags ID, TOP, and HANDLER, and one
;               tag called FOCUS, containing the indices of the current focus
;               point.
;
;               The data to be displayed can be changed through the
;               WIDGET_CONTROL, SET_VALUE=<data> mechanism.
;
;               The status variables that can be set by keywords or changed by
;               the SET_VALUE={tag:value} mechanism are:
;               
;               focus : Current focus point
;               title : Title shown above the data column
;               image_dim : The current image dimensions
;               plot_dim : The current plotted dimension
;               dimnames : The dimension names
;               origin   : The origin of the data axes
;               scale    : The scale of the data axes
;               phys_scale : Array of 1/0 determining which of the data axis
;                            scales are to be regarded as "physical", i.e.,
;                            which data axes should be stretched/compressed
;                            according to their SCALE. 1 means treat scale as
;                            physical. 
;               
;               
; Use         : ID = CW_CUBEVIEW(BASE,VALUE=DATA)
;    
; Inputs      : BASE : The base to put the compound widget on.
;               
; Opt. Inputs : None.
;               
; Outputs     : Returns the ID of the compound widget
;               
; Opt. Outputs: None.
;               
; Keywords    : XSIZE/YSIZE : The sizes of the display windows.
;
;               DIMNAMES : An array of strings containing the names associated
;                          with the dimensions.
;
;               ORIGIN/SCALE : The origin/scale of the data axes
;
;               PHYS_SCALE : Array of 0/1s determining which of the data axis
;                            scales are to be regarded as "physical", i.e.,
;                            which data axes should be stretched/compressed
;                            according to their SCALE. 1 means treat scale as
;                            physical.
;
;               MISSING : The value of missing data points, to be excluded in
;                         the color scaling.
;
;               UVALUE : The uvalue to be associated with the widget.
;
;               IMAGE_DIM : Which dimensions should form the image initially
;
;               PLOT_DIM : Which dimension should be plotted initially
;
;               TITLE : The title of the data column.
;
; Calls       : cw_flipswitch(), cw_plotz(), cw_pzoom(), default,
;               handle_create(), since_version(), trim(), xplotscale(),
;               xtvscale()
;
; Common      : None.
;               
; Restrictions: Data must be at least one-dimensional.
;               
; Side effects: None known.
;               
; Category    : Visualization
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 20 January 1997
;               
; Modified    : Version 2, SVHH, 29 January 1997
;                       Fixed some problems with singular dimensions, and
;                       added "transpose" possibility when only two
;                       nonsingular dimensions.
;               Version 3, SVHH, 29 May 1997
;                       Added possibility of passing handle as pointer to the
;                       data, and made some checks to see if data extraction
;                       was really necessary (to speed up). Added ROW keyword.
;               Version 4, SVHH, 4 June 1997
;                       Added update=0/update=1 calls when updating text to
;                       avoid IDL v 5 bug that increased the base size on
;                       every update.
;               Version 5, SVHH, 17 September 1997
;                       Added return of tag "internal" as part of the "value"
;                       structure under cw_cubeview_getv, added ALL_EVENTS
;                       flag, (both features facilitate overplotting)
;               Version 6, SVHH, 6 May 1998
;                       Will no longer redraw display etc. after a set_value
;                       operation when there is *no* change in the "ext"
;                       part of the info structure
;               Version 7, Martin Wiesmann, 25 August 2021
;                       Implemented method cw_cubeview_force_redraw
;               Version 8, Martin Wiesmann, 7 June 2023
;                       Extended cw_cubeview_force_redraw to redraw also plot
;
; Version     : 8, 7 June 2023
; $Id: 2024-01-19 14:07 CET $
;-

;;
;; Extract plot from data cube
;;
PRO spice_cw_cubeview_get_plot,info,arr,set
  handle_value,info.int.value_h,value,/no_copy
  
  f = info.ext.focus
  nf = n_elements(f)
  IF nf LT 7 THEN f = [f,replicate(0L,7-nf)]
  
  CASE info.ext.plot_dim OF 
     -1 : arr = findgen(10)
     0 : arr = reform(value(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
     1 : arr = reform(value(f(0),*,f(2),f(3),f(4),f(5),f(6)),/overwrite)
     2 : arr = reform(value(f(0),f(1),*,f(3),f(4),f(5),f(6)),/overwrite)
     3 : arr = reform(value(f(0),f(1),f(2),*,f(4),f(5),f(6)),/overwrite)
     4 : arr = reform(value(f(0),f(1),f(2),f(3),*,f(5),f(6)),/overwrite)
     5 : arr = reform(value(f(0),f(1),f(2),f(3),f(4),*,f(6)),/overwrite)
     6 : arr = reform(value(f(0),f(1),f(2),f(3),f(4),f(5),*),/overwrite)
  END
  
  x = findgen(n_elements(arr))*info.ext.scale(info.ext.plot_dim) $
     + info.ext.origin(info.ext.plot_dim)
  arr = [[x],[arr]]
  
  ;; If the user moves in the image, and the plotted dimension is
  ;; one of the image axes, we need to move the plot focus point
  ;; 

  set = {focusi:info.ext.focus(info.ext.plot_dim),$
         xtitle:info.ext.dimnames(info.ext.plot_dim)}
  
  handle_value,info.int.value_h,value,/set,/no_copy
END

;;
;; Extract image from the data cube
;;
PRO spice_cw_cubeview_get_image,info,arr,set,newdata=newdata
  handle_value,info.int.value_h,value,/no_copy
  
  f = info.ext.focus
  nf = n_elements(f)
  IF nf LT 7 THEN f = [f,replicate(0L,7-nf)]
  
  imdim = info.ext.image_dim
  
  ;; Check if we have to extract a new image...
  ;; 
  focus_change_significant = info.ext.focus NE info.int.lastfocus
  ;; Change of position within the picture is not significant
  focus_change_significant(info.ext.image_dim) = 0b 
  
  imchange = total(info.int.lastimdim NE info.ext.image_dim) NE 0
  extract = imchange OR total(focus_change_significant) NE 0 
  extract = extract OR keyword_set(newdata)
  
  IF extract THEN BEGIN 
     imdim_sort = imdim
     
     IF imdim_sort(0) GT imdim_sort(1) THEN $
        imdim_sort = [imdim_sort(1),imdim_sort(0)]
     
     IF imdim_sort(0) EQ imdim_sort(1) THEN BEGIN
        print,"Somehow, the two image dimensions shouldn't be the same!"
        stop
     END
     
     ;; Now - we have values:
     ;; imdim_sort(0) = 0..6
     ;; imdim_sort(1) = imdim_sort(0)+1..7
     
     imdim_num = 7*imdim_sort(0) + imdim_sort(1)
     
     CASE imdim_num OF 
        
        ;; (0,0) impossible
        
        1 : arr = reform(value(*,*,f(2),f(3),f(4),f(5),f(6)),/overwrite)
        
        2 : arr = reform(value(*,f(1),*,f(3),f(4),f(5),f(6)),/overwrite)
        
        3 : arr = reform(value(*,f(1),f(2),*,f(4),f(5),f(6)),/overwrite)
        
        4 : arr = reform(value(*,f(1),f(2),f(3),*,f(5),f(6)),/overwrite)

        5 : arr = reform(value(*,f(1),f(2),f(3),f(4),*,f(6)),/overwrite)
        
        6 : arr = reform(value(*,f(1),f(2),f(3),f(4),f(5),*),/overwrite)
        
        ;; (1,0..1) impossible

        7+2 : arr = reform(value(f(0),*,*,f(3),f(4),f(5),f(6)),/overwrite)

        7+3 : arr = reform(value(f(0),*,f(2),*,f(4),f(5),f(6)),/overwrite)

        7+4 : arr = reform(value(f(0),*,f(2),f(3),*,f(5),f(6)),/overwrite)

        7+5 : arr = reform(value(f(0),*,f(2),f(3),f(4),*,f(6)),/overwrite)
        
        7+6 : arr = reform(value(f(0),*,f(2),f(3),f(4),f(5),*),/overwrite)
        
        ;; (2,0..2) impossible

        14+3 : arr = reform(value(f(0),f(1),*,*,f(4),f(5),f(6)),/overwrite)

        14+4 : arr = reform(value(f(0),f(1),*,f(3),*,f(5),f(6)),/overwrite)

        14+5 : arr = reform(value(f(0),f(1),*,f(3),f(4),*,f(6)),/overwrite)
        
        14+6 : arr = reform(value(f(0),f(1),*,f(3),f(4),f(5),*),/overwrite)
        
        ;; (3,0..3) impossible

        21+4 : arr = reform(value(f(0),f(1),f(2),*,*,f(5),f(6)),/overwrite)

        21+5 : arr = reform(value(f(0),f(1),f(2),*,f(4),*,f(6)),/overwrite)
        
        21+6 : arr = reform(value(f(0),f(1),f(2),*,f(4),f(5),*),/overwrite)
        
        ;; (4,0..4) impossible

        28+5 : arr = reform(value(f(0),f(1),f(2),f(3),*,*,f(6)),/overwrite)

        28+6 : arr = reform(value(f(0),f(1),f(2),f(3),*,f(5),*),/overwrite)
        
        ;; (5,0..5) impossible
        
        35+6 : arr = reform(value(f(0),f(1),f(2),f(3),f(4),*,*),/overwrite)
     END
     
     IF imdim(0) GT imdim(1) THEN arr = transpose(arr)
  END
  
  info.int.lastfocus = info.ext.focus
  info.int.lastimdim = info.ext.image_dim
  
  origin = info.ext.origin(imdim)
  scale = info.ext.scale(imdim)
  squarepix = total(info.ext.phys_scale(imdim)) NE 2
  
  ifo = info.ext.focus(info.ext.image_dim)
  
  set = {xtitle:info.ext.dimnames(imdim(0)),$
         ytitle:info.ext.dimnames(imdim(1)),$
         xfocus:ifo(0),yfocus:ifo(1),$
         origin:origin, scale:scale, $
         replot: extract EQ 0b,$
         squarepix:squarepix}
  
  
  handle_value,info.int.value_h,value,/set,/no_copy
END

;;
;; Form the text showing how the slice (image/plot) is done
;;
FUNCTION spice_cw_cubeview_slicetext,info,dims

  IF dims(0) GT dims(1) THEN t = 'T' ELSE t = ''
  tx = t+'('
  FOR i = 0,info.int.szv(0)-1 DO BEGIN
     IF i EQ dims(0) OR i EQ dims(1) THEN tt = '*' $
     ELSE tt = trim(info.ext.focus(i))
     IF i LT info.int.szv(0)-1 THEN tx = tx+tt+',' $
     ELSE                           tx = tx+tt+')'
  END
  return,tx
END

;;
;; Update texts
;;
PRO spice_cw_cubeview_upd_info,info
  
  IF since_version('4.0.1') THEN widget_control,info.int.mybase,update=0
  
  IF info.int.title_id NE 0L THEN $
     widget_control,info.int.title_id,set_value=info.ext.title
  
  txfocus = spice_cw_cubeview_slicetext(info,[-1,-1])
  txim = spice_cw_cubeview_slicetext(info,info.ext.image_dim)
  txplot = spice_cw_cubeview_slicetext(info,[-1,info.ext.plot_dim])
  
  widget_control,info.int.focustx_id,set_value=txfocus
  widget_control,info.int.imagetx_id,set_value=txim
  widget_control,info.int.plottx_id,set_value=txplot
  
  IF since_version('4.0.1') THEN widget_control,info.int.mybase,update=1
END

;;
;; Redraw the widgets with the images
;;
pro spice_cw_cubeview_force_redraw, id
  stash = widget_info(id,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  ;; Update plot
  spice_cw_cubeview_get_plot,info,arr,set
  widget_control,info.int.plot_id,set_value=set
  widget_control,info.int.plot_id,set_value=arr

  IF info.int.image_id NE 0L THEN BEGIN
    ;; Update image
    spice_cw_cubeview_get_image,info,im_arr,set,/newdata
    widget_control,info.int.image_id,set_value=set
    IF n_elements(im_arr) GT 0 THEN $
      widget_control,info.int.image_id,set_value=im_arr
  END
  widget_control,stash,set_uvalue=info,/no_copy
end

;;
;; Set-value procedure - either a new data cube or new status values
;;
PRO spice_cw_cubeview_setv,id,value
  stash = widget_info(id,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  
  stat_changed = 0b ;; Defaults
  data_changed = 0b ;;
  
  IF datatype(value) EQ 'STC' THEN BEGIN
     ext = info.ext
     copy_tag_values,ext,value,status
     FOR i = 0,n_elements(tag_names(ext))-1 DO BEGIN
        stat_changed = stat_changed OR (total([ext.(i) NE info.ext.(i)]) NE 0) 
     END
     info.ext = ext
  END ELSE BEGIN
     data_changed = 1b
     IF info.int.value_hpass THEN BEGIN
        info.int.value_h = value
     END ELSE BEGIN 
        handle_value,info.int.value_h,value,/set
     END
  END
  
  ;; Update texts
  IF stat_changed THEN spice_cw_cubeview_upd_info,info
  
  IF data_changed OR stat_changed THEN BEGIN 
     ;; Update plot
     spice_cw_cubeview_get_plot,info,arr,set
     widget_control,info.int.plot_id,set_value=set
     widget_control,info.int.plot_id,set_value=arr
     
     IF info.int.image_id NE 0L THEN BEGIN 
        ;; Update image
        spice_cw_cubeview_get_image,info,im_arr,set,/newdata
        widget_control,info.int.image_id,set_value=set
        IF n_elements(im_arr) GT 0 THEN $
           widget_control,info.int.image_id,set_value=im_arr
     END
  END
  
  widget_control,stash,set_uvalue=info,/no_copy
END

;;
;; Returns the status value structure
;;
FUNCTION spice_cw_cubeview_getv,id
  
  stash = widget_info(id,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  value = info.ext
  value=create_struct(value,'internal',info.int)
  widget_control,stash,set_uvalue=info,/no_copy
  return,value
END


;;
;; Event handling
;; 
FUNCTION spice_cw_cubeview_event,ev
  
  help,ev
  
  stash = widget_info(ev.handler,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  
  widget_control,ev.id,get_uvalue=uvalue
  
  uvalue = str_sep(uvalue,':')
  
  event = 0
  
  CASE uvalue(0) OF 
     
  'IMAGE':BEGIN 
     ;;
     ;; A keyclick or similar in the IMAGE - change *plot*
     ;; 
     widget_control,ev.id,set_value=ev.set ; Enforce the update
     ;; 
     ;; Don't propagate tv scaling events unless all-events is set
     ;; 
     
     IF ev.xtvscale AND NOT info.ext.all_events THEN GOTO,skip_event
     imd = info.ext.image_dim   ;Shorthand
     ;; Store the new focus point
     info.ext.focus(imd) = [ev.set.xfocus,ev.set.yfocus]

     ;; Get new plot data and send it to the plotter
     spice_cw_cubeview_get_plot,info,arr,set
     IF n_elements(set) NE 0 THEN $
        widget_control,info.int.plot_id,set_value=set
     widget_control,info.int.plot_id,set_value=arr
     spice_cw_cubeview_upd_info,info
     ENDCASE
     
  'XPLOTSCALER':BEGIN
     dummy = xplotscale(info.int.xplotscaler,/map,iconify=0,/show)
     IF NOT info.ext.all_events THEN GOTO,skip_event
     ENDCASE
     
  'XTVSCALER':BEGIN
     dummy = spice_xtvscale(info.int.xtvscaler,/map,iconify=0,/show)
     GOTO,skip_event
     ENDCASE
     
  'PLOT':BEGIN
     evtype = tag_names(ev,/structure_name)
     ;; Enforce the update
     widget_control,ev.id,set_value=ev.set 
     ;;
     ;; If it's just a scaling event or a zoom in/out event, gobble it, unless
     ;; all-events are set 
     ;; 
     IF (evtype EQ "CW_PLOTZ_XPLOTSCALE" $
         OR ev.set.zoom NE ev.old.zoom) AND NOT info.ext.all_events THEN GOTO,skip_event
     
     ;; Store the new focus
     info.ext.focus(info.ext.plot_dim) = ev.set.focusi
     IF info.int.image_id NE 0L THEN BEGIN
        ;; Get new image data, send to displayer
        spice_cw_cubeview_get_image,info,arr,set
        widget_control,info.int.image_id,set_value=set
        IF n_elements(arr) NE 0 THEN $
           widget_control,info.int.image_id,set_value=arr
     END
     
     spice_cw_cubeview_upd_info,info
     ENDCASE
     
  'PLOT_DIM':BEGIN
     dim = fix(uvalue(1))
     info.ext.plot_dim = dim
     spice_cw_cubeview_get_plot,info,arr,set
     widget_control,info.int.plot_id,set_value=set
     widget_control,info.int.plot_id,set_value=arr     
     spice_cw_cubeview_upd_info,info
     IF NOT info.ext.all_events THEN GOTO,skip_event
     ENDCASE
     
  'IMAGE_DIM':BEGIN
     ;; Which *image* dimension?
     imd = fix(uvalue(1))
     ;; Set to which *data* dimension?
     imdval = fix(uvalue(2))
     ;; What's the *other* image dimension?
     otherd = (imd+1) MOD 2
     
     IF n_elements(info.int.multix) EQ 2 THEN BEGIN
        
        ;; If we only have two dimensions available, switch them
        
        tmp = info.ext.image_dim(0)
        info.ext.image_dim(0) = info.ext.image_dim(1)
        info.ext.image_dim(1) = tmp
        
        ;; Update the flipswitches
        widget_control,info.int.image_dim_id(0),$
           set_value='IMAGE_DIM:0:'+trim(info.ext.image_dim(0))
        widget_control,info.int.image_dim_id(1),$
           set_value='IMAGE_DIM:1:'+trim(info.ext.image_dim(1))
        
     END ELSE BEGIN 
        
        ;; The value of the "other" dimension
        
        odim = info.ext.image_dim(otherd)
        
        IF imdval EQ info.ext.image_dim(otherd) THEN BEGIN
           
           ;; Can't have both dimensions the same, can we? Take the next one!
           
           Print,"Skipping dimension "+trim(imdval)+":"+$
              info.ext.dimnames(imdval)
           
           imdval = (imdval + 1) MOD info.int.szv(0)
           
           ;; Make sure we don't pick a singular one
           
           WHILE info.int.szv(imdval+1) LE 1 OR imdval EQ odim DO BEGIN
              imdval = (imdval + 1) MOD info.int.szv(0)
           END
           
           widget_control,ev.id,$
              set_value='IMAGE_DIM:'+uvalue(1)+':'+trim(imdval)
           
        END
        
        print,"Picked dimension"+trim(imdval)+":"+info.ext.dimnames(imdval)
        info.ext.image_dim(imd) = imdval
     END
     
     IF info.int.image_id NE 0L THEN BEGIN
        spice_cw_cubeview_get_image,info,arr,set
        widget_control,info.int.image_id,set_value=set
        IF n_elements(arr) NE 0 THEN $
           widget_control,info.int.image_id,set_value=arr
     END
     spice_cw_cubeview_upd_info,info
     IF NOT info.ext.all_events THEN GOTO,skip_event
     ENDCASE
     
     'DATA': print,'Do something with the image, this is a signal from xtvscale.'
  END
  
  event = {id:ev.handler,$ ;; cw_cubeview_EVENT - Must have fixed number of
           $                    ; elements in focus!               
           top:ev.top,$
           handler:0l,$
           focus:info.ext.focus,$
           image_id:info.int.image_id,$
           plot_id:info.int.plot_id}
  
skip_event:
  
  widget_control,stash,set_uvalue=info,/no_copy
  
  return,event
END


FUNCTION spice_cw_cubeview_dummy
  s = 20
  f = 4*!pi/(s-1)
  x = rebin(f*findgen(s,1,1,1),s,s,s,s,/sample)
  y = rebin(f*findgen(1,s,1,1),s,s,s,s,/sample)
  z = rebin(f*findgen(1,1,s,1),s,s,s,s,/sample)
  t = rebin(f*findgen(1,1,1,s),s,s,s,s,/sample)
  
  return,cos(x*0.5)+cos(y)+cos(z*3/2)+cos(t*2)
END


FUNCTION spice_cw_cubeview,base,value=value,xsize=xsize,ysize=ysize,$
                     dimnames=dimnames,origin=origin,scale=scale,$
                     phys_scale=phys_scale,missing=missing,$
                     uvalue=uvalue,image_dim=image_dim,plot_dim=plot_dim,$
                     title=title,hvalue=hvalue,row=row,all_events=all_events
  
  value_hpass = 0
  
  IF n_elements(hvalue) EQ 1 THEN BEGIN
     IF handle_info(hvalue,/valid) THEN BEGIN
        handle_value,hvalue,value,/no_copy
        value_hpass = 1
     END
  END
  
  IF NOT value_hpass THEN hvalue = handle_create()
  
  value_h = hvalue
  
  IF n_elements(value) EQ 0 THEN value = spice_cw_cubeview_dummy()
  szv = size(value)
  
  handle_value,value_h,value,/set,no_copy=value_hpass
  
  ;; Handle to be automatically destroyed when widget dies..
  IF NOT value_hpass THEN handle_killer_hookup,value_h,group_leader=base
  
  IF szv(0) LT 1 THEN $
     message,"Value must have 1 or more dimensions"
  
  ;;
  ;; Form default dimension names
  ;;
  dimstr = strcompress(string(lindgen(szv(0))),/remove_all)
  IF n_elements(dimnames) NE szv(0) THEN $
     dimnames = 'Dim:'+dimstr
  
  ;;
  ;; Which dimensions are nonsingular
  ;;
  multidim = replicate(0b,szv(0))
  multix = where(szv(1:szv(0)) GT 1,nmulti)
  multidim(multix) = 1b
  
  IF nmulti LT 1 THEN $
     message,"Value must have at least 1 nonsingular dimension"
  
  IF nmulti EQ 1 then multix = [multix,multix]
  
  ;; Initial/default values
  
  default,xsize,270
  default,ysize,270
  xdsize = xsize-60
  ydsize = ysize-50
  xticklen = -3.0/xsize
  yticklen = -3.0/ysize
  
  default,focus,szv(1:szv(0))/2
  default,image_dim,multix(0:1 < (n_elements(multix)-1))
  
  IF total(multidim(image_dim)) NE n_elements(image_dim) THEN BEGIN
     message,"Ignoring supplied imaging dimensions",/continue
     image_dim = multix(0:1 < (n_elements(multix)-1))
  ENDIF
  
  IF nmulti GT 2 THEN default,plot_dim,multix(2) $
  ELSE                default,plot_dim,multix(0)
  
  IF n_elements(origin) NE 0 THEN iorigin = origin
  IF n_elements(scale) NE 0 THEN iscale = scale
  IF n_elements(phys_scale) NE 0 THEN iphys_scale = phys_scale
  IF n_elements(title) NE 0 THEN ititle = title
  
  default,iorigin,replicate(0.0,szv(0))
  default,iscale,replicate(1.0,szv(0))
  default,iphys_scale,replicate(1,szv(0))
  default,ititle,''
  
  IF n_elements(iorigin) NE szv(0) THEN $
     message,"ORIGIN must have one element per dimension"
  IF n_elements(iscale) NE szv(0) THEN $
     message,"SCALE must have one element per dimension"
  IF n_elements(iphys_scale) NE szv(0) THEN $
     message,"PHYS_SCALE must have one element per dimension"
  
  iorigin = double(iorigin)
  iscale = double(iscale)
  iphys_scale = iphys_scale NE 0
  
  default,missing,-100.0
  
  ext = {focus:focus,$
         title:ititle,$
         image_dim:image_dim,$
         plot_dim:plot_dim,$
         dimnames:dimnames,$
         origin:iorigin,$
         scale:iscale,$
         phys_scale:iphys_scale,$
         all_events:keyword_set(all_events) $
        }
  
  int = {value_h:value_h,$
         value_hpass:value_hpass,$
         multix:multix,$
         lastfocus:focus*0 -1L,$
         lastimdim:[-1,-1],$
         xplotscaler:0L,$
         xtvscaler:0L,$
         szv:szv,$
         mybase:0L,$
         title_id:0L,$
         focustx_id:0L,$
         image_id:0L,$
         imagetx_id:0L,$
         plot_id:0L,$
         plottx_id:0L,$
         image_dim_id:[0L,0L],$
         plot_dim_id:0L $
        }
  
  info = { ext : ext,$
           int : int }
  
  ;; Get data

  IF szv(0) GT 1 THEN spice_cw_cubeview_get_image,info,im
  spice_cw_cubeview_get_plot,info,plt
  
  ;; Build widget
  
  sml = 1
  tight = {xpad:sml,ypad:sml,space:sml}
  
  mybase = widget_base(base,event_func="spice_cw_cubeview_event",$
                       /column,$
                       pro_set_value="spice_cw_cubeview_setv",$
                       func_get_value="spice_cw_cubeview_getv",$
                       _extra=tight)
  info.int.mybase = mybase
  
  IF n_elements(uvalue) NE 0 THEN widget_control,mybase,set_uvalue=uvalue
  
  
  ;; This base is the one to put buttons etc. on, as well as storing the
  ;; info structure on
  
  IF keyword_set(title) THEN BEGIN
     storage = widget_label(mybase,value=title)
     info.int.title_id = storage
  END
  
  
  focustx_id = widget_label(mybase,value=' ')
  
  default,storage,focustx_id
  
  IF keyword_set(row) THEN ibase = widget_base(mybase,/row,_extra=tight) $
  ELSE                     ibase = mybase
    
  
  ;;
  ;; Image section
  ;;
  im_base = widget_base(ibase,/column,/frame,_extra=tight,map=nmulti GT 1)
  
  info.int.xtvscaler = spice_xtvscale(group=mybase,map=0,missing=missing,$
                                sigrange=0, signal=mybase)
  dummy = cw_flipswitch(im_base,value='Adjust color scaling'+["",""],$
                        uvalue='XTVSCALER'+["",""])
  
  imdim_base = widget_base(im_base,/row,_extra=tight)
  image_dim1_id = cw_flipswitch(imdim_base,value=dimnames(multix),$
                                uvalue='IMAGE_DIM:0:'+dimstr(multix))
  dummy = widget_label(imdim_base,value='x')
  image_dim2_id = cw_flipswitch(imdim_base,value=dimnames(multix),$
                                uvalue='IMAGE_DIM:1:'+dimstr(multix))
  
  widget_control,image_dim1_id,set_value='IMAGE_DIM:0:'+trim(image_dim(0))
  widget_control,image_dim2_id,set_value='IMAGE_DIM:1:'+trim(image_dim(1))
  
  dummy = widget_label(imdim_base,value=':')
  imagetx_id = widget_label(imdim_base,value=' ')
  
  squarepix = total(iphys_scale(image_dim) NE 0) NE 2
  
  IF szv(0) GT 1 THEN BEGIN 
     image_id = cw_pzoom(im_base,xwsize=xsize,ywsize=ysize,$
                         xdsize=xdsize,ydsize=ydsize,$
                         xticklen=xticklen,yticklen=yticklen,$
                         origin=iorigin(image_dim),scale=iscale(image_dim),$
                         squarepix=squarepix,missing=missing,$
                         value=im,uvalue='IMAGE',$
                         xtvscale=info.int.xtvscaler,$
                         xtitle=dimnames(image_dim(0)), $
                         ytitle=dimnames(image_dim(1)))
  END ELSE image_id = 0L
  
  ;;
  ;; Plot section
  ;;
  plot_base = widget_base(ibase,/column,/frame,_extra=tight)
  
  info.int.xplotscaler = xplotscale(group=mybase,map=0,missing=missing)
  
  dummy = cw_flipswitch(plot_base,value='Adjust plot scaling'+["",""],$
                        uvalue='XPLOTSCALER'+["",""])
  
  plotdim_base = widget_base(plot_base,/row,_extra=tight)
  plotdim_id = cw_flipswitch(plotdim_base,value=dimnames(multix),$
                             uvalue='PLOT_DIM:'+dimstr(multix))
  
  widget_control,plotdim_id,set_value='PLOT_DIM:'+trim(plot_dim)
  
  dummy = widget_label(plotdim_base,value=' : ')
  plottx_id = widget_label(plotdim_base,value=' ')
  
  plot_id = cw_plotz(plot_base,xwsize=xsize,ywsize=ysize,$
                     value=plt,uvalue='PLOT',missing=missing,$
                     xtitle=dimnames(plot_dim),psym=10,$
                     xplotscale=info.int.xplotscaler)
  ;;
  ;; Store widget IDs in info structure
  ;;
  info.int.focustx_id = focustx_id
  info.int.image_id = image_id
  info.int.imagetx_id = imagetx_id
  info.int.plot_id = plot_id
  info.int.plottx_id = plottx_id
  
  info.int.image_dim_id = [image_dim1_id,image_dim2_id]
  
  IF since_version('4.0.1') THEN BEGIN
     widget_control,info.int.title_id,/dynamic_resize,bad_id=bad
     widget_control,focustx_id,/dynamic_resize
     widget_control,imagetx_id,/dynamic_resize
     widget_control,plottx_id,/dynamic_resize
  END
  
  spice_cw_cubeview_upd_info,info
  
  widget_control,storage,set_uvalue=info,/no_copy
  return,mybase
END



PRO spice_cw_cubeview_test_event,ev
  widget_control,ev.id,get_uvalue=uvalue
  
  IF uvalue EQ "EXIT" THEN BEGIN
     widget_control,ev.top,/destroy
     return
  END
  
  help,ev,/str
END


PRO spice_cw_cubeview_test,value
  
  xkill,/all
  
  base = widget_base(/column)
  
  ibase = widget_base(base,/row)
  IF n_elements(value) NE 0 THEN BEGIN
     phys_scale = [0,1,1]
     origin = [0,0,0]
     scale = [1,2,1]
     dimnames = ['LAMBDA','SOLAR_X','SOLAR_Y']
     cbase = widget_base(ibase,/column)
     cube = spice_cw_cubeview(cbase,value=value,dimnames=dimnames,uvalue='CUBEVIEW',$
                        phys_scale=phys_scale,origin=origin,scale=scale,$
                        title='adfadf')
     cbase = widget_base(ibase,/column)
     cube2 = spice_cw_cubeview(cbase,value=value,dimnames=dimnames,uvalue='CUBEVIEW',$
                         phys_scale=phys_scale,origin=origin,scale=scale,$
                         title='adfadf')
     
  END ELSE BEGIN
     cube = spice_cw_cubeview(base,uvalue='CUBEVIEW')
  END
  
  
  quit = widget_button(base,value='Exit',uvalue='EXIT')
  
  widget_control,base,/realize
  
  xmanager,"spice_cw_cubeview_test",base,/modal
END



PRO spice_cw_cubeview_test_hpass,h
  xkill,/all
  
  base = widget_base(/column)
  
  phys_scale = [0,1,1]
  origin = [0,0,0]
  scale = [1,1,1]
  dimnames = ['LAMBDA','SOLAR_X','SOLAR_Y']
  cube = spice_cw_cubeview(base,dimnames=dimnames,uvalue='CUBEVIEW',$
                     phys_scale=phys_scale,origin=origin,scale=scale,$
                     hvalue=h)
  
  quit = widget_button(base,value='Exit',uvalue='EXIT')
  
  widget_control,base,/realize
  
  xmanager,"spice_cw_cubeview_test",base,/modal
END


