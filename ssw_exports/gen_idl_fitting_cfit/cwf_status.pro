;+
; Project     : SOHO - CDS     
;                   
; Name        : CWF_STATUS
;               
; Purpose     : Comp. widget showing a CFIT structure's CONST/INCLUDE status.
;               
; Explanation : Creates a compound widget reflecting the CONST/INCLUDE status
;               of a component fitting system structure (as in XCFIT_BLOCK).
;
;               Each component's status is shown inside a base with a frame
;               around it, with one checkbox displaying the INCLUDE status,
;               and one checkbox (displaying a cross when CONST=1) for each
;               parameter.
;
;               To set the widget to reflect another status, use
;
;               WIDGET_CONTROL,STATUS_ID,SET_VALUE=FIT
;
;               where FIT is the compound fit structure. It is, however, also
;               possible to set the HILIT status (the number of the parameter
;               to be highlighted) the following way:
;
;               WIDGET_CONTROL,STATUS_ID,SET_VALUE={SET_HILIT,HILIT:HILIT}
;
;               To read the status, use:
;
;               WIDGET_CONTROL,STATUS_ID,GET_VALUE=FIT
;
;               Now FIT will contain a fit structure with the current status.
;
;               Events are generated when the user flicks the status of one of
;               the CONST/INCLUDE checkboxes, generating event structures
;               containing the following tags:
;
;                ID,TOP,HANDLER : As usual.
;                
;                INCLUDE : A byte array reflecting the current INCLUDE status
;                          of all components.
;
;                CONST : A byte array reflecting the current CONST status of
;                        all components.
;  
; Use         : STATUS_ID = CWF_STATUS(BASE,VALUE=FIT)
;    
; Inputs      : BASE : The base to put it on.
;
;               VALUE : The initial value FIT structure (mandatory).
;
; Opt. Inputs : None.
;               
; Outputs     : Returns the ID of the compound widget.
;               
; Opt. Outputs: None.
;               
; Keywords    : COLUMN : Set to make it a column instead of a row.
;
;               UVALUE : Of the compound widget.
;               
;               NO_COPY : Whether to set the UVALUE with NO_COPY or not.
;               
;               FGCOLOR : The foreground color of the checkboxes
;               
;               BGCOLOR : The background color of the checkboxes
;
;               HILIT : The number of a parameter to highlight.
;               
; Calls       : default, exist(), handle_killer_hookup, cw_checkbox()
;
; Common      : None
;               
; Restrictions: ...
;               
; Side effects: ...
;               
; Category    : Line fitting.
;               
; Prev. Hist. : None
;
; Written     : SVH Haugan, UiO, 25 September 1997
;               
; Modified    : Not yet.
;
; Version     : 1,  25 September 1997
;-            

PRO cwf_status_makestat,info,fit
  
  ;; If the container exists - destroy it (we're rebuilding it)
  
  IF widget_info(info.int.container_id,/valid_id) THEN $
     widget_control,info.int.container_id,/destroy
  
  IF since_version('4.0.1') THEN widget_control,info.int.mybase,update=0
  
  sml = {xpad:1,ypad:1,space:1}
  
  IF info.int.column THEN BEGIN
     column = 1 & row = 0
  END ELSE BEGIN
     column = 0 & row = 1
  END
  
  container = widget_base(info.int.mybase,row=row,column=column,_extra=sml)
  info.int.container_id = container
  
  ;; Get all components of the fit structure
  
  tags = tag_names(fit)
  ntags = n_elements(tag_names(fit))
  
  ;; For each component
  ;; 
  FOR i = 0,ntags-1 DO BEGIN
     itx = trim(i)
     
     ;; Frame around this component
     ;; 
     IF row THEN tagb = widget_base(container,/column,_extra=sml,/frame) $
     ELSE        tagb = widget_base(container,/row,_extra=sml,/frame)
     
     ;; The INCLUDE status for this component - upper row (or left hand side)
     ;; 
     incb = cw_checkbox(tagb,xsize=13,thick=2,uvalue='INCLUDE:'+itx,$
                        value=fit.(i).include)
     
     ;; The CONST status for each tag in a row/column below/to the right
     ;; 
     constb = widget_base(tagb,row=row,column=column,_extra=sml)
     
     
     ;; Current status for this component (IDs and values)
     ;; 
     stat_tag = { include_id:incb, $
                  include : fit.(i).include,$
                  const_id:lonarr(n_elements(fit.(i).param)),$
                  const : fit.(i).param(*).const }
     
     ;; Create checkboxes with correct values
     ;;
     FOR j = 0,n_elements(fit.(i).param)-1 DO BEGIN
        jtx = trim(j)
        constbd = cw_checkbox(constb,xsize=11,uvalue='CONST:'+itx+':'+jtx,$
                              value=fit.(i).param(j).const,thick=2,/cross)
        stat_tag.const_id(j) = constbd
     END
     
     ;; Add current status tag to the fit_status structre
        
     fit_status = add_tag(fit_status,stat_tag,'status'+itx)
  END
  
  handle_value,info.int.status_h,fit_status,/set,/no_copy
  
  IF since_version('4.0.1') THEN widget_control,info.int.mybase,update=1
END


PRO cwf_status_showstat,info,fit
  
  newstat = n_elements(fit) NE 0
  
  handle_value,info.int.status_h,fit_status,/no_copy
  
  IF newstat THEN BEGIN
     tags = tag_names(fit)
     ntags = n_elements(tag_names(fit))
     
     ;; Compare this fit with current fit_status - see if the structure
     ;; is the same, update if OK, rebuild if not
     
     ;; Compare number of tags first.
     nstat_tags = n_elements(tag_names(fit_status))
     IF nstat_tags NE ntags THEN BEGIN
        cwf_status_makestat,info,fit
        return
     END
     
     FOR i = 0,ntags-1 DO BEGIN
        nparms = n_elements(fit.(i).param)
        
        ;; Bail out and rebuild display if something's amiss
        ;; (this will also put a new fit_status back on the handle)
        IF nparms NE n_elements(fit_status.(i).const_id) THEN BEGIN
           cwf_status_makestat,info,fit
           return
        END
        
        ;; Everything's OK: Update INCLUDE status
        ;; 
        fit_status.(i).include = fit.(i).include
        
        ;; Update CONST status for each parameter
        ;; 
        fit_status.(i).const = fit.(i).param(*).const
     END
     
  END 
  
  ntags = n_elements(tag_names(fit_status))
  ii = 0
  
  FOR i = 0,ntags-1 DO BEGIN
     nparms = n_elements(fit_status.(i).const)
     
     widget_control,fit_status.(i).include_id,set_value=fit_status.(i).include
     
     ;; Update CONST status for each parameter
     ;; 
     FOR j = 0,nparms-1 DO BEGIN
        
        ;; Shorthand
        id = fit_status.(i).const_id(j)
        
        val = {value:fit_status.(i).const(j),boxed:0,$
               bgcolor:info.ext.bgcolor,$
               fgcolor:info.ext.fgcolor}
        
        IF ii EQ info.ext.hilit THEN BEGIN ;; Swap colors to highlight
           val.boxed = 3
           val.fgcolor = val.bgcolor
           val.bgcolor = info.ext.fgcolor
        END
        widget_control,id,set_value=val
        
        ii = ii+1
     END
  END
  
  ;; Put back fit_status structure
  
  handle_value,info.int.status_h,fit_status,/set,/no_copy
END


;; Value must be a fit

PRO cwf_status_setv,id,value
  
  stash = widget_info(id,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  
  IF tag_names(value,/structure_name) EQ 'SET_HILIT' THEN BEGIN
     info.ext.hilit = value.hilit
     cwf_status_showstat,info
  END ELSE begin
     ;; Update appearance..
     cwf_status_showstat,info,value
  END
  
  widget_control,stash,set_uvalue=info,/no_copy
END


FUNCTION cwf_status_getv,id
  stash = widget_info(id,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  
  handle_value,info.int.status_h,fit
  
  widget_control,stash,set_uvalue=info,/no_copy
  
  return,fit
END

;;
;; The user switched the status of one of the boxes
;;
FUNCTION cwf_status_event,ev
  
  on_error,0
  
  stash = widget_info(ev.handler,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  
  handle_value,info.int.status_h,fit_status,/no_copy
  
  widget_control,ev.id,get_uvalue=uvalue
  
  uval = str_sep(uvalue,':')
  
  CASE uval(0) OF 
  "INCLUDE":BEGIN
     fit_status.(fix(uval(1))).include = ev.value
     ENDCASE
     
  "CONST":BEGIN
     fit_status.(fix(uval(1))).const(fix(uval(2))) = ev.value
     ENDCASE
  END
  
  ;; Make arrays include(0..ntags-1), const(0..nconst-1)
  ;;
  ntags = n_elements(tag_names(fit_status))
  
  include = bytarr(ntags)
  
  FOR i = 0,ntags-1 DO BEGIN
     include(i) = fit_status.(i).include
     IF exist(const) THEN const = [const,fit_status.(i).const] $
     ELSE                 const = fit_status.(i).const
  END
  
  event = {ID:EV.HANDLER,TOP:EV.TOP,HANDLER:0L,$
           include:include,$
           const:const}
  
  handle_value,info.int.status_h,fit_status,/set,/no_copy
  
  widget_control,stash,set_uvalue=info,/no_copy
  return,event
END

;; Needed for getting draw windows to show initial status

PRO cwf_status_realize,id
  stash = widget_info(id,/child)
  widget_control,stash,get_uvalue=info,/no_copy
  
  ;; E.g.:
  ;; widget_control,info.int.window_id,get_value=win
  ;; info.int.window = win
  
  widget_control,stash,set_uvalue=info,/no_copy
END


FUNCTION cwf_status,base,value=value,uvalue=uvalue,no_copy=no_copy,$
                    fgcolor=fgcolor,bgcolor=bgcolor,column=column,hilit=hilit
  
  ;; Decide fgcolor after realization
  
  default,fgcolor,-1
  default,bgcolor,0
  default,column,0
  default,hilit,-1
  
  mybase = widget_base(base,$
                       event_func='cwf_status_event',$
                       pro_set_value='cwf_status_setv',$
                       func_get_value='cwf_status_getv',$
                       notify_realize='cwf_status_realize')
  
  ;; Contains nothing but the uvalue (takes no space since the parent base
  ;; is non-structured
  
  stash = widget_base(mybase)
  
  IF exist(uvalue) THEN BEGIN
     default,no_copy,0
     widget_control,mybase,set_uvalue=uvalue,no_copy=no_copy
  END
  
  int = { mybase : mybase,$
          column : column,$
          container_id : 0L,$
          stati : 0,$
          status_h : handle_create() }
  
  ;; Automatically free handle when I die
  
  handle_killer_hookup,int.status_h,group_leader=mybase
  
  ext = { fgcolor:fgcolor,$
          bgcolor:bgcolor,$
          hilit:hilit}
          
  info = {int:int,ext:ext}
  
  cwf_status_makestat,info,value
  
  stash = widget_info(mybase,/child)
  widget_control,stash,set_uvalue=info,/no_copy
  
  return,mybase
END







