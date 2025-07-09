;+
; Project     : SOHO - CDS     
;                   
; Name        : CWF_FIT
;               
; Purpose     : Comp. widget for showing/editing Component Fit structures
;               
; Explanation : Used by XCFIT to display/manipulate a component fit
;               structure. 
;               
; Use         : ID = CWF_FIT(BASE,CFIT)
;    
; Inputs      : BASE : To put it on.
;
;               CFIT : Component Fit structure.
;               
; Opt. Inputs : None.
;               
; Outputs     : Returns compound widget id
;               
; Opt. Outputs: None.
;               
; Keywords    : UVALUE : of the compound
;
; Calls       : cwf_component(), default, handle_create(),
;               handle_killer_hookup, since_version(), widget_base(),
;               xupdate
;
; Common      : None.
;               
; Restrictions: None.
;               
; Side effects: None.
;               
; Category    : Analysis
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 21 January 1997
;               
; Modified    : Version 2, SVHH, 15 September 1997
;                       Fixed most of the stupid IDL v 5.0 mess.
;
; Version     : 2, 15 September 1997
;-            

PRO cwf_fit_setv,id,val
  
  storage = widget_info(id,/child)
  widget_control,storage,get_uvalue=info
  
  sname = tag_names(val,/structure_name)
  IF sname EQ "CWFIT_COLORFRESH" THEN BEGIN
     FOR i = 0,N_ELEMENTS(info.comp_ids)-1 DO BEGIN
        WIDGET_CONTROL,info.comp_ids(i),set_value=val
     END
  END ELSE BEGIN
     
     ntags = N_ELEMENTS(tag_names(val))
     
     xupdate,id,0
     
     FOR i = 0,ntags-1 DO BEGIN
        WIDGET_CONTROL,info.comp_ids(i),set_value=val.(i)
     END
     
     ;; IDL v 5.0(.2) fix of update mess!
     
     IF since_version('5.0') THEN BEGIN
        dummy = widget_base(info.base,uvalue='IDL v 5 sucks!')
     END
     
     xupdate,id,1
     
     handle_value,info.hfit,val,/set
  END
END


FUNCTION cwf_fit_getv,id
  
  storage = widget_info(id,/child)
  
  widget_control,storage,get_uvalue=info,/no_copy
  
  handle_value,info.hfit,value
  
  widget_control,storage,set_uvalue=info,/no_copy
  return,value
END


FUNCTION cwf_fit_event,ev
  
  storage = widget_info(ev.handler,/child)
  widget_control,storage,get_uvalue=info,/no_copy
  
  widget_control,ev.id,get_uvalue=uvalue
  
  handle_value,info.hfit,value,/no_copy
  
  value.(uvalue) = ev.c
  
  fit_handle = info.hfit
  
  handle_value,info.hfit,value,/set,/no_copy
  widget_control,storage,set_uvalue=info,/no_copy
  
  event = {id:ev.handler, top:ev.top, handler:0L,$
           fit_handle:fit_handle}
  
  return,event
END


FUNCTION cwf_fit,on_base,fit,uvalue=uvalue
  
  default,uvalue,'CWF_FIT'
  
  IF since_version('4.0') THEN sml = 1 ELSE sml = 0
  small = {xpad:sml,ypad:sml,space:sml}
  
  base = widget_base(on_base,/column,uvalue=uvalue,$
                     _extra=small,$
                     event_func = 'cwf_fit_event',$
                     func_get_value = 'cwf_fit_getv',$
                     pro_set_value = 'cwf_fit_setv')
  
  components = tag_names(fit)
  ncomp = n_elements(components)
   
  upper = widget_base(base,/row,_extra=small)
  storage = upper
  
  comp_ids = lonarr(ncomp)
  
  FOR i = 0,n_elements(components)-1 DO BEGIN
     comp_ids(i) = cwf_component(base,fit.(i),uvalue=i,color=2+i)
  END
  
  hfit = handle_create()
  handle_killer_hookup,hfit,group_leader=base ; Kill handle when base dies
  
  handle_value,hfit,fit,/set
  
  info = { base:base,$
           comp_ids : comp_ids,$
           hfit : hfit }
  
  widget_control,storage,set_uvalue=info,/no_copy
  
  return,base
  
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'cwf_fit.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


