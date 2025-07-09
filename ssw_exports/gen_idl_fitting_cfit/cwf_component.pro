;+
; Project     : SOHO - CDS     
;                   
; Name        : CWF_COMPONENT
;               
; Purpose     : Compound widget showing one fit component
;               
; Explanation : Used by XCFIT (CWF_FIT) to display/edit a Component Fit
;               structure. This widget instantiates one Component.
;               
; Use         : ID = CWF_COMPONENT(BASE,COMPONENT)
;    
; Inputs      : BASE : Have a guess
;
;               COMPONENT : The component to be displayed.
;               
; Opt. Inputs : None.
;               
; Outputs     : Returns ID of compound
;               
; Opt. Outputs: None.
;               
; Keywords    : FRAME : set to the desired width (or zero) of the frame around
;                       the component display. Default width is 5
;
;               UVALUE : The uvalue of the compound.
;
;               COLOR : The color that is used to represent this component.
;
; Calls       : cw_enterb(), cw_flipswitch(), default, since_version(),
;               trim(), widget_base(), xack, xinput, xupdate
;
; Common      : None.
;               
; Restrictions: None.
;               
; Side effects: None.
;               
; Category    : Analysis/Compound widget
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 21 January 1997
;               
; Modified    : Version 2, SVHH, 5 February 1997
;                       Made values and trans_b DOUBLE before trim()'ing them,
;                       to have more significant figures visible.
;               Version 3, SVHH, 10 February 1997
;                       Added a print statement that seems to fix most of
;                       the "Xlib: sequence lost.." errors. Don't ask why.
;               Version 4, SVHH, 2 April 1997
;                       Added since_version test in last update=1 statement.
;               Version 5, SVHH, 25 June 1997
;                       Tried to fix widget problems in IDL v 5.0
;               Version 6, SVHH, 15 September 1997
;                       Tried (again!) to fix IDL v 5.0 widgets...
;               Version 7, SVHH, 1 April 2003
;                       Fixed IDL 5.6 problems causing the notify-realize
;                       routine cwf_component_realize_color to crash. Also
;                       fixed a problem that became apparent in 5.3. More
;                       stuff (related to modal bases) should be fixed, but
;                       going with a modal base the way RSI thinks appropriate
;                       is *not* necessarily a good solution!
;               Version 8, SVHH, 2011-01-07
;                       Fixed typo: widget_conttrol => widget_control
;                       
; Version     : 8, 2011-01-07
;-            
  

FUNCTION cwf_component_getv,id
  
  storage = widget_info(id,/child)
  
  widget_control,storage,get_uvalue=info,/no_copy
  
  value = info.c
  
  widget_control,storage,set_uvalue=info,/no_copy
  
  return,value
END


PRO cwf_component_setv,id,val
  
  storage = widget_info(id,/child)
  
  widget_control,storage,get_uvalue=info,/no_copy
  
  ok = 0
  
  IF datatype(val) EQ 'STC' THEN BEGIN
     str_name = tag_names(val,/structure_name)
     IF str_name EQ info.str_name THEN BEGIN
        xupdate,id,0
        info.c = val
        widget_control,info.name_id,set_value=val.name
        FOR i = 0,n_elements(info.nami)-1 DO BEGIN
           widget_control,info.nami(i),set_value=trim(val.param(i).name)
           widget_control,info.mini(i),set_value=trim(val.param(i).min_val)
           widget_control,info.maxi(i),set_value=trim(val.param(i).max_val)
           widget_control,info.inii(i),set_value=trim(val.param(i).initial)
           widget_control,info.resi(i),$
              set_value=trim(double(val.param(i).value))
           widget_control,info.trai(i),$
              set_value=trim(val.param(i).trans_a)
           widget_control,info.trbi(i),$
              set_value=trim(double(val.param(i).trans_b))
        END
        ok = 1
        xupdate,id,1
     END ELSE IF str_name EQ "CWFIT_COLORFRESH" THEN BEGIN
        ;; colorfield should always contain window values by now..
        FOR i = 0,N_ELEMENTS(info.colorfield)-1 DO BEGIN
           IF info.colorfield[i] LT 0 THEN BEGIN
              widget_control,-info.colorfield[i],get_value=win
              info.colorfield[i] = win
           END
           wset,info.colorfield[i]
           erase,info.color
        END
        ok = 1
     END 
  END
  
  IF NOT ok THEN BEGIN
     message,"Attempt to set value of component "+info.c.name+" failed",$
        /continue
     help,val
  END
  
  widget_control,storage,set_uvalue=info,/no_copy
END



FUNCTION cwf_component_event,ev
  
  storage = widget_info(ev.handler,/child)
  widget_control,storage,get_uvalue=info
  
  widget_control,ev.id,get_uvalue=uvalue
  
  event = 0
  
  uvalue = str_sep(uvalue,':')
  
  IF n_elements(uvalue) GT 1 THEN i = fix(uvalue(1))
  
  CASE uvalue(0) OF 
        
     'ON':BEGIN 
        info.c.include = 1b
        ;;
        ;; Turn on fitting for all parameters as well
        ;;
        xupdate,ev.handler,0
        widget_control,info.fit_onoff_id,set_value='FIT_ON'
        FOR i = 0,n_elements(info.nami)-1 DO BEGIN
           info.c.param(i).const = info.c.param(i).const AND (NOT 1b)
           widget_control,info.onoi(i),set_value='FIT_ON'+'#:'+trim(i)
        END
        xupdate,ev.handler,1
        event = 1
        ENDCASE
        
     'OFF':BEGIN
        info.c.include = 0b
        ;;
        ;; Turn off fitting for all parameters as well
        ;;
        xupdate,ev.handler,0
        widget_control,info.fit_onoff_id,set_value='FIT_OFF'
        FOR i = 0,n_elements(info.nami)-1 DO BEGIN
           info.c.param(i).const = info.c.param(i).const OR 1b
           widget_control,info.onoi(i),set_value='FIT_OFF'+'#:'+trim(i)
        END
        xupdate,ev.handler,1
        event = 1
        ENDCASE
        
     'DESCRIPTION':BEGIN 
        xack,info.c.description
        ENDCASE
        
     'NAME':BEGIN               ; Note - *component* name
        info.c.name = ev.value
        widget_control,ev.id,set_value=ev.value
        event = 1
        ENDCASE
        
     'FIT_ON':BEGIN
        xupdate,ev.handler,0
        FOR i = 0,n_elements(info.nami)-1 DO BEGIN
           info.c.param(i).const = info.c.param(i).const AND (NOT 1b)
           widget_control,info.onoi(i),set_value='FIT_ON'+'#:'+trim(i)
        END
        event = 1
        xupdate,ev.handler,1
        ENDCASE
        
     'FIT_OFF':BEGIN
        xupdate,ev.handler,0
        FOR  i = 0,n_elements(info.nami)-1 DO BEGIN
           info.c.param(i).const = info.c.param(i).const OR 1b
           widget_control,info.onoi(i),set_value='FIT_OFF'+'#:'+trim(i)
        END
        event = 1
        xupdate,ev.handler,1
        ENDCASE
        
;
; The following choices pertains to one of the parameters
;
     'NAME#':BEGIN
        value = info.c.param(i).name
        xinput,value,info.c.param(i).description,/modal,status=status,$
           /accept_enter
        IF status THEN BEGIN
           info.c.param(i).name = value
           event = 1
        END
        widget_control,ev.id,set_value=trim(info.c.param(i).name)
        ENDCASE
        
     'FIT_ON#':BEGIN
        info.c.param(i).const = info.c.param(i).const AND (NOT 1b)
        event = 1
        ENDCASE
        
     'FIT_OFF#':BEGIN
        info.c.param(i).const = info.c.param(i).const OR 1b
        event = 1
        ENDCASE
        
     'MIN#':BEGIN
        info.c.param(i).min_val = double(ev.value)
        widget_control,ev.id,set_value=trim(info.c.param(i).min_val)
        IF info.c.param(i).initial LT info.c.param(i).min_val THEN BEGIN
           info.c.param(i).initial = info.c.param(i).min_val
           widget_control,info.inii(i),set_value=trim(info.c.param(i).initial)
        ENDIF
        IF info.c.param(i).value LT info.c.param(i).min_val THEN BEGIN
           info.c.param(i).value = info.c.param(i).min_val
           widget_control,info.resi(i),$
              set_value=trim(double(info.c.param(i).value))
        ENDIF
        event = 1
        ENDCASE
        
     'MAX#':BEGIN
        info.c.param(i).max_val = double(ev.value)
        widget_control,ev.id,set_value=trim(info.c.param(i).max_val)
        IF info.c.param(i).initial GT info.c.param(i).max_val THEN BEGIN
           info.c.param(i).initial = info.c.param(i).max_val
           widget_control,info.inii(i),set_value=trim(info.c.param(i).initial)
        ENDIF
        IF info.c.param(i).value GT info.c.param(i).max_val THEN BEGIN
           info.c.param(i).value = info.c.param(i).max_val
           widget_control,info.resi(i),$
              set_value=trim(double(info.c.param(i).value))
        ENDIF
        event = 1
        ENDCASE
        
     'INITIAL#':BEGIN
        info.c.param(i).initial = double(ev.value)
        info.c.param(i).value = double(ev.value)
        widget_control,ev.id,set_value=trim(info.c.param(i).initial)
        widget_control,info.resi(i),$
           set_value=trim(double(info.c.param(i).value))
        event = 1
        ENDCASE
        
     'VALUE#':BEGIN
        info.c.param(i).value = double(ev.value)
        widget_control,ev.id,set_value=trim(double(info.c.param(i).value))
        event = 1
        ENDCASE
        
     'TRANS_A#':BEGIN
        info.c.param(i).trans_a = double(ev.value)
        widget_control,ev.id,set_value=trim(info.c.param(i).trans_a)
        event = 1
        ENDCASE
        
     'TRANS_B#':BEGIN
        info.c.param(i).trans_b = double(ev.value)
        widget_control,ev.id,set_value=trim(double(info.c.param(i).trans_b))
        event = 1
        ENDCASE
  END
  
  value = info.c
  
  widget_control,storage,set_uvalue=info,/no_copy
  
  IF event THEN BEGIN
     event = {id:ev.handler, top:ev.top, handler:0L,$
              c:value}
  END
  
  return,event
END


PRO cwf_component_realize_color,id
  WIDGET_CONTROL,id,get_uvalue=storage
  WIDGET_CONTROL,storage,get_uvalue=info,/no_copy
  widget_control,id,get_value=win
  ix = where(info.colorfield lt 0L,count)
  if count gt 0 then info.colorfield[ix[0]] = win
  wset,win
  erase,info.color
  WIDGET_CONTROL,storage,set_uvalue=info,/no_copy
END


FUNCTION cwf_component,on_base,component,uvalue=uvalue,frame=frame,color=color
  
  default,uvalue,'CWF_COMPONENT:'+component.name
  default,frame,5
  
  str_name = tag_names(component,/structure_name)
  
  IF since_version('4.0') THEN sml = 1 ELSE sml = 0
  
  small = {xpad:sml,ypad:sml,space:sml}
  scry_label = 17
  scry_butt = 25
  
  
  base = widget_base(on_base,uvalue=uvalue,frame=frame,_extra=small,$
                     event_func = 'cwf_component_event',$
                     pro_set_value = 'cwf_component_setv',$
                     func_get_value = 'cwf_component_getv',map=0)
  
  xupdate,base,0
  
  ;;
  ;; For some bizarre reason, having this print statement here seems to
  ;; cure the "Xlib: unexpected async reply..." errors (in most cases,
  ;; anyway).
  ;; 
  
  print,"  "
  
  nparms = N_elements(component.param)
  
  inner = widget_base(base,/column,_extra=small)
  storage = inner
  
  upper = widget_base(inner,/row,_extra=small)
  
  ;; The stuff here for producing the colored fields identifying each
  ;; component's color may at times seem quite bizarre, but I've found it to
  ;; be necessary by trial and error.
  ;;
  ;; IDL 3.6.1c has a problem with (re)making the draw window the right size,
  ;; when they're made on an already realized base (they come out 1 pixel
  ;; wide no matter what xsize is set to), hence 20 draw windows are made
  ;; with no space between, looking just like one large window!
  ;;
  ;; Under IDL v 4.0.1, { alpha OSF unix 4.0.1}, I've had problems with the
  ;; fact that even if the draw windows are created on a realized base,
  ;; they're not actually realized in the sense that you can draw anything on
  ;; them - despite having a wset'able value! They come out white. This is
  ;; when widget_control,update=0 is set during widget creation, and it seems
  ;; that the DEVICE,RETAIN=2 setting I'm using has something to do with it as
  ;; well..
  ;; 
  ;; To fix this, XCFIT sends a special set-value structure {CWFIT_COLORFRESH}
  ;; to refresh the colors after update=1 has been set.
  ;; 
  ;; IDL v 5.6 introduced yet another quirk - Even though xupdate,0 has been
  ;; called, it seems the colorfield widget_draw realize-callback routine is
  ;; called *during* the creation; since the widget ID was meant to be
  ;; inserted in the info structure, (and the info structure in earlier IDL
  ;; versions wasn't needed until after this routine was done creating
  ;; everything), earlier versions of this routine made no effort to put it in
  ;; place *before* the widget_draw was created. Now it does, however (putting
  ;; a dummy -1L there, that is).
  ;;
  ;; IDL 5.3 (and probably other versions as well) do not seem to honor the
  ;; realize-callback keyword when creating new widgets on realized bases,
  ;; even if update is set to 0 and later the freeze is lifted. Hmm. So, the
  ;; {CWFIT_COLORFRESH} is coming in handy once again...

  IF n_elements(color) EQ 1 THEN BEGIN
     cb = widget_base(upper,/row,xpad=0,ypad=0,space=0,$
                      xsize=20,ysize=scry_label+4)
     ;; A weird fix is necessary for 3.6.1c... see end of function
     if not since_version('4.0') then colorfield = lonarr(N) $
     else colorfield = -1L
  END
  
  name = widget_label(upper,value='Name: ')
  name_id = cw_enterb(upper,value=component.name,uvalue='NAME',$
                      instruct=['Enter component name, e.g.,','He II'])
  
  desc = widget_button(upper,value='Descr.',uvalue='DESCRIPTION')
  
  handling = ['ON','OFF']
  
  handle_b = cw_flipswitch(upper,value='Include: '+handling,$
                           uvalue=handling)
  IF NOT component.include THEN widget_control,handle_b,set_value='OFF'
  
  onoff = ['ON','OFF']
  fit_onoff_id = cw_flipswitch(upper,value='Fit: '+onoff,uvalue='FIT_'+onoff)
  
  IF total(component.param(*).const NE 0) EQ nparms THEN $
     widget_control,fit_onoff_id,set_value='FIT_OFF'
  
  IF since_version('4.0') THEN BEGIN
     widget_control,name,scr_ysize=scry_label
     widget_control,desc,scr_ysize=scry_label
  END
  
  list = widget_base(inner,/row,_extra=small)
  
  cola = widget_base(list,/column,_extra=small,/frame)
  colax = widget_base(list,/column,_extra=small,/frame)
  colb = widget_base(list,/column,_extra=small,/frame)
  colc = widget_base(list,/column,_extra=small,/frame)
  colcx = widget_base(list,/column,_extra=small,/frame)
  cold = widget_base(list,/column,_extra=small,/frame)
  cole = widget_base(list,/column,_extra=small,/frame)
  colf = widget_base(list,/column,_extra=small,/frame)
  
  IF since_version('4.0') THEN ex = {scr_ysize:scry_label} $
  ELSE                         ex = {uvalue:0}
  
  dummy = widget_label(cola,value='Parameter',_extra=ex)
  dummy = widget_label(colax,value='Fit',_extra=ex)
  dummy = widget_label(colb,value='Min value',_extra=ex)
  
  dummy = widget_label(colc,value='Initial',_extra=ex)
  dummy = widget_label(colcx,value='Value',_extra=ex)
  
  dummy = widget_label(cold,value='Max value',_extra=ex)
  dummy = widget_label(cole,value='Lin. A',_extra=ex)
  dummy = widget_label(colf,value='Lin. B',_extra=ex)
  
  nami = lonarr(nparms)
  onoi = nami
  inii = nami
  resi = nami
  mini = nami
  maxi = nami
  trai = nami
  trbi = nami
  
  FOR i = 0,nparms-1 DO BEGIN
     it = '#:'+trim(i)
     param = component.param(i)
     
     nami(i) = widget_button(cola,value=param.name,uvalue='NAME'+it)
     IF since_version('4.0') THEN widget_control,nami(i),scr_ysize=scry_butt
     
     onoi(i) = cw_flipswitch(colax,value=onoff,uvalue='FIT_'+onoff+it)
     IF param.const THEN widget_control,onoi(i),set_value='FIT_OFF'+it
     
     mini(i) = cw_enterb(colb,value=trim(param.min_val),uvalue='MIN'+it,$
                         instruct='Enter minimum parameter value')
     inii(i) = cw_enterb(colc,value=trim(param.initial),uvalue='INITIAL'+it,$
                         instruct='Enter initial parameter value')
     resi(i) = cw_enterb(colcx,value=trim(double(param.value)),$
                         uvalue='VALUE'+it,$
                         instruct='Enter actual parameter value')
     maxi(i) = cw_enterb(cold,value=trim(param.max_val),uvalue='MAX'+it,$
                         instruct='Enter maximum parameter value')
     trai(i) = cw_enterb(cole,value=trim(param.trans_a),$
                         uvalue='TRANS_A'+it,$
                         instruct='Enter linear coefficient')
     trbi(i) = cw_enterb(colf,value=trim(double(param.trans_b)),$
                         uvalue='TRANS_B'+it,$
                         instruct='Enter additive constant')
  END
  
  info = {c:component,name_id:name_id,fit_onoff_id:fit_onoff_id,$
          nami:nami,onoi:onoi,mini:mini,maxi:maxi,inii:inii,$
          resi:resi,trai:trai,trbi:trbi,$
          str_name:str_name,$
          colorfield:colorfield,color:color}

  widget_control,storage,set_uvalue=info,/no_copy
 
  if not since_version('4.0') then xsize=1 else xsize=20

  FOR i=0,N_ELEMENTS(colorfield)-1 DO BEGIN
     notify_realize='cwf_component_realize_color'
     colorfield[i] = WIDGET_DRAW(cb,xsize=20,ysize=scry_label+4,$
                                 notify_realize=notify_realize,$
                                 uvalue=storage)
  END
  
  xupdate,base,1
  widget_control,base,map=1
  
  widget_control,storage,get_uvalue=info,/no_copy
  ix = where(info.colorfield LT 0,count)
  IF count GT 0 THEN info.colorfield[ix] = - colorfield[ix]
  widget_control,storage,set_uvalue=info,/no_copy
  
  return,base
  
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'cwf_component.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




