;+
; Project     : SOHO - CDS     
;                   
; Name        : CW_PSELECT
;               
; Purpose     : Simplified management of a pulldown selection menu
;               
; Explanation : Often an exclusive button menu takes too much space.  This
;               widget makes it possible to use a pulldown menu for the
;               selection in stead, with automated altering of the top-level
;               button for status change.
;
;               This compound widget is in many respects very similar to the
;               standard IDL library routine CW_BSELECT, except that it is
;               possible to specify both a "menu text" (mtext) and a "button
;               text" (btext). This allows fairly long descriptions of the
;               menu choices, whilst keeping down the space required for the
;               label on the top level button.
;
;               EVENTS
;
;               Another difference from CW_BSELECT is that the returned events
;               are always of standard type {WIDGET_BUTTON}, whose ID refers
;               to the compound widget BASE (not the selected button).  The
;               UVALUE of EVENT.ID is always set to the uvalue of the selected
;               button.
;
;               ALTERING THE CURRENT SELECTION
;
;               It is possible to manipulate the current selection from the
;               main program by setting the compund widget value to the uvalue
;               of the desired selection. The displayed value on the root
;               button will be changed accordingly. E.g.:
;
;               WIDGET_CONTROL,CW_PSEL_ID,SET_VALUE="AA"
;
;               where CW_PSEL_ID is the widget ID returned by CW_PSELECT.
;               This will find the menu entry with UVALUE "AA" and update the
;               display as if the user had selected this menu entry manually.
;
;               (DE)SENSITIZING BUTTONS
;
;               Is possible to turn on/off the sensitivity of specific menu
;               items by e.g.:
;
;               WIDGET_CONTROL,CW_PSEL_ID, $
;                              SET_VALUE={INSENSITIVE:["AA","CC"],$
;                                         SENSITIVE:"BB"}
;
;               This will desensitize the menu items with uvalues "AA" and
;               "BB", and sensitize the menu item with uvalue "BB".
;
; Use         : id = cw_pselect(base,title,pdmenu)
;    
; Inputs      : BASE: The base to put the widget on
;               
;               TITLE: A title to be displayed on the "root" button, normally
;                      indicating what type of data the widget is dealing
;                      with, e.g., "Color chosen:"
;                      
;               PDMENU: An array of structures of the following type:
;
;               {PSELECT_S, btext:'', mtext:'', uvalue:'', flags:0}
;
;               BTEXT is the button text, MTEXT is the menu text (enables
;               longer explanations of the selections), UVALUE is the
;               associated uvalue of the button.  Multi-level pulldown menus
;               are possible with the use of the FLAGS field -- see CW_PDMENU.
;
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : INITIAL : The index of the initial selection to be
;               displayed.
;
;               IDS : A named variable into which the button IDs will
;                     be stored as a longword vector.
;
; Calls       : default parcheck cw_pselect() widget_base()
;
; Common      : None.
;               
; Restrictions: UVALUES must be strings.
;               
; Side effects: None...
;               
; Category    : Compound widget.
;               
; Prev. Hist. : None.
;
; Written     : Stein Vidar Hagfors Haugan, 11-December 1994
;               
; Modified    : Version 2, SVHH, 31 May 1996
;                       Added SET_VALUE possibilities and IDS keyword
;               Version 3, SVHH, 16 October 1996
;                       Minor space-saving change.
;               Version 4, SVHH, 23 October 1996
;                       Added /DYNAMIC_RESIZE for IDL v 4.0.1 and later.
;
; Version     : 4, 23 October 1996
;-            


;
; Setting the value of this widget actually means setting the uvalue
; 
; In order to desensitize a menu option, set the value of the widget to
; a structure with a tag named "insensitive" that contains a string array
; with the uvalues of the options to be grayed out.
;
; In order to sensitize an option, set the value to a structure with a tag
; named sensitive, containing a string array with uvalues of options to
; be sensitized.

PRO cw_pselect_setv,id,uvalue
  
  keeper = WIDGET_INFO(id,/child)
  button = WIDGET_INFO(keeper,/child)
  WIDGET_CONTROL,keeper,get_uvalue=info
  
  IF datatype(uvalue) EQ 'STR' THEN BEGIN
     ix = (WHERE(info.menu[*].uvalue EQ uvalue))(0)
     IF ix EQ -1 THEN  $
        MESSAGE,"Cannot set UVALUE="+uvalue+" for button "+info.title,/continue
     
     selection = info.menu[ix].btext
     WIDGET_CONTROL,button,set_value=info.title+selection
  
     WIDGET_CONTROL,id,set_uvalue=info.menu[ix].uvalue
  END ELSE IF datatype(uvalue) EQ 'STC' THEN BEGIN
     
     ;; Sensitize any buttons?
     IF tag_exist(uvalue,"sensitive",/top_level) THEN BEGIN
        sensitive = uvalue.sensitive
        FOR i = 0,N_ELEMENTS(sensitive)-1 DO BEGIN
           ix = WHERE(info.menu[*].uvalue EQ sensitive(i),count)
           IF count GT 0 THEN BEGIN
              FOR ii = 0,N_ELEMENTS(ix)-1 DO BEGIN
                 WIDGET_CONTROL,info.ids(ix(ii)+1),/sensitive
              END
           END
        END
     END
     
     ;; DEsensitize any buttons?
     IF tag_exist(uvalue,"insensitive",/top_level) THEN BEGIN
        insensitive = uvalue.insensitive
        FOR i = 0,N_ELEMENTS(insensitive)-1 DO BEGIN
           ix = WHERE(info.menu[*].uvalue EQ insensitive(i),count)
           IF count GT 0 THEN BEGIN
              FOR ii = 0,N_ELEMENTS(ix)-1 DO BEGIN
                 WIDGET_CONTROL,info.ids(ix(ii)+1),sensitive = 0
              END
           END
        END
     END
  END
  
END



FUNCTION cw_pselect_event,ev
  Widget_CONTROL,ev.id,Get_UVALUE=info
  
  ;; EV.VALUE is INDEX into menu
  selection = info.menu[ev.value-1].btext
  
  ;; Set value of root button
  Widget_CONTROL,widget_info(ev.id,/child),Set_value=info.title+selection
  
  ;; Set UVALUE of the compound widget
  Widget_control,ev.handler,set_uvalue=info.menu[ev.value-1].uvalue
  
  ;; Send on the event, with new ID
  ev.id = ev.handler
  return,ev
END




FUNCTION cw_pselect,base,title,options,initial=initial,IDS=IDS
  
  IF N_params() lt 3 THEN message,"Use: ID = CW_PSELECT(BASE,TITLE,OPTIONS)"
  
  parcheck,base,1,typ(/lon),0,'BASE'
  parcheck,title,2,typ(/str),0,'TITLE'
  parcheck,options,3,typ(/stc),1,'Menu options'
  
  IF tag_names(options,/struct) ne 'PSELECT_S' THEN $
	  message,"OPTIONS must be an array of {PSELECT_S}"
  
  

  default,initial,(WHERE((options.flags AND 1) EQ 0))(0) > 0
  
  parcheck,initial,0,typ(/nat),0,'INITIAL (menu selection)'

  initial = (initial > 0) < (N_elements(options)-1)
  
  dummy = {CW_PDMENU_S, flags:0, name:''}
  
  menu = [{CW_PDMENU_S, 1, title+options(initial).btext},$
	  replicate({CW_PDMENU_S},N_elements(options))]
  
  menu[1:*].flags = options(*).flags
  menu[1:*].name  = options(*).mtext
  menu[N_elements(options)].flags = 2
  
  info = { menu:options, title:title, ids:lonarr(N_ELEMENTS(menu))}
  
  subbase = Widget_BASE(base,event_func='cw_pselect_event',$
                        pro_set_value='cw_pselect_setv')
  
  storage = cw_pdmenu(subbase,menu,IDS=IDS,xoffset = 0,yoffset = 0)
  IF since_version('4.0.1') THEN BEGIN
     widget_control,ids(0),/dynamic_resize
  END
  
  info.ids = ids
  WIDGET_CONTROL,storage,set_uvalue=info
  
  WIDGET_CONTROL,subbase,set_uvalue=options(initial).uvalue
  
  return,subbase
END

; Test program
;----------------------

PRO pselect_event,ev
  WIDGET_CONTROL,ev.id,get_uvalue=uval
  PRINT,"Uvalue :",uval
  
  ab = ['UVALUE_A','UVALUE_B']
  cdequit = "UVALUE_"+['C','D','E','QUIT']
  
  IF total(uval EQ ab) GT 0 THEN BEGIN
     WIDGET_CONTROL,ev.id,set_value={sensitive:cdequit,insensitive:ab}
  END ELSE IF total(uval EQ cdequit) GT 0 THEN BEGIN
     WIDGET_CONTROL,ev.id,set_value={sensitive:ab,insensitive:cdequit}
  END
  
  IF UVAL EQ 'UVALUE_QUIT' THEN WIDGET_CONTROL,ev.top,/destroy
END


PRO pselect
  
  b = Widget_BASE(/column)
  
  dummy = {PSELECT_S, btext:'', mtext:'', uvalue:'', flags:0}
  
  options=[{PSELECT_S, 'Button A', 'Menu A', 'UVALUE_A', 0},$
	   {PSELECT_S, 'Button B', 'Menu B', 'UVALUE_B', 0},$
	   {PSELECT_S, 'Button C', 'Menu C', 'UVALUE_C', 0},$
	   {PSELECT_S, 'Button D', 'Menu D', 'UVALUE_D', 0},$
           {PSELECT_S, 'Button E', 'Menu E', 'UVALUE_E', 0},$
           {PSELECT_S, 'QUIT',     'Menu QUIT', 'UVALUE_QUIT',0}]
  
  c = cw_pselect(b,'Selection:',options)
  WIDGET_CONTROL,c,set_value={insensitive:"UVALUE_"+["C","D","E","QUIT"]}
  
  d = widget_label(b,value='This is a label')
  
  Widget_CONTROL,b,/realize
  
  XMANAGER,'pselect',b,/modal
  
END
