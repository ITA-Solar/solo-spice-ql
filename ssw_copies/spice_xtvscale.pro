;+
; Project     : SOHO - CDS     
;                   
; Name        : XTVSCALE
;               
; Purpose     : A widget interface to control image scaling methods
;               
; Explanation : An XTVSCALE object contains a description of how intensities
;               in an image are to be converted into color values for TV'ing.
;
;               Creating an XTVSCALE object is done by calling XTVSCALE
;               without any parameters. The XTVSCALE ID is returned.
;
;               In order to scale an image according to the current status of
;               the scaling object, call XTVSCALE with two parameters: the ID
;               and the image to be scaled. The scaled image (a byte array)
;               will be returned.
;
;               The XTVSCALE object may or may not be visible on the screen.
;               You can always force an XTVSCALE object to become visible
;               by using
;               
;                   dummy = xtvscale(xtvscale_id,/map,/show,iconify=0)
;
;               Making the XTVSCALE object invisible is done by e.g.,
;
;                   dummy = xtvscale(xtvscale_id,map=0) 
;
;               (or by setting /iconify, or setting show=0).
;               
;               If the XTVSCALE object is visible, the user may alter the
;               method used to scale images. The next time the display program
;               uses XTVSCALE, the new status will be reflected in the scaling
;               of the image. If the display program wishes to be informed
;               about a change in the scaling object right away, it should
;               inform xtvscale about it the following  way:
;
;               1. Create a (usually unmapped) widget_base somewhere in the
;               display program widget hierarchy.
;
;               2. Supply the widget ID of this base to XTVSCALE through the
;               keyword SIGNAL, either when creating XTVSCALE, or at some
;               later time. If you're supplying it after the creation, you'll
;               need to specify the xtvscale ID as a parameter, e.g.,
;
;                     dummy = xtvscale(xtvscale_id,signal=base)
;
;               3. When the XTVSCALE object is altered by the user, an event
;               is generated and sent to the widget id's that have been hooked
;               up through the SIGNAL keyword. The event structure,
;               {XTVSCALE_EVENT}, contains the following tags:
;
;               ID         : The ID of the SIGNAL base (NOT the xtvscale ID).
;               HANDLER    : The ID of the event handling base.
;               TOP        : The ID of the topmost base in the hierarchy.
;               XTVSCALE_ID: The XTVSCALE ID
;
;               
; Use         : XTVSCALE_ID = XTVSCALE()
;    
; Inputs      : None reqired.
;               
; Opt. Inputs : XTVSCALE_ID : The ID of the XTVSCALE object be used/modified.
;
;               IMAGE : (Only as parameter number 2, after XTVSCALE_ID)
;                       The image to be scaled into TV values.
;                       
; Outputs     : Returns the XTVSCALE_ID of the new scaling object when called
;               without any parameters.
;
;               Returns the scaled image if called with XTVSCALE_ID and IMAGE.
;
;               Returns 0 on successful completion, nonzero on failure in
;               other cases.
;               
; Opt. Outputs: None.
;               
; Keywords    : TITLE : A title string to be displayed above the scaling
;                       program.
;
;               EXPERT : Set to 1 to start XTVSCALE in expert mode.
;               
;               MISSING : The value of missing data.
;
;               COMP_MISSING : Comparison method for missing data:
;               
;                   -1 : Values less than/equal to MISSING treated as missing
;                    0 : Values exactly matching MISSING treated as missing
;                    1 : Values greater than or equal to MISSING treated as
;                        missing.
;               
;               COLOR_MISSING : The color to give missing pixels.
;
;               AUTO_MISSING : 1 means pass only good pixels to the the
;                              scaling program.
;
;               PROGRAM : An array of strings that make up the scaling
;                         program. No multi-line statements or blocks.
;                         The program should convert the array DATA from
;                         intensity values to color values (byte).
;
;               SIGNAL : The widget ID(s) of those to be informed about
;                        changes to the scaling object.
;
;               DESTROY : Set this keyword to destroy the scaling object.
;
;               XOFFSET,
;               YOFFSET : The x/y offset of the widget showing the
;                         status of the scaling object.
;
;               GROUP_LEADER : The widget ID of the group leader.
;
;               ICONIFY : Set to 1 to make the widget showing the status
;                         become iconified. Set to 0 to de-iconify.
;
;               MAP : Set to 1 to make the widget visible. Set to 0 to make it
;                     invisible
;
;               SHOW : Set to 1 to raise the widget on top of any other
;                      window. Set to 0 to hide it behind all other windows.
;
; Calls       : AVERAGE(), CW_PSELECT(), DEFAULT, HANDLE_CREATE(),
;               HANDLE_KILLER_HOOKUP, HANDLE_KILLER (indirectly),
;               HANDLE_INFO(), PARCHECK, TRIM(), TYP(), WIDGET_BASE(),
;               WIDGET_TEXT(), XLOAD, XMANAGER
;
; Common      : None.
;               
; Restrictions: The user has to press enter to make program changes effective.
;               
; Side effects: None known.
;               
; Category    : Utility, Image
;               
; Prev. Hist. : None.
;
; Written     : Stein Vidar H. Haugan, UiO, 13 June 1996
;               (s.v.h.haugan@astro.uio.no)
;               
; Modified    : Version 2, SVHH, 14 June 1996
;                       Added NOVICE mode, made it default.
;               Version 3, SVHH, 16 June 1996
;                       Made send_event structure become named.
;               Version 4, SVHH, 20 June 1996
;                       Fixed a bug in handling the Exponential/Logarithmic
;                       buttons.
;               Version 5. SVHH, 2 August 1996
;                       Started using HANDLE_KILLER_HOOKUP for automatic
;                       handle freeing.
;               Version 6, SVHH, 22 August 1996
;                       Fixed logarithmic scaling problem with min/max values.
;                       Added a CW_LOADCT widget.
;               Version 7, SVHH, 4 December 1996
;                       Allowing color_missing <> 0 in novice mode.
;                       Negative/zero values are no longer marked missing in
;                       logarithmic scaling, but replaced with min(data gt 0)
;               Version 8, SVHH, 11 March 1998
;                       Fixed fatal bug when NMIN == NMAX in BSCALE call
;		Version 9, William Thompson, GSFC, 8 April 1998
;			Changed !D.N_COLORS to !D.TABLE_SIZE for 24-bit displays
;               Version 10, William Thompson, GSFC, 24 Sep 2010, use [] indexing
;
; Version     : Version 10, 24 Sep 2010
;-            


;
; EVENT handling
;
PRO spice_xtvscale_event,ev
  
  WIDGET_CONTROL,ev.top,get_uvalue=stash
  handle_value,stash,info,/no_copy
  
  WIDGET_CONTROL,ev.id,get_uvalue=uval
  uval = string(uval)
  
  CASE uval OF 
     
  'EXPERT':BEGIN
     ;; Switch to expert mode
     info.ext.expert = 1
     WIDGET_CONTROL,ev.id,set_value='Switch to NOVICE mode'
     WIDGET_CONTROL,ev.id,set_uvalue='NOVICE'
     ;; Show the correct base
     WIDGET_CONTROL,info.int.nbase,map=0
     WIDGET_CONTROL,info.int.xbase,map=1
     ;; Restore choices about missing handling
     WIDGET_CONTROL,info.int.xmissing_id,sensitive=1
     WIDGET_CONTROL,info.int.color_mwid,sensitive=1
     WIDGET_CONTROL,info.int.missingmenu, $
        set_value={SENSITIVE:["MISS+","MISS-","MISS="]}
     ENDCASE
     
     
;
; NOVICE options
;
  'NOVICE':BEGIN
     ;; Switch to novice mode
     info.ext.expert = 0
     WIDGET_CONTROL,ev.id,set_value='Switch to EXPERT mode'
     WIDGET_CONTROL,ev.id,set_uvalue='EXPERT'
     ;; Show the correct base
     WIDGET_CONTROL,info.int.xbase,map=0
     WIDGET_CONTROL,info.int.nbase,map=1
     ;; No choice about missing values: Auto, Exact match, color=0
     info.ext.auto_missing = 1
     WIDGET_CONTROL,info.int.xmissing_id,set_button=1
     WIDGET_CONTROL,info.int.xmissing_id,sensitive=0
     info.ext.comp_missing = 0
     WIDGET_CONTROL,info.int.missingmenu,set_value="MISS="
     WIDGET_CONTROL,info.int.missingmenu, $
        set_value={INSENSITIVE:["MISS-","MISS+","MISS="]}
     ;; But we do tell the user about it!
     WIDGET_CONTROL,info.int.missingbase,map=1
     ENDCASE
     
  'CDS_CLEAN_IMAGEX':BEGIN
     info.ext.cds_clean_image = ev.select
     ENDCASE
     
  'SIGRANGEX':BEGIN
     info.ext.sigrange = ev.select
     WIDGET_CONTROL,info.int.fractbase,map=info.ext.sigrange
     ENDCASE
     
  'FRACTION':BEGIN
     info.ext.fraction = (info.int.fractmin +  $
                          info.int.fractspan * FLOAT(ev.value)/1000.0)
     WIDGET_CONTROL,info.int.fract_id, $
        set_value=trim(info.ext.fraction,'(f5.3)')
     ENDCASE
     
  'MANUALMIN':BEGIN
     info.ext.manualmin = ev.select
     WIDGET_CONTROL,info.int.minbase,map=ev.select
     ENDCASE
     
  'MANUALMAX':BEGIN
     info.ext.manualmax = ev.select
     WIDGET_CONTROL,info.int.maxbase,map=ev.select
     ENDCASE
     
  'RESETMIN':BEGIN
     info.ext.mindata = 1d-21
     ENDCASE
     
  'RESETMAX':BEGIN
     info.ext.maxdata = 1d-21
     ENDCASE
     
  'MINDATA':BEGIN
     info.ext.mindata = ev.value
     ENDCASE
     
  'MAXDATA':BEGIN
     info.ext.maxdata = ev.value
     ENDCASE
     
  'LOGARITHMIC':BEGIN
     IF ev.select THEN BEGIN
        WIDGET_CONTROL,info.int.exp_id,set_button=0
        info.ext.exponential = 0
     END
     info.ext.logarithmic = ev.select
     ENDCASE
     
  'EXPONENTIAL':BEGIN
     IF ev.select THEN BEGIN
        WIDGET_CONTROL,info.int.log_id,set_button=0
        info.ext.logarithmic = 0
     END
     info.ext.exponential = ev.select
     ENDCASE 
     
  'INVERSE':BEGIN
     info.ext.inverse = ev.select
     ENDCASE
     
  'TOPX':BEGIN
     WIDGET_CONTROL,info.int.top_id,map=ev.select
     IF ev.select THEN top = !d.table_size $
     ELSE              top = -1
     IF top NE -1 THEN BEGIN
        WIDGET_CONTROL,info.int.topslide,set_value=top
        WIDGET_CONTROL,info.int.topval_id,set_value=trim(top,'(I3.3)')
     END
     info.ext.top = top
     ENDCASE
     
  'TOP':BEGIN
     info.ext.top = ev.value
     WIDGET_CONTROL,info.int.topval_id,set_value=trim(ev.value,'(I3.3)')
     ENDCASE
     
  'BSCALE':BEGIN
     info.ext.bscale = ev.select
     WIDGET_CONTROL,info.int.bscalebase,map=ev.select
     ENDCASE
     
  'VELOCITY':BEGIN
     info.ext.velocity = ev.select
     ENDCASE
     
  'COMBINED':BEGIN
     info.ext.combined = ev.select
     ENDCASE
     
  'LOWER':BEGIN
     info.ext.lower = ev.select
     ENDCASE
     
  'PROGRAM':BEGIN
     ;; Program changed
     WIDGET_CONTROL,ev.id,get_value=value
     last_nonempty = MAX(WHERE(STRLEN(STRTRIM(value)) GT 0))
     IF last_nonempty EQ -1 THEN value = [''] $
     ELSE value = value[0:last_nonempty]
     handle_value,info.ext.Hprogram,value,/set
     WIDGET_CONTROL,ev.id,set_value=value
     WIDGET_CONTROL,ev.id,set_text_select = ev.offset
  END
     
  'MISSING':info.ext.missing = ev.value
  
  'MISS=' : info.ext.comp_missing = 0
  'MISS+' : info.ext.comp_missing = 1
  'MISS-' : info.ext.comp_missing = -1
  
  'XMISSING' : BEGIN
     info.ext.auto_missing = ev.select
     WIDGET_CONTROL,info.int.missingbase,map = ev.select
  END 
     
  'COLOR_MISS': info.ext.color_missing = ev.value     
  
  ;; Color table manipulators
     
  'XLOADCT': xloadct,group=ev.top
  'XLOAD': xload,group=ev.top
  'XPALETTE': xpalette,group=ev.top
  
  ;;
  ;; Hide/iconify/kill buttons
  ;;
  
  'ICONIFY': BEGIN
     handle_value,stash,info,/set,/no_copy
     WIDGET_CONTROL,ev.top,/iconify
     RETURN
     ENDCASE
     
  'HIDE' :BEGIN
     IF xalive(info.int.group) THEN WIDGET_CONTROL,ev.top,map = 0
     handle_value,stash,info,/set,/no_copy
     RETURN
     ENDCASE
     
  'KILL' :BEGIN
     handle_value,stash,info,/set,/no_copy
     WIDGET_CONTROL,ev.top,/destroy
     RETURN
     ENDCASE
     
  ELSE :BEGIN
     ; Events from color table, we don't need to do anything. 
     ENDCASE

  END
  
  ;; Get ID's of those that wish to be informed.
  ;;
  handle_value,info.int.signals,eventarr ;;; No use of no-copy here
  
  ;; Put back the info structure so the event handlers we're dialing up are
  ;; allowed to call xtvscale without crashing.
  
  handle_value,stash,info,/no_copy,/set
  
  IF N_ELEMENTS(eventarr) gt 0 THEN BEGIN
     event = {spice_xtvscale_EVENT,ID:0L,TOP:0L,HANDLER:0L,XTVSCALE_ID:stash}
     FOR call = 0L,N_ELEMENTS(eventarr)-1 DO BEGIN
        event.id = eventarr[call]
        WIDGET_CONTROL,event.id,send_event = event,bad_id = bad
        IF bad NE 0 THEN MESSAGE,"BAD widget ID encountered",/continue
     END
  END
  
END

;
; Encapsulating the execute statements so they don't do any damage
; to local variables
;
PRO spice_xtvscale_scale_capsule,xsc_sc_program,data
  
  a=0 & b=0 & c=0 & d=0 & e=0 & f=0 & g=0 & h=0 & i=0 & j=0
  
  FOR xsc_sc_i = 0L, N_ELEMENTS(xsc_sc_program)-1 DO BEGIN
     ;; PRINT,xsc_sc_program[xsc_sc_i]
     dummy = execute(xsc_sc_program[xsc_sc_i])
  END
  
END



FUNCTION spice_xtvscale_novice,info,idata
  data = idata
  
  sz = SIZE(data)
  
  sz[sz[0]+1] = 1 ;; Byte type
  colormiss = byte(info.ext.color_missing)
  
  ;; CDS_CLEAN_IMAGE
  
  IF info.ext.cds_clean_image THEN  $
     cds_clean_image,data,missing=info.ext.missing
  
  ;; SIGRANGE
  
  IF info.ext.sigrange THEN  $
     data = sigrange(data,missing=info.ext.missing, $
                     fraction=info.ext.fraction)
  
  good = data NE info.ext.missing
  goodix = WHERE(good,ngood)

  IF ngood EQ 0 THEN RETURN,make_array(SIZE=sz,value=colormiss)
  
  ;; MANUAL MINIMUM/MAXIMUM
  
  IF info.ext.manualmin THEN BEGIN
     IF abs(info.ext.mindata) eq 1d-21 THEN BEGIN
        info.ext.mindata = MIN(data[goodix])
        PRINT,info.ext.mindata - info.ext.missing
        WIDGET_CONTROL,info.int.minbase,set_value=info.ext.mindata
     END
     data[goodix] = data[goodix] > info.ext.mindata
     MIN = info.ext.mindata
  END ELSE MIN = MIN(data[goodix])
  
  
  IF info.ext.manualmax THEN BEGIN
     IF abs(info.ext.maxdata) EQ 1d-21 THEN BEGIN
        info.ext.maxdata = MAX(data[goodix])
        WIDGET_CONTROL,info.int.maxbase,set_value=info.ext.maxdata
     END
     data[goodix] = data[goodix] < info.ext.maxdata
     MAX = info.ext.maxdata
  END ELSE MAX = MAX(data[goodix])
  
  IF info.ext.logarithmic THEN BEGIN
     good_log = (data GT 0.0)
     good_logix = WHERE(good_log,ngood_log)
     IF ngood_log EQ 0 THEN RETURN,make_array(SIZE=sz,value=0b)
     data[good_logix] = alog10(data[good_logix])
     bad_logix = where(good_log-1b,nbad_log)
     IF nbad_log GT 0 THEN data[bad_logix] = min(data[good_logix])
  END
  
  IF info.ext.exponential THEN BEGIN
     avg = average(data[goodix])
     data[goodix] = exp(data[goodix]/avg)
     MIN = exp(MIN/avg)
     MAX = exp(MAX/avg)
     good = good AND (data EQ data)
     goodix = WHERE(good,ngood)
     IF ngood EQ 0 THEN RETURN,make_array(SIZE=sz,value=colormiss)
  END
  
  
  IF info.ext.inverse THEN BEGIN
     data[goodix] = -data[goodix]
     MIN = -MIN
     MAX = -MAX
  END
  
  top = info.ext.top
  IF top EQ -1 THEN top = !D.TABLE_SIZE
  
  nmin = MIN([MIN,MAX])
  nmax = MAX([MIN,MAX])
  
  IF nmin EQ nmax THEN BEGIN
     nmin = nmin-1
     nmax = nmax+1
  END
  
  badix = WHERE(good XOR 1b,nbad)
  IF nbad GT 0 THEN BEGIN
     IF info.ext.bscale THEN data[badix] = info.ext.missing $
     ELSE                    data[badix] = 0
  END
  
  IF info.ext.bscale THEN BEGIN
     IF info.ext.logarithmic THEN BEGIN
        bscale,data,missing=info.ext.missing,top=top,lower=info.ext.lower,$
           combined=info.ext.combined,velocity=info.ext.velocity
     END ELSE BEGIN
        bscale,data,missing=info.ext.missing,top=top,lower=info.ext.lower,$
           combined=info.ext.combined,velocity=info.ext.velocity, $
           MIN=nmin,MAX=nmax
     END

     IF nbad GT 0 THEN data[badix] = colormiss

     RETURN,data
  END
  
  
  IF info.ext.top NE -1 THEN data = data MOD (info.ext.top+1)
  
  RETURN,byte(data MOD 256)
END

;
; Perform a scaling
;
FUNCTION spice_xtvscale_scale,info,idata
  
  ;;
  ;; Check for NOVICE mode
  ;;
  IF NOT info.ext.expert THEN RETURN,spice_xtvscale_novice(info,idata)
  
  handle_value,info.ext.Hprogram,program ;; /no-copy not advisable
  
  ;; Should we just go ahead?
  ;;
  IF info.ext.auto_missing EQ 0 THEN BEGIN
     data = idata
     spice_xtvscale_scale_capsule,program,data
     RETURN,data
  END
  
  ;; No, we should take out missing values first.
  ;;
  
  ;; Missing above, below, or exact.
  ;; 
  CASE 1 OF
     info.ext.comp_missing EQ  0: test = idata EQ info.ext.missing
     info.ext.comp_missing EQ  1: test = idata GE info.ext.missing
     info.ext.comp_missing EQ -1: test = idata LE info.ext.missing
  END
  
  bad = WHERE(test,nbad)
  
  IF nbad EQ 0 THEN BEGIN
     ;; All idata ok
     data = idata
     ;; PRINT,"All data good"
     spice_xtvscale_scale_capsule,program,data
     RETURN,data
  END
  
  IF nbad EQ N_ELEMENTS(idata) THEN BEGIN
     data = make_array(SIZE = SIZE(idata))
     data[*] = info.ext.color_missing
     ;; PRINT,"All data bad"
     RETURN,data
  END
  
  ;; PRINT,"Some good, some bad"
  good = WHERE(test-1b)
  
  image = idata
  data = idata[good]
  spice_xtvscale_scale_capsule,program,data
  image[good] = data
  image[bad] = info.ext.color_missing
  RETURN,image
END


;
; Explanatory text
;

FUNCTION spice_xtvscale_text
  
  text = ['A widget application is using XTVSCALE() to scale images ',$
          'before TV''ing them.', $
          '',$
          'The editable code snippet below is performing the scaling', $
          'of the array DATA from "intensity" to color values.', $
          'The code is executed LINE BY LINE through repeated ', $
          'EXECUTE() statements, so NO MULTI-LINE STATEMENTS ', $
          'ARE ALLOWED (begin/end blocks, continued ($) lines etc).',$
          '',$
          'It is the contents of the variable "DATA" after execution', $
          'of the statements that is returned for TV''ing to the screen.',$
          '',$
          'Remember that statements such as "BSCALE,EXP(DATA)" do NOT ',$
          'change the value of DATA, since EXP(DATA) is an expression.', $
          'Use something like "DATA=EXP(DATA) & BSCALE,DATA" instead.',$ $
          '', $
          'Although multi-line statements are not allowed, you are of',$
          'course allowed to invoke any user-written function accessible',$
          'through your !path',$
          '',$
          'MISSING values have been taken out of the DATA array before', $
          'processing if automatic handling of missing values is selected.', $
          'Points with value equal to MISSING will be replaced by the', $
          'value of COLOR_MISSING'$
         ]
  RETURN,text
END


;
; EXPERT BASE:
;
PRO spice_xtvscale_xpertbase,info,onbase
  
  xbase = WIDGET_BASE(onbase,map = info.ext.expert, $
                      /column,xpad=0,ypad=0,space=0)
  info.int.xbase = xbase
  ;;
  ;; HELP text
  ;;
  helptext = spice_xtvscale_text()
  dummy = WIDGET_TEXT(xbase,xsize = MAX(STRLEN(helptext)),  $
                      ysize = 10,/scroll)
  WIDGET_CONTROL,dummy,set_value=helptext
  
  ;;
  ;; Program, with title.
  ;;
  handle_value,info.ext.Hprogram,program
  dummy = WIDGET_LABEL(xbase,value=info.int.title)
  info.int.programwid = WIDGET_TEXT(xbase,$
                                    ysize=10,xsize=50,/editable,$
                                    value=program,uvalue='PROGRAM')
END

;
; NOVICE BASE:
;
PRO spice_xtvscale_novicebase,info,onbase
  
  tight = {xpad:0,ypad:0,space:0}
  
  nbase = WIDGET_BASE(onbase,map = (info.ext.expert EQ 0),/column)
  info.int.nbase = nbase
  
  ;;
  ;; PREPROCESSING
  ;; 
  
  prep = WIDGET_BASE(nbase,/column,_extra=tight,/frame)
  width = WIDGET_BASE(prep,/column,_extra=tight,xsize=420,map=0,ysize=1)
  dummy = WIDGET_LABEL(WIDGET_BASE(prep), $
                       value='Preprocessing (in the displayed order)')
  
  ;;
  ;; CDS_CLEAN_IMAGE 
  ;;
  nonex = WIDGET_BASE(prep,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='CDS_CLEAN_IMAGE', $
                        uvalue='CDS_CLEAN_IMAGEX')
  WIDGET_CONTROL,dummy,set_button=info.ext.cds_clean_image
  
  ;;
  ;; SIGRANGE:
  ;;
  sigrb = WIDGET_BASE(prep,/row,_extra=tight)
  nonex = WIDGET_BASE(sigrb,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='SIGRANGE',uvalue='SIGRANGEX')
  WIDGET_CONTROL,dummy,set_button=info.ext.sigrange
  
  ;;
  ;; Map/unmap base with FRACTION text/slider
  ;;
  fractbase = WIDGET_BASE(sigrb,/row,_extra=tight)
  info.int.fractbase = fractbase
  
  dummy = WIDGET_LABEL(fractbase,value='Fraction:')
  info.int.fract_id = WIDGET_LABEL(fractbase, $
                                   value=trim(info.ext.fraction,'(f5.3)'))
  
  value = FIX(1000*(info.ext.fraction-info.int.fractmin)/info.int.fractspan)
  
  lift = WIDGET_BASE(fractbase)
  info.int.fractslide = WIDGET_SLIDER(lift,minimum=0,maximum=1000, $
                                      xsize=201,yoffset=6,/drag,$
                                      /suppress_value,uvalue='FRACTION', $
                                      value=value)
  WIDGET_CONTROL,fractbase,map=info.ext.sigrange
  
  ;;
  ;; MANUAL MIN/MAX
  ;;
  minmax = WIDGET_BASE(prep,/row,_extra=tight)
  
  minb = WIDGET_BASE(minmax,/column,_extra=tight)
  rowmin = WIDGET_BASE(minb,/row,_extra=tight)
  nonex = WIDGET_BASE(rowmin,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='Manual minimum',uvalue='MANUALMIN')
  WIDGET_CONTROL,dummy,set_button=info.ext.manualmin
  dummy = WIDGET_BUTTON(WIDGET_BASE(rowmin),value='Reset',uvalue='RESETMIN')
  
  info.int.minbase = cw_field(minb,title='Min:',value=info.ext.mindata, $
                              uvalue='MINDATA',/FLOAT,/return_events)
  WIDGET_CONTROL,info.int.minbase,map=info.ext.manualmin
  
  
  maxb = WIDGET_BASE(minmax,/column,_extra=tight)
  rowmax = WIDGET_BASE(maxb,/row,_extra=tight)
  nonex = WIDGET_BASE(rowmax,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='Manual maximum',uvalue='MANUALMAX')
  WIDGET_CONTROL,dummy,set_button=info.ext.manualmax
  dummy = WIDGET_BUTTON(WIDGET_BASE(rowmax),value='Reset',uvalue='RESETMAX')
  
  info.int.maxbase = cw_field(maxb,title='Max:',value=info.ext.maxdata,$
                              uvalue='MAXDATA',/FLOAT,/RETURN_events)
  WIDGET_CONTROL,info.int.maxbase,map=info.ext.manualmin
  
  ;;
  ;; Linear/Log/exponential scale
  ;; 
  ex = WIDGET_BASE(prep,/nonexclusive,/row)
  
  info.int.log_id = WIDGET_BUTTON(ex,value='Logarithmic scaling', $
                                  uvalue='LOGARITHMIC')
  WIDGET_CONTROL,info.int.log_id,set_button=info.ext.logarithmic
  
  ;; Exponential
  info.int.exp_id = WIDGET_BUTTON(ex,value='Exponential scaling', $
                                  uvalue='EXPONENTIAL')
  WIDGET_CONTROL,info.int.exp_id,set_button=info.ext.exponential
  
  
  ;;
  ;; Inversion
  ;; 
  nonex = WIDGET_BASE(prep,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='Invert data',uvalue='INVERSE')
  WIDGET_CONTROL,dummy,set_button=info.ext.inverse
  
  ;;
  ;; PROCESSING/BYTE SCALING
  ;;
  proc = WIDGET_BASE(nbase,/column,_extra=tight,/frame)
  width = WIDGET_BASE(proc,/column,_extra=tight,xsize=420,ysize=1,map=0)
  dummy = WIDGET_LABEL(WIDGET_BASE(proc), $
                       value='Byte scaling (truncation is default)')
  
  ;;
  ;; TOP =/= !D.TABLE_SIZE-1
  ;;
  
  toptop = WIDGET_BASE(proc,/row,_extra=tight)
  
  nonex = WIDGET_BASE(toptop,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='Set TOP',uvalue='TOPX')
  WIDGET_CONTROL,dummy,set_button=(info.ext.top NE -1)
 
  ;; TOP_ID, base for topval_id and topslide
  
  topb = WIDGET_BASE(toptop,/row,_extra=tight,map=(info.ext.top NE -1))
  info.int.top_id = topb
  
  ;; TOPVAL_ID
  
  dummy = WIDGET_LABEL(topb,value='Value:')
  info.int.topval_id = WIDGET_LABEL(topb, $
                                    value=trim(!D.TABLE_SIZE-1,'(I3.3)'))
  
  ;; TOPSLIDE
  IF info.ext.top EQ -1 THEN value = !d.table_size-1 $
  ELSE                       value = info.ext.top
  
  lift = WIDGET_BASE(topb)
  info.int.topslide = WIDGET_SLIDER(lift,minimum=1,maximum=255, $
                                    xsize=200,yoffset=6,/drag,$
                                    /suppress_value,uvalue='TOP', $
                                    value=value)
  
  ;;
  ;; BSCALE
  ;;
  
  bscaletop = WIDGET_BASE(proc,/row,_extra=tight)
  
  nonex = WIDGET_BASE(bscaletop,/nonexclusive)
  dummy = WIDGET_BUTTON(nonex,value='BSCALE',uvalue='BSCALE')
  WIDGET_CONTROL,dummy,set_button=info.ext.bscale
  
  ;;
  ;; BSCALE options
  ;;
  
  info.int.bscalebase = WIDGET_BASE(bscaletop,/row,/nonexclusive,_extra=tight)
  WIDGET_CONTROL,info.int.bscalebase,map=info.ext.bscale
  
  opt = info.int.bscalebase
  dummy = WIDGET_BUTTON(opt,value='/VELOCITY',uvalue='VELOCITY')
  WIDGET_CONTROL,dummy,set_button=info.ext.velocity
  
  dummy = WIDGET_BUTTON(opt,value='/COMBINED',uvalue='COMBINED')
  WIDGET_CONTROL,dummy,set_button=info.ext.combined
  
  dummy = WIDGET_BUTTON(opt,value='/LOWER',uvalue='LOWER')
  WIDGET_CONTROL,dummy,set_button=info.ext.lower
  
END


;
; Main program
;
FUNCTION spice_xtvscale,SCALE_ID,DATA, title=title,$
                  expert=expert,$
                  missing=missing,comp_missing=comp_missing, $
                  color_missing=color_missing, $
                  auto_missing=auto_missing, $
                  cds_clean_image=cds_clean_image,$
                  sigrange=sigrange,fraction=fraction,$
                  manualmin=manualmin,mindata=mindata,$
                  manualmax=manualmax,maxdata=maxdata,$
                  logarithmic=logarithmic,exponential=exponential,$
                  inverse=inverse,$
                  bscale=bscale, $
                  top=top,velocity=velocity,combined=combined,lower=lower,$
                  program=program,signal=signal,destroy=destroy, $
                  xoffset=xoffset,yoffset=yoffset,$
                  group_leader=group_leader,$
                  $ ;; These only have defaults when creating the compound.
                  iconify=iconify,map=map,show=show 
  
  ON_ERROR,2
  IF !debug NE 0 THEN ON_ERROR,0
  
  ;; 
  ;; Defaults
  ;; 
  default,title,''
  default,expert,0
  default,missing,-1L
  default,comp_missing,0
  default,color_missing,0
  default,auto_missing,1
  
  default,cds_clean_image,0
  default,sigrange,1
  default,fraction,0.9
  default,manualmin,0
  default,mindata,1d-21         ;; Means fill in with min(data) when avail.
  default,manualmax,0
  default,maxdata,1d-21          ;; Means fill in with max(data) when avail.
  default,logarithmic,0
  default,exponential,0
  default,inverse,0
  
  default,bscale,1
  default,top,-1
  default,velocity,0
  default,combined,0
  default,lower,0
  default,program,"BSCALE,DATA"
  default,signal,0L
  
  default,xoffset,0L
  default,yoffset,0L
  default,group_leader,0L
  
  ;;
  ;; Parameter checking
  ;; 
  parcheck,title,        0,typ(/str),0,    'TITLE'
  parcheck,missing,      0,typ(/rea),0,    'MISSING'
  parcheck,color_missing,0,typ(/nat),0,    'COLOR_MISSING',MINVAL=0,MAXVAL=255
  parcheck,comp_missing, 0,typ(/nat),0,    'COMP_MISSING',MINVAL=-1,MAXVAL=1
  parcheck,fraction,     0,typ(/rea),0,    'FRACTION'
  parcheck,mindata,      0,typ(/rea),0,    'MINDATA'
  parcheck,maxdata,      0,typ(/rea),0,    'MAXDATA'
  parcheck,top,          0,typ(/nat),0,    'TOP'
  parcheck,program,      0,typ(/str),[0,1],'PROGRAM'
  parcheck,signal,       0,typ(/lon),[0,1],'SIGNAL'
  
  expert = KEYWORD_SET(expert)
  auto_missing = KEYWORD_SET(auto_missing)
  cds_clean_image = KEYWORD_SET(cds_clean_image)
  sigrange = KEYWORD_SET(sigrange)
  manualmin = KEYWORD_SET(manualmin)
  manualmax = KEYWORD_SET(manualmax)
  logarithmic = KEYWORD_SET(logarithmic)
  exponential = KEYWORD_SET(exponential)
  inverse = KEYWORD_SET(inverse)
  bscale = KEYWORD_SET(bscale)
  velocity = KEYWORD_SET(velocity)
  combined = KEYWORD_SET(combined)
  lower = KEYWORD_SET(lower)
  
  IF NOT expert THEN BEGIN
     auto_missing = 1
     comp_missing = 0
  END
     
  ;; What to do?
  
  IF N_PARAMS() GT 0 THEN BEGIN
     ;; This means we have to do a job.
     
     ;; Check ID
     parcheck,SCALE_ID,1,typ(/lon),0,'SCALE_ID'

     IF handle_info(SCALE_ID,/valid_id) EQ 0 THEN  $
        MESSAGE,"Invalid SCALE_ID passed to xtvscale"
     
     handle_value,SCALE_ID,info,/no_copy
     IF N_ELEMENTS(INFO) EQ 0 THEN  $
        MESSAGE,"SCALE_ID doesn't point to a scale_info structure"
     
     ;; Here we definitely have a valid ID
     
     IF KEYWORD_SET(destroy) THEN BEGIN
        ;; Destroy toplevel widget if it's still alive
        WIDGET_CONTROL,info.int.wid,/destroy,bad_id=bad
        
        ;; Fetch tucked-away data, free handles
        handle_value,info.ext.Hprogram,dummy,/no_copy
        handle_value,info.int.signals,dummy,/no_copy
        handle_free,info.ext.Hprogram
        handle_free,info.int.signals
        handle_free,SCALE_ID
        RETURN,bad
     END
     
     IF N_PARAMS() EQ 2 THEN BEGIN
        ;;
        ;; Two parameters -- do a scaling and  return
        ;;
        image = spice_xtvscale_scale(info,data)
        missing = info.ext.missing
        color_missing = info.ext.color_missing
        handle_value,SCALE_ID,info,/set,/no_copy
        RETURN,image
     END
     
     ;;
     ;; One parameter - possibly adding an event hook
     ;;
     
     IF signal[0] NE 0L THEN BEGIN
        ;;
        ;; Add event hook(s)
        ;; 
        handle_value,info.int.signals,eventarr,/no_copy
        IF N_ELEMENTS(eventarr) EQ 0 THEN eventarr = [signal] $
        ELSE                              eventarr = [eventarr,signal]
        handle_value,info.int.signals,eventarr,/set,/no_copy
        
        ;; Don't do anything more, put back status and return
        handle_value,SCALE_ID,info,/set,/no_copy
        RETURN,0
     END
     
     ;; adjust show/map/iconfiy status and
     ;; exit if no problem
     
     bad = 0L
     
     IF NOT xalive(info.int.wid) THEN GOTO,new_widget
     
     IF N_ELEMENTS(show) NE 0 THEN $
        WIDGET_CONTROL,info.int.wid,show=show,bad_id=bad
     IF bad NE 0 THEN GOTO,NEW_WIDGET
     
     IF N_ELEMENTS(map) NE 0 THEN $
        WIDGET_CONTROL,info.int.wid,map=map,bad_id=bad
     IF bad NE 0 THEN GOTO,NEW_WIDGET
     
     IF N_ELEMENTS(iconify) NE 0 THEN $
        WIDGET_CONTROL,info.int.wid,iconify=iconify,bad_id=bad
     
     IF bad EQ 0L THEN BEGIN
        handle_value,SCALE_ID,info,/set,/no_copy
        RETURN,0
     END
     
     ;; Since there was a problem with our widget, we'll regenerate it:
     GOTO,NEW_WIDGET
  END
  
  ;;
  ;; NEW XTVSCALE object
  ;; 
  
  IF N_ELEMENTS(SCALE_ID) EQ 0 THEN BEGIN
     SCALE_ID = HANDLE_CREATE()
     handle_killer_hookup,scale_id,group_leader=group_leader
  END
  
  handle_value,SCALE_ID,info,/No_copy
  
  IF N_ELEMENTS(info) EQ 0 THEN BEGIN 
     ;;
     ;; Create new info structure -- new scaling object
     ;; 
     ext = {$ ;; xtvscale_ext
            auto_missing     : auto_missing, $
            missing          : DOUBLE(missing), $           ;;
            comp_missing     : comp_missing, $
            color_missing    : color_missing, $        ;;
            cds_clean_image  : cds_clean_image,$            ;;
            sigrange         : sigrange,$
            fraction         : FLOAT(fraction),$
            manualmin        : manualmin,$
            manualmax        : manualmax,$
            mindata          : DOUBLE(mindata),$
            maxdata          : DOUBLE(maxdata),$
            logarithmic      : logarithmic,$
            exponential      : exponential,$
            inverse          : inverse,$
            bscale           : bscale,$
            top              : top,$
            velocity         : velocity,$
            combined         : combined,$
            lower            : lower, $
            expert           : expert, $
            Hprogram         : HANDLE_CREATE() $           ;; Scaling program
           }
     handle_killer_hookup,ext.Hprogram,group_leader=group_leader
     
     int = {$ ;; xtvscale_internal
            group       : group_leader,$
            title       : title,$
            wid         : 0L, $               ;; Widget ID of TLB
            xmissing_id : 0L, $               ;; ID of missing on/off
            missingbase : 0L, $               ;; Base of missing fields
            missingwid  : 0L, $               ;; WID of missing value
            missingmenu : 0L, $               ;; Menu with Exact/above/below
            color_mwid  : 0L, $               ;; WID of color_missing
            xbase       : 0L,$                ;; Expert base
            nbase       : 0L,$                ;; Novice base
            fractbase   : 0L,$                ;; Base for fraction slider
            fractmin    : 0.8,$               ;; Min. fraction, constant
            fractspan   : 0.199,$             ;; Span in fraction, constant
            fract_id    : 0L,$                ;; ID of fraction text
            fractslide  : 0L,$                ;; ID of fraction slider
            minbase     : 0L,$                ;; Base of mindata cw_field
            maxbase     : 0L,$                ;; Base of maxdata cw_field
            log_id      : 0L,$                ;; ID of LOGARITHMIC button
            exp_id      : 0L,$                ;; ID of EXPONENTIAL button
            top_id      : 0L,$                ;; ID of TOP=/=!D.NCOLOR-1 base
            topval_id   : 0L,$                ;; ID of TOP text label
            topslide    : 0L,$                ;; ID of top slider
            bscalebase  : 0L,$                ;; Base of BSCALE options.
            signals     : handle_create(), $  ;; Where to send events
            programwid  : 0L  $               ;; WID of program text field
           }
     handle_killer_hookup,int.signals,group_leader=group_leader
     
     info = {$ ;; 
             int : int,$                     ;; Internal
             ext : ext $                     ;; Editable
            }
     
     handle_value,info.ext.Hprogram,program,/set,/no_copy
  END
  
  ;; If we got a (list of) signal base(s) to inform, we should store
  ;; their ID's
  
  IF signal[0] NE 0L THEN BEGIN
     handle_value,info.int.signals,eventarr,/no_copy
     IF N_ELEMENTS(eventarr) EQ 0 THEN eventarr = [signal] $
     ELSE                              eventarr = [eventarr,signal]
     handle_value,info.int.signals,eventarr,/set,/no_copy
  END
  
  ;; We have created the scaling object. If the widget is supposed to be
  ;; unmapped then we should not construct it anyway.
  ;; 
  ;; Slightly spaghetti....
  ;; 
  
  ;; Default  is to actually show it...
  ;; 
  default,MAP,1
  
  IF NOT KEYWORD_SET(map) THEN GOTO,DONT_REGISTER
  
NEW_WIDGET:
  
  tight = {xpad:0,ypad:0,space:0}
  
  ;;
  ;; Ok, so we (re-)generate the widget.
  ;;
  
  IF xalive(info.int.group) THEN group_leader = info.int.group $
  ELSE group_leader = 0L
  
  base = WIDGET_BASE(/column,title='spice_xtvscale',uvalue=SCALE_ID, $
                     xoffset=xoffset,yoffset=yoffset, $
                     group_leader=group_leader)
  info.int.wid = base
  
  ;;
  ;; NOVICE/EXPERT choice
  ;;
  lft = WIDGET_BASE(base)
  IF info.ext.expert THEN BEGIN
     dummy = WIDGET_BUTTON(lft,value='Switch to NOVICE mode',uvalue='NOVICE')
  END ELSE BEGIN
     dummy = WIDGET_BUTTON(lft,value='Switch to EXPERT mode',uvalue='EXPERT')
  END
  
  ;;
  ;; MISSING SECTION (PRE-PRE-PROCESSING)
  ;;
  
  mframe = WIDGET_BASE(base,/frame,/column,_extra=tight)
  ;;
  ;; Auto-handle missing?
  ;;
  xmb = WIDGET_BASE(mframe,/nonexclusive,_extra=tight)
  xmissing = WIDGET_BUTTON(xmb,value = 'Auto-handle MISSING values', $
                           uvalue='XMISSING')
  info.int.xmissing_id = xmissing
  WIDGET_CONTROL,xmissing,set_button=info.ext.auto_missing
  IF NOT expert THEN WIDGET_CONTROL,xmissing,sensitive=0
  
  ;;
  ;; The missing status base should only be visible when auto_missing is 1
  ;;
  info.int.missingbase = WIDGET_BASE(mframe,/row,xpad=0,ypad=0, $
                                     map=info.ext.auto_missing)
  mbase = info.int.missingbase
  
  ;;
  ;; Choice of comparison method (COMP_MISSING)
  ;;
  MISS_MENU = $
     [{pselect_s,btext:'Exact',mtext:'Exactly',uvalue:'MISS=',flags:0},$
      {pselect_s,'Above','Above','MISS+',0},$
      {pselect_s,'Below','Below','MISS-',0}]
  
  CASE info.ext.comp_missing OF 
     -1:initial = 2
     00:initial = 0
     01:initial = 1
  END
     
  info.int.missingmenu = cw_pselect(mbase,"Missing: ",miss_menu, $
                                    initial=initial)
  
  ;; IF non-expert take away the choices:
  ;; 
  IF NOT info.ext.expert THEN BEGIN
     WIDGET_CONTROL,info.int.missingmenu, $
        set_value = {INSENSITIVE:["MISS-","MISS+","MISS="]}
     WIDGET_CONTROL,info.int.missingmenu,set_value="MISS="
  END
  
  ;;
  ;; Shrink the height of the input field - was too high due to the pdmenu
  ;; 
  shrbase = WIDGET_BASE(mbase,/row,xpad=0,space=0)
  info.int.missingwid = cw_field(shrbase,title=':', /FLOAT,$
                                 ysize=1,xsize=10,/return_events, $
                                 value=info.ext.missing, $
                                 uvalue='MISSING')
  
  info.int.color_mwid = cw_field(shrbase,title='Color of missing',/integer, $
                                 ysize=1,xsize=10,/return_events, $
                                 value=info.ext.color_missing,$
                                 uvalue='COLOR_MISS')
  ;;
  ;; PROCESSING SECTION
  ;;
  
  modebase = WIDGET_BASE(base)
  
  ;; 
  spice_xtvscale_xpertbase,info,modebase
  spice_xtvscale_novicebase,info,modebase
  
  cw_loader = cw_loadct(base)
  
  ;;
  ;; Bottom row buttons
  ;;
  
  row = WIDGET_BASE(base,/row)
  
  ;; This way of making a pulldown menu is just as easy as the blasted
  ;; cw_pdmenu
  
  menu = WIDGET_BUTTON(row,value='Adjust color tables',menu=2)
  dummy = WIDGET_BUTTON(menu,value='XLOADCT',uvalue='XLOADCT')
  dummy = WIDGET_BUTTON(menu,value='XLOAD',  uvalue='XLOAD')
  dummy = WIDGET_BUTTON(menu,value='XPALETTE',uvalue='XPALETTE')
  
  dummy = WIDGET_BUTTON(row,value='Iconify',uvalue='ICONIFY')
  dummy = WIDGET_BUTTON(row,value='Hide window',uvalue='KILL')
  
  ;; This has to be done in the right order...
  
  default,map,1
  default,show,1
  default,iconify,0
  
  WIDGET_CONTROL,base,/realize  ;,map=map,show=show,iconify=iconify
  
  WIDGET_CONTROL,base,iconify=iconify
  WIDGET_CONTROL,base,show=show
  WIDGET_CONTROL,base,map=map
  
  XMANAGER,'spice_xtvscale',base,/just_reg
  
DONT_REGISTER:
  
  handle_value,SCALE_ID,info,/set,/no_copy
  IF N_PARAMS() EQ 0 THEN RETURN,SCALE_ID
  RETURN,0
  
END

