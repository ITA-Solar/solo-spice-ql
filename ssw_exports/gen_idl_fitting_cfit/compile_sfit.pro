;+
; Project     : SOHO - CDS     
;                   
; Name        : COMPILE_SFIT
;               
; Purpose     : Write and compile evaluation function for given SFIT
;               
; Explanation : Every SFIT corresponds to a specific function that can be
;               compiled and executed by e.g., MCURVEFIT in to evaluate the
;               function being fitted. This routine writes and compiles that
;               function, unless it's already compiled or unless errors
;               occur.
;
;               The output is placed in the directory pointed to by the
;               environment variable "IDL_COMPILE_DIR", or in the current
;               directory if IDL_COMPILE_DIR is not set. The directory pointed
;               to by the environment variable should be in the IDL !path, of
;               course, and be writable.
;
;               NOTE that IDL_COMPILE_DIR should be a *private* directory,
;               to avoid security problems.
;               
; Use         : COMPILE_SFIT,SFIT
;    
; Inputs      : SFIT : "Short" fit structure -- see e.g., MAKE_SFIT_STC
;               
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : concat_dir(), since_version(), test_open(), trim(), wrt_ascii
;
; Common      : COMPILE_SFIT_COMMON
;               To circumvent routine_info() bug.
;               
; Restrictions: See Explanation for the IDL_COMPILE_DIR, though most problems
;               are caught and handled correctly, signalling failure by
;               setting SFIT.COMPILED = 2b
;               
; Side effects: Writes an IDL program to IDL_COMPILE_DIR, or to current
;               directory. 
;               
; Category    : Analysis
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 21 January 1997
;               
; Modified    : Version 2, SVHH, 2 April 1997
;                       Added CATCH,error to work with demo mode.
;               Version 3, SVHH, 15 January 1999
;                       Using common block instead of routine_info() to
;                       log successfully compiled functions.
;                       
; Version     : 3, 15 January 1999
;-            

PRO compile_sfit,sfit
  
  ;; Thanks to bug in routine_info(), routines that have failed to compile
  ;; correctly will show up in the list of compiled routines.
  ;;
  ;; It is therefore necessary with a private list here.
  ;;
  COMMON compile_sfit_common,okay 
  IF n_elements(okay) EQ 0 THEN okay = ['']
  
  ;;
  ;; Just return if already compiled or already failed
  ;; 
  IF sfit.compiled EQ 1b OR sfit.compiled EQ 2bTHEN return
  
  ;;
  ;; Something's wrong if we don't get to the end..
  ;; 
  sfit.compiled = 2b 
  
  catch,error
  IF error NE 0 THEN BEGIN 
     catch,/cancel
     message,"Couldn't write/compile "+sfit.compiledfunc,/continue
     return
  END
  
  name = sfit.compiledfunc
  
  ;
  ; Check for DLM version first!
  ;
  sys = routine_info(/system)
  IF total(strupcase(name) EQ sys) EQ 1 THEN BEGIN
     sfit.compiled = 1b
     return
  END
  
  IF (where(okay EQ strupcase(name)))(0) NE -1 THEN BEGIN
     sfit.compiled = 1b
     return
  END
  
  ;; Check to see if the program has already been written
  
  path = getenv("IDL_COMPILE_DIR")
  
  file = concat_dir(path,name+".pro")
  IF test_open(file,/nodir) AND since_version('4.0') THEN $
     GOTO,WRITTEN  ;; Already written 
  
  ;; Demo mode for IDL will cause an error in the attempt to write...
  IF NOT test_open(file,/nodir,/write) THEN BEGIN
     message,"Could not write program "+file,/continue
     sfit.compiledfunc = '-'
     return
  END
  
; Write program...
  
  call_proc_escape = ''
  
  IF NOT since_version('4.0') THEN $
     call_proc_escape = 'if n_params() eq 0 then return'
  
  protxt = 'PRO '+name+',x,a,f,pder'
  
  prog =[ 'on_error,0',$
          '', $
          call_proc_escape,$ ;; Allows easy compilation by CALL_PROCEDURE
          '', $
          'nx = n_elements(x)',$
          '',$
          'use_pder = (n_params() eq 4)',$
          '',$
          'type = datatype(a,2)',$
          'if use_pder then begin',$
          '    pder = make_array(nx,n_elements(a),type=type,/nozero)',$
          'end',$
          '',$
          'f = make_array(nx,type=type)' $
        ]
  
  use_from = 0
  
  FOR c = 0,n_elements(sfit.functs)-1 DO BEGIN
     use_to = use_from + sfit.n_parms(c) - 1
     func = sfit.functs(c)
     prog = [prog,$
             'atemp = a('+trim(use_from)+':'+trim(use_to)+')',$
             'if use_pder then begin',$
             '   pder_temp = 1',$
             '   '+func+',x,atemp,ftemp,pder_temp',$
             '   pder(0,'+trim(use_from)+') = pder_temp',$
             'end else begin',$
             '   '+func+',x,atemp,ftemp',$
             'end',$
             '',$
             'f = temporary(f) + ftemp'$
            ]
     use_from = use_to + 1
  ENDFOR
  
  comment = ['; Produced by compile_sfit.pro ',$
             ';', $
             '; To direct these automatically produced files to another',$
             '; directory, set the environment variable IDL_COMPILE_DIR',$
             '; to point at the directory.',$
             '']
  
  program = [comment,protxt,'    '+prog,'end']
  
  wrt_ascii,program,concat_dir(path,name)+".pro"
  
WRITTEN:
  
  add_path, path
  IF since_version('4.0') THEN BEGIN
     dummy = execute("resolve_routine,name")
  END ELSE BEGIN
     dummy = execute("call_procedure,name")
  END
  
  IF dummy EQ 0 THEN BEGIN
     message,'Could not compile '+name,/continue
     sfit.compiledfunc = '-'
  END ELSE BEGIN
     ;; Everything's ok... continue...and register
     sfit.compiled = 1b
     okay = [okay,strupcase(name)]
  END
  
END
