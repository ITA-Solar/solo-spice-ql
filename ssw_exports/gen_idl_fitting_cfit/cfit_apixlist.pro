;+
; Project     : SOHO - CDS     
;                   
; Name        : CFIT_APIXLIST()
;               
; Purpose     : Return masked CFIT analysis pixels given masking program
;               
; Explanation : This function works like the masking feature in XCFIT_BLOCK
;               Select the "Edit masking program" from the "Mask/patch points
;               menu". The upper panel of text explains how to write masking
;               programs.
;
;               Given a program that is able to calculate a logical mask
;               corresponding to each "spatial" point in a CFIT analysis, this
;               function will execute the program and return the list of
;               pixels where the calculated mask was "true".
;
;               This function is often used in conjunction with the
;               CFIT_APATCH (or lower-level CFIT_BPATCH) routine(s).
;
;               Example 1:
;
;               ;; "FAIL" all positions with average flux/pixel less than 5
;               ;;
;               ix=cfit_apixlist(ana,'mask=average(data,1,miss=missing) LT 5')
;               cfit_apatch,ana,"FAIL",ix
;
;               Example 2:
;               
;               ;; "FREEZE" parameter 1 ONLY at all positions where
;               ;; average(signal) is less than 2.5 times average(noise):
;
;               progfail = $
;                ['a = sqrt(1./(weights>1e-6)) ; Noise',$
;                 'ix=where(data eq missing or weights eq missing) ; Bad px',$
;                 'if ix[0] ne -1L then a[ix] = missing  ; Take em out',$
;                 'b = average(a,1,missing=missing)     ;Average noise level',$
;                 'c = average(data,1,missing=missing)  ;Average signal',$
;                 'mask = c lt 2.5*b                    ;Decide...']
;
;               ;; Get list of points
;               
;               ix=cfit_apixlist(ana,progfail)
;               
;               cfit_apatch,ana,"FREEZE",cfit_apixlist(ana,progfail),1
;               
; Use         : ix = cfit_apixlist(ana,program)
; 
; Inputs      : ANA : CFIT analysis structure containing data.
;
;               PROGRAM : Text array with a series of one-line statements.
;                         See the explanation given by the 'Edit masking
;                         program' option in XCFIT_BLOCK.
;
; Opt. Inputs : None.
;
; Outputs     : Returns - literally, the result of "where(mask)"
;               
; Opt. Outputs: None.
;               
; Keywords    : None
;
; Calls       : EXIST(), XACK, DELVARX, HAVE_WINDOWS()
;
; Common      : None.
;               
; Restrictions: None.
;               
; Side effects: None.
;               
; Category    : Line fitting.
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 1998
;               
; Modified    : Version 2, 3 February 1999
;                       Added test for have_windows() before using xack.
;               Version 3, 8-Oct-2015, WTT, Use [] for array indices
;
; Version     : 3, 8-Oct-2015
;-            

PRO cfit_apixlist_exec,program,lambda,data,weights,fit,missing,$
                         result,residual,include,const,mask
  
  sz = size(const[0,*,*,*,*,*,*])
  sz[0] = sz[0]-1
  sz = [sz[0],sz[2:*]]
  
  catch,error
  IF error NE 0 THEN BEGIN
errorcatch: catch,/cancel
     msg = ["The following error occured while processing a program ",$
            "to determine a pixel mask:","",!err_string,""]
     IF exist(done_sofar) THEN BEGIN
        msg = [msg,'The following statements had been/was being processed',$
               done_sofar]
     END
     msg = [msg,'',"A blank mask will be returned"]
     
     IF have_windows() THEN xack,msg $
     ELSE FOR i=0,n_elements(msg)-1 DO message,msg[i],/continue
     mask = make_array(size=sz)
     return
  END
  
  a = 0 & b = 0 & c = 0 & d = 0 & e = 0 & f = 0 & ix = 0
  
  delvarx,mask
  
  FOR i = 0,n_elements(program)-1 DO BEGIN
     dummy = execute(program[i])
     IF dummy NE 1 THEN GOTO,errorcatch
  END 
  
  IF NOT exist(mask) THEN BEGIN
     msg = 'MASK was not defined by the program - ' + $
        'a blank mask will be returned'
     IF have_windows() THEN xack,[msg] $
     ELSE message,xack,/continue
     mask = make_array(size=sz)
  ENDIF
  
  mask = reform(mask)
END

FUNCTION cfit_apixlist,ana,prg
  handle_value,ana.lambda_h,lambda,/no_copy
  handle_value,ana.data_h,data,/no_copy
  handle_value,ana.weights_h,weights,/no_copy
  handle_value,ana.fit_h,fit,/no_copy
  missing = ana.missing
  handle_value,ana.result_h,result,/no_copy
  handle_value,ana.residual_h,residual,/no_copy
  handle_value,ana.include_h,include,/no_copy
  handle_value,ana.const_h,const,/no_copy
  
  cfit_apixlist_exec,prg,lambda,data,weights,fit,missing,result,residual,$
     include,const,mask
  
  ix = where(mask)
  
  handle_value,ana.lambda_h,lambda,/no_copy,/set
  handle_value,ana.data_h,data,/no_copy,/set
  handle_value,ana.weights_h,weights,/no_copy,/set
  handle_value,ana.fit_h,fit,/no_copy,/set
  handle_value,ana.result_h,result,/no_copy,/set
  handle_value,ana.residual_h,residual,/no_copy,/set
  handle_value,ana.include_h,include,/no_copy,/set
  handle_value,ana.const_h,const,/no_copy,/set
  
  return,ix
END
