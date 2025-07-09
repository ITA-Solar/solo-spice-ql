;+
; Project     : SOHO - CDS     
;                   
; Name        : CFIT_APATCH
;               
; Purpose     : Patch CFIT analysis result at specific points
;               
; Explanation : Modify fit parameter/status at points given by IX.
;
;               Example - modifying and fixing two parameters on the basis of
;               where another parameter is below a certain threshold
;
;                xcfit_block,analysis=ana
;                handle_value,ana.result_h,result
;
;                ;; Find points below threshold
;                ix = where(result[0,*,*] lt 6 and result[0,*,*] ne -100)
;                
;                ;; Fix line position (parameter 1) at those points
;                average_linepos=average(result[1,*,*],missing=-100)
;                cfit_apatch,ana,"RESULT",ix,1,average_linepos
;                cfit_apatch,ana,"FREEZE",ix,1
;
;               Also, the PARAM and VALUE parameters may be arrays (with equal
;               number of elements!)
;
;               The allowed modes of operation are explained below, see the
;               KEY parameter.
;               
; Use         : CFIT_APATCH,ANALYSIS,KEY,IX [,PARAM,VALUE]
;    
; Inputs      : ANALYSIS : A CFIT analysis structure.
;
;               KEY : A string describing the action to be
;                     taken. Possibilities are:
;
;                     FAIL : Set all RESULT values (and chi^2) to missing,
;                            freeze them, and flag all fit components as
;                            excluded. Ignores PARAM/VALUE.
;
;                     UNFAIL : Restore all RESULT/CONST/INCLUDE values to
;                              initial values (given in the fit structure).
;                              Inores PARAM/VALUE.
;
;                     INITIAL : Restore [all or some] result values to initial
;                               values. PARAM optional, VALUE ignored.
;
;                     FREEZE : Set CONST status for all or some
;                              parameters. PARAM optional, VALUE ignored.
;
;                     THAW : Clear CONST status for all/some parameters. PARAM
;                            optional, VALUE ignored.
;
;                     RESULT : Set RESULT value(s) specified by PARAM to
;                              VALUE.
;
;                     INCLUDE : Set INCLUDE value(s) specified by PARAM to
;                               VALUE.
;
;                     CONST : Set CONST value(s) specified by PARAM to VALUE.
;
;               IX : One-dimensional index into e.g. RESULT[0,*,*,*..]
;                    specifying which points to be affected by the patch
;                    operation. In "pseudo-code" the operation resulting from
;                    using the RESULT/INCLUDE/CONST KEY values can be written:
;
;                       (KEY[PARAM,*,*,...*])[IX] = VALUE
;
; Opt. Inputs : PARAM : Specifies which parameter(s) or component(s) that are
;                       to be patched.
;
;               VALUE : The value(s) of the patched RESULT/CONST/INCLUDE
;                       points. Must have an equal number of elements as
;                       PARAM.
;                       
; Outputs     : Alters the values in the ANALYSIS input.
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : make_sfit_stc(), cfit_bpatch()
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
; Written     : S.V.H. Haugan, 1998
;               
; Modified    : Version 2, 8-Oct-2015, WTT, use [] for array indices
;                       
; Version     : 2, 8-Oct-2015
;-            
PRO cfit_apatch,ana,key,ix,param,value
  on_error,0
  IF !debug GT 0 THEN on_error,0
  
  IF n_params() LT 2 THEN $
     message,"Use: CFIT_APATCH, ANA, KEY, IX [, PARAM, VALUE]"
     
  IF n_elements(value) NE 0 AND n_elements(param) NE n_elements(value) THEN $
     message,"PARAM and VALUE must have same number of elements"
  
  IF ix[0] EQ -1L THEN return
  
  parcheck,key,2,typ(/str),0,'KEY'
  
  handle_value,ana.history_h,history,/no_copy
  handle_value,ana.lambda_h,lambda,/no_copy
  handle_value,ana.data_h,data,/no_copy
  handle_value,ana.weights_h,weights,/no_copy
  handle_value,ana.fit_h,fit,/no_copy
  handle_value,ana.result_h,result,/no_copy
  handle_value,ana.residual_h,residual,/no_copy
  handle_value,ana.include_h,include,/no_copy
  handle_value,ana.const_h,const,/no_copy
  
  missing = ana.missing
  
  ncomp = (size(include))[1]
  nparm = (size(const))[1]
  ndata = (size(data))[1]
  
  iresi = lindgen(ndata) & mresi = missing+dblarr(ndata)
  iresu = lindgen(nparm+1) & mresu = missing+dblarr(nparm+1)
  icons = lindgen(nparm) & mcons = 1b + bytarr(nparm)
  iincl = lindgen(ncomp) & mincl = bytarr(ncomp)
  
  sfit = make_sfit_stc(fit)
  
  CASE STRUPCASE(key) OF
     
     ;; FAIL/UNFAIL ignore PARAM/VALUE
     
  "FAIL":BEGIN
     cfit_bpatch,residual,ix,iresi,mresi  ;; Residual,Result,Const,Include
     cfit_bpatch,result,ix,iresu,mresu
     cfit_bpatch,const,ix,icons,mcons
     cfit_bpatch,iincl,ix,iincl,mincl
     ENDCASE 
     
  "UNFAIL":BEGIN
     cfit_bpatch,result,ix,iresi,sfit.a_nom ;; Set to initial values
     cfit_bpatch,const,ix,icons,sfit.const
     cfit_bpatch,iincl,ix,iincl,sfit.include
     ENDCASE
     
     ;; INITIAL/FREEZE/THAW may take PARAM, ignores VALUE
     
  "INITIAL":BEGIN ;; VALUE not specified - use initial
     IF n_params() EQ 3 THEN param = lindgen(nparm)
     cfit_bpatch,result,ix,param,sfit.a_nom[param]
     ENDCASE
     
     
  "FREEZE":BEGIN 
     IF n_params() EQ 3 THEN param = icons ;; ALL parameters unless specified
     cfit_bpatch,const,ix,param,1b+byte(param*0)
     ENDCASE
     
  "THAW":BEGIN
     IF n_params() EQ 3 THEN param = icons ;; ALL parameters unless specified
     cfit_bpatch,const,ix,param,byte(param*0)
     ENDCASE
     
     ;; The following options require the PARAM/VALUE parameters to be set
     
  "RESULT":BEGIN
     cfit_bpatch,result,ix,param,value
     ENDCASE
  "INCLUDE":BEGIN
     cfit_bpatch,include,ix,param,value
     ENDCASE
  "CONST":BEGIN
     cfit_bpatch,const,ix,param,value
     ENDCASE
  END
  
  handle_value,ana.history_h,history,/no_copy,/set
  handle_value,ana.lambda_h,lambda,/no_copy,/set
  handle_value,ana.data_h,data,/no_copy,/set
  handle_value,ana.weights_h,weights,/no_copy,/set
  handle_value,ana.fit_h,fit,/no_copy,/set
  handle_value,ana.result_h,result,/no_copy,/set
  handle_value,ana.residual_h,residual,/no_copy,/set
  handle_value,ana.include_h,include,/no_copy,/set
  handle_value,ana.const_h,const,/no_copy,/set
END







