;+
; Project     : SOHO - CDS     
;                   
; Name        : CFIT_BPATCH
;               
; Purpose     : Patch block result at specific points
;               
; Explanation : Sets result parameter PARAM in BLOCK at points given by IX to
;               VALUE.
;
;               Example - modifying and fixing two parameters on the basis of
;               where another parameter is below a certain threshold
;
;               > xcfit_block,lam,da,wts,fit,-100,result,residual,inc,const
;                
;               > ix = where(result[0,*,*] lt 6 and result[0,*,*] ne -100)
;               > cfit_bpatch,result,ix,1,average(result[1,*,*],missing=-100)
;               > cfit_bpatch,const,ix,1,1b
;               > cfit_bpatch,result,ix,2,average(result[2,*,*],missing=-100)
;               > cfit_bpatch,const,ix,2,1b
;
;               > xcfit_block,lam,da,wts,fit,-100,result,residual,inc,const
;
;               Also, the PARAM and VALUE parameters may be arrays (with equal
;               number of elements!)
;               
; Use         : CFIT_BPATCH,BLOCK,IX,PARAM,VALUE
;    
; Inputs      : See example above.
;               
; Opt. Inputs : None.
;               
; Outputs     : Alters the values in BLOCK
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : None.
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
; Written     : S.V.H.Haugan, UiO, 5 February 1997
;               
; Modified    : Version 2, 18 September 1997
;                       Added "Use" message.
;               Version 3, 24 April 1998
;                       Added possibility of patching more than one parameter
;                       at a time.
;               Version 4, 8-Oct-2015, WTT, use [] for array indices
;								20-May-2016, TAK, fixed typo
;                       
; Version     : 20-May-2016
;-            
PRO cfit_bpatch,block,ix,param,value
  on_error,2
  
  IF n_params() NE 4 THEN $
     message,"Use: CFIT_BPATCH, BLOCK, IX, PARAM, VALUE"
     
  IF n_elements(param) NE n_elements(value) THEN $
     message,"PARAM and VALUE must have same number of elements"
  
  IF ix[0] EQ -1L THEN return
  
  FOR i = 0,N_elements(param)-1 DO BEGIN 
     par = block[param[i],*,*,*,*,*,*]
     par[ix] = value[i]
     block[param[i],*,*,*,*,*,*] = par
  END
END

