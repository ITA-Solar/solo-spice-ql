;+
; Project     : SOHO - CDS     
;                   
; Name        : MK_COMPONENT_STC()
;               
; Purpose     : Return structure describing N-parameter fitting component
;               
; Explanation : This function creates a "dummy" structure to be filled in by
;               more specific routines, like MK_COMP_GAUSS() or
;               MK_COMP_POLY().
;               
; Use         : STC = MK_COMPONENT_STC(NPARM)
;    
; Inputs      : NPARM : The number of parmeters
;               
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : DOUBLE : Set to make parameters doubles
;
; Calls       : default, mk_parameter_stc, trim()
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
; Modified    : Not yet.
;
; Version     : 1, 21 January 1997
;-            
FUNCTION mk_component_stc,Nparm,double=double
  
  default,Nparm,1
  
  param = replicate(mk_parameter_stc(double=double),Nparm)
  
  stc = {name:"",$
         func_name:"",$
         func_string:"",$
         multiplicative:0b,$  ; This is for future use!
         include:1b,$
         description:strarr(10)}
  
  IF keyword_set(double) THEN d = '_D' $
  ELSE d = ''
  
  stc = create_struct(stc,"param",param,$
                      name="COMPONENT_STC_"+trim(Nparm)+d)
  
  return,stc
END
