;+
; Project     : SOHO - CDS     
;                   
; Name        : MK_PARAMETER_STC()
;               
; Purpose     : Return an "empty" structure describing a fitting parameter
;               
; Explanation : Used by MK_COMPONENT_STC.
;               
; Use         : STC = MK_PARAMETER_STC()
;    
; Inputs      : None.
;               
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : DOUBLE : Set to make parameter value double
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
; Written     : S.V.H.Haugan, UiO, 21 January 1997
;               
; Modified    : Not yet.
;
; Version     : 1, 21 January 1997
;-            

FUNCTION mk_parameter_stc,double=double
  
  IF keyword_set(double) THEN mach = nr_machar(/double) $
  ELSE  mach = nr_machar()
  
  
  IF keyword_set(double) THEN $
     return,{PARAMETER_STC_D,$
             name:"",$
             description:strarr(10),$
             initial : 0.0D,$
             value   : 0.0D,$
             const   : 0b,$
             max_val : mach.xmax,$
             min_val : -mach.xmax,$
             trans_a : 1.0D,$
             trans_b : 0.0D $
            }
  
  return,{PARAMETER_STC,$
          name:"",$
          description:strarr(10),$
          initial : 0.0,$
          value   : 0.0,$
          const   : 0b,$
          max_val : mach.xmax,$
          min_val : -mach.xmax,$
          trans_a : 1.0,$
          trans_b : 0.0 $
         }
END
