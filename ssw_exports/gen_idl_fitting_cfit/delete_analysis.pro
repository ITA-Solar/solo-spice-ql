;+
; Project     : SOHO - CDS     
;                   
; Name        : DELETE_ANALYSIS
;               
; Purpose     : Delete a CFIT ANALYSIS structure, freeing the handles.
;               
; Explanation : See Purpose.
;               
; Use         : DELETE_ANALYSIS,ANALYSIS
;    
; Inputs      : ANALYSIS : A CFIT ANALYSIS structure.
; 
; Opt. Inputs : None
;               
; Outputs     : None
;               
; Opt. Outputs: None
;               
; Keywords    : None
;
; Calls       : DELVARX
;
; Common      : None
;               
; Restrictions: ...
;               
; Side effects: None known
;               
; Category    : Line fitting
;               
; Prev. Hist. : None
;
; Written     : SVH Haugan, UiO, 25 September 1997
;               
; Modified    : Not yet.
;
; Version     : 1,  25 September 1997
;-            
PRO delete_analysis,ana
  
  IF NOT exist(ana) THEN return
  
  IF handle_info(ana.history_h,/valid_id) THEN handle_free,ana.history_h
  IF handle_info(ana.lambda_h,/valid_id) THEN handle_free,ana.lambda_h
  IF handle_info(ana.data_h,/valid_id) THEN handle_free,ana.data_h
  IF handle_info(ana.weights_h,/valid_id) THEN handle_free,ana.weights_h
  IF handle_info(ana.fit_h,/valid_id) THEN handle_free,ana.fit_h
  IF handle_info(ana.result_h,/valid_id) THEN handle_free,ana.result_h
  IF handle_info(ana.residual_h,/valid_id) THEN handle_free,ana.residual_h
  IF handle_info(ana.include_h,/valid_id) THEN handle_free,ana.include_h
  IF handle_info(ana.const_h,/valid_id) THEN handle_free,ana.const_h
  IF handle_info(ana.origin_h,/valid_id) THEN handle_free,ana.origin_h
  IF handle_info(ana.scale_h,/valid_id) THEN handle_free,ana.scale_h
  IF handle_info(ana.phys_scale_h,/valid_id) THEN handle_free,ana.phys_scale_h
  IF handle_info(ana.dimnames_h,/valid_id) THEN handle_free,ana.dimnames_h
  
  delvarx,ana
END
