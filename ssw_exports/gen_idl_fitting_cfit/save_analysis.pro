;+
; Project     : SOHO - CDS     
;                   
; Name        : SAVE_ANALYSIS
;               
; Purpose     : Save a CFIT ANALYSIS structure with data
;               
; Explanation : Saves all the data associated with a CFIT ANALYSIS structure,
;               retrievable by RESTORE_ANALYSIS.
;               
; Use         : SAVE_ANALYSIS,ANALYSIS
;    
; Inputs      : ANALYSIS : Component fitting system (CFIT) analysis structure,
;                          containing the data to be saved, and the file name
;                          to be used when saving. If no file name is given, 
;                          pickfile will be used to prompt for one.
;                          
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : VERBOSE : Propagated to the SAVE command.
;
; Calls       : bigpickfile, break_file, default, datatype()
;
; Common      : None.
;               
; Restrictions: Must have widgets to use pickfile.
;               
; Side effects: None.
;               
; Category    : Line fitting.
;               
; Prev. Hist. : None.
;
; Written     : SVH Haugan, UiO, 25 September 1997
;               
; Modified    : Not yet. 
;
; Version     : 1, 25 September 1997
;-            
PRO save_analysis,ana,verbose=verbose
  
  IF ana.filename EQ '' THEN BEGIN
     file = bigpickfile(/write,get_path=path,filter='*.ana')
     IF file EQ '' THEN BEGIN
        print,"You must give me a file name"
        return
     END
     break_file,file,disk,dir,fnam,ext
     ana.filename = path+fnam+ext
  END
  
  default,verbose,0
  
  print,"Saving to file "+ana.filename
  
  handle_value,ana.history_h,history,/no_copy
  handle_value,ana.lambda_h,lambda,/no_copy
  handle_value,ana.data_h,data,/no_copy
  handle_value,ana.weights_h,weights,/no_copy
  handle_value,ana.fit_h,fit,/no_copy
  handle_value,ana.result_h,result,/no_copy
  handle_value,ana.residual_h,residual,/no_copy
  handle_value,ana.include_h,include,/no_copy
  handle_value,ana.const_h,const,/no_copy
  handle_value,ana.origin_h,origin,/no_copy
  handle_value,ana.scale_h,scale,/no_copy
  handle_value,ana.phys_scale_h,phys_scale,/no_copy
  handle_value,ana.dimnames_h,dimnames,/no_copy
  
  filename = ana.filename
  datasource = ana.datasource
  definition = ana.definition
  missing = ana.missing
  label = ana.label
  
  ;; Add item to history
  htxt = [!stime,'     Saved as '+filename]
  
  IF datatype(history) EQ 'STR' THEN history = [history,htxt] $
  ELSE                               history = [htxt]
  
  save,/xdr,history,lambda,data,weights,fit,result,residual,include,const,$
     origin,scale,phys_scale,dimnames,filename,datasource,definition,$
     missing,label,filename=filename,verbose=verbose
  
  handle_value,ana.history_h,history,/no_copy,/set
  handle_value,ana.lambda_h,lambda,/no_copy,/set
  handle_value,ana.data_h,data,/no_copy,/set
  handle_value,ana.weights_h,weights,/no_copy,/set
  handle_value,ana.fit_h,fit,/no_copy,/set
  handle_value,ana.result_h,result,/no_copy,/set
  handle_value,ana.residual_h,residual,/no_copy,/set
  handle_value,ana.include_h,include,/no_copy,/set
  handle_value,ana.const_h,const,/no_copy,/set
  handle_value,ana.origin_h,origin,/no_copy,/set
  handle_value,ana.scale_h,scale,/no_copy,/set
  handle_value,ana.phys_scale_h,phys_scale,/no_copy,/set
  handle_value,ana.dimnames_h,dimnames,/no_copy,/set
  
END
