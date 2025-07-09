;+
; Project     : SOHO - CDS     
;                   
; Name        : RESTORE_ANALYSIS()
;               
; Purpose     : Restore a CFIT ANALYSIS structure with data
;               
; Explanation : Restores all the data associated with a CFIT ANALYSIS
;               structure saved by SAVE_ANALYSIS.
;               
; Use         : ANALYSIS = RESTORE_ANALYSIS( [ ANALYSIS | FILENAME ] )
;    
; Inputs      : When called with no parameters, the user is prompted for the
;               file name of the saved data.
;
; Opt. Inputs : ANALYSIS : Component fitting system (CFIT) analysis structure,
;                          containing the file name to be restored. Also, it
;                          is assumed that all handles etc in this structure
;                          are valid for storing the restored data.
;
;                          Thus ANA = RESTORE_ANALYSIS(ANA) may be used as a
;                          "revert to last saved version" command.
;
;                          If the /OTHER switch is set, the user is prompted
;                          for a different file (through PICKFILE).
;                          
;                          Using this calling method is effectively a
;                          "recycling" of the ANALYSIS structure as a
;                          "container" for data.
;
;               FILENAME : File name of the previously saved data. A new
;                          analysis structure will be generated and returned
;                          with the saved data.
;                          
; Outputs     : Returns a CFIT ANALYSIS structure.
;               
; Opt. Outputs: None.
;               
; Keywords    : VERBOSE : Propagated to the RESTORE command.
;
;               OTHER : Set to always prompt the user for a file name.
;
; Calls       : bigpickfile(), default, test_open(), chk_dir(), break_file,
;               exist(), datatype()
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

FUNCTION restore_analysis,filename_analysis,verbose=verbose,other=other
  
  sz = size(filename_analysis)
  
  type = sz(sz(0)+1)
  
  IF type EQ 8 THEN BEGIN
     ana = filename_analysis
     filename = ana.filename
  END ELSE IF type EQ 7 THEN filename = filename_analysis
  
  default,filename,''
  
  IF strpos(filename,'*') GE 0 OR strpos(filename,'?') GE 0 THEN other = 1
  
  IF filename NE '' THEN BEGIN
     IF NOT test_open(filename) OR chk_dir(filename) THEN other = 1
  END
  
  IF filename EQ '' OR keyword_set(other) THEN BEGIN
     
     IF filename NE '' THEN BEGIN
        break_file,filename,disk,dir,fnam,ext
        path = disk+dir
        file = fnam+ext
        filter = '*'+fnam+'*'+ext
     END
     
     default,filter,'*.ana'
     
     IF strpos(filename,'*') GE 0 OR strpos(filename,'?') GE 0 THEN file=''
     
     file = bigpickfile(file=file,path=path,/read,get_path=path,filter=filter)
     
     break_file,file,disk,dir,fnam,ext
     
     IF file NE '' THEN filename = path+fnam+ext ELSE filename=''
     
     IF NOT test_open(filename) OR chk_dir(filename) THEN filename = ''
     
     IF filename EQ '' THEN BEGIN
        print,"You must give me a file name of an existing file"
        return,0
     END
     
  END
  
  real_filename = filename
  
  default,verbose,0
  
  restore,filename,verbose=verbose
  
  IF NOT exist(ana) THEN ana = mk_analysis()
  
  ana.filename = real_filename
  ana.datasource = datasource
  ana.definition = definition
  ana.missing = missing
  ana.label = label
  
  ;; Add item to history
  htxt = [!stime,'     Restored from '+real_filename]
  
  IF datatype(history) EQ 'STR' THEN history = [history,htxt] $
  ELSE                               history = [htxt]
     
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
  
  return,ana
END
