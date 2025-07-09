;+
; Project     : SOHO - CDS     
;                   
; Name        : MK_ANALYSIS()
;               
; Purpose     : Create/initialize a CFIT ANALYSIS structure.
;               
; Explanation : A Component Fitting System (CFIT) ANALYSIS structure contains
;               all of the data blocks and the fitting structure associated
;               with one "block analysis" (e.g., (X)CFIT_BLOCK).
;
;               This facilitates a much more simple calling convention for
;               e.g.., CFIT_BLOCK and XCFIT_BLOCK.
;
;               This function returns a CFIT ANALYSIS structure, and
;               optionally initializes it by putting the supplied data blocks
;               in to the structure (on handles).
;
;               When you do not need an analysis structure anymore, you should
;               delete it with DELETE_ANALYSIS.
;
;               To simply create an analysis structure:
;
;               IDL> ana = mk_analysis()
;
;               To initialize it at once with data blocks:
;
;               IDL> ana = mk_analysis(lambda,data,weights,fit,$
;                                      missing,result,residual,include,const)
;
;               When called in this way, MK_ANALYSIS uses NO_COPY=1 when
;               setting the data on the structure's handles (thus making the
;               input variables undefined). To turn off this, set NO_COPY=0 in
;               the call to MK_ANALYSIS.
;
;               MK_ANALYSIS may also be used to make a COPY of an existing
;               analysis block, simply use:
;
;               IDL> ana2 = mk_analysis(source=ana)
;
;               to make an independent copy (new handles) of ANA.
;
;               To copy the contents of one existing analysis structure *into*
;               another analysis structure, use
;
;               IDL> ana2 = mk_analysis(source=ana,destination=ana2)
;
;               Here it is assumed that ana2 is an existing structure.
;
; Use         : ANALYSIS = MK_ANALYSIS()
;    
; Inputs      : 
; 
; Opt. Inputs : See the Explanation section, and e.g., CFIT_BLOCK.
;               
; Outputs     : Returns a CFIT ANALYSIS structure.
;               
; Opt. Outputs: None.
;               
; Keywords    : NO_COPY : Must be explicitly set to zero to turn off NO_COPY
;                         behaviour.
;
; Calls       : default, handle_create(), strextract(), exist()
;
; Common      : None.
;               
; Restrictions: ?
;               
; Side effects: See NO_COPY keyword.
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
FUNCTION mk_analysis,lambda,data,weights,fit,missing,result,residual,$
                     include,const,no_copy=ncp,$
                     source_analysis=source,destination=destination
  
  default,missing,-100d
  
  IF keyword_set(destination) THEN ana = destination ELSE BEGIN 
     ana = {CFIT_ANALYSIS,$
            filename : '',$    ;; Filename for saving this analysis in
            datasource : '',$  ;; Filename (or something else) of original data
            definition : '',$  ;; Filename of analysis definition
            label : '',$       ;; Label (anything)
            $;;
            $;; The history is an array of strings.
            $;; 
            history_h : handle_create(),$
            $;;
            $;; The following point to the "raw" data for (X)CFIT_BLOCK
            $;; 
            lambda_h : handle_create(),$
            data_h : handle_create(),$
            weights_h : handle_create(),$
            fit_h : handle_create(),$
            missing : double(missing),$
            result_h : handle_create(),$
            residual_h : handle_create(),$
            include_h : handle_create(),$
            const_h   : handle_create(),$
            $;;
            $;; The following are "auxiliary" information for XCFIT_BLOCK,
            $;; enabling different sampling distance in different spatial
            $;; directions etc.
            $;;
            origin_h : handle_create(),$
            scale_h  : handle_create(),$
            phys_scale_h : handle_create(),$
            dimnames_h : handle_create() $
           }
  END 
  
  help,calls=calls
  caller = strextract(calls(1),'<','>')
  IF caller EQ '' THEN caller = calls(1)
  history = ['Created '+!stime,$
             '       ('+caller+') for '+getenv("USER")]
  
  IF keyword_set(source) THEN BEGIN
     default,ncp,0
     
     ana.filename = source.filename
     ana.datasource = source.datasource
     ana.definition = source.definition
     ana.label = source.label
     ana.missing = source.missing
     
     handle_value,source.history_h,history,no_copy=ncp
     handle_value,source.lambda_h,lambda,no_copy=ncp
     handle_value,source.data_h,data,no_copy=ncp
     handle_value,source.weights_h,weights,no_copy=ncp
     handle_value,source.fit_h,fit,no_copy = ncp
     handle_value,source.result_h,result,no_copy=ncp
     handle_value,source.residual_h,residual,no_copy=ncp
     handle_value,source.include_h,include,no_copy=ncp
     handle_value,source.const_h,const,no_copy=ncp
     handle_value,source.origin_h,origin,no_copy=ncp
     handle_value,source.scale_h,scale,no_copy=ncp
     handle_value,source.phys_scale_h,phys_scale,no_copy=ncp
     handle_value,source.dimnames_h,dimnames,no_copy=ncp
     IF NOT exist(history) THEN history = ['History started '+!stime]
     history = [history,'Copied structure '+!stime]
  END
  
  default,ncp,1
  
  handle_value,ana.history_h,history,/set,no_copy=ncp
  handle_value,ana.lambda_h,lambda,/set,no_copy=ncp
  handle_value,ana.data_h,data,/set,no_copy=ncp
  handle_value,ana.weights_h,weights,/set,no_copy=ncp
  handle_value,ana.fit_h,fit,/set,no_copy=ncp
  handle_value,ana.result_h,result,/set,no_copy=ncp
  handle_value,ana.residual_h,residual,/set,no_copy=ncp
  handle_value,ana.include_h,include,/set,no_copy=ncp
  handle_value,ana.const_h,const,/set,no_copy=ncp
  handle_value,ana.origin_h,origin,/set,no_copy=ncp
  handle_value,ana.scale_h,scale,/set,no_copy=ncp
  handle_value,ana.phys_scale_h,phys_scale,/set,no_copy=ncp
  handle_value,ana.dimnames_h,dimnames,/set,no_copy=ncp
  
  return,ana
  
END
