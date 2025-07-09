;+
; Project     : SOHO - CDS     
;                   
; Name        : MAKE_SFIT_STC
;               
; Purpose     : Make an SFIT structure from a CFIT structure.
;               
; Explanation : An SFIT structure is a "short" structure describing a
;               component based fit function.
;               
; Use         : SFIT = MAKE_SFIT_STC(CFIT)
;    
; Inputs      : CFIT : Component Fit, designed with e.g., XCFIT
;               
; Opt. Inputs : None.
;               
; Outputs     : Returns the short fit.
;               
; Opt. Outputs: None.
;               
; Keywords    : VALUES : Set this to use the current values instead of the
;                        initial values when constructing the parameter
;                        array.
;
;               DOUBLE : Set this to promote values to doubles.
;
;               KEEP_LIMITS : Set to avoid setting min and max values equal to
;                             the current value even if the const flag is set.
;                             Used by e.g., CFIT_BLOCK.
;               
; Calls       :
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
; Modified    : Version 2, 6 February 1997
;                       Added KEEP_LIMITS keyword
;               Version 3, 6 August 1997
;                       Added A_NOM (nominal values) in the sfits structure.
;
; Version     : 3, 6 August 1997
;-            

;
; Make short fit structure suitable for executing a fit.
;
FUNCTION make_sfit_stc,fit_stc,double=double,values=values,$
                       keep_limits=keep_limits
  
  tags = tag_names(fit_stc)
  sn = tag_names(fit_stc,/structure_name)
  
  IF strmid(sn,0,13) EQ "COMPONENT_STC" THEN BEGIN
     ntags = 1
     comp = fit_stc
     use_stc = 0
  END ELSE BEGIN 
     ntags = n_elements(tags)
     use_stc = 1
  END
  
  compiledfunc = 'cf_'
  
  functs = [""]
  multip = [0b]
  n_parms = [0]
  a = [0]
  a_nom = [0]
  trans_a = [0]
  trans_b = [0]
  max_arr = [0]
  min_arr = [0]
  include = [0b]
  const = [0b]
  keep_limits = keyword_set(keep_limits)
  
  FOR i = 0,ntags-1 DO BEGIN
     IF use_stc THEN comp = fit_stc.(i)
     
     compiledfunc = compiledfunc + comp.func_string + '_'
     
     functs = [functs,comp.func_name]
     multip = [multip,comp.multiplicative]
     include = [include,comp.include]
     n_parms = [n_parms,N_elements(comp.param)]
     
     IF keyword_set(values) THEN new_a = comp.param(*).value $
     ELSE                        new_a = comp.param(*).initial
     
     a_nom = [a_nom,new_a]
     
     new_a = new_a*comp.param(*).trans_a + comp.param(*).trans_b
     
     a = [a,new_a]
     
     new_trans_a = comp.param(*).trans_a
     trans_a = [trans_a,new_trans_a]
     trans_b = [trans_b,comp.param(*).trans_b]
     
     new_max = comp.param(*).max_val*comp.param(*).trans_a + $
        comp.param(*).trans_b
     new_min = comp.param(*).min_val*comp.param(*).trans_a + $
        comp.param(*).trans_b
     
     new_const = comp.param(*).const OR (NOT comp.include AND 1b)
     
     const = [const,new_const]

     ;; "Constant" fix..
     ix = where(new_const AND 1b)
     IF ix(0) NE -1 AND NOT keep_limits THEN BEGIN
        new_max(ix) = new_a(ix)
        new_min(ix) = new_a(ix)
     END
        
     ix = where(new_trans_a LT 0.0,nswap)
     IF nswap GT 0 THEN BEGIN
        temp = new_max(ix)
        new_max(ix) = new_min(ix)
        new_min(ix) = temp
     END
     
     max_arr = [max_arr,new_max]
     min_arr = [min_arr,new_min]
  END
  
  IF keyword_set(double) THEN a = double(a)
  
  sfit_stc = { compiledfunc : compiledfunc,$
               compiled : 0b,$
               functs : functs(1:*),$
               multip : multip(1:*),$
               include : include(1:*),$
               n_parms : n_parms(1:*),$
               a_act   : a(1:*),$
               a_nom   : a_nom(1:*),$
               act_initial : a(1:*),$
               trans_a : trans_a(1:*),$
               trans_b : trans_b(1:*),$
               max_arr : max_arr(1:*),$
               min_arr : min_arr(1:*),$
               const   : const(1:*) $
             }
  
  return,sfit_stc
END
