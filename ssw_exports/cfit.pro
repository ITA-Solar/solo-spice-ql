;+
; Project     : SOHO - CDS     
;                   
; Name        : CFIT
;               
; Purpose     : Make a best fit of the sum of components to the supplied data
;               
; Explanation : Given a structure describing the set of components to be
;               fitted (a CFIT structure), CFIT first compiles the
;               component-wise structure into a "short fit" called SFIT.
;
;               In doing so, for each component it takes the value of each
;               parameter's INITIAL tag as the starting point for the search,
;               unless the keyword /VALUES is set, in which case the parameter
;               tag called VALUE is used instead. Both of these tags store
;               nominal parameter values, but the values are converted to
;               actual values before being stored in the SFIT structure.
;
;               If the SFIT is already supplied through the SFIT keyword, no
;               new SFIT is compiled, and the values stored in the SFIT are
;               kept as they are.
;
;               HOWEVER, if the parameter A_NOM is defined, those (nominal)
;               parameter values are converted into actual parameter values
;               and stored in the SFIT structure, unless the /IGNORE keyword
;               is set. This happens whether or not the SFIT structure was
;               supplied initially.
;
;               If any components are flagged not to be included in the fit,
;               all its parameters are flagged as constant.
;
;               Given these data, CFIT tries to find that set of parameter
;               values which gives the best fit of the model to the supplied
;               function.
;
; Use         : YFIT = CFIT(X,Y,A,FIT [,SIGMAA] [+keywords])
;    
; Inputs      : X,Y : Tabulated function to be fitted.
;
;               A_NOM : Array of (nominal) parameter values before/after fit.
;                   IF DEFINED ON ENTRY, THEN THESE VALUES ARE USED AS INITIAL
;                   VALUES, UNLESS /RESET IS SET.
;
;               FIT : Fit structure containing one tag for each component in
;                     the fit.
;
;               SIGMAA : Errors for each of the parameter values included in A.
;               
; Opt. Inputs : 
;               
; Outputs     : 
;               
; Opt. Outputs: FAILED, FAIL_TYPE : See MCURVEFIT for an explanation. FAILED
;                                   will be nonzero if a failure occured,
;                                   FAIL_TYPE contains information on the type
;                                   of failure. Note that e.g., loss of
;                                   precision failures (type 2) is usually the
;                                   result of starting very *close* to the
;                                   correct parameter values (thus not a
;                                   serious failure).
;               
; Keywords    : WEIGHTS : Set to an array containing the weights to be used in
;                         the \chi^2 calculations. I.e., if the error is
;                         proportional to the square root of the number of
;                         counts in Y, set WEIGHTS=1./Y.
;
;               SFIT : Used for internal purposes to speed up execution:
;
;                      When a fit is made, the component-wise organization of
;                      a FIT STRUCTURE is "compiled" into a short form called
;                      an SFIT structure before subsequent processing.
;                      
;                      When fitting a series of spectra in a loop, set this
;                      keyword to any named variable to avoid re-compilation
;                      of the SFIT structure between each time.
;                      
;                      The parameter values (tag "A_ACT") inside the SFIT
;                      structure are stored as the *actual* value, not the
;                      *nominal* value. For each parameter:
;                      
;                      A_ACT = A_NOM*trans_a + trans_b
;                      
;                      where trans_a/b arrays are compiled from the PARAMETER
;                      structure describing each parameter, and also stored as
;                      an array inside the SFIT structure.
;
;               VALUES: Create the SFIT structure with values taken from the
;                       param[*].value instead of param[*].initial.
;
;                       Whenever SFIT already contains an SFIT structure, the
;                       array of *nominal* parameter values A_NOM must be
;                       supplied to update the starting values.
;
;               ERROR_ONLY: Passed on to MCURVEFIT, causing no change in the
;                           parameter values, but an estimate of the SIGMAA
;                           array will be made.
;
;               IGNORE: Set to ignore any values supplied in A_NOM. 
;
;               NOCOMPILE : Don't try to compile the evaluation function. If
;                           this keyword is not set, the procedure tries to
;                           compile a function evaluating the fitted function.
;
;               DOUBLE : Set to perform fit with double precision arithmetic.
;
;               CONST : An array of bytes, one for each parameter, signifying
;                       which components are to be kept constant.
;
;               INCLUDE : An array of bytes, one for each component, with a
;                         zero for each component that should be left out.
;               
;               CHI2 : Contains the \chi^2 value of the final fit.
;
; Calls       : compile_sfit, make_sfit_stc(), mcurvefit(), update_cfit
;
; Common      : None.
;               
; Restrictions: ...
;               
; Side effects: compile_sfit may try to write a program.
;               
; Category    : Analysis
;               
; Prev. Hist. : Component based fitting inspired by XSPEC
;
; Written     : S.V.H.Haugan, UiO, 20 January 1997
;               
; Modified    : Version 2, 6 February 1997
;                       Added KEEP_LIMITS in call to MAKE_SFIT_STC, to avoid
;                       clinching min/max limits in spite of values in the
;                       CONST keyword.
;               Version 3, 24 September 1997
;                       Added ERROR_ONLY, FAIL_TYPE keywords.
;               Version 4, 20 November 1997
;                       Made sure that it's the *nominal* errors that are
;                       reported through SIGMAA, by dividing with sfit.trans_a
;               Version 5, 19 January 1999
;                       Setting default itmax to 200 instead of 20. This may
;                       seem odd, but the truth is that allowing a lot of
;                       iterations *will* cause the fit to converge in most
;                       cases where it failed earlier due to too many
;                       iterations. 
;               Version 6, 08-Oct-2015, WTT, use [] for array indices
;
; Version     : 6, 08-Oct-2015
;-            

FUNCTION cfit,x,y,a_nom,fit,sigmaa,weights=wt,$
              double=double,tol=tol,itmax=itmax,$
              sfit=sfit,values=values,failed=failed,fail_type=fail_type,$
              chi2=chi2,nocompile=nocompile,noupdate=noupdate,$
              error_only=error_only,$
              quiet=quiet,const=const,include=include,ignore=ignore
  
  default,itmax,200
  
  use_compiled = NOT keyword_set(nocompile)
  
  IF n_elements(sfit) EQ 0 THEN BEGIN
     ;; Make an sfit to start with - but keep the max/min limits regardless of
     ;; the CONST status.
     ;; 
     sfit = make_sfit_stc(fit,values=values,/keep_limits)
  END
  
  ;;
  ;; Keep the original structure's data:
  ;;
  old_sfit = sfit
  
  na = n_elements(sfit.a_act)
  nc = n_elements(sfit.include)
  
  ;;
  ;; Use current values if A_NOM is supplied, unless /IGNORE is set.
  ;; Else use (previous) (initial) values already in the SFIT
  ;; 
  
  IF n_elements(a_nom) EQ na AND NOT keyword_set(ignore) THEN BEGIN
     ;; Remember that user-supplied values are NOMINAL values
     sfit.a_act = a_nom*sfit.trans_a + sfit.trans_b
  END
  
  IF use_compiled THEN BEGIN
     IF sfit.compiled EQ 0b THEN compile_sfit,sfit
     IF sfit.compiled EQ 2b THEN use_compiled = 0
     compfunc = sfit.compiledfunc
  END
  
  aa = [sfit.a_act]  ;; Argh! IDL!!
  
  ;;
  ;; Make sure we have an array of weights
  ;; 
  IF n_elements(wt) EQ 0 THEN wt = replicate(1.0,n_elements(x))
  
  ;;
  ;; Promote to double if keyword set
  ;; 
  IF keyword_set(double) THEN aa = double(aa)
  
  ;;
  ;; Acknowledge constant flags
  ;;
  IF n_elements(const) EQ na THEN sfit.const = const
  
  ;;
  ;; Handle any non-included components
  ;;
  IF n_elements(include) EQ nc THEN sfit.include = include
  
  IF total(sfit.include) NE nc THEN BEGIN
     ;;
     ;; Non-included components cannot be handled correctly by the
     ;; compiled functions
     ;;
     use_compiled = 0
     ;;
     ;; For each non-included component, set all parameters to const
     ;;
     ix = where(sfit.include EQ 0,nix)
     FOR i=0,nix-1 DO BEGIN
        n_comp_start = 0
        IF ix[i] GT 0 THEN n_comp_start = total(sfit.n_parms[0:ix[i]-1])
        sfit.const[n_comp_start:n_comp_start+sfit.n_parms[ix[i]]-1] = 1b
     END
  END 
  
  ;;
  ;; For all constant parameters, set max = min = value
  ;;
  ix = where(sfit.const,nconst)
  
  IF nconst GT 0 THEN BEGIN
     sfit.max_arr[ix] = aa[ix]
     sfit.min_arr[ix] = aa[ix]
  END
  
  ;;
  ;; Make sure we start within bounds
  ;;
  
  aa = (aa < sfit.max_arr) > sfit.min_arr
  
  
  IF NOT use_compiled THEN BEGIN
     IF n_params() EQ 5 THEN $
        yfit = mcurvefit(x,y,wt,aa,private=sfit,function_name='eval_sfit',$
                         maxarr = sfit.max_arr, $
                         minarr = sfit.min_arr, $
                         error_only=error_only,$
                         tol=tol,itmax=itmax,fail_type=fail_type,$
                         chi2=chi2,sigmaa,failed=failed,quiet=quiet) $
     ELSE $
        yfit = mcurvefit(x,y,wt,aa,private=sfit,function_name='eval_sfit',$
                         maxarr = sfit.max_arr, $
                         minarr = sfit.min_arr, $
                         error_only = error_only,$
                         tol=tol,itmax=itmax,fail_type=fail_type,$
                         chi2=chi2,failed=failed,quiet=quiet)  
  END ELSE BEGIN
     IF n_params() EQ 5 THEN $
        yfit = mcurvefit(x,y,wt,aa,function_name=compfunc,$
                         maxarr = sfit.max_arr, $
                         minarr = sfit.min_arr, $
                         error_only = error_only,$
                         tol=tol,itmax=itmax,fail_type=fail_type,$
                         chi2=chi2,sigmaa,failed=failed,quiet=quiet) $
     ELSE $
        yfit = mcurvefit(x,y,wt,aa,function_name=compfunc,$
                         maxarr = sfit.max_arr, $
                         minarr = sfit.min_arr, $
                         error_only = error_only,$
                         tol=tol,itmax=itmax,fail_type=fail_type,$
                         chi2=chi2,failed=failed,quiet=quiet)  
  END
  
  ;;
  ;; Restore original sfit values
  ;;
  compiled = sfit.compiled
  sfit = old_sfit
  sfit.compiled = compiled
  
  a_nom = (aa - sfit.trans_b)/sfit.trans_a
  
  IF n_params() EQ 5 THEN sigmaa = sigmaa/abs(sfit.trans_a)
  
  IF NOT keyword_set(noupdate) THEN update_cfit,fit,a_nom
  return,yfit
END
