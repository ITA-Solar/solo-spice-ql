;+
; Project     : SOHO - CDS     
;                   
; Name        : EVAL_SFIT
;               
; Purpose     : Evaluate "short" fit structure at given points.
;               
; Explanation : This routine is used as the evaluation routine during fit
;               calculations in the component based fit system.
;
; Use         : EVAL_SFIT,X,A,F [,PDER], PRIVATE=SFIT
;    
; Inputs      : X : The points where the fit function should be evaluated.
;               A : The parameters of the fit to be evaluated
;               
;               PRIVATE : Set this keyword to the SFIT ("short" fit) structure
;                         describing the fit.
;               
; Opt. Inputs : None
;               
; Outputs     : F : Returns the evaluated function
;
;               
;
; Opt. Outputs: PDER : If this parameter is present, it will contain the
;                      partial derivatives of the function wrt each parameter.
;               
; Keywords    : PRIVATE : See Inputs
;
; Calls       : datatype()
;
; Common      : None.
;               
; Restrictions: The PRIVATE keyword is not optional!
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


pro eval_sfit,x,a,f,pder,private=private
  COMMON fittest,true
  on_error,0
  
  nx = n_elements(x)
  
  use_from = 0
  
  use_pder = (N_params() EQ 4)
  
  type = datatype(a,2)
  
  IF use_pder THEN BEGIN
     pder = make_array(nx,N_elements(private.a_act),type=type,/nozero)
  END
  
  f = make_array(nx,type=type)
  
  FOR c = 0,N_elements(private.functs)-1 DO BEGIN
     use_to = use_from + private.n_parms(c) - 1
     atemp = a(use_from:use_to)
     
     IF private.include(c) THEN BEGIN
        IF use_pder THEN BEGIN
           pder_temp = 1
           call_procedure,private.functs(c),x,atemp,ftemp,pder_temp
           pder(0,use_from) = pder_temp
        END ELSE BEGIN
           call_procedure,private.functs(c),x,atemp,ftemp
        END
        
        f = temporary(f) + ftemp
     END ELSE BEGIN
        ;; Even though a component is excluded in the calculation, we
        ;; need to have *some* derivatives to avoid NaN values
        IF use_pder THEN BEGIN
           ;; The derivatives of all parameters must not be *equal*, though
           pder(0,use_from) = replicate(1,nx < use_to,private.n_parms(c))
        END
     END
     
     use_from = use_to + 1
  END
  
END



