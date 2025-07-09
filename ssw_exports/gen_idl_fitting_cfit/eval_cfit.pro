;+
; Project     : SOHO - CDS     
;                   
; Name        : EVAL_CFIT
;               
; Purpose     : Evaluates a component fit at given points.
;               
; Explanation : EVAL_CFIT evaluates the component fit described by the CFIT
;               structure, taking the current values for the fit
;               parameters. If the keyword /INITIAL is set, the initial values
;               for the parameters are used instead.
;               
; Use         : EVAL_CFIT,X,Y,CFIT
;    
; Inputs      : X : Array of points where the fit is to be evaluated.
;
;               CFIT : The component fit structure.
;               
; Opt. Inputs : None
;               
; Outputs     : Y : The values of the fit at the given points
;               
; Opt. Outputs: None.
;               
; Keywords    : DOUBLE : Set to use double precision arithmetic
;
;               INITIAL : Set to use parameter initial values instead of
;                         current values.
;
;               SFIT (output) : Set to a named variable to return the SFIT
;                               generated in the process of evaluating the
;                               CFIT.
;
; Calls       : make_sfit_stc(), eval_sfit
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

PRO eval_cfit,x,y,cfit,double=double,initial=initial,sfit=sfit
  
  values = NOT keyword_set(initial)
  
  sfit = make_sfit_stc(cfit,double=double,values=values)
  
  eval_sfit,x,sfit.a_act,y,private=sfit
  
END

