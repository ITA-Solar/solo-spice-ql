;+
; Project     : SOHO - CDS     
;                   
; Name        : COMP_GAUSS
;               
; Purpose     : Evaluate gaussian component for use in CURVEFIT/CFIT/MCURVEFIT
;               
; Explanation : Evaluates a single gaussian component. The parameters have the
;               same meaning as the gaussian parameters in the standard
;               GAUSSFIT procedure.
;               
; Use         : COMP_GAUSS,X,A,F [,PDER]
;    
; Inputs      : As usual for any CURVEFIT function
;               
; Opt. Inputs : PDER : Partial derivatives are calculated if parameter is
;                      present 
;               
; Outputs     : F : The evaluated gaussian at the given points
;               
; Opt. Outputs: PDER
;               
; Keywords    : None.
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
; Modified    : Version 2, SVHH, 27 May 1997
;                       Calculating only points where z2 < 1000, to avoid
;                       unnecessary time spent and unnecessary illegal operand
;                       errors.
;
; Version     : 2, 27 May 1997
;-            
PRO comp_gauss,x,a,f,pder
  on_error,0
  
  nx = n_elements(x)
  
  z = (x-a(1))/a(2)
  z2 = z*z
  ix = where(z2 LT 1000.0) ;; Exp(-1000) == 0 unless quadruple precision
  
  f = make_array(size=size(z))
  
  IF ix(0) EQ -1 THEN BEGIN
     IF n_params() EQ 4 THEN BEGIN
        pder = fltarr(nx,3)
     END
     return
  END
     
     
  IF n_params() EQ 3 THEN BEGIN
     f(ix) = a(0)*exp(-z2(ix)*0.5)
     return
  END
  
  kern = exp(-z2(ix)*0.5)
  
  f(ix) = a(0)*kern
  
  pder = fltarr(nx,3)
  pder(ix,0) = kern         ;;
  pder(ix,1) = f(ix) * z(ix)/a(2)   ;; a(0)exp(-0.5*(x-a(1))^2/a(2)^2) * (x-a(1))/a(2)^2
  pder(ix,2) = pder(ix,1) * z(ix) ;; a(0)exp(...) * (x-a(1))^2/a(2)^3
  
END

