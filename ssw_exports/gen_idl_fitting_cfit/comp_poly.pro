;+
; Project     : SOHO - CDS     
;                   
; Name        : COMP_POLY
;               
; Purpose     : Evaluate polynomial component for fitting.
;               
; Explanation : Input coefficients A determine degree of polynomial, otherwise
;               this is straightforward - see e.g., CURVEFIT for explanations
;               about this type of function.
;               
; Use         : COMP_POLY,X,A,F [,PDER]
;    
; Inputs      : As all CURVEFIT evaluation functions
;               
; Opt. Inputs : PDER
;               
; Outputs     : F : Evaluated function 
;               
; Opt. Outputs: PDER : Partial derivatives.
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
; Modified    : Not yet
;
; Version     : 1, 21 January 1997
;-            
PRO comp_poly,x,a,f,pder
  
  f = poly(x,a)
  
  IF N_params() EQ 4 THEN BEGIN
     nx = n_elements(x)
     nterms = N_elements(a)
     type = datatype(a,2)
     pder = make_array(nx,nterms,type=type,value=1.0)
     
     ;; Zero-order term
     
     ;; pder(*,0) = 1.0 ;;Already done..
     
     ;; First
     IF nterms GT 1 THEN pder(0,1) = x
     
     ;; Subsequent
     FOR i = 2,nterms-1 DO BEGIN
        pder(0,i) = x*pder(*,i-1)
     END
  END
END
