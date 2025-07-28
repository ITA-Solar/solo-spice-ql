;+
; Project     :	SOHO - CDS     
;
; Name        :	COMP_PPOLY
;
; Purpose     :	Evaluate pivoted polynomial component for fitting.
;
; Explanation :	The first parameter is the pivot value.  The remaining
;		parameters are the standard parameters for the polynomial.  The
;		result is returned as
;
;			A1 + A2*(X-A0) + A3*(X-A0)^2 + ...
;
;		The normal usage is to hold A0 constant at a value within the
;		range of X, while fitting the remaining parameters.  Since X-A0
;		is close to zero, roundoff errors are minimized.
;               
; Use         :	COMP_PPOLY,X,A,F [,PDER]
;    
; Inputs      :	X = The points at which the function should be evaluated.
;		A = As explained above.
;               
; Opt. Inputs :	PDER
;               
; Outputs     :	F = Evaluated function 
;               
; Opt. Outputs:	PDER = Partial derivatives.
;               
; Keywords    :	None.
;
; Calls       :	None.
;
; Common      :	None.
;               
; Restrictions:	None.
;               
; Side effects:	None.
;               
; Category    :	Analysis
;               
; Prev. Hist. :	Modified from COMP_POLY by S.V.H.Haugan, UiO, 21 January 1997
;
; History     :	Version 1, 17-Feb-2000, William Thompson, GSFC
;
; Version     :	Version 1, 17-Feb-2000
;-            
	PRO COMP_PPOLY,X,A,F,PDER
;
	F = POLY( X-A(0), A(1:*) )
  
	IF N_PARAMS() EQ 4 THEN BEGIN
	    NX = N_ELEMENTS(X)
	    NTERMS = N_ELEMENTS(A)
	    TYPE = DATATYPE(A,2)
	    PDER = MAKE_ARRAY(NX,NTERMS,TYPE=TYPE)
;
;  Constant term.
;
	    IF NTERMS GT 2 THEN PDER(*,0) =	$
		    -POLY( X-A(0), A(2:*)*(INDGEN(NTERMS-2)+1))
;
;  Zero-order term.
;
	    PDER(*,1) = 1.0
;
;  First-order term.
;
	    IF NTERMS GT 2 THEN PDER(0,2) = X - A(0)
;
;  Subsequent terms.
;
	    FOR I = 3,NTERMS-1 DO PDER(0,I) = (X-A(0)) * PDER(*,I-1)
	ENDIF
;
	END
