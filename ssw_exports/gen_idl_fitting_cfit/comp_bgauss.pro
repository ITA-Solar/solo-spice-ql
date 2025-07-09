;+
; Project     : SOHO - CDS     
;                   
; Name        : COMP_BGAUSS
;               
; Purpose     : Evaluate broadened Gaussian for use in CURVEFIT/CFIT/MCURVEFIT
;               
; Explanation : Evaluates a single Gaussian component, plus broadening wings.
;               The first three parameters have the same meaning as the
;               Gaussian parameters in the standard GAUSSFIT procedure, i.e.:
;               
;			G(x) = A0 * EXP(-((x-A1)/A2)^2/2)
;
;               The wings are defined as
;
;			W(x) = A0 * Alpha / ( ((x-A1)/(Kappa*A2))^2 + 1 )
;
;		where Kappa=2*SQRT(2*ALOG(2)) and Alpha is defined as
;
;			Alpha = A3		;x GE A1 (right wing)
;			Alpha = A3 * A4		;x LT A1 (left wing)
;
;		Thus, A3 is the wing amplitude, and A4 is the left/right
;		asymmetry.  The broadened Gaussian is then defined as
;
;			F(x) = (1 - Alpha) * G(x)  +  W(x)
;
; Use         : COMP_BGAUSS,X,A,F [,PDER]
;    
; Inputs      : As usual for any CURVEFIT function
;               
; Opt. Inputs : PDER : Partial derivatives are calculated if parameter is
;                      present 
;               
; Outputs     : F : The evaluated broadened Gaussian at the given points
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
; Prev. Hist. : Modified from COMP_GAUSS by S.V.H. Haugan.
;
; Written     : William Thompson, GSFC, 05-Jan-1999
;               
; Modified    : Version 1, 05-Jan-1999, William Thompson, GSFC
;		Version 2, 26-Mar-1999, William Thompson, GSFC
;			Corrected bug with extreme parameters--set derivatives
;			to zero.
;		Version 3, 26-Jun-2000, William Thompson, GSFC
;			Corrected bugs in some partial derivatives
;
; Version     : Version 3, 26-Jun-2000
;-
;
	PRO COMP_BGAUSS,X,A,F,PDER
;
	ON_ERROR,0
;
;  Calculate the exponent of the Gaussian (actually twice the exponent).
;  Determine where calculating the Gaussian is valid.  (Exp(-1000) == 0 unless
;  quadruple precision).
;
	Z = (X-A(1))/A(2)
	Z2 = Z*Z
	IX = WHERE(Z2 LT 1000, COUNT_IX)
;
;  Calculate the equivalent for the wings.
;
	KAPPA = 2 * SQRT(2. * ALOG(2))
	Z2P = Z2 / KAPPA^2
;
;  Initialize the function, based on the datatypes of both X and A, and set all
;  the values to zero.
;
	F0 = MAKE_ARRAY(SIZE=SIZE(Z))
;
;  Calculate the function.  Store the kernal of the Gaussian for later use in
;  calculating the partial derivatives.
;
	IF COUNT_IX GT 0 THEN BEGIN
	    KERN = EXP(-Z2(IX)*0.5)
	    F0(IX) = KERN
	END ELSE KERN = F0
;
;  Add in the broadening wings.  Beta expresses the asymmetry of the wings, and
;  has the value A(4) for the left wing, and 1 for the right wing.  Alpha is
;  the left and right wing amplitudes, Alpha = A(3)*Beta.
;
	IF A(3) EQ 0 THEN COUNTL = 0 ELSE BEGIN
	    BETA = MAKE_ARRAY(SIZE=SIZE(Z),VALUE=1)
	    WL = WHERE(X LE A(1), COUNTL)
	    IF COUNTL GT 0 THEN BETA(WL) = BETA(WL)*A(4)
	    ALPHA = BETA * A(3)
	    KERNP = 1 / (Z2P + 1)
	    F0 = (1-ALPHA)*F0 + ALPHA*KERNP
	ENDELSE
;
;  Multiply the entire function by the amplitude.  Save the normalized function
;  for determination of the partial derivative.
;
	F = A(0)*F0
	IF N_PARAMS() EQ 3 THEN RETURN
;
;  If the PDER parameter was passed, then calculate the partial derivatives.
;  The first partial derivative is simply the normalized version of the
;  function.  If there are no valid points, then the remainder of the partial
;  derivatives are zero.
;
	PDER = FLTARR(N_ELEMENTS(X),5)
	PDER(*,0) = F0
	IF COUNT_IX EQ 0 THEN RETURN
;
;  The partial derivative w.r.t. the line position.
;
	PDER(IX,1) = A(0) * KERN * Z(IX)/A(2)
	IF A(3) NE 0 THEN BEGIN
	    KERN2P = KERNP^2
	    PDER(*,1) = (1 - ALPHA) * PDER(*,1) +			$
		(2 * A(0) / ((KAPPA * A(2))^2)) * ALPHA * KERN2P * (X - A(1))
	ENDIF
;
;  The partial derivative w.r.t. the Gaussian width.
;
	PDER(IX,2) = A(0) * KERN * Z2(IX) / A(2)
	IF A(3) NE 0 THEN PDER(*,2) = (1 - ALPHA) * PDER(*,2) +		$
		(2 * A(0) / A(2)) * ALPHA * KERN2P * Z2P
;
;  The partial derivatives w.r.t. the wing parameters.
;
	PDER(IX,3) = -A(0) * KERN
	PDER(*,3) = PDER(*,3) + A(0)*KERNP
	IF COUNTL GT 0 THEN PDER(WL,4) = PDER(WL,3) * A(3)
	PDER(*,3) = PDER(*,3) * BETA
;
	END
