;+
; Project     : SOHO - CDS     
;                   
; Name        : COMP_VOIGT
;               
; Purpose     : Evaluate Voigt profile for use in CURVEFIT/CFIT/MCURVEFIT
;               
; Explanation : Evaluates a single Voigt component.  The parameters are
;		amplitude, line position, doppler width, and rocking width.
;		The Voigt profile is then evaluated as
;
;		   PVOIGT, ABS(A3/A2), (x-A1)/A2, H
;		   F(x) = A0 * H(x) / (SQRT(!PI)*A2)
;               
; Use         : COMP_VOIGT,X,A,F [,PDER]
;    
; Inputs      : As usual for any CURVEFIT function
;               
; Opt. Inputs : PDER : Partial derivatives are calculated if parameter is
;                      present 
;               
; Outputs     : F : The evaluated Voigt profile at the given points
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
; Prev. Hist. : Modified from COMP_GAUSS by S.V.H. Haugan, and from MVOIGT by
;		Dominic Zarro.
;
; Written     : William Thompson, GSFC, 08-Jan-1999
;               
; Modified    : Version 1, 12-Jan-1999, William Thompson, GSFC
;
; Version     : Version 1, 12-Jan-1999
;-            
	PRO COMP_VOIGT,X,A,PROF,PDER
;
	ON_ERROR,0
;
;  Calculate the function.
;
	V = (X - A(1)) / A(2)
	AA = ABS(A(3) / A(2))
	PVOIGT, AA, V, H, F
	STREN = A(0)
	DOPPW = A(2)
	SQPI = SQRT(!PI)
	RAT = STREN / (DOPPW*SQPI)
	PROF = RAT*H
	IF N_PARAMS() EQ 3 THEN RETURN
;
;  Calculate the partial derivatives.
;
	PDER = FLTARR(N_ELEMENTS(X),4)
	DHDV = 2.*(AA*F - V*H)
	DHDA = 2.*(AA*H + V*F - 1./SQPI)
	DVDW = -V/DOPPW
	DADW = -AA/DOPPW
	PDER(*,0) = H/DOPPW/SQPI
	PDER(*,1) = -RAT*DHDV/DOPPW
	PDER(*,2) = -PROF/DOPPW + RAT*(DHDA*DADW + DHDV*DVDW)
	PDER(*,3) = RAT*DHDA/DOPPW
;
	END
