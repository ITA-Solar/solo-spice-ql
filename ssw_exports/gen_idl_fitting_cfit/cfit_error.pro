	PRO CFIT_ERROR, X, Y, A_NOM, FIT, SIGMAA, WEIGHTS=WEIGHTS,	$
		SFIT=SFIT, CHI2=CHISQR, DOUBLE=DOUBLE, ERRMSG=ERRMSG
;+
; Project     :	SOHO - CDS
;
; Name        :	CFIT_ERROR
;
; Purpose     :	Calculate parameter errors from CFIT structure.
;
; Category    :	Fitting, Class3
;
; Explanation :	This procedure bypasses the CURVEFIT error analysis code.
;		Although the CFIT program has the option of returning SIGMAA
;		values, there is a possibility that the values returned by
;		CURVEFIT may be incorrect if the fit did not improve the
;		parameters.  In particular, this can happen if CFIT is called
;		with parameters that have already converged.
;
;		Parameters which are held constant in the fit are treated as
;		true constants, and are not used in determining the errors in
;		fitted parameters.  If a parameter which was held constant
;		should be considered as a variable in the error analysis, then
;		the FIT structure should be modified before this routine is
;		called.
;
; Syntax      :	CFIT_ERROR, X, Y, A_NOM, FIT, SIGMAA
;
; Examples    :	YFIT = CFIT(X, Y, A_NOM, FIT, WEIGHTS=WT)
;		CFIT_ERROR, X, Y, A_NOM, FIT, SIGMAA, WEIGHTS=WT, CHI2=CHI2
;
; Inputs      :	X, Y	= Data to be fitted
;		FIT	= Fit structure as used within CFIT
;
; Opt. Inputs :	A_NOM	= Parameters of the fit.  If not defined, then the
;			  parameters are derived from the FIT structure.
;			  However, this is not recommended, since the
;			  parameters in the FIT structure may not represent the
;			  best fit to the data.
;
; Outputs     :	SIGMAA	= Errors for each of the parameters in A_NOM.  If this
;			  procedure fails, then SIGMAA will be set to -1.
;
; Opt. Outputs:	A_NOM	= Only changed if not initially defined.
;
; Keywords    :	WEIGHTS	= The weights for each of the data points used in the
;			  fit.  If not passed, then constant weighting is
;			  assumed, and the parameter errors are calculated
;			  based on the value of chi-squared.
;
;		CHI2	= Returns the chi-squared value.
;
;		SFIT	= Structure from MAKE_SFIT_STC.  Passing this structure
;			  speeds up execution when the same structure is used
;			  for a series of spectra.  The best way to use this
;			  keyword is to pass it in undefined on the first call,
;			  and then reuse the structure on subsequent calls.
;
;               DOUBLE	= Set to force double precision calculation.  Highly
;                         recommended.
;
;		ERRMSG	= If defined and passed, then any error messages will
;			  be returned to the user in this parameter rather than
;			  depending on the MESSAGE routine in IDL.  If no
;			  errors are encountered, then a null string is
;			  returned.  In order to use this feature, ERRMSG must
;			  be defined first, e.g.
;
;				ERRMSG = ''
;				CFIT_ERROR, ERRMSG=ERRMSG, ...
;				IF ERRMSG NE '' THEN ...
;		
;
; Calls       :	MAKE_SFIT_STC, EVAL_SFIT
;
; Common      :	None.
;
; Restrictions:	The data must first be fit using either CFIT or XCFIT.
;
; Side effects:	None.
;
; Prev. Hist. :	Some of the code is taken from the routine LSTSQR.
;
; History     :	Version 1, 02-Dec-1999, William Thompson, GSFC
;		Version 2, 15-Feb-2000, William Thompson, GSFC
;			Use STATUS parameter from INVERT rather than DETERM
;			function to catch singular matrices--more forgiving.
;		Version 3, 26-Nov-2001, William Thompson, GSFC
;			Fixed bug when only one parameter is fitted.
;			Return SIGMAA=-1 if procedure fails.
;			Added keyword DOUBLE
;               Version 4, 08-Oct-2015, WTT, use [] for array indices
;
; Contact     :	WTHOMPSON
;-
;
	ON_ERROR, 2
;
	IF N_PARAMS() LT 5 THEN BEGIN
	    MESSAGE = 'Syntax: CFIT_ERROR, X, Y, A_NOM, FIT, SIGMAA'
	    GOTO, HANDLE_ERROR
	ENDIF
;
	NDATA = N_ELEMENTS(X)
	IF N_ELEMENTS(Y) NE NDATA THEN BEGIN
	    MESSAGE = 'Arrays X and Y must have the same number of points.'
	    GOTO, HANDLE_ERROR
	ENDIF
;
;  Determine whether WEIGHTS were passed.  If not, the default is 1.
;
	WT_PASSED = N_ELEMENTS(WEIGHTS) NE 0
	IF WT_PASSED THEN BEGIN
	    IF N_ELEMENTS(WEIGHTS) NE NDATA THEN BEGIN
		MESSAGE = 'WEIGHTS must be the same size as X and Y'
		GOTO, HANDLE_ERROR
	    ENDIF
	    WT = WEIGHTS
	END ELSE BEGIN
	    MESSAGE, /INFORMATIONAL,	$
		    'WARNING: No weights supplied - constant weights used'
	    WT = REPLICATE(1.D0, NDATA)
	ENDELSE
;
;  Extract the parameters, the linear transformation coefficients, and whether
;  each parameter is a constant.
;
	FOR I = 0,N_TAGS(FIT)-1 DO BEGIN
	    PP = FIT.(I).PARAM
	    P = PP.VALUE
	    A = PP.TRANS_A
	    B = PP.TRANS_B
	    C = PP.CONST
	    IF I EQ 0 THEN BEGIN
		PARAM   = P
		CONST   = C
		TRANS_A = A
		TRANS_B = B
	    END ELSE BEGIN
		PARAM   = [PARAM,   P]
		CONST   = [CONST,   C]
		TRANS_A = [TRANS_A, A]
		TRANS_B = [TRANS_B, B]
	    ENDELSE
	ENDFOR
;
;  If A_NOM was not passed, then set it to PARAM.  Apply the linear
;  transformation coefficients.
;
	IF N_ELEMENTS(A_NOM) EQ 0 THEN A_NOM = PARAM ELSE PARAM = A_NOM
	PARAM = PARAM * TRANS_A + TRANS_B
	WPAR = WHERE(CONST EQ 0, NPAR)
;
;  Evaluate the function.  Separate out those derivatives which are for
;  non-constant parameters.
;
	IF N_ELEMENTS(SFIT) EQ 0 THEN SFIT = MAKE_SFIT_STC(FIT,DOUBLE=DOUBLE)
	EVAL_SFIT, X, PARAM, F, FDER, PRIVATE=SFIT
	FDER = FDER[*,WPAR]
;
;  Calculate the matrix of partial derivatives.  Extract the diagonal elements.
;
	ARRAY = TRANSPOSE(FDER) # ((WT # REPLICATE(1,NPAR)) * FDER)
	DIAG = INDGEN(NPAR)*(NPAR+1)	;Subscripts of diagonal elements.
	VEC = SQRT(ARRAY[DIAG])
	W = WHERE(VEC EQ 0, N_FOUND)
	IF N_FOUND GT 0 THEN BEGIN
	    MESSAGE = 'Zero partial derivative detected'
	    GOTO, HANDLE_ERROR
	ENDIF
;
;  Renormalize the correlation array based on the diagonal elements.  Invert
;  the array.
;
	ARRAY = ARRAY / (VEC # VEC)
	IF N_ELEMENTS(ARRAY) EQ 1 THEN BEGIN
	    AINV = ARRAY * VEC^2
	    IF AINV[0] EQ 0 THEN BEGIN
		MESSAGE = 'A singular matrix was encountered'
		GOTO, HANDLE_ERROR
	    ENDIF
	    AINV = 1 / AINV
	END ELSE BEGIN
	    AINV = INVERT(ARRAY, STATUS) / (VEC # VEC)
	    IF STATUS EQ 1 THEN BEGIN
		MESSAGE = 'A singular matrix was encountered'
		GOTO, HANDLE_ERROR
	    ENDIF
	ENDELSE
;
;  Calculate chi-squared, and the errors in the parameters.
;
	NFREE = (NDATA - TOTAL(WT EQ 0) - NPAR) > 1
	CHISQR = TOTAL(((F-Y)^2)*WT) / NFREE
	SIGSQR = ((AINV # TRANSPOSE(FDER))^2) # WT
	IF NOT WT_PASSED THEN SIGSQR = SIGSQR * CHISQR
	SIGMAA = 0*PARAM
	SIGMAA[WPAR] = SQRT(SIGSQR) / TRANS_A(WPAR)
	RETURN
;
;  Error handling point.
;
HANDLE_ERROR:
	IF N_ELEMENTS(PARAM) EQ 0 THEN SIGMAA = -1 ELSE BEGIN
	    SIGMAA = 0*PARAM
	    SIGMAA[WPAR] = -1
	ENDELSE
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = MESSAGE ELSE MESSAGE, MESSAGE
;
	END
