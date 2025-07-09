	FUNCTION CFIT_BLOCK_ERROR, LAMBDA, DATA, WEIGHTS, FIT, MISSING,	$
		RESULT, DOUBLE=DOUBLE, ANALYSIS=ANA
;+
; Project     :	SOHO - CDS
;
; Name        :	CFIT_BLOCK_ERROR()
;
; Purpose     :	Calculate parameter errors from CFIT structure.
;
; Category    :	Fitting, Class3
;
; Explanation :	This procedure bypasses the CURVEFIT error analysis code.
;		Although the CFIT_BLOCK program has the option of returning
;		SIGMA values, there is a possibility that the values returned
;		by CURVEFIT may be incorrect if the fit did not improve the
;		parameters.  In particular, this can happen if CFIT is called
;		with parameters that have already converged.
;
;		Parameters which are held constant in the fit are treated as
;		true constants, and are not used in determining the errors in
;		fitted parameters.  This depends solely on the specification of
;		a parameter as a constant in the FIT definition structure.  If
;		a parameter was held constant for specific troublesome pixels,
;		then it is still considered as a variable in the error
;		determination.
;
;		If a parameter which was held constant for all pixels should be
;		considered as a variable in the error analysis, then the FIT
;		structure should be modified before this routine is called.
;
; Syntax      :	SIGMA = CFIT_BLOCK_ERROR(LAMBDA, DATA, WEIGHTS, FIT, MISSING, $
;			RESULT)
;
;		SIGMA = CFIT_BLOCK_ERROR(ANALYSIS=ANALYSIS)
;
; Examples    :	XCFIT_BLOCK, X, Y, WT, FIT, -100, RESULT, RESID
;		SIGMA = CFIT_BLOCK_ERROR(X, Y, WT, FIT, -100, RESULT)
;
;		XCFIT_BLOCK, ANALYSIS=ANALYSIS
;		SIGMA = CFIT_BLOCK_ERROR(ANALYSIS=ANALYSIS)
;
; Inputs      :	LAMBDA	= Array of wavelengths.  Can either be one array for
;			  all pixels, or have the same dimensions as DATA.
;
;		DATA	= Array of data points.
;
;		WEIGHTS	= The weights of each data point.  Alternatively, one
;			  can pass in a single value of 0 to force CFIT_ERROR
;			  to calculate it's own weights, using chi-square
;			  normalization.
;
;		FIT	= Fit structure as used within CFIT
;
;		MISSING = The missing pixel flag value.
;
;		RESULT	= The result array from CFIT_BLOCK (or XCFIT_BLOCK).
;
; Opt. Inputs :	None.
;
; Outputs     :	The result of the function is an array containing the errors
;		for each of the parameters in RESULT.
;
; Opt. Outputs:	None.
;
; Keywords    :	ANALYSIS = Instead of passing in the above input arrays, the
;			   ANALYSIS keyword can be used to pass in a CDS
;			   analysis structure from MK_CDS_ANALYSIS which
;			   contains all of the above arrays.
;
;               DOUBLE	 = Set to force double precision calculation.  Highly
;                          recommended.
;
; Calls       :	CFIT_ERROR
;
; Common      :	None.
;
; Restrictions:	The data must first be fit using either CFIT_BLOCK or
;		XCFIT_BLOCK.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 02-Dec-1999, William Thompson, GSFC
;		Version 2, 06-Feb-2001, William Thompson, GSFC
;			Allow dummy WEIGHTS to be passed.
;		Version 3, 26-Nov-2001, William Thompson, GSFC
;			Added keyword DOUBLE
;               Version 4, 29-May-2015, WTT, use [] for arrays
;
; Contact     :	WTHOMPSON
;-
;
	ON_ERROR, 2
;
;  Check the input parameters, and whether or not the ANALYSIS keyword was
;  passed.
;
	IF NOT EXIST(ANA) THEN BEGIN 
	    IF N_PARAMS() LT 6 THEN MESSAGE, 'Syntax: Result = ' +	$
		    'CFIT_BLOCK(LAMBDA,DATA,WEIGHTS,FIT,MISSING,RESULT)
	END ELSE BEGIN
	    HANDLE_VALUE, ANA.LAMBDA_H,   LAMBDA,   /NO_COPY
	    HANDLE_VALUE, ANA.DATA_H,     DATA,     /NO_COPY
	    HANDLE_VALUE, ANA.WEIGHTS_H,  WEIGHTS,  /NO_COPY
	    HANDLE_VALUE, ANA.FIT_H,      FIT,      /NO_COPY
	    MISSING = ANA.MISSING
	    HANDLE_VALUE, ANA.RESULT_H,   RESULT,   /NO_COPY
	    ERROR = 0
	    IF !DEBUG EQ 0 THEN CATCH, ERROR
	    IF ERROR NE 0 THEN BEGIN
		PRINT, !ERR_STRING
		PRINT, "CAUGHT ERROR, PUTTING BACK DATA BLOCKS.."
		SIGMA = -1
		GOTO, EXIT_POINT
	    ENDIF
	ENDELSE
;
;  Determine the size of the data.
;
	SZL = SIZE(LAMBDA)
	SZD = SIZE(DATA)
	SZR = SIZE(RESULT)
	NPAR = SZR[1] - 1
;
;  Pad out the dimensions to 7.
;
	DIMEN = SZD[1:SZD[0]]
	IF SZD[0] LT 7 THEN DIMEN = [DIMEN,REPLICATE(1L,7-SZD[0])]
;
;  Initialize SIGMA.  Make sure that SFIT is not defined on the first call.
;
	SZS = SZR
	SZS[1] = NPAR
	SIGMA = MAKE_ARRAY(SIZE=SZS)
	DELVARX, SFIT
;
;  Step through the dimensions.
;
	FOR O=0L,DIMEN[6]-1 DO $
		FOR N=0L,DIMEN[5]-1 DO $
		FOR M=0L,DIMEN[4]-1 DO $
		FOR L=0L,DIMEN[3]-1 DO $
		FOR K=0L,DIMEN[2]-1 DO $
		FOR J=0L,DIMEN[1]-1 DO BEGIN
;
;  Extract the arrays used in the point-by-point fit.
;
	    IF SZL[0] EQ 1 THEN X = LAMBDA ELSE X = LAMBDA[*,J,K,L,M,N,O]
	    Y = DATA[*,J,K,L,M,N,O]
	    IF N_ELEMENTS(WEIGHTS) GT 1 THEN WT = WEIGHTS[*,J,K,L,M,N,O]
	    W = WHERE(Y EQ MISSING, COUNT)
	    IF COUNT GT 0 THEN WT[W] = 0
	    A = RESULT[0:NPAR-1,J,K,L,M,N,O]
;
;  Perform the fit.
;
	    MESSAGE = ''
	    CFIT_ERROR, X, Y, A, FIT, SIGMAA, WEIGHTS=WT, SFIT=SFIT,	$
		    DOUBLE=DOUBLE, ERRMSG=MESSAGE
	    IF MESSAGE NE '' THEN MESSAGE, MESSAGE, /INFORMATIONAL
;
;  Put the error values into the SIGMA array.
;
	    SIGMA[*,J,K,L,M,N,O] = SIGMAA
	ENDFOR
;
;  If the ANALYSIS keyword was used, then put the values back in the structure.
;
EXIT_POINT:
	IF EXIST(ANA) THEN BEGIN
	    HANDLE_VALUE, ANA.LAMBDA_H,   LAMBDA,   /NO_COPY, /SET
	    HANDLE_VALUE, ANA.DATA_H,     DATA,     /NO_COPY, /SET
	    HANDLE_VALUE, ANA.WEIGHTS_H,  WEIGHTS,  /NO_COPY, /SET
	    HANDLE_VALUE, ANA.FIT_H,      FIT,      /NO_COPY, /SET
	    HANDLE_VALUE, ANA.RESULT_H,   RESULT,   /NO_COPY, /SET
	ENDIF
;
	RETURN, SIGMA
	END
