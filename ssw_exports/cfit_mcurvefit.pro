; $Id: 2025-06-21 00:35 CEST $
;;
;; Auxiliary routine
;;
PRO cfit_mcurvefit_f_val,fname,x,a,yfit,pder,private=private
  IF n_params() EQ 4 THEN BEGIN
     ;;
     ;; No partial derivatives
     ;;
     IF n_elements(private) EQ 0 THEN call_procedure,fname,x,a,yfit $
     ELSE                        call_procedure,fname,x,a,yfit,private=private

  END ELSE BEGIN
     ;;
     ;; Calculate partial derivatives
     ;;
     IF n_elements(private) EQ 0 THEN call_procedure,fname,x,a,yfit,pder $
     ELSE              call_procedure,fname,x,a,yfit,pder,private=private
  END
END

;-------
; Calculate partial derivatives by varying each parameter a little
; This was extracted from the main body into a subroutine by Kim Tolbert, 3-Dec-2009, so that it
; can be called at the end to calculate sigmas on parameters.

PRO cfit_mcurvefit_f_pder, function_name, x, a, yfit, pder, private=private, $
                      minarr=minarr, maxarr=maxarr, ipder=ipder, noderivative=noderivative, varix=varix, eps=eps

nparam = n_elements(a)
default, varix, indgen(nparam)
nfit = n_elements(varix)

if keyword_set(NODERIVATIVE) then begin
 ;            Evaluate function and estimate partial derivatives
   cfit_mcurvefit_f_val,function_name,x,a,yfit,private=private

   for m =0, nfit-1 do begin  ;only compute partial derivative for free parameters
      term = varix(m)
;     print,'term = ', term
      p = a       ; Copy current parameters
      ; Increment size for forward difference derivative
      inc = eps * abs(p(term))
      if (inc eq 0.) then inc = eps
      count = 0
      origp = p(term)
      originc = inc
      maxdiff = 0.
      ; if changing the term by inc doesn't have any effect, keep increasing inc by powers
      ; of 10 until it does (up to 38 times).  Otherwise derivative is 0 and parameters that
      ; start really small will never have a chance to change.
      p[term] = origp + inc
      while count eq 0 or $
       ( maxdiff lt 1.e-5 and count lt 38 and $
       (exist(minarr)*exist(maxarr) ? $ ;protect against minarr and maxarr undefined, ras 3-jul-2012
       	(p[term] ge minarr[term] and p[term] le maxarr[term]) : 1) ) do begin
;        print,'count,inc,p(term) = ', count, inc,p(term)
         cfit_mcurvefit_f_val,function_name,x,p,yfit1,private=private
         ipder(0,term) = (yfit1-yfit)/inc
         maxdiff = max( abs(cfit_f_div(yfit1-yfit,yfit)) )
         count = count + 1
         inc = originc * 10.^count
         p(term) = origp + inc
      endwhile
   endfor
endif else begin
   ;; The user's procedure will return partial derivatives
   cfit_mcurvefit_f_val,function_name,x,a,yfit,ipder,private=private
ENDELSE

;; Take fixed parameters into account
IF nfit ne nparam THEN pder = ipder(*,varix) ELSE pder = ipder

IF nfit EQ 1 THEN begin
  ny = n_elements(yfit)
  pder = reform(pder,ny,1)
endif

end

;-------

PRO cfit_mcurvefit_param_limit, a, b, minarr, maxarr, rlimit=rlimit
;;
;; Auxiliary routine
;; 21-aug-2008
;; richard.schwartz@nasa.gov

 ;Don't allow b to go out of range, or to change too quickly
 ;Determine which parameters are logarithmic, they have the same
 ;sign, are non-zero, and their ratio of max to min is gt 100.  100 is arbitrary but
 ;the difference between the optimum ratio and 100 is really irrelevant for this test.
 ;
 ;for log parameters, convert them to their alog10 and treat them as the other
 ;parameters, at the end, change them back using exponentiation for base 10
 ;
 ;All variables are forced to take a step size whose maximum is 90% of the distance
 ;to the relevant range stop, ie minarr and maxarr.  The relevant choice is determined
 ;by the sign of the increment to the new paramter.  Only the parameters exceeding
 ;this maximum step size are affected.
 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
If n_params() eq 4  then begin
 ;First peg B at limits so it can never reach nan territory for log manipulations
 default, rlimit, 0.1
 b = (b > minarr) < maxarr

 ;convert log types prior to arithmetic for bounding ops then convert back
 ; use 1.d0 *maxarr in f_div because 1.e20 / 1.e-20 will in float will generate
 ; Floating overflow error message
 sel_log = where( (minarr * maxarr gt 0) and ( cfit_f_div(1.d0*maxarr, minarr) ge 1000.), nsel_log)
 if nsel_log ge 1 then begin
    asn = cfit_f_div( a[sel_log], abs(a[sel_log])) ; gives sign of parameter
    aneg = where( asn lt 0., nneg)
    anz = where( a[sel_log] ne 0., nanz, complement=az, ncomplement=naz)
    bnz = where( b[sel_log] ne 0., nbnz, complement=bz, ncomplement=nbz)
    if naz gt 0 then asn[az] = 1.  ; if a was 0, still want asn to be 1.

    ; for nonzero values, take log
 		if nanz gt 0 then a[sel_log[anz]] = alog10(a[sel_log[anz]]*asn[anz])
 		if nbnz gt 0 then b[sel_log[bnz]] = alog10(b[sel_log[bnz]]*asn[bnz])

 		if nneg ge 1 then begin
     		tempmax  = maxarr[sel_log[aneg]]
     		maxarr[sel_log[aneg]] = minarr[sel_log[aneg]]*asn[aneg]
     		minarr[sel_log[aneg]]  = tempmax[sel_log[aneg]]*asn[aneg]
  	endif
    maxarr[sel_log] = alog10(maxarr[sel_log])
    minarr[sel_log] = alog10(minarr[sel_log])
 	endif

 	bstep =  a - b
 	selneg   = where(bstep lt 0, nselneg)
 	selpos   = where(bstep gt 0, nselpos)
 	if nselpos ge 1 then   bstep[selpos]  =  $
 		bstep[selpos] < ((a-minarr)[selpos] * rlimit)
 	if nselneg ge 1 then   bstep[selneg]  =  $
 		bstep[selneg] > ((a-maxarr)[selneg] * rlimit)
 	b = a - bstep
 	if nsel_log ge 1 then begin ;convert log type back
     		a[sel_log] = asn * 10.^(a[sel_log])
     		b[sel_log] = asn * 10.^(b[sel_log])
     		maxarr[sel_log] = asn * 10.^(maxarr[sel_log])
     		minarr[sel_log] = asn * 10.^(minarr[sel_log])
     		if nneg ge 1 then begin
         		tempmax  = maxarr[sel_log[aneg]]
         		maxarr[sel_log[aneg]] = minarr[sel_log[aneg]]
         		minarr[sel_log[aneg]]  = tempmax[sel_log[aneg]]
     		endif

 	endif

endif

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function cfit_mcurvefit, x, y, w, a, sigmaa, corr=corr, covar=covar, $
                    Function_Name = Function_Name, $
                    itmax=itmax, iter=iter, tol=tol, chi2=chi2, $
                    noderivative=noderivative,maxarr=maxarr,minarr=minarr,$
                    private=private,failed=failed,quiet=quiet,$
                    fail_type=fail_type,error_only=error_only, $
                    show_progress=show_progress, p_noeffect=p_noeffect, $
                    allow_allfixed=allow_allfixed

;
; Fail_type:
;    0 : No failure
;    1 : Not enough data points
;    2 : Loss of precision
;    3 : NaN values occured in calculation
;    4 : Too many repeats - unable to step forward.
;    5 : Maximum number of iterations reached
;

; Copyright (c) 1988-1995, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       MCURVEFIT
;
; PURPOSE:
;       Non-linear least squares fit to a function of an arbitrary
;       number of parameters.  The function may be any non-linear
;       function.  If available, partial derivatives can be calculated by
;       the user function, else this routine will estimate partial derivatives
;       with a forward difference approximation.
;
; CATEGORY:
;       E2 - Curve and Surface Fitting.
;
; CALLING SEQUENCE:
;       Result = CURVEFIT(X, Y, W, A, SIGMAA, FUNCTION_NAME = name, $
;                         ITMAX=ITMAX, ITER=ITER, TOL=TOL, /NODERIVATIVE)
;
; INPUTS:
;       X:  A row vector of independent variables.
;
;       Y:  A row vector of dependent variable, the same length as x.
;
;       W:  A row vector of weights, the same length as x and y.
;               For no weighting,
;               w(i) = 1.0.
;               For instrumental weighting,
;               w(i) = 1.0/y(i), etc.
;
;       A:  A vector, with as many elements as the number of terms, that
;           contains the initial estimate for each parameter.  If A is double-
;           precision, calculations are performed in double precision,
;           otherwise they are performed in single precision.
;
; KEYWORDS:
;       FUNCTION_NAME:  The name of the function (actually, a procedure) to
;       fit.  If omitted, "FUNCT" is used. The procedure must be written as
;       described under RESTRICTIONS, below.
;
;       ITMAX:  Maximum number of iterations. Default = 20.
;       ITER:   The actual number of iterations which were performed
;       TOL:    The convergence tolerance. The routine returns when the
;               relative decrease in chi-squared is less than TOL in an
;               interation. Default = 1.e-3.
;       CHI2:   The value of chi-squared on exit
;       NODERIVATIVE:   If this keyword is set then the user procedure will not
;               be requested to provide partial derivatives. The partial
;               derivatives will be estimated in CURVEFIT using forward
;               differences. If analytical derivatives are available they
;               should always be used.
;       SHOW_PROGRESS:  If set, then print chi-square and params each iteration.
;       P_NOEFFECT: On exit, these are the indices of parameters that have
;             no effect (derivative is 0 when they are changed), which causes NaN's in the
;             results, and fail_type 3 to be returned.  Caller may choose to set these
;             parameters as fixed (min=max) and try again.  -1 means none.
;       ALLOW_ALLFIXED: If set, if all params are fixed (minarr eq maxarr), then just
;             calc chi2 and return yfit without changing any params.  Default behaviour
;             is to generate message event and abort.
;		MINARR: Hard lower limits on parameter vector, A.
;		MAXARR: Hard upper limits on parameter vector, A. MAXARR and MINARR must both
;			be defined.
;       CORR - Correlation matrix
;
; OUTPUTS:
;       Returns a vector of calculated values.
;       A:  A vector of parameters containing fit.
;
; OPTIONAL OUTPUT PARAMETERS:
;       Sigmaa:  A vector of standard deviations for the parameters in A.
;
; COMMON BLOCKS:
;       NONE.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       The function to be fit must be defined and called FUNCT,
;       unless the FUNCTION_NAME keyword is supplied.  This function,
;       (actually written as a procedure) must accept values of
;       X (the independent variable), and A (the fitted function's
;       parameter values), and return F (the function's value at
;       X), and PDER (a 2D array of partial derivatives).
;       For an example, see FUNCT in the IDL User's Libaray.
;       A call to FUNCT is entered as:
;       FUNCT, X, A, F, PDER
; where:
;       X = Vector of NPOINT independent variables, input.
;       A = Vector of NTERMS function parameters, input.
;       F = Vector of NPOINT values of function, y(i) = funct(x(i)), output.
;       PDER = Array, (NPOINT, NTERMS), of partial derivatives of funct.
;               PDER(I,J) = DErivative of function at ith point with
;               respect to jth parameter.  Optional output parameter.
;               PDER should not be calculated if the parameter is not
;               supplied in call. If the /NODERIVATIVE keyword is set in the
;               call to CURVEFIT then the user routine will never need to
;               calculate PDER.
;
; PROCEDURE:
;       Copied from "CURFIT", least squares fit to a non-linear
;       function, pages 237-239, Bevington, Data Reduction and Error
;       Analysis for the Physical Sciences.
;
;       "This method is the Gradient-expansion algorithm which
;       combines the best features of the gradient search with
;       the method of linearizing the fitting function."
;
;       Iterations are performed until the chi square changes by
;       only TOL or until ITMAX iterations have been performed.
;
;       The initial guess of the parameter values should be
;       as close to the actual values as possible or the solution
;       may not converge.
;
; EXAMPLE:  Fit a function of the form f(x) = a * exp(b*x) + c to
;   sample pairs contained in x and y.
;   In this example, a=a(0), b=a(1) and c=a(2).
;   The partials are easily computed symbolicaly:
;     df/da = exp(b*x), df/db = a * x * exp(b*x), and df/dc = 1.0
;
;     Here is the user-written procedure to return F(x) and
;     the partials, given x:
;       pro gfunct, x, a, f, pder   ; Function + partials
;     bx = exp(a(1) * x)
;         f= a(0) * bx + a(2)     ;Evaluate the function
;         if N_PARAMS() ge 4 then $ ;Return partials?
;     pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(x))]]
;       end
;
;         x=findgen(10)        ;Define indep & dep variables.
;         y=[12.0, 11.0,10.2,9.4,8.7,8.1,7.5,6.9,6.5,6.1]
;         w=1.0/y      ;Weights
;         a=[10.0,-0.1,2.0]     ;Initial guess
;         yfit=curvefit(x,y,w,a,sigmaa,function_name='gfunct')
;     print, 'Function parameters: ', a
;         print, yfit
;       end
;
; MODIFICATION HISTORY:
;       Written, DMS, RSI, September, 1982.
;       Does not iterate if the first guess is good.  DMS, Oct, 1990.
;       Added CALL_PROCEDURE to make the function's name a parameter.
;              (Nov 1990)
;       12/14/92 - modified to reflect the changes in the 1991
;            edition of Bevington (eq. II-27) (jiy-suggested by CreaSo)
;       Mark Rivers, U of Chicago, Feb. 12, 1995
;           - Added following keywords: ITMAX, ITER, TOL, CHI2, NODERIVATIVE
;             These make the routine much more generally useful.
;           - Removed Oct. 1990 modification so the routine does one iteration
;             even if first guess is good. Required to get meaningful output
;             for errors.
;           - Added forward difference derivative calculations required for
;             NODERIVATIVE keyword.
;           - Fixed a bug: PDER was passed to user's procedure on first call,
;             but was not defined. Thus, user's procedure might not calculate
;             it, but the result was then used.
;       Stein Vidar Hagfors Haugan, Univ. of Oslo, 7 May 1996
;           - Detecting NaN errors and deadlock repetitions inside the repeat
;             loop.
;       Stein Vidar Hagfors Haugan, 16 October 1996
;           - Added minarr/maxarr keywords for imposed min/max limits.
;           - Added PRIVATE keyword to send info w/o common blocks.
;       Stein Vidar Hagfors Haugan, 17 September 1997
;           - Added FAIL_TYPE (output) and ERROR_ONLY (for quick SIGMAA
;             estimates with no change in parameters.
;       Stein Vidar Hagfors Haugan, 30 July 1999
;           - Only decreasing flambda when > 1e-40 to avoid underflow/hang.
;       Kim Tolbert, 11 February 2004
;           - Added show_progress keyword.  Also, handle 2-D x array.  Also,
;             handle case where x has different length than y.  Use ny, not nx
;             for defining ipder.
;       Linhui Sui, 20 May 2004
;           - To speed up, only compute partial derivative for free parameters
;               (i.e. minarr eq maxarr)
;           - Degrees of freedom (nfree) changed to # of data points minus # of free
;               (or fitting) params, instead of minus total number of params.  Note:
;               this is used in the chisq calculation, so chisq values will be slightly
;               different from before.
;       Kim Tolbert, 23 June 2004
;           - Added p_noeffect keyword.
;           - In chisq calculation, subtract 1 from # degrees of freedom in denominator. This
;             change and Lin's change of 20-May-2004 make Chisq calc more standard -
;             Denominator is now # data points - # free parameters -1
;             Previously denominator was # data points - total # parameters
;       Kim Tolbert, 25 Aug 2005.  Added allow_allfixed keyword
;       Kim Tolbert, 14 Sep 2005. In 'done:' section, only use array if it exists.  For some
;             error conditions (like not enough points), array wasn't created.
;       Kim Tolbert, 15-Aug-2006, use f_div when dividing by c in case there are zeros in c
;       Kim Tolbert, 1-May-2007, For new params (b) to try, changed b = b > minarr to
;             b = b > a/100000. > minarr.  (a is starting param values).  In old way, if
;             any b was negative, it was set to the minarr value, and then the fitting
;             couldn't proceed. New way eases down to the minarr value more gradually
;             so it can recover and still to a good fit.
;       Kim Tolbert, 19_Dec-2007. Don't use double precision epsilon even if input params
;             are double - it's too small and the fits fail.
;       Kim Tolbert, 27-Feb-2008, supply format for 'Iterating...' message
;
;		Richard Schwartz, 21-aug-2008, wrote  Mcurvefit_param_limit
;			This keeps the new param array within the limits of minarr and maxarr
;			which is not guaranteed by the solution for the new B given A, the data, and derivatives
;   Kim Tolbert, 27-Aug-2008, Use f_div in sigma calculation to avoid NaNs from divide by 0.
;   Kim Tolbert, 14-Jan-2009, In mcurvefit_param_limit, handle case where param will be handled in
;     log space, but value is 0.  In mcurvefit partial deriv calc, if changing param by inc has
;     no effect, keep increasing inc by powers of 10 until it does up to 38 times.  Otherwise deriv
;     is 0, and that param will never change.
;   Richard, 19-Feb-2009. In Mcurvefit_param_limit, changed rlimit to .1 (from .9) and value for testing
;     ratio of max to min to determine log parameters to 1000 (from 100)
;   Kim Tolbert, 21-Apr-2009.  Correct maxdiff to be a difference ratio instead of a simple difference
;   Kim Tolbert, 14-Oct-2009. Fixed problem of unreasonably small sigmas.  Changed chi test at end of
;     lambda loop to test for chisq lt chisqr1 OR chisq within tol of chisqr1.  Previously just did < test,
;     but if the two were very close, (like within machine precision) but < wasn't true, lambda
;     kept repeating which meant lambda kept getting bigger and consequently sigmas were extremely
;     small (happened most when you start with a pretty good fit)
;   Kim Tolbert, 3-Dec-2009. Second fix for incorrect sigmas. Now after best-fit params are found, use
;     them to calculate new partial derivatives, re-calculate alpha (with lambda=0) and get sigmas from that.
;     This required pulling out the derivative calculation and making it a separate routine - mcurvefit_f_pder.
;     Also, return correlation matrix (normalized covariance matrix). Undo change of 14-Oct to chi test
;     at end of lambda loop since don't understand it thoroughly, and now doesn't affect sigmas.
;   Kim Tolbert, 15-Dec-2009. Fixed small error in mcurvefit_f_pder - ny undefined.
;   Kim Tolbert, 28-Sep-2010, Check for quiet before printing No params to fit msg.
;   Kim Tolbert, 26-Apr-2012, Added covar keyword to pass out covariance matrix
;	Richard.Schwartz@nasa.gov, 3-Jul-2012, added protection against undefined minarr or maxarr
;		It is highly recommended that mcurvefit be used with minarr and maxarr but it won't
;		crash now without them.
;
;-
       on_error,2             ;Return to caller if error
       IF !debug NE 0 THEN on_error,0

       ;; Print any pending math errors, and then shut up!
       matherr = check_math(1,1)
       matherr = 0 ;; Accumulate new math errors here...

       failed = 0
       fail_type = 0
       quiet = keyword_set(quiet)
       error_only = keyword_set(error_only)

       p_noeffect = -1

       ;;Name of function to fit
       if n_elements(function_name) le 0 then function_name = "FUNCT"
       if n_elements(tol) eq 0 then tol = 1.e-3     ;Convergence tolerance
       if n_elements(itmax) eq 0 then itmax = 20    ;Maximum # iterations
       type = size(a)
       type = type(type(0)+1)
       double = type eq 5
       if (type ne 4) and (type ne 5) then a = float(a) ;Make params floating

       iter = 0

       use_min = n_elements(minarr) EQ n_elements(a)
       use_max = n_elements(maxarr) EQ n_elements(a)

       IF use_min AND use_max THEN BEGIN
          varix = where(minarr NE maxarr,nfit)
          fixix = where(minarr EQ maxarr,nfix)
       END ELSE BEGIN
          nfit = n_elements(a)
          varix = lindgen(nfit)
          fixix = -1L
          nfix = 0
       END

       if n_dimensions(x) eq 2 then nx = n_elements(x)/2 else nx = n_elements(x)
       ny = n_elements(y)

       IF nfit EQ 0 THEN BEGIN
          if keyword_set(allow_allfixed) then begin
             ; if allowed, then just get chi2 and return original params
             if ~quiet then message,"No parameters to fit", /cont
             cfit_mcurvefit_f_val,function_name,x,a,yfit,private=private
             chi2 = total(w*(y-yfit)^2)/( (ny<nx)-1)
             return, yfit
          endif else message, "No parameters to fit"
       END

       IF nfix GT 0 THEN fixed = 1b ELSE fixed = 0b

       ; If we will be estimating partial derivatives then compute machine
       ; precision
       if keyword_set(NODERIVATIVE) then begin
          res = nr_machar(DOUBLE=0)
          eps = sqrt(res.eps)
       endif

       nterms = n_elements(a)   ; # of parameters
       nfree = (ny<nx)-nfit ; Degrees of freedom

       if nfree le 0 then BEGIN
          IF NOT quiet THEN $
             message, 'Curvefit - not enough data points.',/continue
          fail_type = 1
          GOTO,failed
       END

       flambda = 0.001          ;Initial lambda
       diag = lindgen(nfit)*(nfit+1) ; Subscripts of diagonal elements

;      Define the partial derivative array
       if NOT double then ipder = fltarr(ny, nterms) $
       else ipder = dblarr(ny, nterms)

       IF error_only THEN itmax = 1

       for iter = 1, itmax do begin ; Iteration loop

         ; Get partial derivatives (pder) of each param we're fitting
          cfit_mcurvefit_f_pder, function_name, x, a, yfit, pder, private=private, $
            ipder=ipder, minarr=minarr, maxarr=maxarr, noderivative=noderivative, varix=varix, eps=eps

;         Evaluate alpha and beta matricies.

          beta = (y-yfit)*w # pder
          alpha = transpose(pder) # (w # (fltarr(nfit)+1)*pder)
          chisq1 = total(w*(y-yfit)^2)/(nfree-1) ; Present chi squared.

;         Invert modified curvature matrix to find new parameters.

          chisqr = chisq1 ;; Avoid crash if NaN values on first attempt

          c = sqrt(alpha(diag) # alpha(diag))

          repeat BEGIN
             array = cfit_f_div(alpha,c)
;             dprint,'flambda ',flambda, dlevel=6
             array(diag) = array(diag)*(1.+flambda)
             IF nfit GT 1 THEN array = invert(array) $
               ELSE array = reform(1./array,1,1,/overwrite)
;             dprint,'SIGMAS with flambda= ',  sqrt(cfit_f_div(array(diag),alpha(diag))), dlevel=6
             b = a
             b(varix) = a(varix) + cfit_f_div(array,c) # transpose(beta) ; New params

             IF max(abs(b-a)) EQ 0.0 THEN BEGIN
                IF NOT quiet AND NOT error_only THEN $
                   message,"No parameter change, probably loss of precision", $
                   /continue
                fail_type = 2
                GOTO,failed
             END

             IF (where(b ne b))(0) NE -1 THEN BEGIN
                IF NOT quiet THEN $
                   message,"NaN values encountered",/continue
                fail_type = 3
                q = where (alpha(diag) eq 0.)
                if q[0] ne -1 then p_noeffect = varix(q)
                GOTO,failed
             END

              ;minarr and maxarr must be defined here
              if (use_min*use_max) then begin
	             ; don't allow b to change too fast
	             b_in = b
;	             dprint,'b_in: ',b_in, dlevel=5
	             Cfit_mcurvefit_param_limit, a, b, minarr, maxarr
;	             dprint,'After param limits, b: ',b, dlevel=5
             endif
             ; Evaluate function
             cfit_mcurvefit_f_val,function_name,x,b,yfit,private=private

             chisqr = total(w*(y-yfit)^2)/(nfree-1) ; New chisqr
             flambda = flambda*10. ; Assume fit got worse

             IF chisqr NE chisqr THEN BEGIN
                  IF NOT quiet THEN message,"NaN encountered",/continue
                  fail_type = 3
                  GOTO,failed
             END

    		    ;if same_data(b_in, b) eq 0 and chisqr gt chisq1 then b=b_in
             IF flambda GT 1.0e20 THEN BEGIN
                  IF NOT quiet THEN $
                     message,"Too many iterations in lambda loop.  Aborting.",/continue
                  fail_type = 4
                  GOTO,failed
             END
;             dprint, 'In lambda loop: is chisqr <  chisq1 ', chisqr, chisq1, ((chisqr-chisq1)/chisqr), tol, dlevel=6

          endrep until chisqr lt chisq1
;          endrep until ((chisqr-chisq1)/chisqr) le tol  ; kim changed above to this 14-oct-09, undid 3-Dec-09
;          dprint,'Stopped repeat loop.', dlevel=6
;
          if keyword_set(show_progress) then $
             print, ' Iterating... Chisq = ', trim(chisqr, '(g12.3)'), '  Full Chisq = ', trim(chisqr*(nfree-1), '(g12.3)'), $
                '  Parameters = ', arr2str(trim(b,'(g12.4)'), ', ')

          ; Decrease flambda by factor of 10
          IF flambda GT 1e-40 THEN flambda = flambda/100.0

          IF error_only THEN BEGIN
             chisqr = chisq1
             GOTO,done
          END

          a=b                     ; Save new parameter estimate.
;          dprint,' At end of itmax loop, full chisqr, full chisq1: ', chisqr*(nfree-1), chisq1*(nfree-1)
;          dprint,' At end of itmax loop: chisqr, chisq1', chisqr, chisq1, ((chisq1-chisqr)/chisq1)
          if ((chisq1-chisqr)/chisq1) le tol then goto,done ; Finished?

       endfor                   ; itmax iteration loop

       fail_type = 5

FAILED:

       IF NOT quiet AND NOT error_only THEN $
          message, 'Failed to converge, done '+trim(iter)+' iterations', $
          /continue
       failed = 1

       ;; Calculate the fit and the chi2 value given the current parameters
       ;; The user should be aware of the FAILED flag..
       cfit_mcurvefit_f_val,function_name,x,a,yfit,private=private
       chisqr = total(w*(y-yfit)^2)/((nfree-1) > 1)

done:
       IF n_params() EQ 5 or arg_present(corr) or arg_present(covar) THEN BEGIN
          sigmaa = dblarr(n_elements(a))

          ; for some error conditions, didn't make 'array', so check it first.  Kim 14-sep-05
;          if exist(array) then sigmaa(varix) =  sqrt(cfit_f_div(array(diag),alpha(diag))) ; Return sigma's

          ;3-Dec-2009, Kim.  Changed sigma calculation from above line to below

          ; Recalculate derivatives with final best fit parameters, recalculate curvature matrix,
          ;   and calculate sigmas and correlation matrix

          cfit_mcurvefit_f_pder, function_name, x, a, yfit, pder, private=private, $
            ipder=ipder, minarr=minarr, maxarr=maxarr, noderivative=noderivative, varix=varix, eps=eps
          alpha = transpose(pder) # (w # (fltarr(nfit)+1)*pder)
          covar = invert(alpha)
          sigmaa[varix] = sqrt(covar[diag])
          corr = cfit_f_div(covar, (sigmaa[varix] # sigmaa[varix]))

;          print,'corr= ', corr
;          print,'sigmaa =',sigmaa

       ENDIF

       chi2 = chisqr            ; Return chi-squared

       matherr = matherr OR check_math(0,0)

       IF NOT quiet THEN BEGIN
          IF ishft(matherr, 0) THEN message,"Integer divide by zero",/continue
          IF ishft(matherr,-1) THEN message,"Integer overflow",/continue
          IF ishft(matherr,-4) THEN message,"Division by zero",/continue
          IF ishft(matherr,-6) THEN message,"Floating point overflow",/continue
          IF ishft(matherr,-7) THEN message,"Illegal operand",/continue
       ENDIF

       return,yfit              ;return result
END
