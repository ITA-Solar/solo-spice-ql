;+
; Project     :	SOHO - CDS     
;                   
; Name        :	mk_comp_ppoly()
;               
; Purpose     :	Create a structure for a pivoted polynomial fit component
;               
; Explanation :	Creates an Nth order pivoted polynomial component for the
;		component fit system.  The first parameter is the pivot value.
;		The remaining parameters are the standard parameters for the
;		polynomial.  The result is returned as
;
;			A1 + A2*(X-A0) + A3*(X-A0)^2 + ...
;
;		The normal usage is to hold A0 constant at a value within the
;		range of X, while fitting the remaining parameters.  Since X-A0
;		is close to zero, roundoff errors are minimized.
;
; Use         :	COMP = MK_COMP_PPOLY( DEGREE_COEFF )
;    
; Inputs      :	DEGREE_COEFF = Either a scalar giving the degree of the
;			       polynomial, or an array of the initial values
;			       for the coefficients - implicitly determining
;			       the polynomial degree.
;                              
; Opt. Inputs :	None.
;               
; Outputs     :	Returns component structure
;               
; Opt. Outputs:	None.
;               
; Keywords    :	MAX_ARR : Array of maximum values for the coefficients.
;
;		MIN_ARR : Array of minimum values for the coefficients.
;
;		TRANS_A : Array of linear transformation A coefficients.
;
;		TRANS_B : Array of linear transformation B coefficients.
;
;		CONST : Array of (bytes) signifying which coefficients are to
;			be kept constant.  If not passed, then the pivot value
;			(first parameter) is held constant.
;
;		PIVOT : The pivot value.  If DEGREE_COEFF is passed as an
;			array, then this keyword will override the pivot value.
;                       
; Calls       :	default, mk_component_stc(), trim()
;
; Common      :	None.
;               
; Restrictions:	None.
;               
; Side effects:	None.
;               
; Category    :	Analysis
;               
; Prev. Hist. :	From mk_comp_poly by S.V.H.Haugan, UiO, 21 January 1997
;
; History     :	Version 1, 17-Feb-2000, William Thompson, GSFC
;               
; Version     :	Version 1, 17-Feb-2000
;-            

FUNCTION mk_comp_ppoly,degree_coeff,$
                      max_arr=max_arr,min_arr=min_arr,$
                      trans_a=trans_a,trans_b=trans_b,$
                      const=const,pivot=pivot
  
  IF n_params() EQ 0 THEN degree_coeff = [0]
  
  sc = size(degree_coeff)
  
  IF n_elements(degree_coeff) EQ 1 THEN BEGIN
     degree = degree_coeff(0)
     coeff = fltarr(degree+2)
  END ELSE IF sc(0) EQ 1 THEN BEGIN
     degree = sc(1)-2
     coeff = degree_coeff
  END ELSE BEGIN
     message,$
        "Parameter must be either scalar (degree) or vector (coefficients)"
  END

  if n_elements(pivot) eq 1 then coeff(0) = pivot
  
  stc = mk_component_stc(degree+2)
  
  default,max_arr,stc.param(*).max_val
  default,min_arr,stc.param(*).min_val
  default,trans_a,stc.param(*).trans_a
  default,trans_b,stc.param(*).trans_b
  stc.param(0).const = 1b
  default,const,stc.param(*).const
  
  IF n_elements(max_arr) NE degree+2 OR $
     n_elements(min_arr) NE degree+2 OR $
     n_elements(trans_a) NE degree+2 OR $
     n_elements(trans_b) NE degree+2 OR $
     n_elements(const) NE degree+2 THEN BEGIN
     message,"MAX_ARR, MIN_ARR, TRANS_A/B, and CONST must all have "+$
        trim(degree+2)+" elements"
  END
  
  stc.name = 'Pivoted-Polynomial'
  stc.func_name = 'comp_ppoly'
  stc.func_string = 'pp'+trim(degree)
  
  stc.description = $
     ['This component is a pivoted polynomial of degree '+trim(degree)]
  
  stc.param(0).name = 'x0'
  stc.param(0).description = ['This is the pivot value']
  stc.param(0).initial = coeff(0)

  FOR i = 0,degree DO BEGIN
     stc.param(i+1).name = 'c'+trim(i)
     stc.param(i+1).description = $
        ['This is the coefficient for x^'+trim(i)]
     stc.param(i+1).initial = coeff(i+1)
  END
  
  stc.param(*).value = stc.param(*).initial
  ix = indgen(degree+2)
  stc.param(*).max_val = max_arr(ix)
  stc.param(*).min_val = min_arr(ix)
  stc.param(*).trans_a = trans_a(ix)
  stc.param(*).trans_b = trans_b(ix)
  stc.param(*).const = const(ix)
  
  return,stc
END

  
  
