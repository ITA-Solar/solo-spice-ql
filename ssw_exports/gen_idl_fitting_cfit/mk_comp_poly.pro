;+
; Project     : SOHO - CDS     
;                   
; Name        : mk_comp_poly()
;               
; Purpose     : Create a structure describing a polynomial fit component
;               
; Explanation : Creates an Nth order polynomial component for the component
;               fit system.
;
; Use         : COMP = MK_COMP_POLY( DEGREE_COEFF )
;    
; Inputs      : DEGREE_COEFF : Either a scalar giving the degree of the
;                              polynomial, or an array of the initial values
;                              for the coefficients - implicitly determining
;                              the polynomial degree.
;                              
; Opt. Inputs : None.
;               
; Outputs     : Returns component structure
;               
; Opt. Outputs: None.
;               
; Keywords    : MAX_ARR : Array of maximum values for the coefficients.
;
;               MIN_ARR : Array of minimum values for the coefficients.
;
;               TRANS_A : Array of linear transformation A coefficients.
;
;               TRANS_B : Array of linear transformation B coefficients.
;
;               CONST : Array of (bytes) signifying which coefficients are to
;                       be kept constant.
;                       
; Calls       : default, mk_component_stc(), trim()
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

FUNCTION mk_comp_poly,degree_coeff,$
                      max_arr=max_arr,min_arr=min_arr,$
                      trans_a=trans_a,trans_b=trans_b,$
                      const=const
  
  IF n_params() EQ 0 THEN degree_coeff = [0]
  
  sc = size(degree_coeff)
  
  IF sc(0) EQ 0 THEN BEGIN
     degree = degree_coeff
     coeff = fltarr(degree+1)
  END ELSE IF sc(0) EQ 1 THEN BEGIN
     degree = sc(1)-1
     coeff = degree_coeff
  END ELSE BEGIN
     message,$
        "Parameter must be either scalar (degree) or vector (coefficients)"
  END
  
  stc = mk_component_stc(degree+1)
  
  default,max_arr,stc.param(*).max_val
  default,min_arr,stc.param(*).min_val
  default,trans_a,stc.param(*).trans_a
  default,trans_b,stc.param(*).trans_b
  default,const,stc.param(*).const
  
  IF n_elements(max_arr) NE degree+1 OR $
     n_elements(min_arr) NE degree+1 OR $
     n_elements(trans_a) NE degree+1 OR $
     n_elements(trans_b) NE degree+1 OR $
     n_elements(const) NE degree+1 THEN BEGIN
     message,"MAX_ARR, MIN_ARR, TRANS_A/B, and CONST must all have "+$
        trim(degree+1)+" elements"
  END
  
  stc.name = 'Polynomial'
  stc.func_name = 'comp_poly'
  stc.func_string = 'p'+trim(degree)
  
  stc.description = $
     ['This component is a polynomial of degree '+trim(degree)]
  
  FOR i = 0,degree DO BEGIN
     stc.param(i).name = 'c'+trim(i)
     stc.param(i).description = $
        ['This is the coefficient for x^'+trim(i)]
     stc.param(i).initial = coeff(i)
  END
  
  stc.param(*).value = stc.param(*).initial
  IF degree EQ 0 THEN ix = 0 ELSE ix = indgen(degree+1)
  stc.param(*).max_val = max_arr(ix)
  stc.param(*).min_val = min_arr(ix)
  stc.param(*).trans_a = trans_a(ix)
  stc.param(*).trans_b = trans_b(ix)
  stc.param(*).const = const(ix)
  
  return,stc
END

  
  
