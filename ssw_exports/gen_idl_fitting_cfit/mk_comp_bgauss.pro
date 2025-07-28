;+
; Project     : SOHO - CDS     
;                   
; Name        : mk_comp_bgauss()
;               
; Purpose     : Create a structure describing the fit component "bgauss"
;               
; Explanation : Creates a Component Fit sub-structure describing a single
;               Gaussian component with broadened left and right wings.
;               Initial values for the five parameters, INTENSITY, POSITION,
;               FWHM, B_AMP, ASYM, are supplied as a 5-element array parameter.
;
;               All the initial values are supplied as NOMINAL values, except
;               when the VELOCITY keyword is DEFINED - this causes the
;               POSITION to be interpreted as a wavelength, the correct linear
;               transformation coefficients are calculated and the nominal
;               velocity value is taken from the contents of the VELOCITY
;               keyword.
;
;               Velocities are assumed to be in km/s
;
;               When the VELOCITY keyword is used, the default linear
;               transformation coefficients are set up to make BLUESHIFTS
;               correspond to POSITIVE velocities. If you're an observational
;               solar physicist, you'll probably want to switch the sign of
;               the TRANS_A tag of parameter 1 to get it the other way
;               around, i.e.:
;
;                  comp = mk_comp_bgauss([10,300.0,0.3,0,0],velocity=0.0)
;                  comp.param(1).trans_a = - comp.param(1).trans_a
;
;               The evaluation function for a broadened Gaussian component is
;               COMP_BGAUSS, taking three actual parameter values with the same
;               meaning as the three first parameters determined in GAUSSFIT,
;               i.e.:
;               
;                  G(x) = A0 * EXP(-((x-A1)/A2)^2/2)
;
;               The wings are defined as
;
;		   W(x) = A0 * Alpha / ( ((x-A1)/(Kappa*A2))^2 + 1 )
;
;		where Kappa=2*SQRT(2*ALOG(2)) and Alpha is defined as
;
;		   Alpha = A3		;x GE A1 (right wing)
;		   Alpha = A3 * A4	;x LT A1 (left wing)
;
;		The broadened Gaussian is then defined as
;
;                  F(x) = (1 - Alpha) * G(x)  +  W(x)
;
;               The nominal and actual parameter values are related by:
;
;                  ACTUAL = NOMINAL * TRANS_A + TRANS_B
;
;               where e.g., TRANS_A defaults to 0.5/sqrt(2*alog(2)) for the
;               width parameter to be interpreted as the full width at half
;               maximum (FWHM).
;
;               
; Use         : COMP = MK_COMP_BGAUSS(INT_POS_FWHM)
;    
; Inputs      : INT_POS_FWHM : An array with 5 initial values for the
;			       parameters of the Gaussian - intensity, position
;			       and full width at half maximum, wing broadening
;			       amplitude, and the left/right wing asymmetry.
;			       If the VELOCITY keyword is DEFINED, the position
;			       is interpreted as a lab wavelength, and the
;			       linear transformation coefficients for the
;			       velocity are calculated from that.  The value of
;			       VELOCITY is used as the nominal initial value.
;
; Opt. Inputs : None.
;
; Outputs     : Returns structure.
;               
; Opt. Outputs: None.
;               
; Keywords    : CONST : An array with 5 (byte) values, a nonzero entry means
;                       the corresponding parameter is kept constant.
;
;               MAX_ARR : An array of maximum (nominal) parameter values.
;
;               MIN_ARR : An array of minimum (nominal) parameter values.
;
;               TRANS_A : An array with the linear transformation A values for
;                         the parameters. Normally this is only used by the
;                         programs written by e.g., PRINT_CFIT,..,/PROGRAM
;
;               TRANS_B : Array with linear transformation B values. See
;                         TRANS_A
;
;               VELOCITY : Set this equal to the initial velocity if you want
;                          the line position represented by the velocity
;                          relative to a lab wavelength - the lab wavelength
;                          is taken from the supplied POSITION, i.e.,
;                          INT_POS_FWHM(1).
;
;               MAX_INTENS, MIN_INTENS, MAX_VEL, MIN_VEL, MAX_LAM, MIN_LAM,
;               MAX_FWHM, MIN_FWHM, MAX_B_AMP, MIN_B_AMP, MAX_ASYM, MIN_ASYM :
;			   These keywords are alternative methods of specifying
;			   min/max limits.
;
; Calls       : default, mk_component_stc()
;
; Common      : None.
;               
; Restrictions: None.
;               
; Side effects: The output structure will initially be defined to hold the
;		B_AMP and ASYM parameters constant, unless otherwise specified.
;		The routine XCFIT should put in sensible values for these
;		parameters for the CDS NIS-1 and NIS-2 spectra.
;               
; Category    : Analysis
;               
; Prev. Hist. : Modified from MK_COMP_GAUSS by S.V.H. Haugan.
;
; Written     : William Thompson, GSFC, 05-Jan-1999
;               
; Modified    : Version 1, 05-Jan-1999, William Thompson, GSFC
;
; Version     : Version 1, 05-Jan-1999
;-            
FUNCTION mk_comp_bgauss,int_pos_fwhm,const=const,$
                       max_arr=max_arr,min_arr=min_arr,$
                       trans_a=trans_a,trans_b=trans_b,$
                       velocity=velocity,double=double,$
                       max_intens=max_intens,min_intens=min_intens,$
                       max_vel=max_vel,min_vel=min_vel,$
                       max_lam=max_lam,min_lam=min_lam,$
                       max_fwhm=max_fwhm,min_fwhm=min_fwhm,$
                       max_b_amp=max_b_amp,min_b_amp=min_b_amp,$
                       max_asym=max_asym,min_asym=min_asym
  
  c = 3.0e5
  
  IF n_elements(int_pos_fwhm) NE 5 THEN BEGIN
     message,"Use: mk_comp_bgauss([int,pos,fwhm,b_amp,asym])"
  END
  
  intens   = int_pos_fwhm(0)
  position = int_pos_fwhm(1)
  fwhm     = int_pos_fwhm(2)
  b_amp    = int_pos_fwhm(3)
  asym     = int_pos_fwhm(4)
  
  IF n_elements(max_arr) EQ 5 THEN BEGIN
     max_intens = max_arr(0)
     IF n_elements(velocity) NE 0 THEN max_vel = max_arr(1) $
     ELSE                              max_lam = max_arr(1)
     max_fwhm  = max_arr(2)
     max_b_amp = max_arr(3)
     max_asym  = max_arr(4)
  END
  
  IF n_elements(min_arr) EQ 5 THEN BEGIN
     min_intens = min_arr(0)
     IF n_elements(velocity) NE 0 THEN min_vel = min_arr(1) $
     ELSE                              min_lam = min_arr(1)
     min_fwhm  = min_arr(2)
     min_b_amp = min_arr(3)
     min_asym  = min_arr(4)
  END
     
  IF N_elements(velocity) NE 0 THEN BEGIN
     IF n_elements(trans_a) GT 0 OR n_elements(trans_b) GT 0 THEN BEGIN
        message,"Specifying VELOCITY and TRANS_A/TRANS_B may cause conflict",$
           /continue
     END
     default,trans_a,[1,-position/c,0.5/sqrt(2*alog(2)),1,1]
     default,trans_b,[0,position,0,0,0]
     position = velocity(0)
     velocity = 0
  END ELSE BEGIN
     default,trans_a,[1,1,0.5/sqrt(2*alog(2)),1,1]
     default,trans_b,[0,0,0,0,0]
  END
  
  
  
  ;;
  ;; Lambda (or rather actual position) to be used in max/min value calculation
  ;;
  use_lambda = position*trans_a(1) + trans_b(1)
  
  default,max_intens,1e3*intens
  default,min_intens,1e-3*intens
  default,max_vel,0.5*c
  default,min_vel,-0.5*c
  default,max_lam,1.5*use_lambda
  default,min_lam,0.5*use_lambda
  default,max_fwhm,10*fwhm  ;; Better definitions needed....something 
  default,min_fwhm,0.2*fwhm ;; with max/min realistic temperature..?
  default,max_b_amp,1e3*b_amp < 1
  default,min_b_amp,1e-3*b_amp
  default,max_asym,10*asym
  default,min_asym,0.1*asym

  default,const,[0b,0b,0b,1b,1b]
  
  ;; If we're not in velocity mode, the following should be an identity
  ;; operation. If we're in implied velocity mode, this makes max_lam/min_lam
  ;; (which may actually contain min/max velocities) become correct, though
  
  IF n_elements(velocity) EQ 0 THEN BEGIN
     IF n_elements(max_arr) EQ 5 THEN max_lam = max_lam*trans_a(1) + trans_b(1)
     IF n_elements(min_arr) EQ 5 THEN min_lam = min_lam*trans_a(1) + trans_b(1)
     IF max_lam LT min_lam THEN BEGIN
        temp = max_lam
        max_lam = min_lam
        min_lam = temp
     END
  END
  
  ;; mk_comp_bgauss(i,lam,fwhm,velocity=0) should be the same
  ;; as mk_comp_bgauss(i,0.0,fwhm,trans_a=[1,-lam/c,0.5/sqrt(2*alog(2))],$
  ;;                             trans_b=[0, lam,  0])
  
  IF n_elements(trans_a) NE 5 OR n_elements(trans_b) NE 5 THEN BEGIN
     message,"Both TRANS_A and TRANS_B must have 5 elements"
  END
  
  IF n_elements(const) NE 5 THEN BEGIN
     message,"CONST must have 5 elements"
  END
  
  stc = mk_component_stc(5,double=double)
  
  stc.name = 'bgauss'
  stc.func_name = 'comp_bgauss'
  stc.func_string = 'b'
  
  implied_velocity = abs(abs(trans_b(1)/c) - abs(trans_a(1))) LT 0.001
  
  IF implied_velocity THEN positiontx = $
     ['The line position is described as a velocity shift in km/s ',$
      'relative to the "lab wavelength" (~'+trim(trans_b(1))+').',$
      '',$
      'NOTE: Having a negative linear transformation A coefficient', $
      'means that blueshifts correspond to a positive velocity, and',$
      'a positive linear trans. A works the other way around.'] $
  ELSE positiontx = $
     ['The line position is described by the wavelength at the center',$
      'of the Gaussian']
  
  stc.description = $
     ['This component is a single Gaussian, with three parameters,',$
      'intensity, position, and width,',$
      'plus two parameters for the wing broadening amplitude and asymmetry,',$
      'b_amp,asym,',$
      '',$
      positiontx]
  
  
  ;;
  ;; Parameter 0 -- Amplitude
  ;;
  parm = stc.param(0)
  
  parm.name = 'Amplitude'
  parm.description = $
     ['This parameter describes the amplitude of the Gaussian, in the same',$
      'units as the data being fitted']
  parm.initial = intens
  parm.max_val = max_intens
  parm.min_val = min_intens
  
  stc.param(0) = parm
  
  ;;
  ;; Parameter 1 -- Position
  ;;
  parm = stc.param(1)
  
  parm.name = 'Position'
  parm.description = [positiontx]
  
  IF keyword_set(velocity) THEN BEGIN
     
     ;; We're guaranteed that position was given in velocity 
     
     parm.initial = position
     
     ;; When velocity is set, max/min_lam always represent +/- 0.5 c
     ;; max/min_vel are taken from max_arr OR represent +/- 0.5 c
     
     min_vel_from_max_lam = c * (use_lambda - max_lam)/use_lambda
     max_vel_from_min_lam = c * (use_lambda - min_lam)/use_lambda
     
     ;; We want the tighter of the two limits
     
     max_min_vel = min_vel > min_vel_from_max_lam
     min_max_vel = max_vel < max_vel_from_min_lam
     
     parm.max_val = min_max_vel
     parm.min_val = max_min_vel
     
     ;; v = c*(lam0-lam)/lam0
     ;; 
     ;; lam = lam0 - v*(lam0/c) 
     
     parm.trans_a = trans_a(1)
     parm.trans_b = trans_b(1)
     
  END ELSE BEGIN
     parm.initial = position
     
     ;; When velocity is not set, max/min_vel always represent +/- 0.5 c
     ;; max/min_lam are taken from max_arr OR represent +/- 0.5 c
     
     ;; However, it's the *actual* values (transformed with trans_a(1)/b(1))
     ;; of max/min_lam that are used.
     
     max_lam_from_min_vel = use_lambda - use_lambda*min_vel/c
     min_lam_from_max_vel = use_lambda - use_lambda*max_vel/c
     
     max_min_lam = min_lam > min_lam_from_max_vel
     min_max_lam = max_lam < max_lam_from_min_vel
     
     ;; If implied_velocity is set, we have to convert the absolute
     ;; lambda values to velocity values
     
     parm.max_val = (min_max_lam-trans_b(1))/trans_a(1) ;; Takes care of
     parm.min_val = (max_min_lam-trans_b(1))/trans_a(1) ;; implied_velocity
     
     ;; And we have to reverse....
     IF trans_a(1) LT 0.0 THEN BEGIN 
        temp = parm.max_val
        parm.max_val = parm.min_val
        parm.min_val = temp
     END
     
     parm.trans_a = trans_a(1)
     parm.trans_b = trans_b(1)
  END
  
  stc.param(1) = parm
  
  ;;
  ;; Parameter 2 -- Width
  ;;
  
  parm = stc.param(2)
  
  parm.name = 'Width'
  parm.description = $
     ['The width of the Gaussian.',$
      'Described as the FWHM, when a = 0.5/sqrt(2*alog(2)) = 0.4246609,',$
      '(default), or simply as the w in exp( - (deltax/w)^2 * 0.5 ) ',$
      'when a = 1']
  
  parm.initial = fwhm
  parm.max_val = max_fwhm
  parm.min_val = min_fwhm
  parm.trans_a = trans_a(2)
  parm.trans_b = trans_b(2)
  
  stc.param(2) = parm
  
  ;;
  ;; Parameter 3 -- Broadening amplitude
  ;;
  
  parm = stc.param(3)
  
  parm.name = 'B_Amp'
  parm.description = $
     ['The amplitude of the broadening wing,',$
      'Described as a fraction of the Gaussian amplitude']
  
  parm.initial = b_amp
  parm.max_val = max_b_amp
  parm.min_val = min_b_amp
  parm.trans_a = trans_a(3)
  parm.trans_b = trans_b(3)
  
  stc.param(3) = parm
  
  ;;
  ;; Parameter 4 -- Left/right wing broadening asymmetry
  ;;
  
  parm = stc.param(4)
  
  parm.name = 'Asym'
  parm.description = $
     ['The left/right wing broadening asymmetry']
  
  parm.initial = asym
  parm.max_val = max_asym
  parm.min_val = min_asym
  parm.trans_a = trans_a(4)
  parm.trans_b = trans_b(4)
  
  stc.param(4) = parm
  
  ;; Fixes for all of the parameters:
  
  stc.param(*).value = stc.param(*).initial
  stc.param(*).const = const
  
  return,stc
  
END
