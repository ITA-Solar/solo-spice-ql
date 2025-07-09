;+
; Project     : SOHO - CDS     
;                   
; Name        : mk_comp_voigt()
;               
; Purpose     : Create a structure describing the fit component "voigt"
;               
; Explanation : Creates a Component Fit sub-structure describing a single
;		Voigt component. Initial values for the four parameters,
;		INTENSITY, POSITION, WIDTH, RWIDTH, are supplied as a
;		4-element array parameter.
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
;                  comp = mk_comp_voigt([10,300.0,0.3,1.5],velocity=0.0)
;                  comp.param(1).trans_a = - comp.param(1).trans_a
;
;		The evaluation function for a Voigt component is COMP_VOIGT,
;		taking four actual parameter values for the amplitude, line
;		position, doppler width, and transition rate.  The Voigt
;		profile is then evaluated as
;
;		   PVOIGT, ABS(A3/A2), (x-A1)/A2, H
;		   F(x) = A0 * H(x) / (SQRT(!PI)*A2)
;
;               The nominal and actual parameter values are related by:
;
;                  ACTUAL = NOMINAL * TRANS_A + TRANS_B
;
;               
; Use         : COMP = MK_COMP_VOIGT(INT_POS_WIDTH)
;    
; Inputs      : INT_POS_WIDTH : An array with 4 initial values for the
;				parameters of the function - intensity,
;				position, doppler width, and transition rate.
;				If the VELOCITY keyword is DEFINED, the
;				position is interpreted as a lab wavelength,
;				and the linear transformation coefficients for
;				the velocity are calculated from that. The
;				value of VELOCITY is used as the nominal
;				initial value.
;                              
; Opt. Inputs : None.
;               
; Outputs     : Returns structure.
;               
; Opt. Outputs: None.
;               
; Keywords    : CONST : An array with 4 (byte) values, a nonzero entry means
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
;                          INT_POS_WIDTH(1).
;
;               MAX_INTENS, MIN_INTENS, MAX_VEL, MIN_VEL, MAX_LAM, MIN_LAM,
;               MAX_WIDTH, MIN_WIDTH, MIN_RWIDTH, MAX_RWIDTH :
;			   These keywords are alternative methods of specifying
;			   min/max limits.
;
; Calls       : default, mk_component_stc()
;
; Common      : None.
;               
; Restrictions: None.
;               
; Side effects: None.
;               
; Category    : Analysis
;               
; Prev. Hist. : Modified from MK_COMP_GAUSS by S.V.H. Haugan.
;
; Written     : William Thompson, GSFC, 08-Jan-1999
;               
; Modified    : Version 1, 12-Jan-1999, William Thompson, GSFC
;
; Version     : Version 1, 12-Jan-1999
;-            
FUNCTION mk_comp_voigt,int_pos_width,const=const,$
                       max_arr=max_arr,min_arr=min_arr,$
                       trans_a=trans_a,trans_b=trans_b,$
                       velocity=velocity,double=double,$
                       max_intens=max_intens,min_intens=min_intens,$
                       max_vel=max_vel,min_vel=min_vel,$
                       max_lam=max_lam,min_lam=min_lam,$
                       max_width=max_width,min_width=min_width,$
                       max_rwidth=max_rwidth,min_rwidth=min_rwidth
  
  c = 3.0e5
  
  IF n_elements(int_pos_width) NE 4 THEN BEGIN
     message,"Use: mk_comp_voigt([int,pos,width,rwidth])"
  END
  
  intens   = int_pos_width(0)
  position = int_pos_width(1)
  width    = int_pos_width(2)
  rwidth   = int_pos_width(3)
  
  IF n_elements(max_arr) EQ 4 THEN BEGIN
     max_intens = max_arr(0)
     IF n_elements(velocity) NE 0 THEN max_vel = max_arr(1) $
     ELSE                              max_lam = max_arr(1)
     max_width  = max_arr(2)
     max_rwidth = max_arr(3)
  END
  
  IF n_elements(min_arr) EQ 4 THEN BEGIN
     min_intens = min_arr(0)
     IF n_elements(velocity) NE 0 THEN min_vel = min_arr(1) $
     ELSE                              min_lam = min_arr(1)
     min_width  = min_arr(2)
     min_rwidth = min_arr(3)
  END
     
  IF N_elements(velocity) NE 0 THEN BEGIN
     IF n_elements(trans_a) GT 0 OR n_elements(trans_b) GT 0 THEN BEGIN
        message,"Specifying VELOCITY and TRANS_A/TRANS_B may cause conflict",$
           /continue
     END
     default,trans_a,[1,-position/c,1,1]
     default,trans_b,[0,position,0,0]
     position = velocity(0)
     velocity = 0
  END ELSE BEGIN
     default,trans_a,[1,1,1,1]
     default,trans_b,[0,0,0,0]
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
  default,max_width,10*width  ;; Better definitions needed....something 
  default,min_width,0.2*width ;; with max/min realistic temperature..?
  default,max_rwidth,1e3*rwidth
  default,min_rwidth,1e-3*rwidth

  default,const,[0b,0b,0b,0b]
  
  ;; If we're not in velocity mode, the following should be an identity
  ;; operation. If we're in implied velocity mode, this makes max_lam/min_lam
  ;; (which may actually contain min/max velocities) become correct, though
  
  IF n_elements(velocity) EQ 0 THEN BEGIN
     IF n_elements(max_arr) EQ 4 THEN max_lam = max_lam*trans_a(1) + trans_b(1)
     IF n_elements(min_arr) EQ 4 THEN min_lam = min_lam*trans_a(1) + trans_b(1)
     IF max_lam LT min_lam THEN BEGIN
        temp = max_lam
        max_lam = min_lam
        min_lam = temp
     END
  END
  
  ;; mk_comp_voigt(i,lam,width,velocity=0) should be the same
  ;; as mk_comp_voigt(i,0.0,width,trans_a=[1,-lam/c,1],$
  ;;                             trans_b=[0, lam,  0])
  
  IF n_elements(trans_a) NE 4 OR n_elements(trans_b) NE 4 THEN BEGIN
     message,"Both TRANS_A and TRANS_B must have 4 elements"
  END
  
  IF n_elements(const) NE 4 THEN BEGIN
     message,"CONST must have 4 elements"
  END
  
  stc = mk_component_stc(4,double=double)
  
  stc.name = 'voigt'
  stc.func_name = 'comp_voigt'
  stc.func_string = 'v'
  
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
      'of the Voigt profile']
  
  stc.description = $
     ['This component is a single Voigt function, with four parameters,',$
      'intensity, position, width, and rwidth,',$
      '',$
      positiontx]
  
  
  ;;
  ;; Parameter 0 -- Amplitude
  ;;
  parm = stc.param(0)
  
  parm.name = 'Amplitude'
  parm.description = $
     ['This parameter describes the amplitude of the Voigt profile,',$
      'in the same units as the data being fitted']
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
     ['The doppler width of the Voigt profile.']
  
  parm.initial = width
  parm.max_val = max_width
  parm.min_val = min_width
  parm.trans_a = trans_a(2)
  parm.trans_b = trans_b(2)
  
  stc.param(2) = parm
  
  ;;
  ;; Parameter 3 -- Rocking width
  ;;
  
  parm = stc.param(3)
  
  parm.name = 'Rwidth'
  parm.description = $
     ['The rocking width of the Voigt profile.']
  
  parm.initial = rwidth
  parm.max_val = max_rwidth
  parm.min_val = min_rwidth
  parm.trans_a = trans_a(3)
  parm.trans_b = trans_b(3)
  
  stc.param(3) = parm
  
  ;; Fixes for all of the parameters:
  
  stc.param(*).value = stc.param(*).initial
  stc.param(*).const = const
  
  return,stc
  
END
