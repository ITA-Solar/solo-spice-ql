;+
; Project     : SOHO - CDS     
;                   
; Name        : SPICE_XCFIT
;               
; Purpose     : Interactive design of component fit structure
;               
; Explanation : The component based fit system allows the user to design a
;               model with any number of components to be included in a chi^2
;               fit calculation. This program simplifies the design process.
;
;               See the EXAMPLE section below for a hands-on example session
;               using the program.
;
;               DISPLAY EXPLANATION
;
;               
;               THE LOWER PART OF THE DISPLAY shows one plot of the spectrum
;               and the fitted function, and one plot of the current residual.
;               You may zoom out/move around/zoom in by clicking the left,
;               center and right mouse buttons inside the plots. To move one
;               pixel left or right, click outside the plot boundaries.
;
;               To adjust how the plot scaling (i.e., controlling the YRANGE
;               of the plot) is performed, click the buttons labeled "Adjust
;               plot scaling" to pop up the respective XPLOTSCALE boxes.
;
;               
;               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               
;               THE MIDDLE PART OF THE DISPLAY shows the details of the
;               current fit structure:
;
;               Each component in the fit structure is displayed with one
;               status line for the whole component, showing:
;
;               Color field: Each component is plotted separately, with the
;                            indicated color, in the spectrum/fit plot.
;                            
;               Name : press the button with the name on it to alter the
;                      component name. 
;
;               Descr.: Press this button to see a short description of the
;                       component type.
;
;               Include:ON(OFF) : This button toggles whether a component is
;                                 actually included in the fit model or
;                                 not. If the component is not included, the
;                                 fit will be done (almost) as if this
;                                 component was not present at all.
;
;               Fit:ON(OFF) : This button is used to switch ALL the
;                             component's parameters between the Fit:ON or
;                             Fit:OFF status. When a parameter is in Fit:OFF
;                             status, it will be set to it's INITIAL value
;                             when a fit is calculated, and treated as a
;                             constant.
; 
;               Each component has a certain number of parameters associated
;               with it, and the information about each parameter is displayed
;               as one line, showing (left to right)
;
;               Parameter (name) : Press the button to alter the parameter
;                                  name
;
;               Fit (ON/OFF) : Each component may be held constant during a
;                              fit calculation. Press the button to toggle the
;                              state. Prior to a fit calculation, the
;                              parameter values are set to their INITIAL
;                              values, and then kept at that value if fitting
;                              is turned OFF.
;
;               Min value : The minimum allowed value for the
;                           parameter. During the fit calculation, the
;                           parameters are kept within the specified min/max
;                           values.
;
;               Initial : The initial value of a parameter. The chi^2 fit
;                         (re-)calculations start at the initial values.
;
;               Value : The current value of a parameter, i.e., the end point
;                       of a fit calculation. Note that the chi^2 fit some
;                       times does not converge, so this value does not
;                       necessarily reflect a value where the chi^2 is
;                       minimized.
;
;               Max value : The maximum allowed value for the parameter.
;
;               "Lin. A" & "Lin. B"
;
;               In order to allow e.g., line positions to be shown as
;               velocities (km/s), relative to a lab wavelength, component
;               parameter values may be represented as NOMINAL values or
;               ACTUAL values, related by a LINEAR TRANSFORMATION:
;
;                       ACTUAL = NOMINAL * TRANS_A + TRANS_B
;
;               (and of course, NOMINAL = (ACTUAL - TRANS_B)/TRANS_A)
;
;               This means that to make the positional parameter of a gaussian
;               component have positive velocities (in km/s) when blueshifted,
;               TRANS_A should be -(lam0/c) and TRANS_B should be lam0, where
;               lam0 is the lab wavelength and c is the speed of light in
;               km/s. Of course, making positive velocities correspond to
;               redshifted lines, is achieved by flipping the sign of TRANS_A.
;
;
;               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               
;               THE UPPER PART OF THE DISPLAY contains various buttons:
;
;               Exit : Save changes - will exit and return the current fit
;                      structure in the FIT parameter.
;
;               Exit : Discard changes - will exit and leave the FIT parameter
;                      as it was on entering SPICE_XCFIT (if it was previously
;                      undefined it will now contain a fit structure with a
;                      zero-order background tag).
;
;               Exit : Flag as FAILED/IMPOSSIBLE - is used when the SPICE_XCFIT
;                      program is called from other programs, such as
;                      XCFIT_BLOCK or SPICE_XCFIT_BLOCK.
;
;               Auto-fit:OFF(ON) : This is a status toggle, turning on or off
;                                  automatic refitting after each modification
;                                  of the fit structure.
;
;               Redo fit : Calculate a fit to minimize chi^2 based on the
;                          current fit model, starting with the INITIAL values
;                          for each parameter.
;
;               Reset values:(initial -> value) Sets all current parameter
;                            values equal to their initial values. Useful to
;                            visualize the actual shape of the model in the
;                            initial state.
;
;               Use as initial state:(value -> initial) Sets all parameter
;                                    initial values equal to the parameters'
;                                    current values. This is usually done when
;                                    you're satisfied with the design of a fit
;                                    structure.
;
;               Purge components : This button purges all components in the
;                                  current fit structure which are flagged as
;                                  not included (see "Include:ON(OFF)" above).
;
;               Sort components (various) : The options on this menu will sort
;                                           all the line profiles (gaussian, or
;                                           other types of line profiles that
;                                           are added to the component fit
;                                           system) in various ways.
;
;               Add component : This menu is used to add new components, at
;                               the *current position* (or focus point),
;                               highlighted by an asterisk in the plots. You
;                               may select components showing the absolute
;                               position of the line as the nominal position
;                               value, or components showing the velocity
;                               relative to some lab wavelength.
;
;                               The lab wavelength is taken to be equal to the
;                               current position in the plots, and may have to
;                               be adjusted manually - by adjusting the linear
;                               transformation coefficients A and B (see
;                               discussion under "Lin. A" & "Lin. B" above).
;
;                               You may choose which convention should be used
;                               for blueshift vs the sign of the velocity.
;
;                                  
;               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;               EXAMPLE
;               
;               As an example of how the program works, simply start the
;               program with three (undefined) variables, X, Y and FIT, i.e.,
;
;               IDL> delvar,x,y,fit
;               IDL> spice_xcfit,x,y,fit
;               
;               When the input parameter X is not defined, a dummy spectrum
;               will be created, with a constant background and three gaussian
;               components, plus noise. An array with weights to be used in
;               the fitting procedure is also created. The fit structure will
;               be initialized with a zero-order polynomial if it is not
;               defined.
;
;               Now, MOVE AROUND inside the spectrum/residual plots with the
;               (middle) mouse button, positioning the focus point at a close
;               to the center of the LEFT emission line.
;
;               Select Add component : ..showing absolute position.
;
;               The middle part of the display is updated to show the new
;               component along with the background. The program makes some
;               initial guesses for the fit parameters, and the resulting
;               function is displayed below. No chi^2 fit has yet been
;               performed - press "Redo fit" to improve the fit, and the
;               "Value" column for each component is updated with the ending
;               point of the fit calculation.
;
;               Now, it seems like this is a good fit for this particular
;               component and this particular part of the spectrum, so we
;               press "Use as initial state" to store the current values as
;               initial values for the two components. The initial values are
;               updated on the screen.
;
;               We'll also edit the name of the gaussian component by pressing
;               the button labeled "gauss" - an XINPUT dialog box appears and
;               we alter the name to e.g., "Left", since this we suspect this
;               to be the leftmost component.
;
;               Now we focus on the emission line on the right. Move to the
;               position of the peak, and add another component here.
;
;               We'll see how well this emission feature is matched by a
;               single gaussian by pressing "Redo fit" again. Obviously, the
;               result is not quite good, and in fact we got a much more
;               "credible" result with the initial values, revealing the
;               second component of the blend, so we use the "Reset values -
;               (initial -> values)" button to undo the best fit calculation.
;
;               Now, move to the peak that's visible in the residual (should
;               be at about 503.1) and add a third gaussian.
;
;               Although the results from the initial value guesses could
;               indicate a fourth compoonent, we'll try to "Redo fit" once
;               first. We see that the residual now reflects mostly noise, and
;               we can name the last two components e.g., "Center" and
;               "Right".
;
;               To visualize the effect of leaving out one of the components,
;               press the "Include" button and see what happens with the
;               resultant fit. If you wish to remove a component, make sure
;               its Include switch is turned "OFF", and then press the "Purge
;               components" button.
;
;               When you're satisfied with a fit, exit the SPICE_XCFIT program using
;               the Exit - Save changes button.
;
;               Now, if you'd like to use the designed fit inside e.g., a
;               program, use the command:
;
;               IDL> print_cfit,fit,/program
;
;               This will print out a series of lines that may be inserted
;               into an IDL program to construct the structure describing the
;               component fit. This may then be applied to other data sets
;               in a non-interactive way.
;               
; Use         : SPICE_XCFIT,X,Y,FIT [,WEIGHTS=WEIGHTS]
;    
; Inputs      : X,Y : The spectrum to be fitted. One-dimensional arrays, same
;                     number of elements.
;
;               FIT : Component fit structure, to be modified/created.
;               
; Opt. Inputs : None.
;               
; Outputs     : FIT is changed
;               
; Opt. Outputs: None.
;               
; Keywords    : WEIGHTS : Weights attributed to each data point in the chi^2
;                         fitting procedure.
;
;               USE_CURRENT_VALUE - Normally the best fit is computed at
;               startup, based on the initial values. Setting this keyword
;               avoids this, showing the user the result of the current
;               values.
;
;               NO_CHANGE : Set flag to disallow adding/removing/sorting
;                           components 
;               
;               FAILED : Set to a named variable, which will be equal to 1 if
;                        the user selects the "Flag as FAILED/IMPOSSIBLE" exit
;                        option.
;
; Calls       : eval_cfit, sort_cfit, cw_flipswitch(), cw_plotz(), cwf_fit(),
;               default, handle_create(), mk_comp_poly(), since_version(),
;               trim(), widget_base(), xalive()
;
; Common      : None.
;               
; Restrictions: hmmm
;               
; Side effects: Has been known to cause problems with the X manager.
;               
; Category    : Analysis
;               
; Prev. Hist. : The component based fitting scheme is inspired by the program
;               XSPEC.
;
; Written     : S.V.H. Haugan, UiO, 20 January 1997
;               
; Modified    : Version 2, SVHH, 10 February 1997
;                       Minor modifications in xcfit_remake to minimize
;                       problems with the "Xlib: sequence lost" errors.
;               Version 3, SVHH, 19 March 1997
;                       Added some type checking on inputs, and forced
;                       subsamp>1.
;               Version 4, SVHH, 25 June 1997
;                       Made sure duplicate tag names are not attempted, and
;                       tried to fix various widget problems under IDL v 5.0
;               Version 5, SVHH, 22 October 1997
;                       Added plotting of error bars.
;		Version 6, 12-Jan-1999, William Thompson
;			Added BGauss and Voigt profiles.
;		Version 7, 11-Feb-2000, William Thompson
;			Allow sorting of BGauss and Voigt profiles.
;               Version 8, Martin Wiesmann, 25 August 2021
;                       Copied to SPICE rep. and renamed to spice_xcfit
;
; Version     :
; $Id: 2021-09-02 14:36 CEST $
;-            

;
; Re-evaluates the current fit, computes finely sampled
; values for overplotting
;
PRO spice_xcfit_reeval,info
  
  handle_value,info.fit_handle,fit,/no_copy
  
  eval_cfit,info.x,yfit,fit
  info.yfit = yfit

  eval_cfit,info.xfine,yfine,fit
  info.yfine = yfine
  
  handle_value,info.fit_handle,fit,/set,/no_copy
END


;
; Re-plots the current, evaluated fit
;
PRO spice_xcfit_replot,info
  widget_control,info.spec_id,set_value={replot:1}
  oplot,info.xfine,info.yfine,color=1
  widget_control,info.spec_id,get_value=plott
  IF 1 THEN BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     baseline = !Y.CRANGE(0)
     names = tag_names(fit)
     ncomp = n_elements(tag_names(fit))
     FOR c = 0,ncomp-1 DO BEGIN
        eval_cfit,info.xfine,ycomp,fit.(c)
        okix = where(ycomp GT baseline/500.0,nok)
        IF nok GT 1 THEN BEGIN
           IF names(c) NE 'BG' THEN ycomp = ycomp + baseline
           oplot,info.xfine(okix),ycomp(okix),linestyle=2,color=c+2
        END
     END
     handle_value,info.fit_handle,fit,/set,/no_copy
  END
  IF info.ploterr_f THEN $
     oploterr,info.x,info.y,1./sqrt(info.wt),color=0,max_value=min(info.y)-1
  
  resid = info.y-info.yfit
  widget_control,info.resid_id,set_value=[[info.x],[resid]]
  IF info.ploterr_r THEN BEGIN
     widget_control,info.resid_id,set_value={focusi:plott.focusi,$
                                             psym:3,replot:1}
     oplot,[min(info.x),max(info.x)],[0,0],linestyle=1
     oploterr,info.x,resid,1./sqrt(info.wt),max_value=min(resid)-1
  END ELSE BEGIN
     widget_control,info.resid_id,set_value={focusi:plott.focusi,$
                                             psym:10,replot:1}
  END
     
END


;
; Find best fit based on current state
;
PRO spice_xcfit_refit,info,nofit=nofit
  ;; This could take time..
  widget_control,/hourglass
  
  ;; Get hold of the fit.
  handle_value,info.fit_handle,fit,/no_copy
  
  ;; Instrumental errors..
  wt = info.wt
  
  sfit = make_sfit_stc(fit)
  ;; Calculate fit, but only if wr're not supposed to start with
  ;; current values
  IF NOT keyword_set(nofit) THEN BEGIN
     IF total(sfit.const EQ 0) GT 0 THEN BEGIN
        yfit = cfit(info.x,info.y,aa,fit,/double,weights=wt,failed=failed)
        IF keyword_set(failed) THEN print,string(7b)
     END ELSE BEGIN
        xack,["All parameters are constant - cannot make a fit"],/modal
     END
  ENDIF
  
  
  ;; Store it
  handle_value,info.fit_handle,fit,/set,/no_copy 
  
  ;; Plot results
  spice_xcfit_reeval,info
  spice_xcfit_replot,info
  
  ;; Update displayed values
  handle_value,info.fit_handle,fit,/no_copy 
  
  widget_control,info.fit_id,set_value=fit
  
  handle_value,info.fit_handle,fit,/set,/no_copy
END


;
; Remake the section of the display showing details of the fit structure
;
PRO spice_xcfit_remake,info
  handle_value,info.fit_handle,fit,/no_copy
  ;; Update screen
  xupdate,info.fit_base,0
  
  widget_control,info.fit_id,/destroy
  info.fit_id = cwf_fit(info.fit_base,fit,uvalue='FIT')
  ;; This is that blasted IDL v 5 again...
  info.fit_chtag = widget_label(info.fit_id,value=' ')
  
  xupdate,info.fit_base,1
  WIDGET_CONTROL,info.fit_id,set_value={CWFIT_COLORFRESH,DUMMY:0}
  handle_value,info.fit_handle,fit,/no_copy,/set
END


;
; Add a line component, guess initial/max/min components etc.
;
PRO spice_xcfit_addcomp,info,velocity=velocity,blue_means_negative_velocity=blue
  ;; This takes time..
  widget_control,/hourglass
  
  ;; Get current fit
  handle_value,info.fit_handle,fit
  
  ;; Get residual information for first guess..and default values..
  
  widget_control,info.resid_id,get_value=resid
  ptx = resid.xfocus
  pty = resid.yfocus
  i = resid.focusi
  residual = info.y - info.yfit
  nx = n_elements(residual)
  
  WHILE residual(i)/pty GT 0.5 AND i LT nx-1 DO BEGIN
     i = i+1
     IF NOT exist(xstep) THEN xstep = info.x(i)-info.x(i-1)
  END
  
  imax = i
  i = resid.focusi
  WHILE residual(i)/pty GT 0.5 AND i GT 0 DO BEGIN
     i = i-1
     IF NOT exist(xstep) THEN xstep = info.x(i+1)-info.x(i)
  END
  
  imin = i
  
  maxx = max(info.x,min=minx)
  
  i = resid.focusi
  
  IF NOT exist(xstep) THEN xstep = 1e-3
  
  defwid = 2*((info.x(imax)-info.x(i)) < (info.x(i)-info.x(imin)))
  maxwid = 10*defwid < (maxx-minx)/2.0
  minwid = 0.1*defwid > 0.1*xstep
  
  maxpos = ptx + defwid
  minpos = ptx - defwid
  
  
  ;; Make the new component
  IF keyword_set(velocity) THEN vel = 0.0
  
  ;; Find out what kind of component to add.

  widget_control,info.ctype,get_uvalue=ctype
  case ctype of
      'BGAUSS': begin		;Broadened gaussian

  ;; Customize the wing parameters for NIS-1 and NIS-2

	  if ptx lt 400 then begin	;NIS-1
	     def_wing = 0.8
	     def_asym = 1.0
	  end else begin
	     def_wing = 0.317
	     def_asym = 0.279
	  endelse

	  gauss = mk_comp_bgauss([pty,ptx,defwid,def_wing,def_asym],$
                        max_lam=maxpos,min_lam=minpos,$
                        min_fwhm = minwid,max_fwhm = maxwid,$
                        min_intens=0.0001,velocity=vel)
	  IF keyword_set(velocity) AND keyword_set(blue) THEN BEGIN
	     gauss.param(1).trans_a = -gauss.param(1).trans_a
	  ENDIF

	  ENDCASE

      'VOIGT': begin		;Voigt profile

	  defrwid = defwid / 2
	  defwid = defwid / 100
	  pvoigt, defrwid/defwid, 0, vcorr
	  vcorr = vcorr / defwid / sqrt(!pi)
	  pty = pty / vcorr
	  gauss = mk_comp_voigt([pty,ptx,defwid,defrwid],	$
                        max_lam=maxpos,min_lam=minpos,$
                        min_width = minwid/10000,max_width = maxwid,$
                        min_intens=0.0001/vcorr,velocity=vel)
	  IF keyword_set(velocity) AND keyword_set(blue) THEN BEGIN
	     gauss.param(1).trans_a = -gauss.param(1).trans_a
	  ENDIF

	  ENDCASE

      ELSE: begin		;Default is normal Gaussian profile

	  gauss = mk_comp_gauss([pty,ptx,defwid],	$
			max_lam=maxpos,min_lam=minpos,$
                        min_fwhm = minwid,max_fwhm = maxwid,$
                        min_intens=0.0001,velocity=vel)
	  IF keyword_set(velocity) AND keyword_set(blue) THEN BEGIN
	     gauss.param(1).trans_a = -gauss.param(1).trans_a
	  ENDIF

	  ENDCASE
  
  endcase

  ;; Create unique name, add tag to get new fit
  
  tags = tag_names(fit)
  ntags = n_elements(tags)
  newname = 'IGAUSS'+trim(ntags+1)
  WHILE (where(newname EQ tags))(0) NE -1 DO BEGIN
     ntags = ntags+1
     newname = 'IGAUSS'+trim(ntags+1)
  ENDWHILE
  fit = create_struct(newname,gauss,fit)
  
  info.ncomp = ntags + 1
  ;; Store fit info..
  handle_value,info.fit_handle,fit,/set,/no_copy
  
  spice_xcfit_remake,info
END


;
; Event handling
;
PRO spice_xcfit_event,ev
  
  widget_control,/hourglass
  
  storage = ev.top
  widget_control,storage,get_uvalue=info,/no_copy
  widget_control,ev.id,get_uvalue=uvalue
  
  CASE uvalue OF
     
;
; Exit options - EXIT returns current state, DISCARD reverts to original,
; FLAG_FAILED reverts to original and signals a failure
;
  'EXIT':BEGIN
     widget_control,ev.top,/destroy
     return
     ENDCASE
     
  'ERRPLOT_F_ON':BEGIN
     info.ploterr_f = 1b
     spice_xcfit_replot,info
     ENDCASE
     
  'ERRPLOT_F_OFF':BEGIN
     info.ploterr_f = 0b
     spice_xcfit_replot,info
     ENDCASE
     
  'ERRPLOT_R_ON':BEGIN
     info.ploterr_r = 1b
     spice_xcfit_replot,info
     ENDCASE
     
  'ERRPLOT_R_OFF':BEGIN
     info.ploterr_r = 0b
     spice_xcfit_replot,info
     ENDCASE
     
  'DISCARD':BEGIN                
     ;; Note: this option leaves a copy of org_fit at the handle
     handle_value,info.org_fit_handle,org_fit
     handle_value,info.fit_handle,org_fit,/set,/no_copy
     widget_control,ev.top,/destroy
     return
     ENDCASE
     
  'FLAG_FAILED':BEGIN
     ;; Note: this option leaves *no* copy of org_fit - signals FAILED
     handle_value,info.org_fit_handle,org_fit,/no_copy
     handle_value,info.fit_handle,org_fit,/set,/no_copy
     widget_control,ev.top,/destroy
     return
     ENDCASE
     
;
; Automatic fitting on/off
;
  'AUTOFIT_ON':BEGIN
     info.autofit = 1
     spice_xcfit_refit,info
     ENDCASE
     
  'AUTOFIT_OFF':BEGIN
     info.autofit = 0
     spice_xcfit_reeval,info
     spice_xcfit_replot,info
     ENDCASE

;
; Calculate fit
;
  'REFIT':BEGIN
     spice_xcfit_refit,info
     ENDCASE
     
;
; Fit structure has been modified
;
  'FIT':BEGIN
     handle_value,ev.fit_handle,fit
     handle_value,info.fit_handle,fit,/set,/no_copy
     IF info.autofit THEN spice_xcfit_refit,info $
     ELSE BEGIN
        spice_xcfit_reeval,info
        spice_xcfit_replot,info
     END
     ENDCASE
     
;
; Add components : ADD_ABS for absolute position, ADD_VEL+/- for velocity
;                  position
;
  'ADD_ABS':BEGIN
     spice_xcfit_addcomp,info
     IF info.autofit THEN spice_xcfit_refit,info $
     ELSE begin
        spice_xcfit_reeval,info
        spice_xcfit_replot,info
     END
     ENDCASE
     
  'ADD_VEL+':BEGIN
     spice_xcfit_addcomp,info,/velocity
     IF info.autofit THEN spice_xcfit_refit,info $
     ELSE BEGIN
        spice_xcfit_reeval,info
        spice_xcfit_replot,info
     END 
     ENDCASE

  'ADD_VEL-':BEGIN
     spice_xcfit_addcomp,info,/velocity,/blue_means_negative_velocity
     IF info.autofit THEN spice_xcfit_refit,info $
     ELSE BEGIN
        spice_xcfit_reeval,info
        spice_xcfit_replot,info
     END 
     ENDCASE

;
; Purge non-included components
;
  'PURGE':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     sort_cfit,fit,/nosort,/purge
     info.ncomp = n_elements(tag_names(fit))
     handle_value,info.fit_handle,fit,/set,/no_copy
     spice_xcfit_remake,info
     spice_xcfit_replot,info
     END
     
;
; Sort components one way or other.. 
;
  'SORT_COMP_POS+':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     sort_cfit,fit,['comp_gauss','comp_bgauss','comp_voigt'],1
     handle_value,info.fit_handle,fit,/set,/no_copy
     spice_xcfit_remake,info
     spice_xcfit_replot,info
     ENDCASE
     
  'SORT_COMP_POS-':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     sort_cfit,fit,'comp_gauss',1,/decreasing
     handle_value,info.fit_handle,fit,/set,/no_copy
     spice_xcfit_remake,info
     spice_xcfit_replot,info
     ENDCASE
     
  'SORT_COMP_AMP+':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     sort_cfit,fit,'comp_gauss',0
     handle_value,info.fit_handle,fit,/set,/no_copy
     spice_xcfit_remake,info
     spice_xcfit_replot,info
     ENDCASE
     
  'SORT_COMP_AMP-':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     sort_cfit,fit,'comp_gauss',0,/decreasing
     handle_value,info.fit_handle,fit,/set,/no_copy
     spice_xcfit_remake,info
     spice_xcfit_replot,info
     ENDCASE
     
;
; RESET VALUES (INITIAL -> VALUE)
;
  'INI_2_VAL':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     FOR c = 0,info.ncomp-1 DO BEGIN
        nparm = n_elements(fit.(c).param)
        FOR p = 0,nparm-1 DO BEGIN
           fit.(c).param(p).value = fit.(c).param(p).initial
        END
     END
     handle_value,info.fit_handle,fit,/set,/no_copy
     spice_xcfit_reeval,info
     spice_xcfit_replot,info
     handle_value,info.fit_handle,fit,/no_copy
     widget_control,info.fit_id,set_value=fit
     handle_value,info.fit_handle,fit,/set,/no_copy
     ENDCASE
     
;
; NEW INITIAL VALUES (VALUE -> INITIAL)
;
  'VAL_2_INI':BEGIN
     handle_value,info.fit_handle,fit,/no_copy
     FOR c = 0,info.ncomp-1 DO BEGIN
        nparm = n_elements(fit.(c).param)
        FOR p = 0,nparm-1 DO BEGIN
           fit.(c).param(p).initial = fit.(c).param(p).value
        END
     END
     IF info.autofit THEN spice_xcfit_refit,info
     widget_control,info.fit_id,set_value=fit
     handle_value,info.fit_handle,fit,/no_copy,/set
     ENDCASE
     
;
; Signals from the spectrum/fit or residual plots.
;
  'SPEC':BEGIN
     ev.set.replot = 0
     widget_control,info.resid_id,set_value=ev.set
     widget_control,ev.id,set_value=ev.set
     spice_xcfit_replot,info
     widget_control,info.wle_id,set_value='@'+trim(ev.set.xfocus)
     ;; This doesn't make sense, but it's that bloody IDL v 5 bug again
     ;; We must fool the fit_id base into thinking that it's size has
     ;; been changed... so that it will redisplay it's content, *without*
     ;; telling the parent base to redisplay, thinking it's contents have
     ;; *not* been changed since it's initial creation
     IF since_version('5.0') THEN BEGIN
        widget_control,info.fit_chtag,/destroy
        xupdate,info.fit_id,0
        info.fit_chtag = widget_label(info.fit_id,value=' ')
        xupdate,info.fit_id,1
     END 
     info.zoom = ev.set.zoom
     info.focusi = ev.set.focusi
     ENDCASE
     
  'RESID':BEGIN
     ev.set.replot = 0
     widget_control,ev.id,set_value=ev.set
     widget_control,info.spec_id,set_value=ev.set
     spice_xcfit_replot,info
     widget_control,info.wle_id,set_value='@'+trim(ev.set.xfocus)
     ;; This doesn't make sense, but it's that bloody IDL v 5 bug again
     ;; We must fool the fit_id base into thinking that it's size has
     ;; been changed... so that it will redisplay it's content, *without*
     ;; telling the parent base to redisplay, thinking it's contents have
     ;; *not* been changed since it's initial creation
     IF since_version('5.0') THEN BEGIN
        widget_control,info.fit_chtag,/destroy
        xupdate,info.fit_id,0
        info.fit_chtag = widget_label(info.fit_id,value=' ')
        xupdate,info.fit_id,1
     END 
     info.zoom = ev.set.zoom
     info.focusi = ev.set.focusi
     ENDCASE
;
; Show plot scalers for spectrum or residual
;
  'XPLOTSCALE_SPEC':BEGIN
     dummy = xplotscale(info.spec_scaler,/map,iconify=0,/show)
     ENDCASE
     
  'XPLOTSCALE_RESID':BEGIN
     dummy = xplotscale(info.resid_scaler,/map,iconify=0,/show)
     ENDCASE
     
  'REPLOT':BEGIN
     spice_xcfit_reeval,info
     spice_xcfit_replot,info
     ENDCASE
;
; Decide which kind of profile to add next.
;
   'TYPE_GAUSS': widget_control, info.ctype, set_value='Gauss',	$
	set_uvalue='GAUSS'
   'TYPE_BGAUSS': widget_control, info.ctype, set_value='BGauss', $
	set_uvalue='BGAUSS'
   'TYPE_VOIGT': widget_control, info.ctype, set_value='Voigt',	$
	set_uvalue='VOIGT'
     
  END
  
  widget_control,storage,set_uvalue=info,/no_copy
END



PRO spice_xcfit,x,y,fit,use_current_value=use_current_value,no_change=no_change,$
         weights=weights,failed=failed
  on_error,0
  ;;
  ;; Povide an example
  ;;
  IF n_elements(x) EQ 0 THEN BEGIN
     x = findgen(200)
     x = x/50. + 500.0
     y = 2.0 + 50*exp(-0.5*(x-502.0)^2/.2^2) + $
        40*exp(-0.5*(x-503.0)^2/.2^2) + $
        70*exp(-0.5*(x-502.8)^2/.1^2)
     noiselevel = sqrt(y)
     y = y + noiselevel*randomn(seed,n_elements(y))
     weights = 1.0d/noiselevel^2
  END
  
  ;;
  ;; Provide at least a background
  ;;
  IF n_elements(fit) EQ 0 THEN BEGIN
     fit = {bg:mk_comp_poly(0)} 
  END
  
  parcheck,x,1,typ(/rea),1,'X'
  parcheck,y,2,typ(/rea),1,'Y'
  IF n_elements(x) NE n_elements(y) THEN $
     message,"X and Y must have same number of elements"
  
  parcheck,fit,3,typ(/stc),1,'FIT'
  
  ;;
  ;; Subsampling used for the fine-spaced overplots
  ;;
  subsamp = 10 < (5000/n_elements(x) > 1)
  
  default,xsize,750
  default,ysize,180
  
  IF n_elements(weights) EQ 0 THEN $
     message,"WARNING: No weights supplied - constant weights used",/continue
  
  default,weights,replicate(1.,n_elements(x))
  
  ;; Make sure we have initialized the window system
  window,/free,/pixmap,xsize=5,ysize=5
  tvlct,r,g,b,/get
  wdelete
  
  ;;
  ;; Modify color table to distinguish lots of components, but save
  ;; the old one
  ;; 
  r_old = r
  g_old = g
  b_old = b
  
  r(1) = [255,0,0,0,255,255,255,255,255,128,0,128,128,128,70,170,255,$
          255,126,71,140,255,220]
  g(1) = [0,255,0,255,0,255,128,0,128,128,128,128,0,64,138,0,180,91,255,$
          74,140,254,220]
  b(1) = [0,0,255,255,255,0,0,128,128,0,128,128,128,0,47,26,0,20,156,255,$
          255,213,220]
  tvlct,r,g,b
  
  ;;
  ;; Build widget.
  ;;
  base = widget_base(/column,title='SPICE_XCFIT')
  
  upper = widget_base(base,/row)
  upper2 = widget_base(base,/row)
  
  upi = widget_base(upper,/row)
  quit = widget_button(upi,value='Exit',menu=2)
  nice = widget_button(quit,value='Save changes',uvalue="EXIT")
  skipit = widget_button(quit,value='Discard changes',uvalue="DISCARD")
  flagit = widget_button(quit,value='Flag as FAILED/IMPOSSIBLE',$
                         uvalue="FLAG_FAILED")
  
  fitting = widget_base(upi,/row,/frame)
  autofit = cw_flipswitch(fitting,value='Auto-fit:'+["OFF","ON"],$
                          uvalue="AUTOFIT_"+["OFF","ON"])
  refit = cw_flipswitch(fitting,value='Redo fit'+['',''],$
                        uvalue='REFIT'+["",""])
  
  reset0 = widget_button(upi,value='Reset values',menu=2)
  reset = widget_button(reset0,value='(initial -> value)',uvalue='INI_2_VAL')
  
  reset0 = widget_button(upi,value='Use as initial state',menu=2)
  reset = widget_button(reset0,value='(value -> initial)',uvalue='VAL_2_INI')
  
  upi2 = widget_base(upper2,/row)
  
  purge = widget_button(upi2,value='Purge components',uvalue='PURGE')
  sort = widget_button(upi2,value='Sort components',menu = 2)
  increasep = widget_button(sort,value='Sort lines by position (left->right)',$
                            uvalue='SORT_COMP_POS+')
  decreasep = widget_button(sort,value='Sort lines by position (right->left)',$
                            uvalue='SORT_COMP_POS-')
  decreasea = widget_button(sort,$
                            value='Sort lines by amplitude (strong->weak)',$
                            uvalue='SORT_COMP_AMP-')
  increasea = widget_button(sort,$
                            value='Sort lines by amplitude (weak->strong)',$
                            uvalue='SORT_COMP_AMP+')
  
  ctype = widget_button(upi2,value='Gauss',uvalue='GAUSS',menu=2)
  ctype2 = widget_button(ctype,value='Gauss',uvalue='TYPE_GAUSS')
  ctype3 = widget_button(ctype,value='Broadened Gauss',uvalue='TYPE_BGAUSS')
  ctype4 = widget_button(ctype,value='Voigt',uvalue='TYPE_VOIGT')

  addc = widget_button(upi2,value='Add component..',menu = 2)
  addc2 = widget_button(addc,value='..showing absolute position', $
                        uvalue='ADD_ABS')
  addc3 = widget_button(addc,value='..showing velocity position',menu=2)
  addc4 = widget_button(addc3,value='..blueshift <=> positive velocity',$
                        uvalue='ADD_VEL+')
  addc4 = widget_button(addc3,value='..blueshift <=> negative velocity',$
                        uvalue='ADD_VEL-')
  sensl = [purge,sort,increasep,increasea,decreasep,decreasea,addc,addc2,addc3]
  
  IF keyword_set(no_change) THEN $
     FOR i = 0,n_elements(sensl)-1 DO widget_control,sensl(i),sensitive=0
  
  wle_id = widget_label(upi2,value='@0')
  IF since_version('4.0.1') THEN BEGIN
     widget_control,wle_id,/DYNAMIC_RESIZE
  END
  
  fit_base = widget_base(base,x_scroll_size=xsize,$
                         y_scroll_size=2*ysize,/scroll)
  fit_id = cwf_fit(fit_base,fit,uvalue='FIT')
  ;; This is that blasted IDL v 5 again...
  fit_chtag = widget_label(fit_id,value=' ')
  IF since_version('4.0.1') THEN BEGIN 
     widget_control,fit_chtag,/dynamic_resize
  END
  
  ncomp = n_elements(tag_names(fit))
  zoom = 1.0
  nx = n_elements(x)
  focusi = nx/2
  
  nfine = nx*subsamp
  xfine = interpol(x,nfine)
  
  IF n_elements(lastinfo) EQ 1 THEN BEGIN
     zoom = lastinfo.zoom
     focusi = lastinfo.focusi
  END
  
  widget_control,wle_id,set_value='@'+trim(x(focusi))
  
  n = 0
  FOR c = 0,ncomp-1 DO n = n+n_elements(fit.(c).param(*))
  
  aa = fltarr(n)
  
  fit_handle = handle_create(value=fit)
  org_fit_handle = handle_create(value=fit,/no_copy)
  
  info = {x:x,$
          y:y,$
          xfine:xfine,$
          yfine:xfine*0,$
          wt:weights,$
          yfit:y*0.0,$
          aa : aa,$
          autofit:0,$
          intitial:0,$
          ploterr_f:0b,$
          ploterr_r:1b,$
          wle_id:wle_id,$
          fit_base:fit_base,$
          fit_id:fit_id,$
          fit_chtag:fit_chtag,$
          ncomp:ncomp,$
          org_fit_handle:org_fit_handle,$
          fit_handle:fit_handle,$
          spec_id:0L,$
          spec_scaler:0L,$
          resid_id:0L,$
          resid_scaler:0L,$
          zoom:zoom,focusi:focusi,$
	  ctype:ctype}
  
  onoff = ["ON","OFF"] & offon = shift(onoff,1)
  
  ;; Plot scaler & button to invoke it, plus error bars on/off
  info.spec_scaler = xplotscale(group_leader=base,map=0)
  butt = widget_base(base,/row)
  dummy = cw_flipswitch(butt,value='Adjust plot scaling',$
                        uvalue='XPLOTSCALE_SPEC')
  errbars = cw_flipswitch(butt,value='Error bars:'+offon,$
                          uvalue='ERRPLOT_F_'+offon)
  info.spec_id = cw_plotz(base,xwsize=xsize,ywsize=ysize,uvalue='SPEC',$
                          value=[[x],[y]],psym=10,zoom=zoom,focusi=focusi,$
                          title = 'SPECTRUM/FIT',xplotscale=info.spec_scaler)
  
  spice_xcfit_reeval,info
  
  info.resid_scaler = xplotscale(group_leader=base,map=0)
  butt = widget_base(base,/row)
  dummy = cw_flipswitch(butt,value='Adjust plot scaling',$
                        uvalue='XPLOTSCALE_RESID')
  
  errbars = cw_flipswitch(butt,value='Error bars:'+onoff,$
                          uvalue='ERRPLOT_R_'+onoff)
  
  info.resid_id = cw_plotz(base,xwsize=xsize,ywsize=ysize,$
                           uvalue='RESID',psym=10,$
                           focusi=focusi,value=[[info.x],[info.y-info.yfit]],$
                           title = 'RESIDUAL',xplotscale=info.resid_scaler)
  
  widget_control,base,set_uvalue=info
  
  widget_control,base,/realize
  
  spice_xcfit_refit,info,nofit=keyword_set(use_current_value)
  
  widget_control,base,set_uvalue=info,/no_copy
  
  xmanager,'spice_xcfit',base,/modal
  
  WHILE xalive(base) DO xmanager
  
  ;; Retrieve final fit - might be the original fit if changes are discarded
  handle_value,fit_handle,fit
  
  ;; Retrieve original fit - none present signals FAILED
  handle_value,org_fit_handle,org_fit
  
  IF n_elements(org_fit) EQ 0 THEN FAILED = 1 $
  ELSE                             failed = 0
  
  ;; Free the handles used...
  
  handle_free,fit_handle
  handle_free,org_fit_handle
  
  ;; And restore the color table
  
  tvlct,r_old,g_old,b_old
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'spice_xcfit.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


