;+
; Project     : SOHO - CDS     
;                   
; Name        : XCFIT_BLOCK
;               
; Purpose     : Design/apply multi-component fit to data block
;               
; Explanation : See documentation for XCFIT, CFIT, and CFIT_BLOCK first.
;
;               XCFIT_BLOCK is an interface to visualize and modify component
;               fits applied to a block of spectral data, and to keep track of
;               parameter values and the INCLUDE/CONST values for locally
;               excluded components and locally constant parameters.
;
;               The data block may have anywhere from 2 to 7 dimensions, the
;               only requirement is that the first dimension is the
;               "dispersion" dimension, i.e., each point spectrum to be fitted
;               is located in e.g., DA(*,i,j,k,l,m,n).
;
;               The three display columns are, from left to right, the
;               original data, the fit result (one result parameter shown at a
;               time), and the residual data array.
;
;               You may view the data in any way you like, try pushing the
;               buttons just above the image displays to change the dimensions
;               currently displayed, or the button just above the profile
;               plots to change the dimension being plotted.
;
;               To move around in the displayed data, use the middle mouse
;               button. To zoom out/in, use the left/right buttons. All the
;               display columns will be focused on the same physical point in
;               the data even though the displayed dimensions may vary.
;
;               To alter the currently displayed result parameter, select from
;               the pulldown menu with the label "Result:...".
;
;               To adjust the color/plot scaling method of any of the display
;               sections, press the corresponding "Adjust color(plot) scaling"
;               buttons.
;
;               Command buttons:
;
;               
;          View/tweak
;               
;               Pushing this button starts XCFIT, showing the data and the
;               corresponding fit from the current point in the data
;               array. You can modify permanently the INCLUDE and CONST status
;               for any component/parameter for this point. You may also
;               adjust the MIN/MAX limits, INITIAL value etc to circumvent
;               problems with finding a good fit, but these values are not
;               stored individually for each point, and WILL REVERT to the
;               global values stored in the original CFIT structure.
;
;               
;          FAIL
;
;               If the fitting process for any reason (like cosmic rays etc)
;               breaks down completely at some point (usually easily
;               detectable if you view the Chi^2 values), and no tweaking of
;               initial values etc can produce a good fit, you can declare the
;               fit in this point as FAILED. This will flag the values of all
;               the result parameters (and the Chi^2 value) with the MISSING
;               value, and at the same time declares all the variables as
;               CONSTANT at this point.  This will also signal to CFIT_BLOCK
;               that it should not worry about trying to fit this point again.
;
;               
;          Adjust / Adjust (global) MIN/MAX values, names etc
;
;               This button starts XCFIT in the same mode as when you press
;               the View/tweak button, but if you alter the MIN/MAX values, or
;               the component names, variable names etc, this will be
;               permanently changed in the global fit. Be careful not to leave
;               components flagged with INCLUDE:OFF or parameters with FIT:OFF
;               status, since this will be imposed on all the data array
;               points when you do a recalculation from global initial
;               values. You may, however, not add, remove or change the order
;               of any components.
;
;          Redesign / Discard all results, redesign fit structure
;
;               Use this button to start XCFIT in a mode where you can change
;               the fit structure by adding, removing (purging), and sorting
;               components. This will, however, leave XCFIT in the blue as to
;               which parts of any calculated results correspond to which
;               components/parameters, so unless you use either the "Flag as
;               FAILED/IMPOSSIBLE" or "Discard changes" exit options, ALL
;               RESULTS and RESIDUALS will be discarded.
;
;          Calculate / Recalculate based on current result
;
;               This option runs cfit_block over your data, using the current
;               RESULT and INCLUDE/CONST arrays as input. Normally, it's
;               quicker to recalculate a fit from current results than to
;               recalculate from global initial values (since the starting
;               points will normally be much closer to the final values).
;
;          Calculate / Recalculate from global initial values
;
;               This option runs cfit_block over your data, after resetting
;               the current RESULT and INCLUDE/CONST arrays to contain the
;               INITIAL value and INCLUDE/CONST values of the current fit
;               structure.  Normally, it's quicker to recalculate a fit from
;               current results than to recalculate from global initial values
;               (since the starting points will normally be much closer to the
;               final values).
;               
; Use         : XCFIT_BLOCK,LAM,DA,WTS,FIT,MISS,RESULT,RESID [,INCLUDE,CONST]
;    
; Inputs      : LAM : An array of wavelength values. Either one value for
;                     every point in the data array, or a one-dimensional
;                     array to go with all the spectra in the data array.
;
;               DA : Data Array. Up to 7-dimensional data array, with spectra
;                    along the first dimension.
;
;               WTS : Weights to use in the fitting process. No default!
;
;               FIT : The component fit structure
;               
;               MISS : The MISSING value, used to flag missing data points,
;                      and parameter values at points where the fit has been
;                      declared as "FAILED".
;               
;               RESULT : The array to contain the result parameter values (and
;                        the Chi^2) values. May contain current results.
;
;               RESID : Array to contain the residual. Same size as DA, may be
;                       undefined on input.
;
;               INLUCDE : Array to keep the INCLUDE status of each component
;                         at each point.
;
;               CONST : Array to keep the CONST status of each parameter at
;                       each point.
;
;               TITLE : A string to be used as the title of the widget.
;
;               ANALYSIS : A structure containing all the necessary information.
;                          Same structure as is returned by mk_analysis().
;                          If ANALYSIS is provided the following inputs are ignored:
;                          LAM, DA, WTS, FIT, MISS
;
;                      
; Opt. Inputs : INCLUDE, CONST, TITLE, ANALYSIS
;               
; Outputs     : FIT, RESULT, RESID, INCLUDE, CONST
;               
; Opt. Outputs: None.
;               
; Keywords    : ORIGIN, SCALE : As in e.g., PLOT_IMAGE, but always with one
;                               entry for each dimension.
;
;               PHYS_SCALE : Array with same number of elements as ORIGIN and
;                            scale, signifying which dimension scale is to be
;                            taken as physical (i.e., to be used for scaling
;                            the image size).
;
; Calls       : cw_cubeview(), cw_flipswitch(), cw_loadct(), cw_plotz(),
;               cw_pselect(), cwf_status(), default, exist(),
;               handle_killer_hookup, mk_analysis(), mk_comp_poly(),
;               ndim_indices(), parcheck, typ()
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
; Modified    : Version 2, SVHH, 15 December 1997
;                       Circumventing IDL v 5 bug with scrollable bases.
;               Version 3, SVHH, 6 May 1998
;                       Smartened routines a bit to have less useless
;                       redraw operations.
;               Version 4, SVHH, 16 November 1998
;                       Fixed a bug that ignored input MISSING value.
;               Version 5, SVHH, 15 January 1999
;                       Renamed get_indices() -> ndim_indices()
;               Version 6, SVHH, 19 January 1999
;                       Fixed some minor points.
;               Version 8, SVHH, 26 September 2017
;                       Use square brackets when indexing lambda[...] to avoid
;                       collision with new IDL built-in lambda function.
;               Version 9, Martin Wiesmann, 25 August 2021
;                       handles new event from cw_loadct and calls cw_cubeview_force_redraw
;                       in xcfit_block_event
;               Version 10, Martin Wiesmann, 25 May 2023
;                       Whenever variables are checked for 'missing' values, it uses now
;                       the procedures WHERE_MISSING, WHERE_NOT_MISSING, IS_MISSING or IS_NOT_MISSING
;
; Version     :
; $Id: 2023-05-25 11:18 CEST $
;-


;; Getting/setting all data blocks

PRO spice_xcfit_block_gs,info,lam,da,wts,fit,result,residual,include,const,$
                      set=set,copy=copy
  set = keyword_set(set)
  no_copy = 1-keyword_set(copy)
  
  handle_value,info.int.a.lambda_h,lam,no_copy=no_copy,set=set
  handle_value,info.int.a.data_h,da,no_copy=no_copy,set=set
  handle_value,info.int.a.weights_h,wts,no_copy=no_copy,set=set
  handle_value,info.int.a.fit_h,fit,no_copy=no_copy,set=set
  handle_value,info.int.a.result_h,result,no_copy=no_copy,set=set
  handle_value,info.int.a.residual_h,residual,no_copy=no_copy,set=set
  handle_value,info.int.a.include_h,include,no_copy=no_copy,set=set
  handle_value,info.int.a.const_h,const,no_copy=no_copy,set=set
END


; 
; Extracting the current result "image"
;
PRO spice_xcfit_block_get_result,info,showres,title
  handle_value,info.int.a.result_h,result,/no_copy
  handle_value,info.int.titles_h,titles,/no_copy
  
  showres = result(info.ext.result_no,*,*,*,*,*,*)
  szres = size(showres)
  showres = dimreform(showres,szres(2:szres(0)),/overwrite)
  
  ;; showres = reform(result(info.ext.result_no,*,*,*,*,*,*))
  title = titles(info.ext.result_no)
  
  mx = 25
  IF strlen(title) GT mx THEN title = "..."+strmid(title,strlen(title)-mx,mx)
     
  widget_control,info.int.status1_id,$
     set_value={SET_HILIT,hilit:info.ext.result_no}
  widget_control,info.int.status2_id,$
     set_value={SET_HILIT,hilit:info.ext.result_no}
  
  handle_value,info.int.a.result_h,result,/set,/no_copy
  handle_value,info.int.titles_h,titles,/set,/no_copy
END

;
; Extract the fit structure with values and const/include status taken from
; corresponding arrays at the current point - leaves a *copy* of the original
; global values, which will be conserved by spice_xcfit_block_set_fit!
;
PRO spice_xcfit_block_get_fit,info,lam,spec,weight,ix,fit,failed
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const
  
  orgf = fit  ;; *COPY*
  
  f = info.ext.focus
  nf = n_elements(f)
  IF nf LT 7 THEN f = [f,replicate(0L,7-nf)]
  
  spec = reform(data(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
  weight = reform(weights(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
  this_result = reform(result(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
  inc = reform(include(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
  cons = reform(const(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
  
  this_result = this_result(0:n_elements(this_result)-2)
  
  failed = where_not_missing(this_result, count, missing=info.int.a.missing)
  failed = count EQ 0
  
  szl = size(lambda)
  IF szl(0) EQ 1 THEN lam = lambda $
  ELSE lam = reform(lambda[*,f(1),f(2),f(3),f(4),f(5),f(6)],/overwrite)
  
  ix = where_not_missing(spec, ngood, missinginfo.int.a.missing)
  
  IF ngood GT 0 THEN BEGIN
     spec = spec(ix)
     lam = lam(ix)
     weight = weight(ix)
  END
  
  update_cfit,fit,this_result,inc=inc,const=cons
  
  ;; *COPY* of original fit put back..
  spice_xcfit_block_gs,info,lambda,data,weights,orgf,result,residual,include,const,$
     /set
END


;
; Put the results of viewing/tweaking a fit back into place
; (and update status display)
;
; If there is a fit structure present at the handle, leave it intact
; (assume it's the original global values)
;
PRO spice_xcfit_block_set_fit,info,lam,spec,weight,ix,fit,failed,nochange=nochange
  
  spice_xcfit_block_gs,info,lambda,data,weights,orgf,result,residual,include,const
  
  ;; Set the change flag
  info.int.changed = 1b
  
  f = info.ext.focus
  nf = n_elements(f)
  IF nf LT 7 THEN f = [f,replicate(0L,7-nf)]
  
  IF NOT failed THEN BEGIN 
     eval_cfit,lam,yfit,fit,/double,sfit=sfit
     nfree = n_elements(ix)-total(sfit.const EQ 0)
     IF nfree LT 1 THEN failed = 1
  END
  
  
  IF NOT failed AND NOT keyword_set(nochange) THEN BEGIN
     
     residual_here = reform(data(*,f(1),f(2),f(3),f(4),f(5),f(6)),/overwrite)
     residual_here(ix) = spec-yfit
     
     chi2_here = total(weight*residual_here(ix)^2)/nfree
     
     sfit_value = make_sfit_stc(fit,/values)
     a_nom = sfit.a_nom
     
     ;;
     ;; Find if any component has been excluded, and flag with missing
     ;;
     iix = where([sfit_value.include] EQ 0,nex)
     IF nex GT 0 THEN BEGIN
        FOR i=0,nex-1 DO BEGIN
           IF iix(i) GT 0 THEN s_parm = total(sfit_value.n_parms(0:iix(i)-1)) $
           ELSE s_parm = 0
           a_nom(s_parm:s_parm+sfit_value.n_parms(iix(i))-1) = $
              info.int.a.missing
        END
     END
     
     residual(ix,f(1),f(2),f(3),f(4),f(5),f(6)) = residual_here(ix)
     result(*,f(1),f(2),f(3),f(4),f(5),f(6)) = [a_nom,chi2_here]
     include(*,f(1),f(2),f(3),f(4),f(5),f(6)) = sfit_value.include
     const(*,f(1),f(2),f(3),f(4),f(5),f(6)) = sfit_value.const
     
  END ELSE IF failed THEN BEGIN
     
     result(*,f(1),f(2),f(3),f(4),f(5),f(6)) = info.int.a.missing
     residual(*,f(1),f(2),f(3),f(4),f(5),f(6)) = info.int.a.missing
     const(*,f(1),f(2),f(3),f(4),f(5),f(6)) = 1b
     include(*,f(1),f(2),f(3),f(4),f(5),f(6)) = 0b
     
     ;; This is simply to make all parameters const in this case
     ;; before we update the const/include status display
     
     update_cfit,fit,reform(result(*,f(1),f(2),f(3),f(4),f(5),f(6))),$
        const = reform(const(*,f(1),f(2),f(3),f(4),f(5),f(6))),$
        include = reform(include(*,f(1),f(2),f(3),f(4),f(5),f(6)))
  END
  
  ;;
  ;; Create error bar overplot info
  ;;
  yerr = 1./sqrt(weight)
  errp = {x:lam,y:spec,err:yerr}
  handle_value,info.int.errplot_h,errp,/set
     
  ;; Update the microplot
  ;; 
  val = [[lam],[spec]]
  widget_control,info.int.microplot_id,set_value=val
  
  ;;
  ;; Evaluate fit on fine grid, overplot microplot
  ;;
  IF NOT failed THEN BEGIN 
     nfine = n_elements(lam)*10 < 5000
     
     finegrid = interpol(lam,nfine)
     eval_cfit,finegrid,finefunc,fit,/double
     
     handle_value,info.int.microfine_h,[[finegrid],[finefunc]],/set
     oplot,finegrid,finefunc
  END ELSE BEGIN
     ;; Undefine the fine grid result
     handle_value,info.int.microfine_h,dummy,/no_copy
  END
  
  ;;
  ;; Overplot error information on microplot (if present)
  ;;
  handle_value,info.int.errplot_h,errp
  IF exist(errp) AND info.ext.plot_err THEN $
     oploterr,errp.x,errp.y,errp.err,max_value=min(errp.y)-1
  
  ;; Update local status display according to the values at this point
  widget_control,info.int.status2_id,set_value=fit
  widget_control,info.int.status2_id,$
     set_value={SET_HILIT,hilit:info.ext.result_no}
  
  ;;
  ;; Put data blocks back
  ;;
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const,$
     /set
  
  ;; Leave the original fit intact (if present)
  IF exist(orgf) THEN handle_value,info.int.a.fit_h,orgf,/set,/no_copy
  
  ;; Update global status display (with whatever ended up there..)
  handle_value,info.int.a.fit_h,fit,/no_copy
  widget_control,info.int.status1_id,set_value=fit
  widget_control,info.int.status1_id,set_value=$
     {SET_HILIT,hilit:info.ext.result_no}
  handle_value,info.int.a.fit_h,fit,/set,/no_copy
  
  ;;
  ;; Give the new residual *handle* to the residual viewer *after* putting 
  ;; the data back
  ;;
  IF NOT keyword_set(nochange) THEN $
     widget_control,info.int.residual_id,set_value=info.int.a.residual_h
  
  
  ;;
  ;; Give the new result array to the result viewer (assumes result at handle)
  ;;
  IF NOT keyword_set(nochange) THEN BEGIN 
     spice_xcfit_block_get_result,info,showres
     widget_control,info.int.result_id,set_value=showres
  END
END




;
; Register (possibly new) fit, (re-)create result/residual/inc/const data
; arrays when necessary, rebuild result choice menu
;
PRO spice_xcfit_block_register,info
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const
  
  ;; Get the initial values etc.
  sfit = make_sfit_stc(fit)
  a_nom = sfit.a_nom
  
  na = n_elements(a_nom)
  ni = n_elements(sfit.include)
  
  szd = size(data)
  szr = size(result)
  szi = size(include)
  szc = size(const)
  
  
  ;; If RESULT has wrong number of dimensions, wrong number of parameters
  ;; (including Chi^2) or wrong size in other dimensions, then discard and
  ;; rebuild
  
  res_dim = [na+1,szd(2:szd(0))]
  
  IF szr(0) NE szd(0) OR szr(1) NE na+1 OR $
     total(szr(2 : szr(0) > 2) NE szd(2 : szd(0) > 2)) NE 0 THEN BEGIN

     message,"Making new RESULT array",/continue
     
     res_dim1 = res_dim
     res_dim1(1:*) = 1
     resa = [a_nom,info.int.a.missing]
     result = dimrebin(dimreform(resa,res_dim1,/overwrite),res_dim,/sample)
     info.ext.result_no = 0
  END
  
  ;; If INCLUDE has wrong number of dimensions, wrong number of components, or
  ;; wrong size in other dimensions, then discard and rebuild
  
  inc_dim = [n_elements(sfit.include),szd(2:szd(0))]
  
  IF szi(0) NE szd(0) OR szi(1) NE ni OR $
     total(szi(2 : szi(0) > 2) NE szd(2 : szd(0) > 2)) NE 0 THEN BEGIN
     
     message,"Making new INCLUDE array",/continue
     
     inc_dim1 = inc_dim
     inc_dim1(1:*) = 1
     inca = [sfit.include]
     include = dimrebin(dimreform(inca,inc_dim1,/overwrite),inc_dim,/sample)
  END
  
  ;; If CONST has: wrong number of dimensions, wrong number of components, or
  ;;               wrong size in other dimensions, then discard and rebuild
  
  con_dim = [n_elements(sfit.const),szd(2:szd(0))]
  
  IF szc(0) NE szd(0) OR szc(1) NE na OR $
     total(szc(2 : szc(0) > 2) NE szd(2 : szd(0) > 2)) NE 0 THEN BEGIN
     
     message,"Making new CONST array",/continue
     
     con_dim1 = con_dim
     con_dim1(1:*) = 1
     cona = [sfit.const]
     const = dimrebin(dimreform(cona,con_dim1,/overwrite),con_dim,/sample)
     
     ix = where(total(is_missing(result, missing=info.int.a.missing),1) EQ na+1,nfailed)
     IF ix(0) NE -1 THEN BEGIN
        print,"Keeping "+trim(nfailed)+" points constant (FAILED)"
        FOR i = 0,na-1 DO cfit_bpatch,const,ix,i,1b
     END
  END
  
  ;;
  ;; Residual should always have same size as data
  ;;
  IF total(size(residual) NE szd) NE 0 THEN BEGIN
     message,"Making new RESIDUAL array",/continue
     residual = make_array(size=szd,value=info.int.a.missing)
  END
  
  ;;
  ;; Build pulldown menu for choosing the result to be displayed
  ;;
  menu = {PSELECT_S,btext:'',mtext:'',uvalue:'',flags:0}
  
  p_no = 0
  FOR c = 0,n_elements(tag_names(fit))-1 DO BEGIN
     cname = fit.(c).name
     menu = [menu,{PSELECT_S,cname,cname,'NEVER',1}]
     np = n_elements(fit.(c).param)
     FOR p = 0,np-1 DO BEGIN
        IF p EQ np-1 THEN flag = 2 ELSE flag = 0
        menu = [menu,{PSELECT_S,cname+":"+fit.(c).param(p).name,$
                      fit.(c).param(p).name,"RESULT#:"+trim(p_no),flag}]
        p_no = p_no + 1
     END
  END
  
  menu = [menu,{PSELECT_S,'Chi^2','Chi^2','RESULT#:'+trim(p_no),0}]
  
  menu = menu(1:*)
  
  titles = menu(where((menu(*).flags AND 1) XOR 1)).btext
  
  handle_value,info.int.titles_h,titles,/set
  
  last_id = widget_info(info.int.result_pdb,/child)
  IF last_id NE 0L THEN widget_control,last_id,/destroy
  
  IF since_version('4.0.1') THEN widget_control,info.int.result_pdb,update=0
  
  dummy = cw_pselect(info.int.result_pdb,'Result:',menu)
  
  widget_control,dummy,set_value='RESULT#:'+trim(info.ext.result_no)
  
  IF since_version('4.0.1') THEN widget_control,info.int.result_pdb,update=1
  
  ;; Update status (const/include)
  ;;widget_control,info.int.status2_id,set_value=fit
  
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const,$
     /set
  
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The following section deals with pixel grabbing/manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION spice_xcfit_block_pix_defprog
  COMMON  spice_xcfit_block_pix_edit,lastprog
  IF exist(lastprog) THEN return,lastprog
  return,$
     ['a = sqrt(1./(weights>1e-6))          ; Noise, if the weights ' + $
      'are right',$
      'ix = where(data eq missing or weights eq missing) ' + $
      '; Bad points - take''em out',$
      'if ix(0) ne -1L then a(ix) = missing',$
      '',$
      'b = average(a,1,missing=missing)     ;Average/total noise level',$
      'c = average(data,1,missing=missing)  ;Average/total signal level',$
      'mask = c gt 1.5*b                    ;Decide...']
END

FUNCTION spice_xcfit_block_pix_explain
  
  return,'  '+$
     ['',$
      'This widget allows editing a sequence of one-line statements that',$
      'are executed in order to calculate a "point mask" - a logical (byte)',$
      'array with one point per "spatial" pixel.',$
      '',$
      'The mask is then used by XCFIT_BLOCK to do things like',$
      '',$
      '     ix = where(mask)',$
      '     cfit_bpatch,result,ix,parameter,value', $
      '',$
      'I.e., it''s used to patch the result/const/include arrays of the',$
      'analysis with values taken from the global fit structure.',$
      '',$
      '(Scroll down to see several examples of valid scripts with comments)',$ 
      '',$
      'If your data block has dimensions (25,50,147), the sequence',$
      'of statements should produce a MASK which has dimensions (50,147)',$
      '(although e.g., (1,50,147) is also accepted).',$
      '',$
      'The following variables are declared when the execution starts:',$
      '',$
      '  LAMBDA,DATA,WEIGHTS,FIT,MISSING,RESULT,RESIDUAL,INCLUDE,CONST',$
      '',$
      'i.e., all variables normally associated with a block fit.',$
      'The meaning and dimensionality of all the variables are as in calls',$
      'to e.g., CFIT_BLOCK.',$
      '',$
      'Since the statements are processed by IDL''s EXECUTE() function, they',$
      'should not try to define any *new* variables - so the following',$
      'variable names have already been "defined" (and may be thus be used):',$
      '',$
      '  A,B,C,D,E,F,IX,MASK',$
      '',$
      'In addition, of course, you may write a completely general *function*',$
      'or *procedure* of your own, that may be called as a one-line',$
      'statement from the script.',$
      '',$
      'The program must be written such that it ultimately calculates',$
      'a point mask, stored in the variable MASK.',$
      '',$
      'The resulting MASK will be REFORM''ed to remove any dangling singular',$
      'dimensions.',$
      '',$
      'Examples:',$
      '----------------------------------------------------',$
      '; Flag all points having one or more parameter(s) kept constant',$
      '',$
      ' MASK = total(const,1) gt 0b',$
      '----------------------------------------------------',$
      '; Flag all points having parameter number two kept constant',$
      '',$
      ' MASK = const(1,*,*)',$
      '----------------------------------------------------',$
      '; Flag all points with more than a certain amount of total flux',$
      '',$
      ' MASK = total(data,1) gt 10.5',$
      '',$
      ';(note that it''s really better to work with AVERAGE(), since',$
      ';TOTAL() does not recognize MISSING values). To specify exactly',$
      ';the same criterion as above use e.g.:',$
      '',$
      ' MASK=average(data,1,missing=missing) gt 10.5/n_elements(data(*,0,0))',$
      '----------------------------------------------------',$
      '',$
      '']
END

;
; Execute the grabbing program - make sure data etc. are available
;
PRO spice_xcfit_block_pix_exec,program,lambda,data,weights,fit,missing,$
                         result,residual,include,const,mask
  
  sz = size(const(0,*,*,*,*,*,*))
  sz(0) = sz(0)-1
  sz = [sz(0),sz(2:*)]
  
  catch,error
  IF error NE 0 THEN BEGIN
     
errorcatch:
     
     catch,/cancel
     msg = ["The following error occured:","",!err_string,""]
     IF exist(done_sofar) THEN BEGIN
        msg = [msg,'The following statements had been/was being processed',$
               done_sofar]
     END
     msg = [msg,'',"A blank mask will be returned"]
     
     xack,msg
     mask = make_array(size=sz)
     return
  END
  
  a = 0 & b = 0 & c = 0 & d = 0 & e = 0 & f = 0 & ix = 0
  
  delvarx,mask
  
  FOR i = 0,n_elements(program)-1 DO BEGIN
     dummy = execute(program(i))
     IF dummy NE 1 THEN GOTO,errorcatch
  END 
  
  IF NOT exist(mask) THEN BEGIN
     
     xack,['MASK was not defined by the program - ' + $
           'a blank mask will be returned']
     mask = make_array(size=sz)
  ENDIF
  mask = reform(mask)
END

PRO spice_xcfit_block_pix_getmask,info,mask,recalculate=recalculate
  
  handle_value,info.int.pix_mask_h,mask
  
  IF NOT exist(mask) OR keyword_set(recalculate) THEN BEGIN 
     handle_value,info.int.pix_prog_h,prog
     
     spice_xcfit_block_gs,info,lam,da,wts,fit,result,residual,include,const,/copy
     
     missing = info.int.a.missing
     
     spice_xcfit_block_pix_exec,prog,lam,da,wts,fit,missing,result,residual,$
        include,const,mask
     
     handle_value,info.int.pix_mask_h,mask,/set
  END
END


PRO spice_xcfit_block_pix_wmask,info,mask
  
  IF NOT exist(mask) THEN BEGIN 
     
     ;; Get the mask
     
     spice_xcfit_block_pix_getmask,info,mask
     
     ;; Find which fits have been flagged as failed.
     
     handle_value,info.int.a.result_h,result,/no_copy
     failed = total(where_missing(result, missing=info.int.a.missing),1) $
        EQ n_elements(result(*,0,0,0,0,0,0))
     handle_value,info.int.a.result_h,result,/set,/no_copy
     
     nfail = total(failed)
     IF nfail GT 0 THEN BEGIN
        xack,trim(nfail)+' points are flagged as FAILED' + $
           ' - will not touch them',/turn_off
     END
     
     mask = mask AND (1b-reform(failed,/overwrite))
     
     IF total(mask) EQ 0 THEN BEGIN
        xack,'No points were masked'
        return
     END
     
  END
  
END


PRO spice_xcfit_block_pix_flicker,info
  
  spice_xcfit_block_pix_getmask,info,mask
  
  ix = where(mask)
  
  IF ix(0) EQ -1L THEN return
  
  spice_xcfit_block_get_result,info,showres
  
  shres = showres
  shres(ix) = max(showres)
  
  FOR jj = 0,2 DO BEGIN 
     IF jj NE 0 THEN wait,.1
     widget_control,info.int.result_id,set_value=shres
     wait,.5
     widget_control,info.int.result_id,set_value=showres
  END
END

PRO spice_xcfit_block_pix_edit_setv,id,value
  widget_control,id,get_uvalue=top
  widget_control,top,get_uvalue=info
  handle_value,info.int.pix_prog_h,value,/set
  
  spice_xcfit_block_pix_getmask,info,/recalculate
  spice_xcfit_block_pix_flicker,info
END



PRO spice_xcfit_block_pix_edit,info
  COMMON spice_xcfit_block_pix_edit,lastprog
  
  handle_value,info.int.pix_prog_h,prog
  
  defprog = spice_xcfit_block_pix_defprog()
  
  expl = spice_xcfit_block_pix_explain()
  
  default,prog,defprog
  
  ;; So that the pix_flicker program may find its way
  widget_control,info.int.top_id,set_uvalue=info 
  
  xtextedit,prog,explanation=expl,setv_id=info.int.pix_id,$
     setv_text='Test program'
  
  handle_value,info.int.pix_prog_h,prog,/set
  lastprog = prog
END


PRO spice_xcfit_block_pix_setconst,info,mask=mask,novisit=novisit,one=one
  
  spice_xcfit_block_pix_wmask,info,mask
  
  ix = where(mask)
  
  ;; Get the global const status
  
  handle_value,info.int.a.fit_h,fit
  sfit = make_sfit_stc(fit)
  
  handle_value,info.int.a.const_h,const,/no_copy
  
  IF keyword_set(one) THEN BEGIN 
     j = info.ext.result_no
     cfit_bpatch,const,ix,j,sfit.const(j)
  END ELSE FOR j = 0,n_elements(sfit.const)-1 DO BEGIN
     cfit_bpatch,const,ix,j,sfit.const(j)
  END
  
  handle_value,info.int.a.const_h,const,/set,/no_copy   ;; That's it!
  
  ;; Revisit point to update local status
  IF NOT keyword_set(novisit) THEN spice_xcfit_block_visitp,info
  
END

PRO spice_xcfit_block_exclude_patch,info
  ;; Make sure parameter values for non-included components are set to
  ;; missing
  spice_xcfit_block_gs,info,lam,da,wts,fit,result,residual,include,const
  
  sfit = make_sfit_stc(fit)
  
  FOR c = 0,n_elements(include(*,0,0,0,0,0,0))-1 DO BEGIN
     ix = where(include(c,*,*,*,*,*,*) EQ 0b,count)
     IF count GT 0 THEN BEGIN
        pstart = 0
        IF c GT 0 THEN pstart = total(sfit.n_parms(0:c-1))
        FOR j = 0,sfit.n_parms(c)-1 DO BEGIN
           cfit_bpatch,result,ix,pstart+j,info.int.a.missing
           cfit_bpatch,const,ix,pstart+j,1b
        END
     END
  END
  
  spice_xcfit_block_gs,info,lam,da,wts,fit,result,residual,include,const,/set
END

PRO spice_xcfit_block_pix_setinclude,info,mask=mask,novisit=novisit,one=one
  
  spice_xcfit_block_pix_wmask,info,mask
  
  ix = where(mask)
  
  ;; Get the global include status
  
  handle_value,info.int.a.fit_h,fit
  sfit = make_sfit_stc(fit)
  
  handle_value,info.int.a.include_h,include,/no_copy
  
  IF keyword_set(one) THEN BEGIN
     j = info.ext.result_no
     c = 0
     WHILE sfit.n_parms(c) LE j DO BEGIN 
        j = j-sfit.n_parms(c)
        c = c+1
     END
     
     ;; And then....
     cfit_bpatch,include,ix,c,sfit.include(c)
  END ELSE FOR j = 0,n_elements(sfit.include)-1 DO BEGIN
     cfit_bpatch,include,ix,j,sfit.include(j)
  END
  
  handle_value,info.int.a.include_h,include,/set,/no_copy   ;; That's it!
  
  spice_xcfit_block_exclude_patch,info
  
  ;; Revisit point to update local status
  IF NOT keyword_set(novisit) THEN spice_xcfit_block_visitp,info
  
END

PRO spice_xcfit_block_pix_reset,info,mask=mask,novisit=novisit,one=one
  
  spice_xcfit_block_pix_wmask,info,mask
     
  ix = where(mask)
  
  ;; Get the global const status
  
  handle_value,info.int.a.fit_h,fit
  sfit = make_sfit_stc(fit)

  handle_value,info.int.a.result_h,result,/no_copy
  
  IF keyword_set(one) THEN BEGIN
     j = info.ext.result_no
     cfit_bpatch,result,ix,j,sfit.a_nom(j)
  END ELSE FOR j = 0,n_elements(sfit.a_nom)-1 DO BEGIN
     cfit_bpatch,result,ix,j,sfit.a_nom(j)
  END
  
  handle_value,info.int.a.result_h,result,/set,/no_copy   ;; That's it!
  
  ;; Revisit point to update local status
  IF NOT keyword_set(novisit) THEN spice_xcfit_block_visitp,info
  
END


PRO spice_xcfit_block_pix_recalc,info,mask=mask,novisit=novisit
  
  spice_xcfit_block_pix_wmask,info,mask
     
  ix = where(mask)
  
  ;; Get the global const status
  
  handle_value,info.int.a.fit_h,fit
  sfit = make_sfit_stc(fit)
  
  handle_value,info.int.a.result_h,result,/no_copy
  cfit_bpatch,result,ix,n_elements(result(*,0,0,0,0,0,0))-1,0.0
  handle_value,info.int.a.result_h,result,/set,/no_copy   ;; That's it!
  
  spice_xcfit_block_calculate,info,smart=2
END


PRO spice_xcfit_block_pix_fail,info,restore=restore
  
  ;; Note - we should *not* take away failed, so use getmask insted of wmask
  
  spice_xcfit_block_pix_getmask,info,mask
  
  ix = where(mask)
  
  handle_value,info.int.a.fit_h,globfit,/no_copy
  sfit = make_sfit_stc(globfit)
  handle_value,info.int.a.fit_h,globfit,/set,/no_copy
  
  spice_xcfit_block_gs,info,lam,da,wts,fit,result,residual,include,const
  
  IF restore THEN BEGIN
     resultv = [sfit.a_nom,0.0]
     constv = sfit.const
     includev = sfit.include
  END ELSE BEGIN
     resultv = [sfit.a_nom*0.0,0.0]+info.int.a.missing
     constv = sfit.const OR 1b
     includev = sfit.include AND 0b
  END 
  
  FOR j = 0,(size(resultv))(1)-1 DO $
     cfit_bpatch,result,ix,j,resultv(j)
  
  FOR j = 0,(size(constv))(1)-1 DO $
     cfit_bpatch,const,ix,j,constv(j)
  
  FOR j = 0,(size(da))(1)-1 DO $
     cfit_bpatch,residual,ix,j,info.int.a.missing
  
  spice_xcfit_block_gs,info,lam,da,wts,fit,result,residual,include,const,/set
  
  spice_xcfit_block_visitp,info
  
END

PRO spice_xcfit_block_pix_apply_all,info,one=one
  spice_xcfit_block_pix_reset,info,mask=mask,/novisit,one=one
  spice_xcfit_block_pix_setconst,info,mask=mask,/novisit,one=one
  spice_xcfit_block_pix_setinclude,info,mask=mask,/novisit,one=one
  spice_xcfit_block_pix_recalc,info,mask=mask,/novisit
  spice_xcfit_block_visitp,info
END

;;
;; Calculate results for the whole block
;;
PRO spice_xcfit_block_calculate,info,smart=smart
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const
  
  spice_cfit_block,lambda,data,weights,fit,info.int.a.missing,result,residual,$
     include,const,/double,/x_face,smart=smart
  
  ;;
  ;; Put back data.
  ;;
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const,$
     /set
  
  ;;
  ;; Show new residual
  ;;
  widget_control,info.int.residual_id,set_value=info.int.a.residual_h
  
  ;;
  ;; Display new results
  ;;
  spice_xcfit_block_get_result,info,showres
  widget_control,info.int.result_id,set_value=showres
END

;
; Visit a new point
; 
; : calculate the fit if not already calculated (or if the /recalculate flag
;   is set)
;
; : 
;
PRO spice_xcfit_block_visitp,info,recalculate=recalculate,restart=restart
  
  ;; Need result to get chi2 (or to recalculate from current value)
  ;; Need const to verify that a fit can be made (or recalculate)
  
  f = info.ext.focus
  nf = n_elements(f)
  IF nf LT 7 THEN f = [f,replicate(0L,7-nf)]
  
  handle_value,info.int.a.result_h,result,/no_copy
  handle_value,info.int.a.const_h,const,/no_copy
  this_p_result = reform(result(*,f(1),f(2),f(3),f(4),f(5),f(6)))
  this_p_const = reform(const(*,f(1),f(2),f(3),f(4),f(5),f(6)))
  handle_value,info.int.a.result_h,result,/no_copy,/set
  handle_value,info.int.a.const_h,const,/no_copy,/set
  
  nres = n_elements(this_p_result)
  chi2 = this_p_result(nres-1)
  
  ;; Extract the fit from this point (original fit structure is preserved
  ;; by spice_xcfit_block_get_fit/set_fit calls)
  
  spice_xcfit_block_get_fit,info,lambda,spec,weights,ix,fit,failed
  
  restart = keyword_set(restart)
  recalculate = keyword_set(recalculate)
  
  ;; If chi2==missing, the point should be done (unless it's failed)

  IF is_not_missing(chi2, missing=info.int.a.missing) $
     AND NOT recalculate AND NOT restart THEN BEGIN
     
     spice_xcfit_block_set_fit,info,lambda,spec,weights,ix,fit,failed,/nochange
     return
  END
  
  fail_type = 0
  
  ;;
  ;; If chi2 is missing, or the recalculate flag is set, and if at least one
  ;; parameter is not constant, then calculate best fit
  ;;
  some_variable = total(this_p_const EQ 0b) NE 0
  recalculate = (recalculate OR is_missing(chi2, missing=info.int.a.missing)) $
     AND some_variable OR restart
  
  IF recalculate THEN BEGIN
     ;;
     ;; If it's the chi2 that's missing, we should start from initial values,
     ;; if it's the recalculate flag, we should start from current values.
     ;;
     ;; Unless, of course, the /restart flag is set...
     ;; 
     IF is_missing(chi2, missing=info.int.a.missing) THEN delvarx,start_aa $
     ELSE                               start_aa = this_p_result(0:nres-2)
     
     IF keyword_set(restart) THEN BEGIN
        delvarx,start_aa
        handle_value,info.int.a.fit_h,fit ;; copy
     END
     
     IF exist(start_aa) THEN print,"Starting from:",start_aa
     thisfit = cfit(lambda,spec,start_aa,fit,/double,weights=weights,$
                    fail_type=fail_type)
     failed = 0
  END ELSE BEGIN
     changed = info.int.changed
  END
  
  IF fail_type NE 0 AND fail_type NE 2 THEN BEGIN
     xack,['CFIT failed']
     failed = 1
  END
  
  spice_xcfit_block_set_fit,info,lambda,spec,weights,ix,fit,failed
  
  IF NOT recalculate THEN BEGIN
     info.int.changed = changed
  END
END

PRO spice_xcfit_block_sensitize,info,title
  IF title EQ 'Chi^2' THEN BEGIN
     widget_control,info.int.initval_id,sensitive=0
     FOR j = 0,n_elements(info.int.pix_reset1_id)-1 DO  $
        widget_control,info.int.pix_reset1_id(j),sensitive=0
  END ELSE BEGIN 
     widget_control,info.int.initval_id,sensitive=1
     FOR j = 0,n_elements(info.int.pix_reset1_id)-1 DO  $
        widget_control,info.int.pix_reset1_id(j),sensitive=1
  END
END


PRO spice_xcfit_block_adjustfit,info
  ;; The user clicked on the ADJUST button 
  
  ;; First of all, take note of the original global value (copy)
  handle_value,info.int.a.fit_h,orgfit
  
  ;; This one fills in the current values & const status at *this* point
  ;; 
  spice_xcfit_block_get_fit,info,lambda,spec,weights,ix,fit,failed
  spice_xcfit_block_set_fit,info,lambda,spec,weights,ix,fit,failed ;; *No* change
  
  ;; But we want the *global* values for the const/include..etc..
  ;; Allow editing - of the original fit, but with the data from this point
  
  xcfit,lambda,spec,orgfit,weights=weights,/no_change,failed=ignore_failed
  
  ;; Update status display
  widget_control,info.int.status1_id,set_value=orgfit
  
  handle_value,info.int.a.fit_h,orgfit,/set
  
  ;; Update component pulldown menu with any new names (and rebuild
  ;; CONST array if desired).
  
  spice_xcfit_block_register,info
  spice_xcfit_block_get_result,info,this_result,title
  widget_control,info.int.result_id,set_value={title:title}
  widget_control,info.int.initval_id,set_value=title
  spice_xcfit_block_sensitize,info,title
END



PRO spice_xcfit_block_alterfit,info
  spice_xcfit_block_get_fit,info,lambda,spec,weights,ix,fit
  orgfit = fit
  xcfit,lambda,spec,fit,weights=weights,/use_current_value,failed=failed
  handle_value,info.int.a.fit_h,fit,/set
  IF NOT match_struct(orgfit,fit) THEN BEGIN
     widget_control,/hourglass
     ;; Delete result/residual/const/include
     handle_value,info.int.a.result_h,result,/no_copy
     handle_value,info.int.a.residual_h,result,/no_copy
     handle_value,info.int.a.const_h,result,/no_copy
     handle_value,info.int.a.include_h,result,/no_copy
     info.ext.result_no = 0
     ;; Regenerate result/residual arrays
     spice_xcfit_block_register,info
     ;; Update global status display
     widget_control,info.int.status1_id,set_value=fit
     ;; Visit this point
     spice_xcfit_block_visitp,info
     ;; Extract new result "image" and show it
     spice_xcfit_block_get_result,info,this_result,title
     widget_control,info.int.result_id,set_value=this_result
     widget_control,info.int.result_id,set_value={title:title}
     widget_control,info.int.initval_id,set_value=title
     ;; Make residual display aware that a change has occurred
     widget_control,info.int.residual_id,set_value=info.int.a.residual_h
     spice_xcfit_block_sensitize,info,title
  END
END



PRO spice_xcfit_block_save_as,info
  break_file,info.int.a.filename,disk,dir,fnam,ext
  
  file = bigpickfile(/write,path=disk+dir,file=fnam+ext,$
                     group=info.int.top_id,$
                     filter='*.ana',get_path=path)
  
  IF file EQ '' THEN return
  
  break_file,file,disk,dir,fnam,ext
  info.int.a.filename = path+fnam+ext
  save_analysis,info.int.a,/verbose
  info.int.changed = 0b
END



PRO spice_xcfit_block_restore,info,other=other
  
  other = keyword_set(other) OR info.int.a.filename EQ ''
  
  IF NOT other THEN BEGIN
     info.int.a = restore_analysis(info.int.a)
  END ELSE BEGIN 
     break_file,info.int.a.filename,disk,dir,fnam,ext
     
     file = bigpickfile(/write,path=disk+dir,file=fnam+ext,$
                        group=info.int.top_id,$
                        filter='*.ana',get_path=path,/must_exist)
     
     IF file EQ '' THEN return
     break_file,file,disk,dir,fnam,ext
     new_ana = restore_analysis(path+fnam+ext)
     
     ;; Check for valid sizes
     handle_value,new_ana.data_h,new_data,/no_copy
     handle_value,info.int.a.data_h,data,/no_copy
     szd = size(data)
     szn = size(new_data)
     handle_value,new_ana.data_h,new_data,/set,/no_copy
     handle_value,info.int.a.data_h,data,/set,/no_copy
     
     IF total(szn EQ szd) NE n_elements(szd) THEN BEGIN
        xack,['Cannot change the dimensionality of the data' + $
              ' with a restore operation']
        delete_analysis,new_ana
        return 
     END
     
     ;; Copy contents of the new analysis into the existing info.int.a
     ;; 
     info.int.a = mk_analysis(source_analysis=new_ana,destination=info.int.a)
     delete_analysis,new_ana
     
     ;; We don't know how many results we have...
     info.ext.result_no = 0
  END
  
  ;; Update pulldown menus etc in case fit changed
  spice_xcfit_block_register,info
  spice_xcfit_block_visitp,info
  
  ;; Change flag, find first/next status
  info.int.changed = 0b
  info.int.find_ix = -1L
  handle_value,info.int.find_h,dummy,/no_copy
  
  ;; These things had better have the correct size....
  ;; 
  handle_value,info.int.a.origin_h,origin
  handle_value,info.int.a.scale_h,scale
  
  set_data_resid = {focus:info.ext.focus,$
                    origin:origin,$
                    scale :scale }
  
  ;; Data blocks have been passed through handles, so the data will
  ;; automatically be updated, but other stuff needs to be set to
  ;; alert about this..
  widget_control,info.int.data_id,set_value=set_data_resid
  widget_control,info.int.residual_id,set_value=set_data_resid
  
  spice_xcfit_block_get_result,info,this_result,title
  
  set_result = {focus:info.ext.focus(1:*),$
                origin:origin(1:*),$
                scale:scale(1:*),$
                title:title}
  
  widget_control,info.int.result_id,set_value=this_result
  widget_control,info.int.result_id,set_value=set_result
  widget_control,info.int.initval_id,set_value=title
  spice_xcfit_block_sensitize,info,title
END


PRO spice_xcfit_block_findspot,info,what_to_find
  handle_value,info.int.a.result_h,result,/no_copy
  
  handle_value,info.int.find_h,ix,/no_copy
  
  IF info.int.what_found NE what_to_find $
     OR NOT exist(ix) THEN info.int.find_ix = -1L
  
  info.int.what_found = what_to_find
  
  thisresult = result(info.ext.result_no,*,*,*,*,*,*)
  
  IF what_to_find EQ 'MAX' OR what_to_find EQ 'MIN' THEN BEGIN
     missix = where_missing(thisresult, missing=info.int.a.missing)
     IF missix(0) NE -1L THEN BEGIN
        mini = min(thisresult,max=maxi)
        IF what_to_find EQ 'MAX' THEN thisresult(missix) = mini $
        ELSE                          thisresult(missix) = maxi
     END
  END
  
  IF info.int.find_ix EQ -1L THEN BEGIN 
     CASE what_to_find OF 
        'ZERO': ix = where(thisresult EQ 0)
        'MISS': ix = where_missing(thisresult, missing=info.int.a.missing)
        'MAX': ix = reverse(sort(thisresult))
        'MIN': ix = sort(thisresult)
     END
  END
  
  info.int.find_ix = info.int.find_ix+1
  
  IF ix(0) EQ -1L OR info.int.find_ix GE n_elements(ix) THEN BEGIN
     IF ix(0) EQ -1L THEN xack,["None found"] $
     ELSE                 xack,["No more points found"]
     info.int.find_ix = -1L
  END ELSE BEGIN 
     
     info.ext.focus = ndim_indices(thisresult,ix(info.int.find_ix))

     widget_control,info.int.data_id,set_value={focus:info.ext.focus}
     widget_control,info.int.result_id,set_value={focus:info.ext.focus(1:*)}
     widget_control,info.int.residual_id,set_value={focus:info.ext.focus}
  END
  
  handle_value,info.int.a.result_h,result,/set,/no_copy
  handle_value,info.int.find_h,ix,/set,/no_copy
  
  spice_xcfit_block_visitp,info
END


PRO spice_xcfit_block_set_initial,info,average=average_flag
  
  spice_xcfit_block_get_result,info,this_result
  handle_value,info.int.a.fit_h,globfit,/no_copy
  
  handle_value,info.int.a.result_h,res,/no_copy
  chi2 = res((size(res))(1)-1,*,*,*,*,*,*)
  handle_value,info.int.a.result_h,res,/set,/no_copy
  
  handle_value,info.int.a.const_h,const,/no_copy
  cons = const(info.ext.result_no,*,*,*,*,*,*)
  handle_value,info.int.a.const_h,const,/set,/no_copy
  
  ;; Find invalid points
  ix = where(chi2 EQ 0.0 OR is_missing(chi2, missing=info.int.a.missing) OR cons)
  
  ;; Set this result for those points to MISSING
  IF ix(0) NE -1L THEN this_result(ix) = info.int.a.missing
  
  ;; Now calculate average/median for good points
  
  IF keyword_set(average_flag) THEN BEGIN
     new_init = average(this_result,missing=info.int.a.missing)
  END ELSE BEGIN
     ix = where_not_missing(this_result, missing=info.int.a.missing)
     IF ix(0) NE -1 THEN new_init = median(this_result(ix)) $
     ELSE BEGIN
        xack,['No non-missing points! - No new initial value set']
        handle_value,info.int.a.fit_h,globfit,/no_copy
     END
  END
  
  sfit = make_sfit_stc(globfit) ;; Will use initial values by default
  a_nom = sfit.a_nom
  
  a_nom(info.ext.result_no) = new_init
  
  update_cfit,globfit,a_nom,/initial
  handle_value,info.int.a.fit_h,globfit,/set,/no_copy
END

PRO spice_xcfit_block_event,ev
  widget_control,/hourglass
  widget_control,ev.top,get_uvalue=info,/no_copy
  widget_control,ev.id,get_uvalue=uvalue
  if tag_names(ev, /Structure_name) eq 'CW_LOADCT_NEW_CT' then begin
    cw_cubeview_force_redraw, info.int.data_id
    cw_cubeview_force_redraw, info.int.residual_id
    cw_cubeview_force_redraw, info.int.result_id
    widget_control,ev.top,set_uvalue=info,/no_copy
    return
  endif

  uvalue = str_sep(uvalue,':')
  evtype = tag_names(ev,/structure_name)
  
  mark = n_elements(uvalue) GT 1

  CASE uvalue(0) OF
  'EXIT':BEGIN
     handle_value,info.int.store_info_h,info,/set,/no_copy
     widget_control,ev.top,/destroy
     return
     ENDCASE
     
  'SAVE':BEGIN
     IF mark THEN spice_xcfit_block_save_as,info $
     ELSE BEGIN 
        save_analysis,info.int.a
        info.int.changed = 0b
     END 
     ENDCASE 
     
  'RESTORE':BEGIN
     spice_xcfit_block_restore,info,other = mark
     ENDCASE
     
  'EDIT_HISTORY':BEGIN
     handle_value,info.int.a.history_h,history
     xtextedit,history,group=ev.top
     handle_value,info.int.a.history_h,history,/set
     ENDCASE
     
;
; Events from the display draw windows.
;
  'DATA':BEGIN
     IF total([info.ext.focus NE ev.focus]) GT 0 THEN BEGIN 
        info.ext.focus = ev.focus
        widget_control,info.int.residual_id,set_value={focus:ev.focus}
        widget_control,info.int.result_id,set_value={focus:ev.focus(1:*)}
        spice_xcfit_block_visitp,info
     END
     ENDCASE
     
  'RESIDUAL':BEGIN
     IF total([info.ext.focus NE ev.focus]) GT 0 THEN BEGIN 
        info.ext.focus = ev.focus
        widget_control,info.int.result_id,set_value={focus:ev.focus(1:*)}
        widget_control,info.int.data_id,set_value={focus:ev.focus}
        spice_xcfit_block_visitp,info
     END
     ENDCASE
     
  'RESULT':BEGIN
     IF total([info.ext.focus(1:*) NE ev.focus]) GT 0 THEN BEGIN 
        info.ext.focus(1:*) = ev.focus
        widget_control,info.int.data_id,set_value={focus:info.ext.focus}
        widget_control,info.int.residual_id,set_value={focus:info.ext.focus}
        spice_xcfit_block_visitp,info
     END
     ENDCASE
;
;
;
  'FIND':BEGIN
     ;; Restart find operation
     handle_value,info.int.find_h,dummy,/no_copy
     spice_xcfit_block_findspot,info,uvalue(1)
     ENDCASE
     
  'FIND_AGAIN':BEGIN
     spice_xcfit_block_findspot,info,info.int.what_found
     ENDCASE
     
  'RESULT#':BEGIN
     info.ext.result_no = fix(uvalue(1))
     spice_xcfit_block_get_result,info,this_result,title
     widget_control,info.int.result_id,set_value=this_result
     widget_control,info.int.result_id,set_value={title:title}
     widget_control,info.int.initval_id,set_value=title
     spice_xcfit_block_sensitize,info,title
     handle_value,info.int.find_h,dummy,/no_copy
     ENDCASE
     
  'STATUS1':BEGIN
     ;; Update include/const status for one component (global value)
     handle_value,info.int.a.fit_h,orgfit
     update_cfit,orgfit,const=ev.const,include=ev.include
     widget_control,info.int.status1_id,set_value=orgfit
     handle_value,info.int.a.fit_h,orgfit,/set,/no_copy
     ENDCASE 
     
  'STATUS2':BEGIN
     ;; Update include/const status for one component (local value)
     spice_xcfit_block_get_fit,info,lambda,spec,weights,ix,fit,failed
     sfit0 = make_sfit_stc(fit,/values)
     update_cfit,fit,const=ev.const,include=ev.include
     sfit1 = make_sfit_stc(fit)
     ;; We should freeze (at the *initial* value) these points:
     to_freeze = sfit1.const AND NOT sfit0.const
     freezix = where(to_freeze)
     IF freezix(0) NE -1L THEN BEGIN
        sfit0.a_nom(freezix) = sfit1.a_nom(freezix) ;; sfit1 has initial values
        update_cfit,fit,sfit0.a_nom
     END 
     spice_xcfit_block_set_fit,info,lambda,spec,weights,ix,fit,failed
     IF 1 THEN spice_xcfit_block_visitp,info,/recalculate
     ENDCASE 
     
  'MICROPLOT':BEGIN
     ;; Acknowledge event - replot
     widget_control,ev.id,set_value=ev.set
     
     ;; Overplot
     handle_value,info.int.microfine_h,microfine
     IF exist(microfine) THEN oplot,microfine(*,0),microfine(*,1)
     handle_value,info.int.errplot_h,errp
     IF exist(errp) AND info.ext.plot_err THEN $
        oploterr,errp.x,errp.y,errp.err,max_value=min(errp.y)-1
     ENDCASE
     
  'ERRPLOT':BEGIN
     info.ext.plot_err = (uvalue(1) EQ 'ON')
     
     ;; Replot microplot
     widget_control,info.int.microplot_id,set_value={replot:1}
     
     ;; Overplot
     handle_value,info.int.microfine_h,microfine
     IF exist(microfine) THEN oplot,microfine(*,0),microfine(*,1)
     handle_value,info.int.errplot_h,errp
     IF exist(errp) AND info.ext.plot_err THEN $
        oploterr,errp.x,errp.y,errp.err,max_value=min(errp.y)-1
     ENDCASE
     
  
  'FAILFIT':BEGIN
     handle_value,info.int.a.fit_h,orgfit
     spice_xcfit_block_get_fit,info,lambda,spec,weights,ix,fit
     spice_xcfit_block_set_fit,info,lambda,spec,weights,ix,fit,1
     handle_value,info.int.a.fit_h,orgfit,/set,/no_copy
     spice_xcfit_block_visitp,info
     ENDCASE
     
  'REFIT':BEGIN
     spice_xcfit_block_visitp,info,/recalculate,/restart
     ENDCASE
     
  'VIEWFIT':BEGIN
     handle_value,info.int.a.fit_h,orgfit
     spice_xcfit_block_get_fit,info,lambda,spec,weights,ix,fit
     currentfit = fit
     xcfit,lambda,spec,fit,weights=weights,/use_current_value,/no_change,$
        failed=failed
     IF NOT match_struct(currentfit,fit) OR failed THEN $
        spice_xcfit_block_set_fit,info,lambda,spec,weights,ix,fit,failed
     handle_value,info.int.a.fit_h,orgfit,/set,/no_copy
     ENDCASE
     
     ;; This is the "Adjust" button 
  'ADJUSTFIT':BEGIN
     spice_xcfit_block_adjustfit,info
     ENDCASE
     
     ;; Set initial value of result to the current median or average
  'SET_INITIAL':BEGIN 
     spice_xcfit_block_set_initial,info,average = mark
     ENDCASE
     
;
;
;
  'ALTERFIT':BEGIN
     spice_xcfit_block_alterfit,info
     ENDCASE 
     
  'RECALCULATE':BEGIN
     IF mark THEN BEGIN 
        ;; Delete result/residual/const/include when starting from scratch
        handle_value,info.int.a.result_h,result,/no_copy
        handle_value,info.int.a.residual_h,result,/no_copy
        handle_value,info.int.a.const_h,result,/no_copy
        handle_value,info.int.a.include_h,result,/no_copy
        spice_xcfit_block_register,info
     END 
     spice_xcfit_block_calculate,info
     ENDCASE
;
; Mask/modify options
;
  'PIX_EDIT':BEGIN
     spice_xcfit_block_pix_edit,info
     spice_xcfit_block_pix_getmask,info,/recalculate
     spice_xcfit_block_pix_flicker,info
     ENDCASE
     
  'PIX_EXECUTE':BEGIN
     spice_xcfit_block_pix_getmask,info,/recalculate
     spice_xcfit_block_pix_flicker,info
     ENDCASE
     
  'PIX_FLICKER':BEGIN
     spice_xcfit_block_pix_flicker,info
     ENDCASE
     
  'PIX_SETCONST':BEGIN
     spice_xcfit_block_pix_setconst,info,one = mark
     ENDCASE
     
  'PIX_SETINCLUDE':BEGIN
     spice_xcfit_block_pix_setinclude,info,one = mark
     ENDCASE
     
  'PIX_RESET':BEGIN 
     spice_xcfit_block_pix_reset,info,one = mark
     ENDCASE
     
  'PIX_APPLY_ALL':BEGIN
     spice_xcfit_block_pix_apply_all,info,one = mark
     ENDCASE
     
  'PIX_RECALC':BEGIN
     spice_xcfit_block_pix_recalc,info
     ENDCASE
     
  'PIX_FAIL':BEGIN
     spice_xcfit_block_pix_fail,info,restore=mark
     ENDCASE
     
  END
  
  widget_control,ev.top,set_uvalue=info,/no_copy
END


PRO spice_xcfit_block,lambda,data,weights,fit,missing,result,residual,include,const,$
                origin=origin,scale=scale,phys_scale=phys_scale,$
                analysis=ana, title=title
  
  on_error,2
  
  IF !debug NE 0 THEN on_error,0
  
  ;; Internally, the data blocks will be stored as parts of an analysis
  ;; structure. If the data blocks are supplied individually, an internal
  ;; analysis structure must be made.
  
  IF keyword_set(ana) THEN iana = ana $
  ELSE iana = mk_analysis()
  
  IF keyword_set(ana) THEN BEGIN
     
     ;; Get out data blocks as if they were supplied individually as
     ;; parameters, for type checking etc.
     
     handle_value,ana.lambda_h,lambda,/no_copy
     handle_value,ana.data_h,data,/no_copy
     handle_value,ana.weights_h,weights,/no_copy
     handle_value,ana.fit_h,fit
     missing = ana.missing
     handle_value,ana.result_h,result,/no_copy
     handle_value,ana.residual_h,residual,/no_copy
     handle_value,ana.include_h,include,/no_copy
     handle_value,ana.const_h,const,/no_copy
     
     handle_value,ana.origin_h,origin
     handle_value,ana.scale_h,scale
     handle_value,ana.phys_scale_h,phys_scale
     handle_value,ana.dimnames_h,dimnames
     
;     catch,error
     error = 0
     IF error NE 0 THEN BEGIN
        catch,/cancel
        print,!err_string
        print,"Caught error, putting back data blocks"
        handle_value,ana.lambda_h,lambda,/set,/no_copy
        handle_value,ana.data_h,data,/set,/no_copy
        handle_value,ana.weights_h,weights,/set,/no_copy
        handle_value,ana.result_h,result,/set,/no_copy
        handle_value,ana.residual_h,residual,/set,/no_copy
        handle_value,ana.include_h,include,/set,/no_copy
        handle_value,ana.const_h,const,/set,/no_copy
        message,"Stopping"
     END
     
  END ELSE BEGIN
     
     IF n_params() LT 7 THEN BEGIN 
        message,"Use: XCFIT_BLOCK,LAMBDA,DATA,WEIGHTS,FIT,MISSING," + $
           "RESULT,RESIDUAL [,INCLUDE,CONST]"
     END
     
  END

  IF N_ELEMENTS(title) EQ 0 THEN title=''
  IF NOT exist(fit) THEN fit = {bg:mk_comp_poly([median(data)])}
  
  szd = size(data)
  szl = size(lambda)
  
  parcheck,data,   2,typ(/rea),[2,3,4,5,6,7] ,"DATA"
  parcheck,lambda, 1,typ(/rea),[1,szd(0)],    "LAMBDA"
  parcheck,weights,3,typ(/rea),[szd(0)],      "WEIGHTS"
  parcheck,fit,    4,typ(/stc),1,             "FIT"
  parcheck,missing,5,typ(/rea),0,             "MISSING"
  
  ;; Make sure we're not taking things for granted here (Thanks to
  ;; Anja Czaykowska)
  iana.missing = missing 
  
  default,dimnames,(['Lambda','X','Y','T','A','B','C'])(0:szd(0)-1)
  
  result_no = 0
  
  IF szl(0) EQ szd(0) AND total(szl NE szd) NE 0 $
     OR szl(1) NE szd(1) THEN BEGIN
     message,"LAMBDA and DATA have incompatible sizes"
  END
  
  focus = szd(1:szd(0))/2
  
  ext = { result_no : result_no,$
          plot_err : 1b,$
          focus : focus}
  
  sml = {xpad:1,ypad:1,space:1}
  
  base = widget_base(/row,title='SPICE_XCFIT_BLOCK '+title,_extra=sml)
  
  leftside_col = widget_base(base,/column,_extra=sml)
  center_col = widget_base(base,/column,_extra=sml)
  rightside_col = widget_base(base,/column,_extra=sml)
  
  titles_h = handle_create()
  handle_killer_hookup,titles_h,group_leader=base
  
  IF NOT keyword_set(ana) THEN BEGIN
     h_to_kill = [iana.lambda_h,iana.data_h,iana.weights_h,iana.fit_h,$
                  iana.result_h,iana.residual_h,iana.include_h,iana.const_h]
  END
  
  int = { top_id       : base,$
          a            : iana,$
          status1_id    : 0L,$
          status2_id    : 0L,$
          microplot_id : 0L,$
          microfine_h  : handle_create(),$ 
          errplot_h    : handle_create(),$
          changed      : 0b,$                    ;; Change flag
          find_ix      : -1L,$                   ;; Find first/next status
          find_h       : handle_create(),$
          pix_id       : 0L,$
          pix_reset1_id: lonarr(4),$
          pix_prog_h   : handle_create(value=spice_xcfit_block_pix_defprog()),$
          pix_mask_h   : handle_create(),$
          what_found   : 'ZERO',$
          titles_h     : titles_h,$
          store_info_h : handle_create(),$
          data_id      : 0L,$
          residual_id  : 0L,$
          result_pdb   : 0L,$
          initval_id   : 0L,$
          result_id    : 0L}
  
  handle_killer_hookup,int.store_info_h   ;; Note: Don't kill when base dies
  
  handle_killer_hookup,group_leader=base,$
     [int.microfine_h,int.find_h,int.pix_prog_h,int.errplot_h,$
      int.pix_mask_h]
  
  info = { int:int,$
           ext:ext }
  
  upper = widget_base(center_col,/row,_extra=sml)
  
  ;; Switched to make microplot go *left*
  
  upper_right_c = widget_base(upper,/column,_extra=sml)
  upper_left_c = widget_base(upper,/column,_extra=sml)
  
  microplot_base = widget_base(upper_right_c)
  
  buttons_n_colors_r = widget_base(upper_left_c,/row,_extra=sml)
  
  buttons_col = widget_base(buttons_n_colors_r,/column,_extra=sml)
  
  ;; Color table selector: CW_LOADCT
  color_selector = widget_base(buttons_n_colors_r, /row, _extra=sml)
  colors = cw_loadct(color_selector,/frame)

  buttons1 = widget_base(buttons_col,/row,_extra=sml)
  buttons2 = widget_base(buttons_col,/row,_extra=sml)
  buttons3 = widget_base(buttons_col,/row,_extra=sml)
  buttons4 = widget_base(upper_left_c,/row,_extra=sml,/frame) ;; Note base!
  
  disp_b = widget_base(center_col,/row,_extra=sml)
  
  ;; Local/Global status
  
  sta = widget_base(leftside_col,/row,_extra=sml)
  gstatus = widget_base(sta,/column,_extra=sml,frame = 0)
  lstatus = widget_base(sta,/column,_extra=sml,frame = 0)
  
  ;;lstatus = widget_base(lefttside_col,/column,_extra=sml,/frame)
  ;;gstatus = widget_base(rightside_col,/column,_extra=sml,/frame)
  
  
  label1 = widget_label(widget_base(lstatus),value='Local') 
  label2 = widget_label(widget_base(lstatus),value='status')
  
  label1 = widget_label(widget_base(gstatus),value='Global') 
  label2 = widget_label(widget_base(gstatus),value='status')
  
  xsize = 35
  lstatusx = widget_base(lstatus,/column,xpad=1,ypad=1,space=5,ysize=6000,$
                         x_scroll_size=xsize,y_scroll_size=750)
  
  gstatusx = widget_base(gstatus,/column,xpad=1,ypad=1,space=5,ysize=6000,$
                         x_scroll_size=xsize,y_scroll_size=750)
  
  ;; Switched - makes local left, global right
  status2 = lstatusx ;; widget_base(lstatusx,/column,_extra=sml,/frame)
  status1 = gstatusx ;; widget_base(gstatusx,/column,_extra=sml,/frame)
  
  ;; File menu
  ;;
  file_m = widget_button(buttons1,value='File/exit',menu=2)
  save_b = widget_button(file_m,value='Save',uvalue='SAVE')
  save_q = widget_button(file_m,value='Save as..',uvalue='SAVE:AS')
  restore_last = widget_button(file_m,value='Restore last saved',$
                               uvalue='RESTORE')
  restore_other = widget_button(file_m,value='Restore other',$
                                uvalue='RESTORE:OTHER')
  edit_hist = widget_button(file_m,value='View/edit History',$
                            uvalue='EDIT_HISTORY')
  dummy = widget_button(file_m,value='Exit',uvalue='EXIT')
  
  ;;
  ;; Adjust, Redesign, Calculate buttons (Global action line)
  ;;
  
  ;; Adjust fit, including update of initial values
  ;;
  adjust = widget_button(buttons1,value='Adjust',menu=2)
  dummy = widget_button(adjust, uvalue='ADJUSTFIT', $
                        value='Adjust (global) MIN/MAX values, names etc')
  initval = widget_button(adjust,value='Update (global) initial value for ', $
                          menu=2)
  initval_id = widget_button(initval,value=' ',menu=2)
  info.int.initval_id = initval_id
  dummy = widget_button(initval_id,value='Use *median* of free result',$
                        uvalue='SET_INITIAL')
  dummy = widget_button(initval_id,value='Use *average* of free result',$
                        uvalue='SET_INITIAL:AVERAGE')
  
  ;; Redesign (discard)
  ;;
  dummy = widget_button(buttons1,value='Redesign',menu=2)
  dummy = widget_button(dummy,$
                        value='Discard all results, redesign fit structure',$
                        uvalue='ALTERFIT')
  
  ;; Calculate (from current or scratch)
  ;;
  dummy = widget_button(buttons1,value='Calculate',menu=2)
  dummy2 = widget_button(dummy,value='Recalculate based on current result',$
                         uvalue='RECALCULATE')
  dummy2 = widget_button(dummy,value='Recalculate from global initial values',$
                         uvalue='RECALCULATE:SCRATCH')
  
  
  ;;
  ;; Second row - Find-buttons and Mask/modify
  ;;
  find_base = buttons2 ;; widget_base(buttons3,/row,_extra=sml,/frame)
  fmenu = [{pselect_s,btext:'zero',mtext:'Find zero',uvalue:'FIND:ZERO',$
            flags:0},$
           {pselect_s,'missing','Find missing','FIND:MISS',0},$
           {pselect_s,'max','Find max value','FIND:MAX',0},$
           {pselect_s,'min','Find min value','FIND:MIN',0}]
  dummy = cw_pselect(find_base,'Find: ',fmenu)
  find_again = widget_button(widget_base(find_base,/column),$
                             value='..next',uvalue='FIND_AGAIN')
  
  ;;
  ;; Pixel grabbing/manipulation
  ;;
  gbase = widget_base(buttons2)
  
  ;; This is the base to which the program text is sent for testing.
  ;; It needs the uvalue to point to the top base (to get at the info stc).
  
  info.int.pix_id = widget_base(gbase,pro_set_value=$
                                 'spice_xcfit_block_pix_edit_setv')
  widget_control,info.int.pix_id,set_uvalue=base
  
  ;;
  ;; This is the pixel grabbing/manipulation menu
  ;;
  pix = widget_button(gbase,value='Mask/patch points',menu=2)
  flick = widget_button(pix,value='Edit masking program',$
                        uvalue='PIX_EDIT')
  grab = widget_button(pix,value='Re-execute masking program',$
                       uvalue='PIX_EXECUTE')
  zhonk = widget_button(pix,value='Show masked points',$
                        uvalue='PIX_FLICKER')
  sub1 = widget_button(pix,value='Patch masked points',menu=2)
  
  all = '..ALL parameters'
  one = '..THIS parameter'
  allc = '..ALL components'
  onec = '..THIS component'
  mark = ':ONE'
  oni = 0L
  
  v = ['Patch CONST status from global status','PIX_SETCONST']
  zhonk = widget_button(sub1,value=v(0),menu=2)
  ali = widget_button(zhonk,value=all,uvalue=v(1))
  oni = [oni,widget_button(zhonk,value=one,uvalue=v(1)+mark)]
  
  v = ['Patch INCLUDE status from global status','PIX_SETINCLUDE']
  zhonk = widget_button(sub1,value=v(0),menu=2)
  ali = widget_button(zhonk,value=allc,uvalue=v(1))
  oni = [oni,widget_button(zhonk,value=onec,uvalue=v(1)+mark)]
  
  v = ['Patch RESULT from global initial value','PIX_RESET']
  zhonk = widget_button(sub1,value=v(0),menu=2)
  ali = widget_button(zhonk,value=all,uvalue=v(1))
  oni = [oni,widget_button(zhonk,value=one,uvalue=v(1)+mark)]
  
  zhonk = widget_button(sub1,value='Recalc. masked points ' + $
                        '(from curr. values)',uvalue='PIX_RECALC')
  
  v = ['Patch all from global status, then recalc.','PIX_APPLY_ALL']
  zhonk = widget_button(sub1,value=v(0),menu=2)
  ali = widget_button(zhonk,value=all,uvalue=v(1))
  oni = [oni,widget_button(zhonk,value=one,uvalue=v(1)+mark)]
  
  zhonk = widget_button(sub1,value='Fail masked points',uvalue='PIX_FAIL')
  zhonk = widget_button(sub1,value='UNFail masked points',uvalue='PIX_FAIL:0')
  
  info.int.pix_reset1_id = oni(1:*)
  
  onoff = ["ON","OFF"]
  
  ;; Second row of buttons (Find-buttons,View/tweak,Refit,Fail)
  ;;
  viewtweak = buttons3 ;; widget_base(buttons3,/column,_extra=sml)
  dummy = cw_flipswitch(viewtweak,value='Errplot:'+onoff,$
                        uvalue='ERRPLOT:'+onoff)
  dummy = cw_flipswitch(viewtweak,value='View/tweak',uvalue='VIEWFIT')
  dummy = cw_flipswitch(viewtweak,value='Redo fit',uvalue='REFIT')
  dummy = cw_flipswitch(viewtweak,value='FAIL',uvalue='FAILFIT')
  
  
  
  ;;
  ;; Third row - pulldown menu for displayed result 
  ;;
  result_pdb = widget_base(buttons4,_extra=sml)
  ;; 
  
  ;; const/include status (global value)
  ;;
  info.int.status1_id = cwf_status(status1,value=fit,uvalue='STATUS1',/column)
  ;; const/include status (current point)
  ;;
  info.int.status2_id = cwf_status(status2,value=fit,uvalue='STATUS2',/column)
  
  
  ;;
  ;; Micro-plot..
  ;;
  mx = 195 & my = 160
  microplot_id = cw_plotz(microplot_base,uvalue='MICROPLOT',$
                          xwsize=mx,ywsize=my,xdsize=mx,ydsize=my, $
                          origo=[0,0],psym=10)
;  microplot_id = cw_plotz(widget_base(upper_right_c),uvalue='MICROPLOT',$
;                          xwsize=mx,ywsize=my,xdsize=mx,ydsize=my, $
;                          origo=[0,0],psym=10)
  info.int.microplot_id = microplot_id
  
  data_b = widget_base(disp_b,/column,_extra=sml)
  result_b = widget_base(disp_b,/column,_extra=sml)
  residual_b = widget_base(disp_b,/column,_extra=sml)
  
  info.int.result_pdb = result_pdb
  
  no_copy = 0
  
  ;; Put data blocks into their handles
  
  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const,$
     /set,/copy

  spice_xcfit_block_register,info
  spice_xcfit_block_get_result,info,this_result,title
  
;  spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const
  
  info.int.data_id = cw_cubeview(data_b,hvalue=info.int.a.data_h,$
                                 missing=missing,$
                                 uvalue="DATA",dimnames=dimnames,$
                                 title='Original data',origin=origin, $
                                 scale=scale,phys_scale=phys_scale)
  
  info.int.residual_id = cw_cubeview(residual_b,hvalue=info.int.a.residual_h,$
                                     missing=missing,$
                                     uvalue="RESIDUAL",dimnames=dimnames,$
                                     title='Residual',origin=origin, $
                                     scale=scale,phys_scale=phys_scale)
  
  IF keyword_set(origin) THEN r_origin = origin(1:*)
  IF keyword_set(scale) THEN r_scale = scale(1:*)
  IF keyword_set(phys_scale) THEN r_phys_scale = phys_scale(1:*)
  
  info.int.result_id = cw_cubeview(result_b,value=this_result,$
                                   missing=missing,$
                                   uvalue="RESULT",dimnames=dimnames(1:*),$
                                   title=title, origin=r_origin, $
                                   scale=r_scale,phys_scale=r_phys_scale)
  
  widget_control,info.int.initval_id,set_value=title
  spice_xcfit_block_sensitize,info,title
  
  widget_control,base,/realize
  
  spice_xcfit_block_visitp,info
  
  widget_control,base,set_uvalue=info
  
  xmanager,"spice_xcfit_block",base,/modal
  
  ;; Make sure changes (like RESTORE operations) are reflected.
  
  handle_value,info.int.store_info_h,info
  handle_free,info.int.store_info_h
  
  IF NOT keyword_set(ana) THEN BEGIN
     spice_xcfit_block_gs,info,lambda,data,weights,fit,result,residual,include,const
     
     FOR h = 0,n_elements(h_to_kill)-1 DO handle_free,h_to_kill(h)
  END ELSE ana = info.int.a
  
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   ana = restore_analysis("$HOME/idl/solo-spice-ql/test_data/eis_l1_20210806_105401_0.ana")
   spice_xcfit_block, ana=ana
END

END
