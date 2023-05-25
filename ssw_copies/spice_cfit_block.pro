;+
; Project     : SOHO - CDS     
;                   
; Name        : CFIT_BLOCK
;               
; Purpose     : Apply a component fit to a block of data.
;               
; Explanation : Given a block of spectra in an array with dimensions
;               (LAMBDA,X,Y,..), with corresponding arrays containing the
;               wavelengths, fitting weights etc, this routine applies a
;               component fit to all the individual spectra, yielding a
;               resulting array (PARAMETERS,X,Y,...).
;
;               The first dimension of the result will accommodate all
;               parameters (stored consecutively as they appear in the fit
;               structure) and the chi^2 value of the fit at that point, i.e,
;               the first dimension will have N_PARAMETERS + 1 elements.
;
;               The input data array may have up to 7 dimensions.
;
;               It is possible to supply a "suggestion" result array, whose
;               parameter values will be used as initial values for the fit at
;               each point, unless the keyword USE_RESULT is explicitly set to
;               zero. The RESULT array should have the same size on input as
;               it will have on output.
;
;               It is also possible to supply an array controlling where
;               specific components should be turned "off" (INCLUDE), as well
;               as an array controlling where specific parameters are to be
;               kept constant (CONST). The first dimension of these arrays
;               should be of size N_COMPONENTS and N_PARAMETERS, respectively.
;               
; Use         : CFIT_BLOCK, ANALYSIS = ANALYSIS_STRUCTURE
;
;               or
;
;               CFIT_BLOCK,LAM,DA,WT,CFIT,MISS,RESULT,RESIDUAL [,INCLUDE,CONST]
;    
; Inputs      : Either
;
;     ANALYSIS_STRUCTURE : A structure containing handles pointing to the data
;                          associated with a data block analysis (all of the
;                          below mentioned stuff, incorporated into one
;                          structure)
;
;     or ALL of the following:
;     
;               LAM : Array containing the wavelength (or similar) for each
;                     data point. If the wavelength calibration does not vary
;                     from point to point, a one-dimensional array with the
;                     same size as the first dimension in DA may be used.
;
;               DA : Data Array, containing the counts/fluxes to be fitted.
;
;               WT : The weights of each data point.
;
;               CFIT : Component Fit structure, describing the function to be
;                      fitted.
;
;               MISS : The value used for missing pixels. May be undefined, in
;                      which case it will default to MIN(DA)-1
;
;               RESULT: (input and output) The values for the fitted
;                       parameters and the chi^2 values of the fits, for each
;                       data point (X,Y,...). If the CFIT has NP parameters,
;                       the first dimension of the RESULT will have NP+1
;                       elements. If defined on input, RESULT is taken as the
;                       initial values for the fit, unless USE_RESULT is
;                       explicitly set to zero.
;
;                       If the fit fails to converge at any point, the
;                       corresponding chi^2 value will be set to zero.
;
; Opt. Inputs : INCLUDE : An array describing where components should be
;                         included (or not!) in the fitted function. The first
;                         dimension should have the same number of elements as
;                         the number of components in the CFIT structure, and
;                         the other dimensions should have the same sizes as
;                         the input array. A zero in the data array means that
;                         the corresponding component is excluded at that
;                         point.
;
;               CONST : An array describing where parameters should be kept
;                       constant. The first dimension should have the same
;                       number of elements as the number of parameters in the
;                       CFIT structure, and the other dimensions should have
;                       the same sizes as the input array. A nonzero entry in
;                       this array means that the corresponding parameter is
;                       to be kept constant at that point.
;
; Outputs     : RESIDUAL : The residual after subtracting the fitted function
;                          from the data array. Has identical dimensions as
;                          the data array.
;               
; Opt. Outputs: SIGMA : An array with the numerically determined 1-sigma
;                       values for each parameter. This is only calculated if
;                       the keyword MAKE_SIGMA is set.
;
;                       NOTE! Numerically determined sigma levels for
;                       parameters depend on several crucial assumptions to be
;                       valid - the WEIGHTS should be *correct*, and the
;                       fitted function should be *linear* in the
;                       parameters.
;
;                       Of course, the fitted function *isn't* linear, but as
;                       long as it can be linearized in a neighbourhood around
;                       the result that is significantly *larger* than the
;                       true 1-sigma region, that's OK. So - for poor signal
;                       to noise ratios (where the errors are usually large)
;                       the SIGMA values are probably *not* correct.
;               
; Keywords    : SMART : Set SMART=1 to sort the data points according to total
;                       intensity, and processes them in descending order,
;                       using the previous result as the starting point for
;                       each new fit (will revert to the "default" set of
;                       initial values if a failure results from this
;                       approach). Saves some time in the initial fit (order
;                       10-20%) on some data sets.
;
;                       Set SMART=2 to recalculate ONLY those points where the
;                       initial result entry for chi^2 is ZERO - this assumes,
;                       of course, that the initial result was supplied, as
;                       well as an appropriate *residual* array.
;                       
;               DOUBLE : Set to force double precision fit calculation. Highly
;                        recommended.
;
;               MAKE_SIGMA : Set this to produce the 1-sigma values (SIGMA).
;
;               ERROR_ONLY : Set this to leave parameter values as they are,
;                            but produce a sigma estimate.
;
;               QUIET : Set to suppress messages from MCURVEFIT about failed
;                       attempts.
;                       
;               PCT_SLIDER_ID : Set to the ID of a slider with MAX=100, MIN=0
;                               to make it reflect the progress of the
;                               calculation.
;
;               X_FACE : Set this keyword to make CFIT_BLOCK create a widget
;                        showing the progress of the calculation, as well as
;                        providing a button to halt the process.
;
;               USE_RESULT : Set to zero to avoid using any supplied RESULT as
;                            initial values for the fitting. Alternatively you
;                            may of course use DELVARX,RESULT before the call.
;
;               FILL_ONLY : Set to skip calculations, to just fill out any
;                           non-existing arrays.
;
; Calls       : cfit(), default, dimrebin(), dimreform(), make_sfit_stc(),
;               parcheck, trim(), typ(), widget_base(), xkill, xrealize
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
; Modified    : Version 2, SVHH, 28 January 1997
;                       Fixed bug in calculating inital value array when not
;                       supplied.
;               Version 3, SVHH, 4 February 1997
;                       Added /QUIET flag.
;               Version 4, SVHH, 17 September 1997
;                       Added /SMART, /MAKE_SIGMA, SIGMA and /ERROR_ONLY
;                       keywords, and the ANALYSIS=ANALYSIS_STC calling option.
;               Version 5, SVHH, 6 May 1998
;                       Added /FILL_ONLY
;               Version 6, WTT, 8-Oct-2015, use [] for array indices
;
; $Id: 2023-05-25 13:19 CEST $
;-            

PRO spice_cfit_block_point,lambda,data,weights,fit,missing,$
                     result,residual,include,const,$
                     j,k,l,m,n,o,lam,new_lam,npar,sfit,$
                     double=double,quiet=quiet,$
                     sigma=sigm,make_sigma=make_sigma,$
                     error_only=error_only,$
                     restart=restart
  
  nvary = total(const[*,j,k,l,m,n,o] EQ 0)
  IF nvary GT 0 THEN BEGIN
     ;;
     ;; Extract spectrum and discard bad points
     ;;
     spec = data[*,j,k,l,m,n,o]
     ix = where_not_missing(spec, ngood, missing=missing)
     
     ;;
     ;; No dice if we have too few good points
     ;;
     IF ngood LE nvary THEN BEGIN
        residual[*,j,k,l,m,n,o] = missing
        result[*,j,k,l,m,n,o] = missing
     END ELSE BEGIN
restart_point:
        
        IF new_lam THEN lam = lambda[*,j,k,l,m,n,o]
        const_here = const[*,j,k,l,m,n,o]
        include_here = include[*,j,k,l,m,n,o]
        a_nom = result[0:npar-2,j,k,l,m,n,o]
        weights_here = weights[ix,j,k,l,m,n,o]
        IF NOT make_sigma THEN BEGIN 
           ff = cfit(lam[ix],spec[ix],a_nom,fit,$
                     /noupdate,chi2=chi2,quiet=quiet,$
                     sfit=sfit,const=const_here,include=include_here,$
                     weights=weights_here,failed=failed,fail_type=ftype,$
                     error_only=error_only)
        END ELSE BEGIN
           ff = cfit(lam[ix],spec[ix],a_nom,fit,sigma_here,$
                     /noupdate,chi2=chi2,quiet=quiet,$
                     sfit=sfit,const=const_here,include=include_here,$
                     weights=weights_here,failed=failed,fail_type=ftype,$
                     error_only=error_only)
           sigm[0,j,k,l,m,n,o] = sigma_here
        END
        
        residual[ix,j,k,l,m,n,o] = spec[ix]-ff
        failed = total(ftype EQ [1,3,4,5]) NE 0  ;; Ignore loss of prec.
        
        IF failed THEN BEGIN
           IF n_elements(restart) GT 0 THEN BEGIN
              result[0,j,k,l,m,o] = restart
              delvarx,restart
              GOTO,restart_point
           END
           chi2 = 0.0
        END
        
        result[0,j,k,l,m,n,o] = [a_nom,chi2]
        IF failed AND NOT quiet THEN BEGIN
           message,"Failed at ("+trim(j)+","+trim(k)+","+trim(l)+","+$
              trim(m)+","+trim(n)+","+trim(o)+'), chi^2 set to zero',$
              /continue
           print,string(7b)
        END
        
     END
  END ELSE BEGIN
     
     ;;
     ;; If no parameters variable - check if parameters are flagged
     ;;
     
     IF total(is_missing(result[0:npar-2,j,k,l,m,n,o], missing=missing)) NE npar-1 THEN BEGIN
        spec = data[*,j,k,l,m,n,o]
        ix = where_not_missing(spec, ngood, missing=missing)
        
        IF ngood NE 0 THEN BEGIN 
           IF new_lam THEN lam = lambda[*,j,k,l,m,n,o]
           a_act = reform(result[0:npar-2,j,k,l,m,n,o])*sfit.trans_a $
              +sfit.trans_b
           nsfit = sfit
           nsfit.include = include[*,j,k,l,m,n,o]
           eval_sfit,lam[ix],a_act,ff,private=nsfit
           residual[ix,j,k,l,m,n,o] = spec[ix]-ff
        END $
        ELSE residual[*,j,k,l,m,n,o] = missing
        
     END ELSE residual[*,j,k,l,m,n,o] = missing
     
  END
  
  IF n_elements(restart) GT 0 THEN delvarx,restart
END

PRO spice_cfit_block_progress,pct,lastpct,pctage,pct_slider_id,interrupt_id,halt,$
                        quiet
  IF pct GE lastpct+pctage THEN BEGIN
     WHILE pct GT lastpct DO lastpct = lastpct+pctage
     IF NOT quiet THEN print,lastpct,format='($,i4,a)','%'+string(13b)
     IF n_elements(pct_slider_id) EQ 1 THEN $
        widget_control,pct_slider_id,set_value=lastpct,/show
     IF n_elements(interrupt_id) EQ 1 THEN BEGIN
        ev = widget_event(interrupt_id,/nowait,/save_hourglass)
        IF ev.id NE 0L THEN BEGIN
           halt = 1
        END
     END 
  ENDIF
END

     
PRO spice_cfit_block,lambda,data,weights,fit,missing,result,residual,include,const,$
               double=double,use_result=use_result,quiet=quiet,$
               pct_slider_id=pct_slider_id,x_face=x_face,smart=smart,$
               analysis=ana,$
               make_sigma=make_sigma,sigma=sigma,error_only=error_only,$
               fill_only=fill_only
  
  IF NOT exist(ana) THEN BEGIN 
     IF n_params() LT 7 THEN BEGIN
        message,"Use: CFIT_BLOCK,LAMBDA,DATA,WEIGHTS,FIT,MISSING," + $
           "RESULT,RESIDUAL [,INCLUDE,CONST]"
     END
  END ELSE BEGIN
     handle_value,ana.lambda_h,lambda,/no_copy
     handle_value,ana.data_h,data,/no_copy
     handle_value,ana.weights_h,weights,/no_copy
     handle_value,ana.fit_h,fit,/no_copy
     missing = ana.missing
     handle_value,ana.result_h,result,/no_copy
     handle_value,ana.residual_h,residual,/no_copy
     handle_value,ana.include_h,include,/no_copy
     handle_value,ana.const_h,const,/no_copy
     error = 0
     IF !debug EQ 0 THEN catch,error
     IF error NE 0 THEN BEGIN
        print,!err_string
        print,"Caught error, putting back data blocks.."
        
        handle_value,ana.lambda_h,lambda,/no_copy,/set
        handle_value,ana.data_h,data,/no_copy,/set
        handle_value,ana.weights_h,weights,/no_copy,/set
        handle_value,ana.fit_h,fit,/no_copy,/set
        handle_value,ana.result_h,result,/no_copy,/set
        handle_value,ana.residual_h,residual,/no_copy,/set
        handle_value,ana.include_h,include,/no_copy,/set
        handle_value,ana.const_h,const,/no_copy,/set
        return
     END
  END
  
  quiet = keyword_set(quiet)
  t = systime(1)
  
  
  make_sigma = keyword_set(make_sigma)
  
  szd = size(data)
  szl = size(lambda)
  
  ;; Convert to 7 dimensions in all cases!
  dimen = szd[1:szd[0]]
  IF szd[0] LT 7 THEN dimen = [dimen,replicate(1L,7-szd[0])]
  
  parcheck,data,   2,typ(/rea),[2,3,4,5,6,7] ,"DATA"
  parcheck,lambda, 1,typ(/rea),[1,szd[0]],      "LAMBDA"
  parcheck,weights,3,typ(/rea),szd[0],          "WEIGHTS"
  parcheck,fit,    1,typ(/stc),1,               "FIT"
  
  ;;
  ;; If LAMBDA has same number of dims as data, it must have exact same size
  ;;
  IF szl[0] EQ szd[0] AND total(szl NE szd) NE 0 $
     OR szl[1] NE szd[1] THEN BEGIN
     message,"LAMBDA and DATA have incompatible sizes"
  END
  
  ;;
  ;; We could cope with an undfined missing value
  ;;
  default,missing,min(data)-1
  
  parcheck,missing,5,typ(/rea),0,               "MISSING"
  
  
  ;;
  ;; Make the short-fit structure - for more info.., but ignore "global" const
  ;; status when setting the min/max limits!
  ;;
  sfit = make_sfit_stc(fit,double=double,/keep_limits)
  

  ;;
  ;; How many parameters (including chi2) ? 
  ;; 
  npar = n_elements(sfit.a_act)+1
  
  ;;
  ;; This is the correct size of RESULT
  ;; 
  res_dim = [npar,szd[2:szd[0]]]
  
  ;;
  ;; If RESULT is already supplied, test size etc.
  ;;
  IF n_elements(result) GT 0 THEN BEGIN
     szr = size(result)
     IF szr[0] NE szd[0] OR szr[1] NE res_dim[0] THEN $
        message,"Suggested RESULT has incompatible size"
     
     IF total(szr[2:szr[0]] NE szd[2:szr[0]]) GT 0 THEN $
        message,"Suggested RESULT has incompatible size"
     default,use_result,1
     IF use_result THEN print,"Using previous result array"
  END ELSE BEGIN
     result = make_array(dimension=res_dim,/float,double=double)
     use_result = 0
  END
  
  IF NOT use_result THEN BEGIN
     ;;
     ;; Don't use the current result - make up new array with initial values
     ;;
     a_nom = (sfit.a_act-sfit.trans_b)/sfit.trans_a
     res_dim1 = res_dim
     res_dim1[1:*] = 1
     result = $
        dimrebin(dimreform([a_nom,0.0],res_dim1),res_dim)
     
     ;;
     ;; Result now has initial values, so...
     ;;
     use_result = 1
  END
  
  default,smart,0
  
  ;;
  ;; The residual is always same size as data - discard any input unless
  ;; SMART=2 is set
  ;;
  IF NOT (smart EQ 2 AND n_elements(residual) NE 0) THEN residual = data
  
  ;;
  ;; How many components?
  ;;
  ncomp = n_elements(sfit.include)
  
  ;;
  ;; This is the correct size of INCLUDE
  ;;
  inc_dim = [ncomp,szd[2:szd[0]]]
  
  ;;
  ;; If INCLUDE already supplied, test size
  ;;
  IF n_elements(include) GT 0 THEN BEGIN
     szi = size(include)
     IF szi[0] NE szd[0] OR szi[1] NE inc_dim[0] THEN $
        message,"INCLUDE array has incompatible size"
     
     IF total(szi[2:szi[0]] NE szd[2:szi[0]]) GT 0 THEN $
        message,"INCLUDE array has incompatible size"
  END ELSE BEGIN
     ;;
     ;; INCLUDE not supplied - make it up
     ;;
     include = make_array(dimension=inc_dim,/byte,value=1b)
  END
  
  ;;
  ;; If CONST is supplied, test size (should be (almost) same as RESULT)
  ;;
  const_dim = res_dim
  const_dim[0] = const_dim[0]-1
  IF n_elements(const) NE 0 THEN BEGIN
     szc = size(const)
     IF szc[0] NE szd[0] OR szc[1] NE const_dim[0] THEN $
        message,"CONST array has incompatible size"
     
     IF total(szc[2:szc[0]] NE szd[2:szc[0]]) GT 0 THEN $
        message,"CONST array has incompatible size"
  END ELSE BEGIN
     ;;
     ;; Not supplied - make it up
     ;;
     const = make_array(dimension=const_dim,/byte,value=0b)
     FOR j = 0,const_dim[0]-1 DO const[j,*,*,*,*,*,*] = sfit.const[j]
  END
  
  ;;
  ;; Now we're getting there....except abort here if no action
  ;;
  IF keyword_set(FILL_ONLY) THEN GOTO,halt
  
  new_lam = 0
  IF szl[0] EQ 1 THEN lam = lambda ELSE new_lam = 1
  
  pctage = 2
  ndo = dimen[6]*dimen[5]*dimen[4]*dimen[3]*dimen[2]*dimen[1]
  ndone = 0L
  lastpct = -pctage
  halt = 0
  
  ;;
  ;; Create a widget to inform about progress if X_FACE is set
  ;;
  IF keyword_set(x_face) THEN BEGIN
     base = widget_base(/column,title='Progress')
     pct_slider_id = widget_slider(base,xsize = 400,maximum=100,minimum=0,$
                                   title='Percent done')
     interrupt_id = widget_button(base,value='Press here to halt calculation')
     xrealize,base,/center
  END
  
  
  default,smart,0
  
  IF smart NE 0 THEN print,"Smart: "+trim(smart)
  
  IF make_sigma THEN sigma = make_array(dimension=const_dim,/float)
  
  CASE smart OF 
  0:BEGIN 
     ;;
     ;; This works for up to 7-dimensional data
     ;;
     FOR o=0L,dimen[6]-1 DO $
        FOR n=0L,dimen[5]-1 DO $
        FOR m=0L,dimen[4]-1 DO $
        FOR l=0L,dimen[3]-1 DO $
        FOR k=0L,dimen[2]-1 DO $
        FOR j=0L,dimen[1]-1 DO BEGIN
        ;;
        ;; Only touch results with one or more non-constant parameters
        ;; 
        
        spice_cfit_block_point,lambda,data,weights,fit,missing,$
           result,residual,include,const,$
           j,k,l,m,n,o,lam,new_lam,npar,sfit,$
           double=double,quiet=quiet,$
           make_sigma=make_sigma,sigma=sigma,error_only=error_only
        ;;
        ;; Keep user informed about the progress
        ;; 
        ndone = ndone+1L
        pct = (100*ndone)/ndo
        spice_cfit_block_progress,pct,lastpct,pctage,pct_slider_id,$
           interrupt_id,halt,quiet
        IF halt THEN GOTO,halt
     ENDFOR 
     ENDCASE
     
  1:BEGIN
     ;; "SMART" option
     avdata = average(data,1,missing=missing)
     sortix = sort(avdata)
     ;; Start with highest S/N ration
     FOR jj = ndo-1L,0L,-1L DO BEGIN
        ix = sortix[jj]
        j = ix MOD dimen[1] & ix = ix/dimen[1]
        k = ix MOD dimen[2] & ix = ix/dimen[2]
        l = ix MOD dimen[3] & ix = ix/dimen[3]
        m = ix MOD dimen[4] & ix = ix/dimen[4]
        n = ix MOD dimen[5] & ix = ix/dimen[5]
        o = ix MOD dimen[6]
        IF jj NE ndo-1L THEN BEGIN
           restart = result[*,j,k,l,m,n,o]
           result[*,j,k,l,m,n,o] = last_result
        END
        spice_cfit_block_point,lambda,data,weights,fit,missing,$
           result,residual,include,const,$
           j,k,l,m,n,o,lam,new_lam,npar,sfit,$
           double=double,quiet=quiet,restart=restart,$
           make_sigma=make_sigma,sigma=sigma,error_only=error_only
        last_result = result[*,j,k,l,m,n,o]
        
        ;;
        ;; Keep user informed about the progress
        ;; 
        ndone = ndone+1L
        pct = (100*ndone)/ndo
        spice_cfit_block_progress,pct,lastpct,pctage,pct_slider_id,$
           interrupt_id,halt,quiet
        IF halt THEN GOTO,halt
     END
     ENDCASE 
     
  2:BEGIN
     ;; "SMART" option #2 - only those with chi^2 equal to zero
     
     doix = where(result[npar-1,*,*,*,*,*,*] EQ 0.0,ndo)
     print,"Doing "+trim(ndo)+" points"
     ;; Start with highest S/N ration
     FOR jj = ndo-1L,0L,-1L DO BEGIN
        ix = doix[jj]
        j = ix MOD dimen[1] & ix = ix/dimen[1]
        k = ix MOD dimen[2] & ix = ix/dimen[2]
        l = ix MOD dimen[3] & ix = ix/dimen[3]
        m = ix MOD dimen[4] & ix = ix/dimen[4]
        n = ix MOD dimen[5] & ix = ix/dimen[5]
        o = ix MOD dimen[6]
        spice_cfit_block_point,lambda,data,weights,fit,missing,$
           result,residual,include,const,$
           j,k,l,m,n,o,lam,new_lam,npar,sfit,$
           double=double,quiet=quiet,restart=restart,$
           make_sigma=make_sigma,sigma=sigma
        last_result = result[*,j,k,l,m,n,o]
        
        ;;
        ;; Keep user informed about the progress
        ;; 
        ndone = ndone+1L
        pct = (100*ndone)/ndo
        spice_cfit_block_progress,pct,lastpct,pctage,pct_slider_id,$
           interrupt_id,halt,quiet
        IF halt THEN GOTO,halt
     END
     ENDCASE 
     
  END
  
halt:
  
  IF n_elements(interrupt_id) EQ 1 THEN $
     ev = widget_event(interrupt_id,/nowait,save_hourglass=0)
  IF NOT quiet THEN print,string(7b)
  xkill,base
  IF NOT quiet THEN print,string(7b)
 
  print,trim(systime(1)-t)+" seconds used"
  failix = where(result(npar-1,*,*,*,*,*,*) EQ 0,nfailed)
  
  IF exist(ana) THEN BEGIN
        handle_value,ana.lambda_h,lambda,/no_copy,/set
        handle_value,ana.data_h,data,/no_copy,/set
        handle_value,ana.weights_h,weights,/no_copy,/set
        handle_value,ana.fit_h,fit,/no_copy,/set
        handle_value,ana.result_h,result,/no_copy,/set
        handle_value,ana.residual_h,residual,/no_copy,/set
        handle_value,ana.include_h,include,/no_copy,/set
        handle_value,ana.const_h,const,/no_copy,/set
  END
  
END
