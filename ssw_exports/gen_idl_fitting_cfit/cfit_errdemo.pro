;+
; Project     : SOHO - CDS     
;                   
; Name        : CFIT_ERRDEMO
;               
; Purpose     : Demonstrate analytical vs empirical errors of line fitting.
;               
; Explanation : Generates a known model for NIS data, adding noise according
;               to a known formula, and applies a least-square fit with
;               calculation of estimated errors (sigmas) on the fitted
;               parameters, then prints out various results.
;
;               In general, the estimated errors are correct when the signal
;               to noise ratio is "good" and the background is negligible in
;               comparison to the flux from the emission line.
;
;               The correct formula for the sigma(line_flux) based on the
;               estimated sigmas seem to be:
;
;                 sig_I = 1./sqrt(2) * sqrt((sig_I*w)^2 + (sig_w*I)^2)
;
;               due to the fact that the width and the intensity of a line fit
;               are *not* independently drawn quantities (use e.g.,
;               CFIT_ERRDEMO,10000,0,NX=1000 to see that the estimated sigma
;               without the "normalization factor" becomes approx. sqrt(2)
;               times it's correct value.
;
;               There seems to be some coupling between the background level
;               and the line intensity error estimate that I haven't yet quite
;               figured out - try very high values for the background
;               (relative to the emission line) to see what I mean..  The
;               fitted result is better than what should be expected from the
;               actual noise level - this could be due to the fact that some
;               of the (background-related) noise is simply situated outside
;               the line profile.
;
;               Hopefully the program is well enough documented to allow any
;               logical errors to be caught by others..
;               
; Use         : CFIT_ERRDEMO [,INTENSITY [,BACKGROUND]] [,NX=NX]
;    
; Inputs      : INTENSITY : Line intensity (amplitude) in peak counts.
;
;               BACKGROUND : Background intensity in peak counts.
;
;               NX : Number of realizations of the data with noise
;               
; Opt. Inputs : All..
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : See inputs
;
; Calls       : default pix2wave() mk_comp_gauss() mk_comp_poly() eval_cfit
;               cfit_block average()
;
; Common      : None
;               
; Restrictions: ...
;               
; Side effects: ...
;               
; Category    : 
;               
; Prev. Hist. : None
;
; Written     : SVH Haugan, UiO, 13 October 1997
;               
; Modified    : Version 2, 08-Oct-2015, WTT, use [] for array indices

;
; Version     : 2, 08-Oct-2015
;-            

PRO cfit_errdemo,intens,background,nx=nx
  dummy = check_math(trap=0)
  
;
; This section sets up some parameters
;
;

  ;; What are the true values of our model
  ;; 
  
  default,intens,1000.0d
  default,background,0.0
  
  intens = double(intens)
  background = double(background)
  
  ;; Line position & width (sort of O III 525 ..)
  
  position = 525.79926d 
  width = .2d
  
  print,"Parameters used:"
  print
  print,"Intensity="+trim(intens)+" Position="+trim(position)+$
     " Width="+trim(width)+" Background level="+trim(background)
  print
  
  floor = 1.5 ;; Minimum number of counts when calculating poisson noise
  
  ;; Number of "spatial points" - i.e., how many realizations of the model.
  ;; 
  default,nx,200L & nx = long(nx)
  
  
  ;; Number of lambda pixels (width of data window)
  npix = 21
  
  
  ;; We need a lambda array
  
  lam = pix2wave('N2',findgen(npix)+99)
  delta_lam = (max(lam)-min(lam))/(npix-1)
  
  
  ;; And a fit model - single Gaussian with constant background (zero order
  ;; polynomial).
  ;;
  ;; Note that the TRANS_A array is [1,1,], which is not the default!
  ;; 
  
  IGAUSS2 = mk_comp_gauss([intens,position,width],trans_a=[1,1,1])
  
  BG = mk_comp_poly([background])
  
  fit = { IGAUSS2 : IGAUSS2,$ 
          BG : BG} 

  ;; This expands to a constant background plus a single gaussian:
  ;;
  ;; Counts pr pixel = intens * exp(-z^2/2) + bg
  ;;
  ;; where z^2 = ((lambda-position)/width)^2

  
  ;; The analytical number (integrating the Gaussian) of total counts
  ;; should be 
  ;;
  ;; N = intens * width * sqrt(2*!PI)/delta_lambda + background * npix
  ;; 
  ;; with corresponding Poisson noise = sqrt(N)
  
  ;; Note that the noise in the total counts after *correct* background
  ;; subtraction is still equal to sqrt(N)!
  ;;
  
  ;; This adjustment factor will come in handy later:
  adjust = sqrt(2*!PI)/delta_lam
  
  ;; 
  ;; Now, let's calculate our model on our pixels
  ;; 
  eval_cfit,lam,true,fit,/double
  
  ;; And then we fan it out to a block of data
  ;; 
  trueblock = rebin(reform(true,npix,1),npix,nx,/sample)
  
  ;; The noise level per pixel is sqrt(ncounts), but for very few
  ;; counts this must somehow be modified if we're *really* dealing with
  ;; Poisson noise.
  ;; 
  noiselevel = sqrt(trueblock > floor)
  
  ;;
  ;;The weights follow immediately
  ;;
  wts = 1.0/noiselevel^2
  
  ;;
  ;; (Note that in our "experiment" we add Gaussian noise, *not* Poisson
  ;; noise, which would be the case in real life!)
  ;;
  noise = noiselevel*randomn(seed,npix,nx)
  
  ;;
  ;; And this is the data that is observed
  ;;
  da = noise+trueblock
  
  ;;
  ;; Now, make a best fit to these data, getting the result and residuals
  ;;
  cfit_block,lam,da,wts,fit,-100,resu,resi,inc,const,$
     /double,/make_sigma,sigma=sig,/quiet
  
  ;;
  ;; We can use our analytical formula to calculate how many counts there
  ;; should be in the line given the fitted parameter values.
  ;;
  ;; But, we'd like to check how many counts our line model actually subtracts
  ;; from the data, so we'll calculate how the Gaussian looks on the pixels.
  ;;
  ;; To do this, we use the fact that:
  ;; 
  ;; residual = data - model   <=>    model = da - resid
  ;;
  ;; The line flux (background excluded) of this model can thus be calculated
  ;; from the residual (when the background is excluded) and the original data
  ;; cube.
  
  ;; Ok - we turn off component number two (the background) and set all
  ;; parameters constant.
  
  inc[1,*,*] = 0b
  const[*,*,*] = 1b
  
  ;; The calculation of counts in the emission line is done by calculating the
  ;; new residual - since all parameters are kept constant, only the temporary
  ;; residual array will be changed
  ;;
  ;; This could have been done far easier by simply subtracting the calculated
  ;; background, but for educational purposes...
  
  cfit_block,lam,da,wts,fit,-100,resu,temp_resid,inc,const,$
     /double,/quiet
  
  pixel_line = da-temp_resid
  
  ;; The line flux in our real data is (here we subtract the *correct*
  ;; background value!):
  ;; 
  dataflux = total(da-background,1)
  datasigma = sigma(dataflux)
  dataflux = average(dataflux)
  
  ;;
  ;; The line flux in our pixelated model:
  ;;
  pixel_flux = total(pixel_line,1)
  pixel_sigma = sigma(pixel_flux)
  pixel_flux = average(pixel_flux)
  
  ;;
  ;; The true (but pixelated!) flux, *true* background subtracted
  ;;
  trueflux = total(true-background)
  truesigma = sqrt(total(noiselevel[*,0]^2))
  
  
  ;; Analytical flux based on fit parameters - note adjustment factor
  ;;  
  analyt_flux = reform(resu[0,*,*]*resu[2,*,*]*adjust,/overwrite)
  analyt_sigma = sigma(analyt_flux)
  analyt_flux = average(analyt_flux)
  
  ;; The output sigmas, added in quadrature
  ;; 
  sigma_out = adjust*sqrt((sig[0,*,*]*resu[2,*,*])^2  $
                          + (resu[0,*,*]*sig[2,*,*])^2)
  sigma_out = average(sigma_out)
  
  print
  print,"Number of realizations:",nx
  print
  print,"Signal/noise ratio of line flux",trueflux/truesigma
  print
  Print,"Average line fluxes, and empirical sigmas in units of true_sigma"
  print
  print,"True value :",trueflux,truesigma/truesigma
  print,"Data pixsum:",dataflux,datasigma/truesigma,$
     " (after correct bg subtraction)"
  print
  print,"Pixel-model:",pixel_flux,pixel_sigma/truesigma,$
     " (sum of fitted result on pixels)"
  print,"Analytical :",analyt_flux,analyt_sigma/truesigma,$
     " (I * w of fit)"
  print
  print,"sigma (out):",0.0d,sigma_out/truesigma,$
     " sqrt((sig_I*w)^2 + (sig_w*I)^2)"
  print
  print,"Raw parameters "
  print
  et = " (empirical avg. + sigma)"
  tt = " (true value, estimated sigma)"
  print,"Intensity  :",average(resu[0,*,*]),sigma(resu[0,*,*]),et
  print,"Intensity  :",intens,1.0d*average(sig[0,*,*]),tt
  print
  print,"Position   :",average(resu[1,*,*]),sigma(resu[1,*,*]),et
  print,"Position   :",position,1.0d*average(sig[1,*,*]),tt
  print
  print,"Width      :",average(resu[2,*,*]),sigma(resu[2,*,*]),et
  print,"Width      :",width,1.0d*average(sig[2,*,*]),tt
  print
  print,"Background :",average(resu[3,*,*]),sigma(resu[3,*,*]),et
  print,"Background :",background,1.0d*average(sig[3,*,*]),tt
  print
  print,"Various averages and empirical sigmas"
  print,"Chi^2      :",average(resu[4,*,*]),sigma(resu[4,*,*])
  print,"Residual   :",average(resi),1.0d*sigma(resi)
  print,"Chi^2 contr:",average(resi^2*wts),1.0*sigma(resi^2*wts)
  print,"(Chi^2 contribution: Average squared residual times weight)"
  
  print
  
END

