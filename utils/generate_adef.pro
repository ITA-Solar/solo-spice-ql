;+
; NAME:
;      GENERATE_ADEF
;
; PURPOSE:
;      This function returns an array of fits headers made from an ANA object or file.
;
;
;
;
; I don't use any advanced commands in Git, stash very seldom, squashing and reordering never so far.
; I do quite often create a new branch, do some changes there (commit, push)
; and then merge it to develop branch (or some other)
; to create a new branch I use the terminal for some reason:
;
; create new branch
; git checkout -b mwiesman/feature/sqflags
;
; push new branch
; git push --set-upstream origin mwiesman/feature/dqptypes
;
; but I'm sure that's also possibly using the DeskHub
;
; Merging I do that on the github webpage, not locally on my computer
; although that's of course also possible.
; On github, you can view a branch, and then there is a button to Create a new pull request.
; Edit it and create it. And then you can merge the branches, or someone else
;
;
;
; CATEGORY:
;      XXX -- utility
;
; CALLING SEQUENCE:
;      adef = generate_adf(data, lambda)
;
; INPUTS:
;      data: data
;      lambda: lambda
;
; OUTPUTS:
;      adef
;
; OPTIONAL OUTPUTS:
;
; HISTORY:
;      Ver. 1, 18-Oct-2021, Martin Wiesmann
;-
; $Id: 2022-01-17 10:26 CET $


FUNCTION generate_adef, data, lam, widmin=widmin
  ;; Automatically generate cfit analysis definitions based on input intensity and
  ;; wavelength arrays
  sz = size(data)
  
  badix = where(data ne data, n_bad, complement=goodix)
  IF n_bad GT 0 THEN data[badix] = min(data[goodix]) > 0
  
  meanprofile = rebin(data,sz[1],1,1)
  
  IF n_bad GT 0 THEN data[badix] = (typename(data) EQ 'FLOAT') ? !values.f_nan : !values.d_nan
  print,'I want to check in this line'
  peakinds = gt_peaks(meanprofile, fwhm=fwhm, minmedian=4.5,/sort,/plot)
  npeaks = n_elements(peakinds)

  gaussians = replicate(mk_comp_gauss([0,0,0]), npeaks)

  int0 = meanprofile[peakinds]
  lampeak = lam[peakinds,*,*]
  lamfwhm = lam[peakinds-fwhm,*,*]

  lam0 = fltarr(npeaks)
  wid0 = fltarr(npeaks)

  FOR i=0,npeaks-1 DO lam0[i] = median(lampeak[i,*,*])
  FOR i=0,npeaks-1 DO wid0[i] = lam0[i] - median(lamfwhm[i,*,*])

  v = 75.                       ; Max shift in km/s
  dlam = v*lam0/3.e5            ; Max shift in Aangstrom

  intmin = fltarr(npeaks)          ; minimum intensity is 0
  lammin = (lam0 - dlam) > min(lam); v0 - v
  IF NOT keyword_set(widmin) THEN widmin = (wid0 - 0.04) >  0.02   ; random guess...

  intmax = int0*100;30000                 ; More random guessing
  lammax = (lam0 + dlam) < max(lam) ; v0 + v
  widmax = wid0 + 0.04              ; A final shot in the dark

  FOR i=0,n_elements(peakinds)-1 DO BEGIN
     gauss = mk_comp_gauss([int0[i],lam0[i],wid0[i]], $
                           max_arr=[intmax[i],lammax[i],widmax[i]], $
                           min_arr=[intmin[i],lammin[i],widmin[i]], $
                           trans_a=[1,1,0.424661], trans_b=[0,0,0], $
                           const=[0b,0b,0b])
     lam0txt = trim(lam0[i],'(F6.2)')
     gauss.name = 'AutoGauss'+lam0txt
     gaussians[i] = gauss
  ENDFOR


  bg = mk_comp_poly([0.5*median(meanprofile)], max_arr=[30000],min_arr=[0],trans_a=[1],$
                    trans_b=[0],const=[0b])
  bg.name = 'Background'

  IF npeaks EQ 1 THEN adef = {igauss2:gaussians[0], bg:bg}
  IF npeaks EQ 2 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], bg:bg}
  IF npeaks EQ 3 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], bg:bg}
  IF npeaks EQ 4 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], igauss5:gaussians[3], bg:bg}
  IF npeaks EQ 5 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], igauss5:gaussians[3], $
                                igauss6:gaussians[4], bg:bg}
  IF npeaks EQ 6 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], igauss5:gaussians[3], $
                                igauss6:gaussians[4], igauss7:gaussians[5], bg:bg}
  IF npeaks EQ 7 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], igauss5:gaussians[3], $
                                igauss6:gaussians[4], igauss7:gaussians[5], $
                                igauss8:gaussians[6], bg:bg}
  IF npeaks EQ 8 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], igauss5:gaussians[3], $
                                igauss6:gaussians[4], igauss7:gaussians[5], $
                                igauss8:gaussians[6], igauss9:gaussians[7], $
                                bg:bg}
  IF npeaks GE 9 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
                                igauss4:gaussians[2], igauss5:gaussians[3], $
                                igauss6:gaussians[4], igauss7:gaussians[5], $
                                igauss8:gaussians[6], igauss9:gaussians[7], $
                                igauss10:gaussians[8], bg:bg}


  IF npeaks GE 9 THEN box_message,'Found '+trim(npeaks)+', fitting the 9 highest...'

  return, adef
END
