; 31/10/2022 - Martin Wiesmann, copied from hinode/eis/idl/atest/osdc/gt_peaks.pro
;                               to make SPICE independent of EIS.
; $Id: 2022-10-31 10:44 CET $

FUNCTION spice_gt_peaks, int, fwhm=fwhm, minmedian=minmedian, sort=sort, plot=plot
  ;; Find all peaks in input array int. Returns -1 if no suitable peaks are found.
  ;; KEYWORDS: 
  ;;          fwhm: IF set to a named variable fwhm returns a very rough
  ;;                 estimate of the FWHM of the peaks
  ;;          minmedian: peaks with values less than minmedian/FWHM times the
  ;;                     median value of int are ignored
  ;;          sort: If keyword set the index of the highest peak will be the
  ;;                first value of the returned array, the index of the lowest
  ;;                peak will be the last.
  ;;          plot: plot the input array and mark the the peaks with solid
  ;;                lines. Rejected peaks are marked with a broken line
  
  default, minmedian, 6
  
  int -= min(int)               ; Must subtract pedestal before calculating median
                                ; int, might as well do it now...
  
  dd = deriv(deriv(int))        ; The double derivative of input array
  lt0 = where(dd LT 0)          ; Find all indices of where dd is lt 0.  
  
  ;; For each int peak there will be a series of concecutive lt0 values
  ;; (centered around the peak). To indentify such a series, we first find the
  ;; difference between two neighbouring lt0 values:
  difflt0 = lt0-shift(lt0,1)   
  
  ;; The start-indices of concecutive lt0-series
  consecutive_ind = [0]                ; If there's only one peak
  lt0ind0 = where(difflt0 GT 1, count) ; There are more peaks if lt0ind0 NE -1
  IF lt0ind0[0] NE -1 THEN consecutive_ind = [consecutive_ind, lt0ind0] ; Add peaks 
  
  ;; The indices in the lt0 arrays are of little value; we need to find the
  ;; int start and end indices of each lt0 concecutive series: 
  ind0 = lt0[consecutive_ind]
  ind1 = last_nelem(lt0)                     ; If there's only one peak
  IF n_elements(consecutive_ind) GT 1 THEN $ ; Add end indices if more than one peak
     ind1 = [(lt0[consecutive_ind-1])[1:*], ind1] 
  
  ;; Find real peaks consisting of more than 1 int value
  goodpeaks = where(ind1-ind0 GT 0, count, complement=badpeaks, $
                    ncomplement=nbadpeaks)
  
  npeaks = n_elements(ind0)
  peakinds = intarr(npeaks)
  fwhm = intarr(npeaks)
  FOR i=0,npeaks-1 DO BEGIN
     fragment = int[ind0[i]:ind1[i]] ; Finally we look at input array values!
     fwhm[i] = n_elements(fragment)  ; The roughest estimate in the galaxy
     mx = max(fragment, indadd)      ; Here's the maximum value
     peakinds[i] = ind0[i]+indadd    ; The index in the input array of the peak
  ENDFOR
  
  IF keyword_set(plot) THEN BEGIN
     loadct,12
     ;; plot int array, mark peaks
     plot,int,/yst,/xst,ytitle='Intensity',xtitle='Pixel number', $
          title='Fitting gaussians to peaks marked with solid blue line',thick=2
     mnplot = min(int)
     mxplot = 0.25*max(int) > mnplot+0.1*mnplot
     FOR i=0,npeaks-1 DO $
        plots,[peakinds[i],peakinds[i]],[mnplot,mxplot],color=120,thick=2
     
     m = median(int)
     plots,[0,n_elements(int)-1],[m,m], color=190, line=1
     plots,[0,n_elements(int)-1],[2*m, 2*m], color=230, line=1
     
     IF nbadpeaks GT 0 THEN FOR i=0,n_elements(badpeaks)-1 DO $
        plots,[peakinds[badpeaks[i]],peakinds[badpeaks[i]]],[mnplot,mxplot],$
              color=250,thick=2
  ENDIF
  
  ;; Remove all peaks consisting of 1 int value
  peakinds = peakinds[goodpeaks]
  fwhm = fwhm[goodpeaks]
  
  ;; Ignore weak peaks. What is a weak peak? It turns out that all real,
  ;; strong peaks have a FWHM of 3-5 pixels, while most weak peaks that are
  ;; mostly due to noise, have av FWHM of 1 or 2. We use this to get an
  ;; additional factor than just the intensity value: ignore peaks with
  ;; intensity lower than minmedian/fwhm*median(int), where minmedian is 6. 
  IF keyword_set(minmedian) THEN BEGIN
     goodind = where(int[peakinds] GT minmedian/fwhm*median(int),$
                     complement=lowind) ;> 0
    
     ;; Partially erase the peak markers of weak peaks
     IF keyword_set(plot) THEN BEGIN
        plots,peakinds,minmedian/fwhm*median(int),psym=2,color=40
        IF lowind[0] NE -1 THEN BEGIN
           lowpeaks = peakinds[lowind]
           FOR i=0,n_elements(lowind)-1 DO $
              plots,[lowpeaks[i],lowpeaks[i]],[mnplot,mxplot],color=0,$
                    line=2,thick=2
        ENDIF
     ENDIF
     
     IF goodind[0] NE -1 THEN BEGIN 
        peakinds = peakinds[goodind]
        fwhm = fwhm[goodind]
     ENDIF ELSE BEGIN 
        peakinds = -1
        fwhm = -1
        box_message,'No valid peaks found.'
     ENDELSE 
     
  ENDIF
 
  ;; Sort the indices of the peaks and the FWHMs in order of decreasing height
  ;; of the peaks
  IF keyword_set(sort) THEN BEGIN
     srt = sort(int[peakinds]) 
     peakinds = rotate(peakinds[srt],2)
     fwhm = rotate(fwhm[srt],2)
  ENDIF
  
  return,peakinds
END

