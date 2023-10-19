FUNCTION iris_histo_opt, image, cutoff, ix, top_only=top, bot_only=bot, $
  missing=missing, low_limit=low_limit, high_limit=high_limit, silent=silent
;+
; NAME:
;       IRIS_HISTO_OPT
; PURPOSE:
;       Clip image values which are the CUTOFF brightest or darkest,
;       resp.
; CATEGORY:
;
; CALLING SEQUENCE:
;       CLIP_IMAGE = IRIS_HISTO_OPT ( IMAGE [, CUTOFF [, IX]] [,<keywords>])
; INPUTS:
;       IMAGE : Array with data. may be 1 to 3dim
; OPTIONAL PARAMETERS:
;       IX    : (Output) Contains indices of the clipped values
; KEYWORDS:
;       TOP_ONLY : (Flag) Clip only the upper values
;       BOT_ONLY : (Flag)  "     "  "   lower   "
;       MISSING  : If set ignore
;                  pixels of value missing in
;                  histogram calculation
;       LOW_LIMIT: if set ignore pixels of value low_limit and lower
;                  all NAN values and -INF values will be set to this value
;       HIGH_LIMIT:if set ignore pixels of value high_limit and higher
;                  all +INF values will be set to this value
;       SILENT   : do not produce error messages
; OUTPUTS:
;       CLIP_IMAGE : Image with the CUTOFF fraction lowest and highest
;                    values set to the value of the next highest/lowest
;                    point.
; RESTRICTIONS:
;       Maybe this should be a procedure, as it uses a lot of memory
;       for big arrays. OTOH it is used mainly for displaying, so you
;       wouldn't want to change the real data.
; PROCEDURE:
;       Compute histogram, evaluate the boundaries and return
;       IMAGE>LOW<HIGH
; MODIFICATION HISTORY:
;       06-Jul-1993  P.Suetterlin, KIS
;       16-Feb-1995  P.Suetterlin, KIS: Take care for float
;                    arrays. Histogram doesn't like them.
;       09-Aug-2013  V.Hansteen, ITA: Added missing keyword.
;       26-Sep-2013  V.Hansteen, ITA: Changed name to iris_histo_opt
;                    to avoid clashes with other peoples old histo_opts
;       14-Jan-2014  M.Carlsson, ITA: added silent keyword
;       27-Sep-2016  M.Wiesmann, ITA: added keywords low_limit and high_limit
;-

  on_error, 2
  
  IF n_params() EQ 0 THEN BEGIN
    message, 'Usage: RESULT = IRIS_HISTO_OPT ( IMAGE [,CUTOFF,MISSING=MISSING,/SILENT] )', /cont
    return, undefined
  ENDIF
  
  IF n_params() LT 2 THEN cutoff = 1e-3
  s = size(image)
;;;
;;; If the image is in a float format, then histogram() doesn't know
;;; what to do. In that case, convert to fix. But then you have to be
;;; shure that the range is ok (especially for normalized images with
;;; a range from 0. to 1.).
;;;
  good=finite(image)
  if (where(good))[0] eq -1 then begin
    if ~keyword_set(silent) then message,'All data is NaN! Returning',/info
    return,image
  endif
; A bit convoluted to keep the behaviour the same as in the old
; histo_opt routine in case of no missing keyword given
  if n_elements(missing) ne 0 then begin
    imsave0=image
    bad = where(good eq 0,nbad)
    if nbad ne 0 then image[bad]=missing
    good=where(image ne missing,ngood)
    if ngood eq 0 then begin
      if ~keyword_set(silent) then message,'All data is missing! Returning',/info
      return,image
    endif
    image=image[good]
  endif
  
  if n_elements(low_limit) ne 0 then begin
    imsave1=image
    low = where(finite(image, /nan) OR finite(image, /INFINITY, sign=-1),nlow)
    if nlow ne 0 then image[low]=low_limit
    good=where(image gt low_limit,ngood)
    if ngood eq 0 then begin
      if ~keyword_set(silent) then message,'All data is too low! Returning',/info
      if n_elements(imsave0) ne 0 then return,imsave0 else return,image
    endif
    image=image[good]
  endif
  
  if n_elements(high_limit) ne 0 then begin
    imsave2=image
    high = where(finite(image, /INFINITY, sign=1),nhigh)
    if nhigh ne 0 then image[high]=high_limit
    good=where(image lt high_limit,ngood)
    if ngood eq 0 then begin
      if ~keyword_set(silent) then message,'All data is too high! Returning',/info
      if n_elements(imsave0) ne 0 then return,imsave0 $
      else if n_elements(imsave1) ne 0 then return,imsave1 else return,image
    endif
    image=image[good]
  endif

  IF s(s(0)+1) GT 3 THEN BEGIN
    fak = 10000./(max(image, min = hmin)-hmin)
    h = histogram(fix((image-hmin)*fak))
  ENDIF ELSE BEGIN
    h = histogram(image)
    hmin = min(image)
    fak = 1
 ENDELSE
  ;if n_elements(missing) ne 0 then image=imsave0
  if n_elements(imsave0) ne 0 then image=imsave0 $
  else if n_elements(imsave1) ne 0 then image=imsave1 $
  else if n_elements(imsave2) ne 0 then image=imsave2
  
  nh = n_elements(h)
;;;
;;; Integrate the histogram so that h(i) holds the number of points
;;; with equal or lower intensity.
;;;
  FOR i = 1l, nh-1 DO h(i) = h(i)+h(i-1)
;;;
;;; and normalize it to unity
;;;
  h = float(h)/h(nh-1)
;;;
;;; As CUTOFF is in percent and h is normalized to unity,
;;; cmin/cmax are the indices of the point where the number of pixels
;;; with lower/higher intensity reach the given limit. This has to be
;;; converted to a real image value by dividing by the scalefactor
;;; FAK and adding the min value of the image
;;;
  cmin = max(where(h LE cutoff))/fak+hmin
  cmax = min(where(h GE (1.-cutoff)))/fak+hmin
;;;
;;; Where is slow. Only compute if requested.
;;;
  IF n_params() EQ 3 THEN ix = where((image LE cmin) OR (image GE cmax))
  
  IF keyword_set(top) THEN $
    return, image < cmax $
  ELSE IF keyword_set(bot) THEN $
  return, image > cmin $
ELSE $
  return, image > cmin < cmax
END

