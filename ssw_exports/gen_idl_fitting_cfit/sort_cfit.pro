;+
; Project     : SOHO - CDS     
;                   
; Name        : SORT_CFIT
;               
; Purpose     : Sort some of the components in a component fit structure
;               
; Explanation : Those components in the structure matching one of the strings
;               supplied in the FUNC_NAMES parameter will be sorted according
;               to the values of the parameter with index PARM
;
;               Additionally, this routine can be used to purge all components
;               having INCLUDE=0 from the structure.
;               
; Use         : SORT_CFIT,CFIT,FUNC_NAMES,PARM
;    
; Inputs      : CFIT : Component Fit structure
;
;               FUNC_NAMES : The evaluation function name(s) (e.g.,
;                            "comp_gauss" or "comp_poly") used to distinguish
;                            those components to be included in the sorting
;                            process. This can be an array enabling sorting of
;                            e.g., both gaussian and voigt profiles according
;                            to their line position. The sort is performed "in
;                            place", i.e., the non-sorted components do not
;                            change their place within the structure.
;
;               PARM : The index of the parameter to sort on.
;               
; Opt. Inputs : None.
;               
; Outputs     : The CFIT structure is modified
;               
; Opt. Outputs: None.
;               
; Keywords    : DECREASING : Set to sort in decreasing order
;
;               PURGE : Set to purge all INCLUDE = 0 components
;
;               NOSORT : Set to avoid sorting alltogether - useful if only
;                        purging is wanted.
;
; Calls       : None.
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
; Written     : S.V.H.Haugan, UiO,
;               
; Modified    : Not yet
;
; Version     : 1, 21 January 1997
;-            


PRO sort_cfit,fit,func_names,parm,decreasing=decreasing,$
            purge=purge,nosort=nosort
  
  comps = tag_names(fit)
  ncomp = n_elements(comps)
     
  
  IF NOT keyword_set(nosort) THEN BEGIN
     
     nparmnames = n_elements(parmnames)
     
     ;; Find out which tags to sort.
     
     func_names = strlowcase(func_names)
     
     FOR c = 0,ncomp-1 DO BEGIN
        dummy = where(func_names EQ fit.(c).func_name)
        IF dummy(0) NE -1 THEN BEGIN
           IF n_elements(compix) EQ 0 THEN compix = [c] $
           ELSE compix = [compix,c]
        END
     END
     
     ncomp2sort = n_elements(compix)
     
     IF ncomp2sort EQ 0 THEN BEGIN
        message,"No components matching given FUNC_NAMES, no sort performed",$
           /informational
        return
     END
     
     IF ncomp2sort EQ 1 THEN BEGIN
        message,"Only one component matching given FUNC_NAMES, " + $
           "no sort performed",/informational
        return
     END
     
     varval = fltarr(ncomp2sort)
     
     nrmach = nr_machar()
     IF NOT keyword_set(decreasing) THEN missing = nrmach.xmax $
     ELSE                                missing = nrmach.xmin
     
     FOR c = 0,ncomp2sort-1 DO BEGIN
        IF n_elements(fit.(compix(c)).param) LT (parm+1) THEN  $
           varval = missing $
        ELSE varval(c) = fit.(compix(c)).param(parm).value
     END
     
     newix = lindgen(ncomp) ;; By default, no sorting... tag n == tag n
     
     sortix = sort(varval)  ;; The succession of the sorted components
     
     IF keyword_set(decreasing) THEN sortix = rotate(sortix,2)
     
     ;; We're sorting the tags given by compix, with internal succession given
     ;; by sortix
     
     newix(compix) = compix(sortix)
     
  END ELSE BEGIN
     ;;
     ;; We're not sorting, but (possibly) purging
     ;;
     newix = lindgen(n_elements(comps))
     
  END
  
  
  ;; Recreate structure with sorted components
  ;; Do not insert excluded components if PURGE is set.
  purge = keyword_set(purge)
  
  FOR c = 0,ncomp-1 DO BEGIN
     IF NOT purge OR fit.(newix(c)).include THEN BEGIN
        IF n_elements(newfit) EQ 0 THEN $
           newfit = create_struct(comps(newix(c)),fit.(newix(c))) $
        ELSE newfit = create_struct(newfit,comps(newix(c)),fit.(newix(c)))
     END
  END
  
  fit = newfit
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'sort_cfit.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
