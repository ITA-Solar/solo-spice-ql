;+
; Project     : SOHO - CDS     
;                   
; Name        : UPDATE_CFIT
;               
; Purpose     : Update values in a fit structure.
;               
; Explanation : Use this routine to store e.g., new parameter values into a
;               component fit structure. You may also use it to simultaneously
;               update the INCLUDE and CONST fields.
;               
; Use         : UPDATE_CFIT,CFIT,AA_NOMINAL
;    
; Inputs      : CFIT : Component fit structure
;
;               AA_NOMINAL : Array of nominal values for the parameters. May
;                            be left out, if only the const/include status is
;                            to be updated.
;               
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : INITIAL : Set this to update the initial values instead of the
;                         current values.
;
;               INCLUDE : Supply as an array of bytes to update the INCLUDE
;                         fields in the fit structure.
;
;               CONST : Supply as an array of bytes to update CONST field.
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
; Modified    : Version 2, SVHH, 15 September 1997
;                       Added calling mode without AA_NOM defined.
;
; Version     : 2, 15 September 1997
;-            

PRO update_cfit,fit,aa_nom,initial=initial,include=include,const=const
  
  n = n_elements(tag_names(fit))
  
  nsofar = 0
  
  FOR c = 0,n-1 DO BEGIN
     
     nparam = n_elements(fit.(c).param(*))
     
     IF exist(aa_nom) THEN BEGIN 
        IF keyword_set(initial) THEN BEGIN
           
           ;; It is in fact necessary with an if test here...
           IF nparam EQ 1 THEN fit.(c).param(0).initial = aa_nom(nsofar) $
           ELSE fit.(c).param(*).initial = aa_nom(nsofar:nsofar+nparam-1)
           
        END ELSE BEGIN
           
           ;; ..and here
           IF nparam EQ 1 THEN fit.(c).param(0).value = aa_nom(nsofar) $
           ELSE fit.(c).param(*).value = aa_nom(nsofar:nsofar+nparam-1)
           
        ENDELSE
     END
     
     IF keyword_set(include) THEN fit.(c).include = include(c)
     
     IF keyword_set(const) THEN BEGIN
        
        IF nparam EQ 1 THEN fit.(c).param(0).const = const(nsofar) $
        ELSE fit.(c).param(*).const = const(nsofar:nsofar+nparam-1)
        
     END
     
     nsofar = nsofar+nparam
  END
END


