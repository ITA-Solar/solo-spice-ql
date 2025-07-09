;+
; Project     : SOHO - CDS     
;                   
; Name        : PRINT_CFIT
;               
; Purpose     : Print some contents of a Component Fit structure
;               
; Explanation : This routine prints the contents of a component fit structure,
;               either in the form of a list of components' parameters and
;               their values, or in the form of a series of IDL statements
;               necessary to build up the structure inside a program.
;               
; Use         : PRINT_CFIT,CFIT
;    
; Inputs      : CFIT : Component Fit structure
;               
; Opt. Inputs : None.
;               
; Outputs     : To screen
;               
; Opt. Outputs: None.
;               
; Keywords    : INITIAL : Set this to use the initial values instead of
;                         current values.
;
;               PROGRAM : Set this keyword to produce output in the form of
;                         IDL statements building a similar CFIT structure.
;
; Calls       : trim()
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
; Modified    : Version 2, SVHH, 5 February 1997
;                       Made values & trans_b parameters DOUBLES before
;                       trim()'ing them, to have more significant figures.
;                       
; Version     : 2, 5 February 1997
;-            

FUNCTION print_cfit_comptext,comp,trimmed=trimmed,initial=initial,$
                             program=program
  
  IF keyword_set(program) THEN BEGIN
     txt = 'mk_'+comp.func_name+'(['
     nparm = n_elements(comp.param)
     max_arr = "max_arr=["
     min_arr = "min_arr=["
     trans_a = "trans_a=["
     trans_b = "trans_b=["
     const = "const=["
     FOR i = 0,n_elements(comp.param)-1 DO BEGIN
        parm = comp.param(i)
        IF keyword_set(initial) THEN v = parm.initial $
        ELSE                         v = parm.value
        IF i LT nparm-1 THEN sep = "," ELSE sep = ""
        txt = txt+trim(double(v))+sep
        max_arr = max_arr + trim(parm.max_val) + sep
        min_arr = min_arr + trim(parm.min_val) + sep
        trans_a = trans_a + trim(parm.trans_a) + sep
        trans_b = trans_b + trim(double(parm.trans_b)) + sep
        const = const + trim(fix(parm.const)) + "b" + sep
     END
     txt = txt + "],$" ;; Necessary to continue function call
     max_arr = max_arr + "],"
     min_arr = min_arr + "],$"
     IF strlen(max_arr)+strlen(min_arr) LT 76 THEN BEGIN
        minmax = max_arr + min_arr
     END ELSE BEGIN
        minmax = [max_arr +"$",min_arr]
     END
     trans_a = trans_a + "],"
     trans_b = trans_b + "],$"
     IF strlen(trans_a)+strlen(trans_b) LT 76 THEN BEGIN
        trans = trans_a + trans_b
     END ELSE BEGIN
        trans = [trans_a+"$",trans_b]
     END
     const = const + "])"
     txt = [txt,'    '+[minmax,trans,const]]
     return,txt
  END
        
  txt = strupcase(comp.name)+': '
  
  FOR i = 0,n_elements(comp.param)-1 DO BEGIN
     IF keyword_set(initial) THEN v = trim(double(comp.param(i).initial)) $
     ELSE                         v = trim(double(comp.param(i).value))
     IF keyword_set(trimmed) THEN t = trim(v) $
     ELSE                         t = string(v)
     txt = txt + comp.param(i).name + "="+t
     IF i NE n_elements(comp.param)-1 THEN txt = txt + ", "
  END
  
  return,txt
END


PRO print_cfit,fit,initial=initial,program=program
  
  name = tag_names(fit)
  names = n_elements(name)
  
  tx = ['']
  
  qq = !quiet
  !quiet = 1
  
  FOR i = 0,names-1 DO BEGIN
     IF keyword_set(program) THEN BEGIN
        print,name(i)+" = ",format="($,A)"
     END
     
     print,print_cfit_comptext(fit.(i),initial=initial,program=program),$
        format="(A)"
     
     IF keyword_set(program) THEN BEGIN
        print,name(i)+".name = '"+fit.(i).name+"'"
     END
     
  END
  
  space =  "        "
  
  IF keyword_set(program) THEN BEGIN
     FOR i = 0,names-1 DO BEGIN
        IF i EQ 0 THEN pretext = "fit = { " ELSE pretext = space
        IF i LT names-1 THEN sep = ",$" ELSE sep = "}"
        print,pretext+name(i)+" : "+name(i)+sep
     END
  END
  
  !quiet = qq
  
END

