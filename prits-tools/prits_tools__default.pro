;+
; Project     : PRITS-TOOLS
;                   
; Name        : PRITS_TOOLS.DEFAULT
;               
; Purpose     : Supply default values for variables
;               
; Explanation : If the first parameter is not defined, it is
;		set to the value of the second parameter.
;               
; Use         : PRITS_TOOLS.DEFAULT,VARIABLE,DEFAULT_VALUE
;    
; Inputs      : VARIABLE : The variable that could take on the default value
;
;		DEFAULT_VALUE : The default value.
;               
; Opt. Inputs : None.
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : None.
;
; Common      : None.
;               
; Restrictions: None.
;               
; Side effects: None.
;               
; Category    : Utility, Misc.
;               
; Prev. Hist. : From CDS routine DEFAULT, taken from my private library.
;
; Written     : Stein Vidar Hagfors Haugan, 1995-09-04
;               
; Modified    : Never
;
; Version     : 2, 2023-04-07
;-            

PRO PRITS_TOOLS::DEFAULT,VAR,VAL
  compile_opt static

  If N_params() lt 2 then message,"Use: DEFAULT,VARIABLE,DEFAULT_VALUE"
  
  IF N_ELEMENTS(VAR) EQ 0 THEN VAR=VAL
  
END
