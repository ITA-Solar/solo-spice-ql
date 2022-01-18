;+
; Project     : SOHO - CDS     
;                   
; Name        : GET_CALLDETAILS()
;               
; Purpose     : Return details of calling program (at any stack depth)
;               
; Explanation : The HELP,CALLS=CALLS utility is nice, but it's often a bit
;               awkward to extract information.  This routine returns the
;               information in a more edible form, as a structure:
;
;               {GET_CALLDETAILS_STC,
;                TEXT   : The full text of CALLS(DEPTH)
;                MODULE : The procedure or function name of CALLS(DEPTH)
;                FILE   : The source file for the procedure 
;                LINENO : The line number of the calling statement
;                DEPTH  : The depth used (default 2)
;                TOTALDEPTH : The maximum possible depth allowed in this call} 
;               
;                Depth=0 means *this* program (GET_CALLDETAILS) 
;                      1 means the caller of GET_CALLDETAILS
;                      2 means the caller of the program calling 
;                        GET_CALLDETAILS (DEFAULT)
;                      3 means the caller of the caller of the program calling
;                        GET_CALLDETAILS ... etc
;
; Use         : STC = GET_CALLDETAILS( [ DEPTH ] )
; 
; Inputs      : None required
;
; Opt. Inputs : DEPTH : See Explanation
;
; Outputs     : Returns information structure.
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
; Category    : General.
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 6 May 1998
;               
; Modified    : Not yet.
;
; Version     : 1, 6 May 1998
;
; $Id: 2020-11-25 21:19 CET $
;-            

FUNCTION spice_get_calldetails,depth
  
  IF n_elements(depth) EQ 0 THEN depth = 2
  
  help,calls=calls
  
  IF depth GE n_elements(calls) THEN message,"Depth exceeds call stack"
  
  IF depth LT 0 THEN message,"Do whatever you want! I can't foretell it"
  
  line = calls(depth)
  
  IF strpos(line,'$MAIN$') EQ 0 THEN $
     line = '$MAIN$ <$MAIN$(  0)>'
  
  blank = strpos(line,' ')
  langle = strpos(line,'<')
  rangle = strpos(line,'>')
  lparen = strpos(line,'(')
  rparen = strpos(line,')')
  
  module = strmid(line,0,blank)
  file = strmid(line,langle+1,lparen-(langle+1))
  lineno = long(strmid(line,lparen+1,rparen-(lparen+1)))
  
  stc = {get_calldetails_stc,$
         text:line,$
         module:module,$
         file:file,$
         lineno:lineno,$
         depth:depth,$
         totaldepth:n_elements(calls)-1}
  return,stc
  
END
