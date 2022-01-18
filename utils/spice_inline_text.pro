FUNCTION spice_inline_text_help
  forward_function spice_inline_text & return,inline_text(';-')
;+
; Project     : SOHO - CDS     
;                   
; Name        : INLINE_TEXT()
;               
; Purpose     : Return inline text immediately following call line
;               
; Explanation : Returns verbatim text immediately following the line calling
;               INLINE_TEXT, up to and including the line that contains the
;               end MARKER.
;
;               Uses GET_CALLDETAILS to find out the file name and the line
;               number of the calling line.
;
;               The default MARKER is ';-' (the standard "end of
;               documentation" marker in the library routines).
;
; Use         : TEXT = INLINE_TEXT( [MARKER] )
; 
; Inputs      : None required
;
; Opt. Inputs : MARKER : The text appearing at the *beginning* of the last
;                        line of inline text.
;
; Outputs     : Returns the inline text, or message about failure to read.
;               
; Opt. Outputs: None.
;               
; Keywords    : None.
;
; Calls       : GET_CALLDETAILS()
;
; Common      : None.
;               
; Restrictions: Needs to find the program source file!
;               
; Side effects: None.
;               
; Category    : General.
;               
; Prev. Hist. : None.
;
; Written     : S.V.H.Haugan, UiO, 31 July 1998
;               
; Modified    : Not yet.
;
; Version     : 1, 31 July 1998
;
; $Id: 2020-11-25 21:19 CET $
;-            
END


FUNCTION spice_inline_text,marker
  
  ;; Default maker ';-'
  
  IF n_params() EQ 0 THEN marker = ';-'
  
  ;; Get caller details 
  
  info = spice_get_calldetails()
  
  ;; Return message instead of inline text in case of trouble
  
  ON_IOERROR,abort
  
  openr,lun,info.file,/get_lun
  
  tx = strarr(info.lineno)
  readf,lun,tx
  
  start = fstat(lun)
  
  nlines = 0
  
  tx = ''
  
  REPEAT BEGIN
     readf,lun,tx
     nlines = nlines+1
  END UNTIL strpos(tx,marker) EQ 0
  
  point_lun,lun,start.cur_ptr
  
  text = strarr(nlines)
  readf,lun,text
  
  close,lun
  free_lun,lun
  
  return,text
  
  
abort:
  
  IF n_elements(lun) EQ 1 THEN BEGIN
     close,lun
     free_lun,lun
  END
  
  return,['Error in reading inline text from file :'+info.file]
  
END

