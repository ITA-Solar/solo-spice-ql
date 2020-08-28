; Scalar input is no problem: foreach handles ok, and an
; array of 1 struct is the same as a scalar struct.

FUNCTION spice_keyword_info,keywords,all=all
  kwds = [$
          {SPICE_KEYWORD_INFO, keyword: "FILENAME", display_width: 20, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "DATE-BEG", display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "COMPRESS", display_width:  5, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "STUDY_ID", display_width:  2, type: "i"},$
          {SPICE_KEYWORD_INFO, keyword: "OBS_ID",   display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "STUDYTYP", display_width:  5, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "STUDYDES", display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "STUDY",    display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "AUTHOR",   display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "PURPOSE",  display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "READMODE", display_width:  5, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "SOOPNAME", display_width: 10, type: "t"},$
          {SPICE_KEYWORD_INFO, keyword: "NWIN",     display_width:  2, type: "i"},$
          {SPICE_KEYWORD_INFO, keyword: "NWIN_PRF", display_width:  2, type: "i"},$
          {SPICE_KEYWORD_INFO, keyword: "NWIN_DUM", display_width:  2, type: "i"},$
          {SPICE_KEYWORD_INFO, keyword: "NWIN_INT", display_width:  2, type: "i"} $
         ]
  
  IF keyword_set(all) THEN keywords = kwds[*].keyword
  spice_default, keywords, kwds[*].keyword
  
  keyword_info_hash = orderedhash()
  foreach keyword, keywords, index DO BEGIN
     info = reform(kwds[where(kwds.keyword EQ keyword,count)])
     IF count NE 1 THEN message,"Huh? This shouldn't happen! Should be one and only one match!"
     keyword_info_hash[keyword] = info
  END

  return,keyword_info_hash
END
