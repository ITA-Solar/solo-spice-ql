; Scalar input is no problem: foreach handles ok, and an
; array of 1 struct is the same as a scalar struct.

FUNCTION spice_keyword_info,keywords,all=all,hash=hash
  k = "FILENAME,COMPRESS,STUDY_ID,OBS_ID,STUDYTYP,STUDYDES,STUDY,"
  w = "      20,       5,       2,    10,       5,      10,   10,"
  t = "       t,       t,       i,     t,       t,       t,    t,"
  
  k += "AUTHOR,PURPOSE,READMODE,SOOPNAME,NWIN,NWIN_PRF,NWIN_DUM,"
  w += "    10,     10,       5,      10,   2,       2,        2,"
  t += "     t,      t,       t,       t,   i,       i,        i,
  
  k += "NWIN_INT"
  w += "       2"
  t += "       n"

  k = trim(strsplit(k,",",/extract))
  w = fix(strsplit(w,",",/extract))
  t = trim(strsplit(t,",",/extract))
  
  display_widths = hash(k,w)
  types = hash(k,t)
  
  info_array = []
  IF keyword_set(all) THEN keywords = k
  foreach keyword,keywords DO BEGIN
     keyword_info = {spice_keyword_info, $
                     keyword:keyword,$
                     display_width:display_widths[keyword],$
                     type:types[keyword] $
                    }
     info_array = [info_array,keyword_info]
  END
  IF keyword_set(hash) THEN return,hash(info_array.keyword,info_array)
  return,info_array
END
