FUNCTION oslo_fits_util::init
  return,1
END

PRO oslo_fits_util::cleanup
END

PRO oslo_fits_util::add,hdr,keyword,value,comment, before=before, after=after
  ;;
  ;;+
  ;; Adds KEYWORD with VALUE and COMMENT to HDR. If KEYWORD is not set, a
  ;; placeholder (the string "-[number]-") will be inserted instead. If clean_header
  ;; is run, all placeholder strings will be replaced by an empty string.
  ;;
  ;;-
  ;;
  COMMON add_fitsparam,blankno
  default,blankno,1
  IF blankno EQ 2^15-1 THEN blankno =  1 ;; to prevent the next blankno to become -32768

  default,keyword,''
  default,value,''
  default,comment,''

  IF comment NE '' THEN comment = ' '+comment
  IF keyword EQ '' THEN BEGIN
    keyword = '-'+trim(blankno)+'-'
    blankno = blankno+1
  END
  IF keyword EQ 'COMMENT' THEN BEGIN
    keyword = '+'+trim(blankno)+'+'
    blankno = blankno + 1
  END

  ; Simple case: single value
  ;
  IF n_elements(value) EQ 1 THEN BEGIN
    fxaddpar,hdr,keyword,value,comment, before=before, after=after
    return
  END

  ; Several values (comments)
  ;
  FOR i=0,n_elements(value)-1 DO BEGIN
    self.add,hdr,keyword,value[i],comment, before=before, after=after
  END
END


PRO oslo_fits_util::add_description, hdr, description
  ;;
  ;; Adds an "empty" line to the HDR, followed by a DESCRIPTION string
  ;; surrounded by a decorative dashed box. The keyword entries of these lines
  ;; will hold "-[number]-" placeholder strings, and will be sustituted by an
  ;; empty string after ::clean_header is run.
  ;;
  linelen = strlen(description)+4
  line = strjoin(replicate('-', linelen))

  self.add,hdr,'','',''
  self.add,hdr,'','',''
  self.add,hdr,'',line,''
  self.add,hdr,'','| ' + description + ' |'
  self.add,hdr,'',line,''
END


PRO oslo_fits_util::_remove_from_header, hdr, keyword=keyword, description=description,  $
  n_lines_to_remove_after=n_lines_to_remove_after, remove_comment=remove_comment
  ix = keyword_set(keyword) ? where(trim(strmid(hdr,0,8)) EQ keyword,/NULL) : where(hdr.contains(description),/NULL)
  IF ix EQ !NULL THEN return

  n_lines_to_be_removed = (keyword_set(keyword)) ? 1 : 3 + n_lines_to_remove_after

  continued = (hdr[ix].substring(-2) EQ "&'")
  WHILE continued DO BEGIN
    continued = (hdr[ix+n_lines_to_be_removed].substring(0,7) EQ "CONTINUE")
    IF continued THEN n_lines_to_be_removed++
  ENDWHILE

  IF keyword_set(remove_comment) THEN BEGIN
    comment = trim(strmid(hdr[ix+n_lines_to_be_removed],0,7)) EQ 'COMMENT'
    WHILE comment DO BEGIN
      n_lines_to_be_removed++
      comment = trim(strmid(hdr[ix+n_lines_to_be_removed],0,7)) EQ 'COMMENT'
    ENDWHILE
  ENDIF
  
  header_without = make_array(n_elements(hdr)-n_lines_to_be_removed,/string)

  end_ix_before  = (keyword_set(keyword)) ? ix-1 : ix-2
  start_ix_after = (keyword_set(keyword)) ? ix+n_lines_to_be_removed : ix+2+n_lines_to_remove_after
  IF end_ix_before LT 0 THEN header_without = hdr[start_ix_after:*] $
  ELSE IF start_ix_after GE n_elements(hdr) THEN header_without = hdr[0:end_ix_before] $
  ELSE header_without = [hdr[0:end_ix_before],hdr[start_ix_after:*]]
  hdr = header_without

END


PRO oslo_fits_util::remove_description, hdr, description, n_lines_to_remove_after=n_lines_to_remove_after
  self->_remove_from_header, hdr, description=description, n_lines_to_remove_after=n_lines_to_remove_after
END


PRO oslo_fits_util::remove_keyword, hdr, keyword, n_lines_to_remove_after=n_lines_to_remove_after, remove_comment=remove_comment
  self->_remove_from_header, hdr, keyword=keyword, n_lines_to_remove_after=n_lines_to_remove_after, remove_comment=remove_comment
END


PRO oslo_fits_util::clean_header,hdr
  ;;
  ;; Substitute placeholder "-[number]-" strings with blank fields
  ;;
  s = stregex(hdr,'[-][1-9][0-9]*[-] *= ')
  ix = where(s EQ 0,count)
  FOR i=0,count-1 DO BEGIN
    tmp = strmid(hdr[ix[i]],11,1000)
    tmp = str_replace(tmp,'''',' ')
    tmp = str_replace(tmp,'/',' ')
    hdr[ix[i]] = '           '+tmp
  END

  s = stregex(hdr,'[+][1-9][0-9]*[+] *= ')
  ix = where(s EQ 0,count)
  FOR i=0,count-1 DO BEGIN
    tmp = strmid(hdr[ix[i]],11,1000)
    tmp = str_replace(tmp,'''',' ')
    tmp = str_replace(tmp,'/',' ')
    hdr[ix[i]] = 'COMMENT    '+tmp
  END

END


PRO oslo_fits_util__define
  d = {oslo_fits_util, dummy:0}
END
