function spice_fitshead2struct, head, template, DASH2UNDERSCORE=dash2underscore, $
	   ncomment=ncomment, nhistory=nhistory, add_standard=add_standard, $
           debug=debug,nofill=nofill, wcs=wcs, SILENT=silent, MULTIVALUE=MULTIVALUE, _extra=_extra
;+
;   Name: fitshead2struct
;
;   Purpose: convert FITS header->struct, optionally add useful standard TAGS
;
;   Input Parameters:
;      head - FITs header array (ex: output from <headfits>)
;      template - use this template structure (after first call, for example)
;     
;   Keyword Parameters:
;      ncomment - set maximum number of COMMENT records expected
;      nhistory - set maxinum number of HISTORY records expected 
;                 (default for ncomment and nhistory is acutal# X padding)
;      add_standard - if switch, add some standard tags, including 
;                         TIME, DAY,  MJD fields (if not present)
;                     if add_struct is a STRUCTURE, add these fields
;      wcs      - If set, add WCS structure
;     /DASH2UNDERSCORE - Convert all dashes and dots in keywords to underscore (instead of _D$)
;     /SILENT     Don't print messages
;     /MULTIVALUE - Allow multiple occurences of keywords in the header, only first occurence is preserved.
;
;      Also takes any keywords accepted by FITSHEAD2WCS.
;
;   Calling Sequence:
;      struct=fitshead2struct(header)
;      struct=fitshead2struct(header, /add_standard) ; include SSW standards
;
;   History:
;      24-feb-1997 - S.L.Freeland - based upon some other SSW routines...
;                    (inspired by fits_interp.pro by Barry Labonte)
;      27-feb-1997 - improve ADD_STANDARD logic (use <sswfits_struct> tags)
;                    increased protection against malformed headers
;       9-apr-1997 - Allow user specified structure for ADD_STANDARD 
;      19-Jan-1998 - Added id_esc to the tagname conversion -- CED
;      14-Jun-1998 - Added check for error-causing non-numeric structure fields (Zarro, SAC/GSFC)
;       2-Mar-1999 - Use 'id_unesc' in fxpar calls 
;      19-mar-1999 - per DMZ, assure predefined tags expand to fit input
;      10-Dec-1999 - S.L.Freeland, per Barry Labonte - fixed logic bug
;                    which would 'miss' keywords 1 character in length
;      09-May-2003, William Thompson - Use ssw_strsplit instead of strsplit
;      10-Jul-2003, William Thompson - Treat CROTA1/CROTA2 correctly
;               Use DATE-OBS/TIME-OBS as substitute for DATE_OBS
;      3-Nov-2003, Zarro (L-3/GSFC) - fixed string to integer conversion errors
;      14-Apr-2006, William Thompson, GSFC, Added /WCS keyword
;      26-Oct-2010, Peter W. Schuck, GSFC, Replaced call to MAKE_STR with call
;                   to MRD_STRUCT to allow FITSHEAD2STRUCT to work with 
;                   IDL run-time licenses
;      24-Nov-2010, N.Rich - Implement /DASH2UNDERSCORE and /SILENT 
;      15-sep-2014, S. Freeland - uniq -> ssw_uniq (avoid 8.3/Exelis uniq collision)
;      20-jun-2019, William Thompson - support multivalued distortion keywords
;       9-May-2023, Martin Wiesmann - Add keyword /MULTIVALUE
; 
;   Motivation:
;      use by mreadfits if no structure template is passed 
;      (to permit vectorized FITS file header operations)
;
;   Routines Called:
;      fxpar, fxaddpar, make_str, data_chk, ssw_strsplit, str_replace, 
;      strjustify, fmt_tag, ssw_uniq, rem_elem, prstr, id_esc, fitshead2wcs
;
;   Restrictions:
;      NEED TO RESOLVE DATE-OBS/DATE_OBS mapping
;      (scheduled for 28-feb-1997) (Done, CED 19-Jan-1998)
;-             
retval=''  
if not data_chk(head,/string)  then begin
    message,/info,"Nead FITS header as input
    return,retval
endif

ohead=head                           ; dont corrupt input

; -------- protect against FITS rules violation -------------
badch=where(strpos(strupcase(strcompress(ohead,/remove)),'COMMENT=') eq 0 or $
            strpos(strupcase(strcompress(ohead,/remove)),'HISTORY=') eq 0,bchcnt)
if bchcnt gt 0 then begin
   IF ~keyword_set(SILENT) THEN message,/info,"Correcting COMMENT or HISTORY record rule violation"
   ohead(badch)=str_replace(ohead(badch),'=',' ')
endif
; --------------------------------------------------------------------

; ----- extract multi-lined tags (COMMENT & HISTORY) -------------------
fxaddpar,ohead,'COMMENT','FITSHEAD2STRUCT '
fxaddpar,ohead,'HISTORY','FITSHEAD2STRUCT run at: ' + systime()
comments=fxpar(ohead,'COMMENT',count=ccnt)
history =fxpar(ohead,'HISTORY',count=hcnt) 
if not keyword_set(nhistory) then nhistory=(hcnt > 1) * 5   ; allow flexibility
if not keyword_set(ncomment) then ncomment=(ccnt > 1) * 5
cmax=-1 & hmax=-1
comarr='COMMENT:'+fmt_tag(size(strarr(ncomment)))
hisarr='HISTORY:'+fmt_tag(size(strarr(nhistory)))
; --------------------------------------------------------------------------

; --------------------------------------------------------------------

if not data_chk(template,/struct) then begin
;
;  Use DATE-OBS (and possibly TIME-OBS) as a default for DATE_OBS.
;
   date_obs = fxpar(ohead,'DATE_OBS',count=ndate_obs)
   if ndate_obs eq 0 then begin
       date_obs = fxpar(ohead,'DATE-OBS',count=ndate_obs)
       if ndate_obs eq 1 then begin
           if strlen(date_obs) eq 8 then date_obs =     $       ;Old format
                   anytim2utc(date_obs,/ccsds,/dmy,/date)
           if strlen(date_obs) eq 10 then begin
               time_obs = fxpar(ohead,'TIME-OBS',count=ntime_obs)
               if ntime_obs eq 1 then date_obs = date_obs+'T'+time_obs
           endif
           fxaddpar,ohead,'DATE_OBS',date_obs
       endif
   endif
;
   if keyword_set(add_standard) then begin               ; Add some STANDARDS
       if data_chk(add_standard,/struct) then standard=add_standard else $
          standard=sswfits_struct()                         ; template
       fields=tag_names(standard)                        ; standard tags/fields
       first=where(strpos(strtrim(ohead,2),'NAXIS') eq 0,fcnt)
       after=strtrim((ssw_strsplit(ohead(first),'=',/head))(fcnt-1),2)
;
       for i=0,n_elements(fields)-1 do begin
          tagname = id_unesc(fields(i))
          if tagname eq 'CROTA' then tagname = 'CROTA1'
          if (tagname eq 'CROTA1') or (tagname eq 'CROTA2') then begin
              help, tagname
              value = FLOAT(fxkvalue(ohead,['CROTA1','CROTA2','CROTA','CROT','SC_ROLL']))
              if !err eq -1 then count=0 else count=1
          end else value=fxpar(ohead,tagname,count=count); use existing if present
          if count eq 0 then value=standard.(i)          ; otherwise, from struct
          fxaddpar,ohead,tagname,value, $                ; add param
             after=after,'Ref: sswfits_struct.pro'       ;              
          after=fields(i)                                ; force sequential block
       endfor
   endif    
; --------------------------------------------------------------------

; -------------- some useful cleanup --------------------------------
   deblank=strarrcompress(ohead)        ; version with no null lines
   comp=strcompress(deblank,/remove)    ; compressed version
   epos=strpos(comp,'=')                             ; 
;

   keys=where( epos ge 1 and epos le 9 and (1-strspecial(comp)),kcnt)
   keys=ssw_strsplit(strmid(comp(keys),0,9),'=',/head,tail=rest)

   IF keyword_set(DASH2UNDERSCORE) THEN tags0=str_replace(str_replace(keys,'-','_'),'.','_') $; map into valid chars
   ELSE tags0=keys
   tags = id_esc(tags0)	 			; map all chars to valid ones
   utags=ssw_uniq(tags,sort(tags),/first)

;  -------- eliminate duplicate tags -------           ; DATE_OBS DATE-OBS...
   if n_elements(utags) lt n_elements(tags) then begin
      atags=lindgen(n_elements(tags))
      dtags=rem_elem(atags,utags)
;
;  Test if the duplicate tags are distortion keywords.
;
      test_tags = tags[dtags]
      for k=0,n_elements(test_tags)-1 do $
        if wcs_test_distortion(test_tags[k]) then dtags[k] = -1
      wdtags = where(dtags ge 0, count)
      IF (~keyword_set(SILENT)) and (count gt 0) THEN begin
          dtags = dtags[wdtags]
          prstr,strjustify(['Duplicate Tags: (only preserving first occurence)',$
                            '  ' + tags(dtags)],/center,/box)
      ENDIF
;     ---------------   update ----------------
      order=utags(sort(utags))
      rest=rest(order)
      keys=keys(order)
      tags=tags(order)
      comp=comp(order)
      kcnt=n_elements(order)
;     -----------------------------------------
   endif     

;  -------- create the structure --------------------------------
   structarr=strarr(kcnt)
   for i=0,kcnt-1 do begin
       multivalue_use = keyword_set(MULTIVALUE) || wcs_test_distortion(id_unesc(keys[i]))
       structarr[i]=fmt_tag(size(fxpar(ohead,id_unesc(keys[i]),multivalue=multivalue_use)))
   endfor
   strstr='{dummy,' + arr2str(tags+':'+structarr) + ','+comarr+','+hisarr + '}'
   retval=mrd_struct([tags,'COMMENT','HISTORY'],[structarr,'strarr('+string(ncomment)+')','strarr('+string(nhistory)+')'],1)

;  ------------------------------------------------
endif else begin                                     ; else, use input template
   retval=template                               
   keys=tag_names(template)                          ; use tags
   kcnt = n_tags(template)                           ; how many
endelse
string_tags=intarr(kcnt)                             ; track string tags
not_found=intarr(kcnt)

;  ---------------- fill the structure -------------
if not keyword_set(nofill) then begin
 for i=0,kcnt-1 do  begin
  unesc_key = id_unesc(keys(i))
  multivalue_use = keyword_set(MULTIVALUE) || wcs_test_distortion(unesc_key)
  if strlen(strtrim(unesc_key,2)) le 8 then begin    ;Don't try to process overlong tag names (e.g. WCS_STRUCT)
   temp=fxpar(ohead,unesc_key,count=fcount,multivalue=multivalue_use)
   dtype=datatype(retval.(i),2)
   ok=1
   if (dtype ge 1) and (dtype le 5) then ok=is_number(temp)
   case 1 of 
      n_elements(retval.(i)) eq 1 and n_elements(temp) eq 1: begin
       do_it=1b
       if is_string(temp(0)) then $
        if (not is_number(temp(0))) then $
         if (is_number(retval.(i))) then do_it=0b
       if do_it then retval.(i)=temp(0)
      end
      n_elements(temp) gt n_elements(retval.(i)): $
         retval=boost_tag(temporary(retval),temp,keys(i))
      ok: retval.(i)=temp
   endcase

   string_tags(i)=data_chk(retval.(i),/string)       ; boolean STRINGS?
   not_found(i)=(fcount eq 0)                        ; success??
  endif
 endfor
endif

; trim string tags                 ; default?? /notrim keyword??
string_tags=where(string_tags,stcnt)    
for i=0,stcnt-1 do retval.(string_tags(i))=strtrim(retval.(string_tags(i)),2)

; mapping problem??
ssbad=where(not_found and strpos(keys,'_') ne -1,bcnt)
for i=0,bcnt-1 do retval.(ssbad(i))= $
   fxpar(ohead,str_replace(keys(ssbad(i)),'_','-'))

if tag_index(retval,'COMMENT') ne -1 then cmax=n_elements(retval.COMMENT)
if tag_index(retval,'HISTORY') ne -1 then hmax=n_elements(retval.HISTORY)
if cmax ge 0 and ccnt gt 0 then retval.COMMENT(0:(ccnt < cmax)-1) =  $
                                      comments(0:(ccnt < cmax)-1)
if hmax ge 0 and hcnt gt 0 then retval.HISTORY(0:(hcnt < hmax)-1) =  $
                                       history(0:(hcnt < hmax)-1)

;
;  If requested, add the World Coordinate System structure.  If the structure
;  is already there, then refresh it.
;
if keyword_set(wcs) then begin
    wcs_struct = fitshead2wcs(retval, _extra=_extra)
    if tag_exist(retval,'wcs_struct') then $
      retval.wcs_struct = wcs_struct else $
      retval = add_tag(retval, /top_level, wcs_struct, 'wcs_struct')
endif

; --------------------------------------------------
if keyword_set(debug) then stop,'before return

return,retval
end
