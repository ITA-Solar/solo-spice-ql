; idl-disable
PRO WHICH, name, all=all, search=search, outfile=outfile,quiet=quiet
;+
; PROJECT:
;       SOHO - CDS
;
; NAME:
;       WHICH
;
; PURPOSE:
;       Search for and print file or routine in IDL !path
;
; EXPLANATION:
;       Use to find where IDL finds a program file, and, if
;	multiple definitions exist, which is loaded (the first
;	one). Splits path into all different directories,
;	searches for a file with the given NAME + '.PRO'.
;
; CALLING SEQUENCE:
;       WHICH, NAME
;
; INPUTS:
;       NAME - Name of the routine to search for (string scalar).
;
; OPTIONAL INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; OPTIONAL OUTPUTS:
;       outfile - return file name found
;
; KEYWORD PARAMETERS:
;       ALL    - Report all occurrences if set. Usually WHICH reports the
;                first occurrence of a found routine (which is to be
;                executed by IDL). WHICH can take a while, especially on
;                VMS system, to search through all directories (and text
;                libraries on VMS system) if ALL is set. ALL is
;                automatically set if SEARCH is set.
;       SEARCH - Turn on the search mode, if set, that would match any
;                given string pattern found in the path.
;
; CALLS:
;       CONCAT_DIR, STR_CHOP, GREP
;
; COMMON BLOCKS:
;       WHICH -- Mainly for speeding things up
;
; RESTRICTIONS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; CATEGORY:
;       General utility
;
; PREVIOUS HISTORY:
;       Written Stein Vidar Haugan, 1993
;
; MODIFICATION HISTORY:
;       19 May, 1994, SVHH, Doc. added
;	21 May, 1994, SVHH, Version 2, with on_error,2 and 'Use:'
;       Liyun Wang, GSFC/ARC, September 20, 1994
;          Added IDL internal routine checkings.
;       Liyun Wang, GSFC/ARC, October 5, 1994
;          Current directory also gets searched now
;       Version 3, Liyun Wang, GSFC/ARC, December 16, 1994
;          Made it capable of finding files in text libraries on VMS system
;          Added the ALL keyword
;       Version 4, Liyun Wang, GSFC/ARC, January 23, 1995
;          Added the SEARCH keyword
;       Version 5, Liyun Wang, GSFC/ARC, January 24, 1995
;          Used an undocumented IDL function routine ROUTINE_NAMES to get IDL
;             intrinsic routine names.
;       Version 6, Liyun Wang, NASA/GSFC, October 1, 1996
;          Added Windows support
;       Version 7 Add output variable.  CDP, RAL 14-Mar-97
;	Version 8, 23-Oct-1997, William Thompson, GSFC
;		Use OS_FAMILY() instead of !VERSION.OS
;	Version 9, 4-July-1998, Zarro (SAC/GSFC)
;               Returned scalar out variable for single element value
; Version 10, 28-Jun-1999, Zarro (SM&A/GSFC)
;               Added /quiet 
; Version 11, 19-Feb-2016, Zarro (ADNET) 
;        - replaced OPENR test by FILE_TEST
;        - changed () to []
; Version 12, 10-Jul-2025, Stein Haugan (prits-group@astro.uio.no)
;        - Using documented routine_info(/system) instead of undocumented
;          routine_names() to get internal routines
;        - Look up internal routines each call, in case new DLMs have
;          added new routines, and drop common block
; Version 13, 11-Nov-2026, Stein Haugan (prits-group@astro.uio.no)
;        - Introduce caching with hash, taking ALL factors into account
;        - Only printing at the end (yes, goto, but no hidden RETURNs)
;-

   ON_ERROR,2
   loud=~keyword_set(quiet)

;----------------------------------------------------------------------
;  A list of IDL internal routine can be obtained by entering the "?"
;  command at IDL prompt, provided that the current device is set to
;  'tek'.
;----------------------------------------------------------------------

   IF N_ELEMENTS(name) EQ 0 THEN BEGIN
      PRINT, 'WHICH -- Syntax error.'
      PRINT, '   Usage: WHICH, ''filename'' [,/ALL, file=file]'
      PRINT, ' '
      RETURN
   ENDIF

;
;  keep an array of the answers
;
outfile = ''

;----------------------------------------------------------------------
;  In case a file name with an extension is entered, strip off the
;  extension:
;----------------------------------------------------------------------
   ipos =  STRPOS(name,'.')
   IF (ipos NE -1) THEN name = STRMID(name,0,ipos)
   cd,current = cur_dir

   CASE os_family() OF
      'vms': path_sep = ','
      'Windows': path_sep = ';'
      ELSE: path_sep = ':'
   ENDCASE

;----------------------------------------------------------------------
;  First check to see if it is an IDL internal routine. We will call an
;  undocumented IDL function called ROUTINE_NAMES to get all names of
;  the IDL built-in function and procedure names.
;----------------------------------------------------------------------
   pro_names = routine_info(/system)
   func_names = routine_info(/system, /functions)
   internal =  [pro_names, func_names]

;----------------------------------------------------------------------
;  Caching: result is unchanged if ALL of the following are unchanged:
;  * name
;  * all
;  * search
;  * internal routines (can only be added, so just check count)
;  * !path
;  * current WD (since it's effectively part of path)
;  We build a string containing all of the above and use as hash key
;----------------------------------------------------------------------

   COMMON which,cache
   if n_elements(cache) eq 0 then cache = hash()
   n_internals = n_elements(internal)
   paths = cur_dir+path_sep+!path
   all = keyword_set(all)
   search = keyword_set(search)
   cache_key = strjoin([name,trim(all),trim(search),trim(n_internals),paths],', ')
   ; print,"Cache key:",cache_key
   if cache.hasKey(cache_key) then begin
     ; print,"Retrieving from cache"
     outfile = cache[cache_key]
     goto, cleanup__cache_result__return
   end

   IF search THEN BEGIN
      exact = 0
      all = 1
      ff = grep(name, internal)
      IF ff[0] NE '' THEN BEGIN
         temp = arr2str(ff,', ')
         outfile = [outfile,'IDL Built-in: '+temp]
      ENDIF
   ENDIF ELSE BEGIN
      exact = 1
      aa = WHERE(STRUPCASE(name) EQ internal, count)
      IF count NE 0 THEN BEGIN
         outfile = [outfile,STRUPCASE(name)+' is an IDL built-in routine.']
         IF NOT all THEN goto, cleanup__cache_result__return
      ENDIF
   ENDELSE

   dirs = str_sep(paths,path_sep)

   CASE (!version.os) OF
      'vms': BEGIN
         FOR i = 0,N_ELEMENTS(dirs)-1 DO BEGIN
            allfiles = str_chop(STRUPCASE(get_mod(dirs[i])),'.PRO')
            ff = grep(name, allfiles, exact=exact)
            IF (ff[0] NE '') THEN BEGIN
               FOR j = 0, N_ELEMENTS(ff)-1 DO BEGIN
                  filename = concat_dir(dirs[i],ff[j])
                  outfile = [outfile,filename+'.PRO']
               ENDFOR
               IF NOT all THEN GOTO, cleanup
            ENDIF
         ENDFOR
cleanup:
;----------------------------------------------------------------------
;        Clean up scratch files when library is listed
;----------------------------------------------------------------------
         look = findfile('sys$login:*._sdac_*;*',count=nf)
         IF nf GT 0 THEN BEGIN
            FOR i=0,nf-1 DO BEGIN
               spawn,'DELETE/NOLOG/NOCONFIRM '+look[i]
            ENDFOR
         ENDIF
      END
      ELSE: BEGIN
         IF KEYWORD_SET(search) THEN BEGIN
            FOR i = 0, N_ELEMENTS(dirs)-1 DO BEGIN
               filenames = findfile(concat_dir(dirs[i],'*'+name+'*.pro'),$
                           count = cnt)
               IF cnt GT 0 THEN outfile = [outfile,filenames]
            ENDFOR
         ENDIF ELSE BEGIN
            FOR i = 0,N_ELEMENTS(dirs)-1 DO BEGIN
               filename = concat_dir(dirs[i],name+'.pro')
               chk=file_test(filename,/regular)
               IF chk THEN BEGIN
                  outfile = [outfile,filename]
                  IF NOT all THEN goto, cleanup__cache_result__return
               ENDIF 
            ENDFOR
         ENDELSE
      END
   ENDCASE

cleanup__cache_result__return:
;
;  tidy up output variable
;
if n_elements(outfile) gt 1 && outfile[0] eq '' then outfile = outfile[1:*]
if n_elements(outfile) eq 1 then outfile=outfile[0]
print,outfile,format='(a)'
cache[cache_key] = outfile
END
; Testing: uncomment print,"Retrieving from cache"
; which,'which'
; which,'which'
; which,'which',/all
; which,'which',/all
; which,'which',/search
; which,'which',/search
; which,'which',/search,/all
; 
; cd,'/tmp',current=cwd
; which,'which' ; should not cache
; which,'which'
; cd,cwd
; which,'which'
;