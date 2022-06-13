;+
; NAME:
;      SPICE_CREATE_LEVEL3_JPEG
;
; PURPOSE:
;      This function creates images out of level 3 data.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      spice_create_level3_jpeg
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;
; HISTORY:
;      Ver. 1, 13-Jun-2022, Martin Wiesmann
;
;-
; $Id: 2022-06-13 15:24 CEST $


PRO spice_create_level3_jpeg_presentation

  files = spice_find_file('1-jan-2022', time_end='1-jun-2022', count_file=count_file)
  help,files

  goodfiles = []
  goodwinno = []
  goodextname = []
  goodcat = []
  for ifile=0,count_file-1 do begin
    mreadfits_header, files[ifile], hdrtemp, only_tags='PURPOSE,NWIN'
    ;print,hdrtemp.purpose
    if hdrtemp.purpose ne 'Science' then continue
    cur_cat = 5
    for iext=0,hdrtemp.nwin-1 do begin
      mreadfits_header, files[ifile], hdrtemp, only_tags='extname', ext=iext
      ;print,'  '+hdrtemp.extname
      if hdrtemp.extname.contains('C III') || $
        hdrtemp.extname.contains('Ly Beta') || $
        hdrtemp.extname.contains('Lyman Beta') || $
        hdrtemp.extname.contains('Lyb') || $
        hdrtemp.extname.contains('O VI 1032') || $
        hdrtemp.extname.contains('Ne VIII 770') $
        then begin
          
        print,'  '+hdrtemp.extname
        if hdrtemp.extname.contains('C III') then cat = 1
        if hdrtemp.extname.contains('Ly Beta') then cat = 2
        if hdrtemp.extname.contains('Lyman Beta') then cat = 2
        if hdrtemp.extname.contains('Lyb') then cat = 2
        if hdrtemp.extname.contains('O VI 1032') then cat = 3
        if hdrtemp.extname.contains('Ne VIII 770') then cat = 4
        
        if cat lt cur_cat then begin
          cur_cat = cat
          cur_winno = iext
          cur_extname = hdrtemp.extname
        endif
         
      endif ; hdrtemp.extname.contains
    endfor ; iext=0,hdrtemp.nwin-1
    
    if cur_cat lt 5 then begin
      
      if N_ELEMENTS(goodfiles) eq 0 then begin
        goodfiles = files[ifile]
        goodwinno = cur_winno
        goodextname = cur_extname
        goodcat = cur_cat
      endif else begin
        goodfiles = [goodfiles, files[ifile]]
        goodwinno = [goodwinno, cur_winno]
        goodextname = [goodextname, cur_extname]
        goodcat = [goodcat, cur_cat]
      endelse

    endif ; cur_cat lt 5

    ;stop
  endfor ; ifile=0,count_file-1
  
  help,goodfiles
  sorted_index = sort(goodcat)
  goodfiles = goodfiles[sorted_index]
  goodwinno = goodwinno[sorted_index]
  goodextname = goodextname[sorted_index]
  goodcat = goodcat[sorted_index]
  print,goodcat, goodextname
END
