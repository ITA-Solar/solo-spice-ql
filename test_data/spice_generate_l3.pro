pro spice_generate_l3

  meta_data_file = '/Users/mawiesma/Documents/spice/generate_l3_meta_data.sav'
  meta_data_template = { $
    file:'', $
    winno:0, $
    extname:'', $
    category:0, $
    l3_created:0b, $
    l3_file:'', $
    image_small_created:0b, $
    image_large_created:0b $
  }

  if ~file_exist(meta_data_file) then begin

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
    
    ndata = N_ELEMENTS(goodfiles)
    meta_data = make_array(ndata, value=meta_data_template)
    for i=0,ndata-1 do begin
      meta_data[i].file = goodfiles[i]
      meta_data[i].winno = goodwinno[i]
      meta_data[i].extname = goodextname[i]
      meta_data[i].category = goodcat[i]
    endfor ; i=0,N_ELEMENTS(goodfiles)-1
    save, meta_data, filename=meta_data_file

  endif else begin ; ~file_exist(meta_data_file)
    
    restore, meta_data_file
    ndata = N_ELEMENTS(meta_data)
    meta_data[*].l3_created = 0
    
  endelse ; ~file_exist(meta_data_file)
  
  
  for idata=0,ndata-1 do begin
    
    print, meta_data[idata].file
    print, '    ', meta_data[idata].extname
    if meta_data[idata].l3_created then continue
    
    print, '    ... Creating level 3 ...'
    spice_object = spice_object(meta_data[idata].file)
    l3_file = spice_object->create_l3_file(meta_data[idata].winno, /no_widget)
    print, '    ... Creating level 3 done ...'
    print, '  '
    
    meta_data[idata].l3_created = 1b
    meta_data[idata].l3_file = l3_file
    save, meta_data, filename=meta_data_file
    
    spice_create_level3_jpeg_presentation, meta_data[idata]
    
  endfor ; idata=0,ndata-1

end
