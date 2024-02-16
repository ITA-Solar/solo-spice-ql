pro spice_test_progress

  test = 2

  IF test eq 1 then begin


    files = ['/Users/ich/data/1-asdfa','/Users/ich/data/2-wrgwrgasdf','/Users/ich/data/qer/3-wer','/Users/ich/data/asdf/4-asdf']
    n_windows = [2, 3, 4, 2]
    name_windows = ['WIN 1', 'WIN 2', 'WIN 3', 'WIN 4', 'WIN 5']

    o=spice_create_l3_progress(files=files)

    for i=0,N_ELEMENTS(files)-1 do begin
      print,'new file',i
      o->next_file, n_windows[i], filename='WRONG', window_name=name_windows[0], halt=halt
      if halt then begin
        print,'stopped'
        return
      endif
      wait, 2

      for j=0,n_windows[i]-1 do begin
        print,'new window',j
        ;o->next_window, window_name=name_windows[j], halt=halt
        o->next_window, halt=halt
        if halt then begin
          print,'stopped'
          return
        endif
        wait, 2
      endfor

    endfor


  endif else if test eq 2 then begin


    n_files = 20
    o=spice_create_l3_progress(n_files)


    for i=0,n_files-1 do begin
      print,'new file',i
      n_windows = ceil(randomu(seed)*10)
      o->next_file, n_windows, halt=halt, filename='done'
      if halt then begin
        print,'stopped'
        return
      endif
      wait, 1

      for j=0,n_windows-1 do begin
        print,'new window',j
        o->next_window, halt=halt
        if halt then begin
          print,'stopped'
          return
        endif
        wait, 0.5
      endfor

    endfor


  endif else begin
    print, 'test not defined yet'
  endelse


end
