PRO spice_test_progress
  test = 2

  IF test EQ 1 THEN BEGIN
    files = ['/Users/ich/data/1-asdfa', '/Users/ich/data/2-wrgwrgasdf', '/Users/ich/data/qer/3-wer', '/Users/ich/data/asdf/4-asdf']
    n_windows = [2, 3, 4, 2]
    name_windows = ['WIN 1', 'WIN 2', 'WIN 3', 'WIN 4', 'WIN 5'] ; idl-disable-line unused-var

    o = spice_create_l3_progress(files = files)

    FOR i = 0, n_elements(files) - 1 DO BEGIN
      print, 'new file', i
      o.next_file, n_windows[i], filename = 'WRONG', halt = halt
      IF halt THEN BEGIN
        print, 'stopped'
        return
      ENDIF
      wait, 2

      FOR j = 0, n_windows[i] - 1 DO BEGIN
        print, 'new window', j
        ; o->next_window, window_name=name_windows[j], halt=halt
        o.next_window, halt = halt
        IF halt THEN BEGIN
          print, 'stopped'
          return
        ENDIF
        wait, 2
      ENDFOR
    ENDFOR
  ENDIF ELSE IF test EQ 2 THEN BEGIN
    n_files = 20
    o = spice_create_l3_progress(n_files)

    FOR i = 0, n_files - 1 DO BEGIN
      print, 'new file', i
      n_windows = ceil(randomu(seed) * 10) ; idl-disable-line unused-var
      o.next_file, n_windows, halt = halt, filename = 'done'
      IF halt THEN BEGIN
        print, 'stopped'
        return
      ENDIF
      wait, 1

      FOR j = 0, n_windows - 1 DO BEGIN
        print, 'new window', j
        o.next_window, halt = halt
        IF halt THEN BEGIN
          print, 'stopped'
          return
        ENDIF
        wait, 0.5
      ENDFOR
    ENDFOR
  ENDIF ELSE BEGIN
    print, 'test not defined yet'
  ENDELSE
END
