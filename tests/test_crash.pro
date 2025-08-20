PRO test_crash
  catch, err
  IF err NE 0 THEN BEGIN
    catch, /cancel
    print, "Error in test_crash: " + err.tostring()
    help, /not_a_keyword
  END
  a = b
END
