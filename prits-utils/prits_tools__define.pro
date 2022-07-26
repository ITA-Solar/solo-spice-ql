FUNCTION prits_toolbox::init
  return, 1
END


PRO prits_toolbox::cleanup
END 

PRO prits_toolbox::static_test
  compile_opt static
  print, "STATIC_TEST succeeded"
END

  
PRO prits_toolbox__define
  struct = {prits_toolbox, dummy:0b}  
END

