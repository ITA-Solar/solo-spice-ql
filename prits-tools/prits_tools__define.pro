FUNCTION prits_tools::init
  return, 1
END


PRO prits_tools::cleanup
END 

PRO prits_tools::static_test
  compile_opt static
  print, "PRITS_TOOLS.STATIC_TEST succeeded"
END

  
PRO prits_tools__define
  struct = {prits_tools, dummy:0b}  
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   prits_tools.static_test
END 
END
