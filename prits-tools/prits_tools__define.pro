FUNCTION prits_tools::init
  COMMON prits_tools, first_pt
  IF n_elements(first_pt) EQ 0 THEN first_pt = self
  self.vso_addons_init
  return, 1
END


PRO prits_tools::cleanup
END 

PRO prits_tools::static_test
  compile_opt static
  print, "PRITS_TOOLS.STATIC_TEST succeeded"
END

  
PRO prits_tools__define
  vso = prits_tools_vso_addons()
  struct = {prits_tools, vso:vso, dummy:0b}  
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   prits_tools.static_test
END 

END
  
