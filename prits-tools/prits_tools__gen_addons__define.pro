FUNCTION prits_tools::list_of_days, start, final, reverse=reverse, delimiter=delimiter
  start_jd = anytim2jd(start + 'T12:00')
  final_jd = anytim2jd(final + 'T12:00')
  
  start_jd = start_jd.int + start_jd.frac
  final_jd = final_jd.int + final_jd.frac
  
  julian_days = timegen(start=start_jd, final=final_jd, units='days')
  IF keyword_set(reverse) THEN julian_days = reverse(julian_days)
  
  ;; See calendar C() format codes in 
  ;; file:///mn/alruba2/astro/local/harris/idl88/help/online_help/help.htm#../Subsystems/idl/
  ;; Content/Creating%20IDL%20Programs/Components%20of%20the%20IDL%20Language/Format_Codes_Fortran.htm
  
  days = string(julian_days, format='(C(CYI, "-", CMOI2.2, "-", CDI2.2))')
  
  IF keyword_set(delimiter) THEN days = days.replace('-', delimiter)
  return, days
END


PRO prits_tools::gen_addons_init
  ; Nothing to do
END

PRO prits_tools__gen_addons__define
  compile_opt static
  vso = {prits_tools__gen_addons, $
         dummy:0b $
        }
END
