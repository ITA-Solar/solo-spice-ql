FUNCTION prits_tools::julday2iso, juldays, time_zone = time_zone, decimals = decimals, delimiter = delimiter
  COMPILE_OPT STATIC
  pt = prits_tools.singleton_instance()
  pt.default, time_zone, 'Z'
  pt.default, decimals, 0
  pt.default, delimiter, '-'

  ; ; See calendar C() format codes in
  ; ; file:///mn/alruba2/astro/local/harris/idl88/help/online_help/help.htm#../Subsystems/idl/
  ; ; Content/Creating%20IDL%20Programs/Components%20of%20the%20IDL%20Language/Format_Codes_Fortran.htm

  date = ('CYI, "-", CMOI2.2, "-", CDI2.2').replace('-', delimiter)
  hhmm = 'CHI2.2, ":", CMI2.2'
  ss = 'CSI2.2'
  IF keyword_set(decimals) THEN ss = 'CSF0' + (decimals + 3).tostring() + '.' + decimals.tostring()
  time = hhmm + ', ":", ' + ss

  format = '(C(' + date + ', "T", ' + time + '))'

  iso = string(juldays, format = format) + time_zone

  return, iso
END

FUNCTION prits_tools::anytim2julday, anytim
  COMPILE_OPT STATIC
  jd = anytim2jd(anytim)
  return, jd.int + jd.frac
END

FUNCTION prits_tools::list_of_days, start, final, reverse_list = reverse_list, delimiter = delimiter
  COMPILE_OPT STATIC

  addon = strpos(start, 'T') EQ -1 ? 'T12:00' : ''

  start_jd = prits_tools.anytim2julday(start + addon)
  final_jd = prits_tools.anytim2julday(final + addon)

  julian_days = timegen(start = start_jd, final = final_jd, units = 'days')
  IF keyword_set(reverse_list) THEN julian_days = reverse(julian_days)

  dates = prits_tools.julday2iso(julian_days, delimiter = delimiter)

  return, strmid(dates, 0, 10)
END

PRO prits_tools::date_addons_init
  ; Nothing to do
END

PRO prits_tools__date_addons__define
  COMPILE_OPT STATIC
  !NULL = {prits_tools__date_addons, $
    dummy: 0b $
    }
END
