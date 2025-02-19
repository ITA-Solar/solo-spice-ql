; Returns physical path from a relative and/or symlinked path
FUNCTION prits_tools::physical_path, path
  COMPILE_OPT STATIC
  cd, path, current = current
  cd, current, current = physical_path
  return, physical_path
END
