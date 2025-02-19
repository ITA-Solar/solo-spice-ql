; Returns physical path from a relative and/or symlinked path
FUNCTION prits_tools::physical_path, path
  COMPILE_OPT STATIC
  cwd, path, current = current
  cwd, current, current = physical_path
  return, physical_path
END
