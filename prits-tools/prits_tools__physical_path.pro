PRO prits_tools__physical_path, path
  cwd, path, current = current
  cwd, current, current = physical_path
  return, physical_path
END
