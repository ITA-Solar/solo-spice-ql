PRO nuke_path, path
  paths = str_sep(!path, ':')
  ix = where(paths NE path)
  !path = strjoin(paths[ix], ':')
END
!quiet = 1
  
  rm_path, '$HOME/sf', /expand
  rm_path, '$HOME/solo-spice-ql', /expand
  rm_path, '$HOME/spice-svn-wd', /expand
  
  ssw = getenv("SSW")
  nuke_path, ssw + '/hinode/eis/idl/atest'
  nuke_path, ssw + '/hinode/sot/idl/atest'
  nuke_path, ssw + '/hinode/xrt/idl/atest'
  nuke_path, ssw + '/iris/idl/atest'
  nuke_path, ssw + '/packages/sunspice/idl/atest'
  nuke_path, ssw + '/sdo/aia/idl/atest'
  nuke_path, ssw + '/so/spice/idl/atest'
  nuke_path, ssw + '/soho/cds/idl/atest'
  nuke_path, ssw + '/soho/eit/idl/atest'
  nuke_path, ssw + '/vobs/ontology/idl/atest'
  
  rm_path, ssw + '/hinode', /expand
  rm_path, ssw + '/iris', /expand
  rm_path, ssw + '/packages/sunspice', /expand
  rm_path, ssw + '/packages/azam', /expand
  rm_path, ssw + '/sdo', /expand
  rm_path, ssw + '/so', /expand
  rm_path, ssw + '/soho/cds', /expand
  rm_path, ssw + '/soho/eit', /expand
  rm_path, ssw + '/soho/gen', /expand
  rm_path, ssw + '/trace', /expand
  rm_path, ssw + '/vobs', /expand
  rm_path, ssw + '/spartan', /expand
 
  add_path, '$HOME/solo-spice-ql/ssw_export', /prepend
  add_path, '$HOME/idl/solo-spice-ql/ssw_exports', /prepend
  
  print, "", "", "", format='(a)'
  print,"  " + str_sep(!PATH,':'),format='(a)'
  
  resolve_routine, "xcfit_block2", /compile_full_file
  xcfit_block_test2
END
