;+
; NAME:
;      RGET_FETCH_FILES
;
; PURPOSE:
;
;      Given a URL pointing to a remote directory and the path to an existing
;      local directory, an inventory of the files contained in the local
;      directory is created using RGET_MAKE_LIST (see that for an explanation
;      of what an RGET-LIST contains).
;
;      A corresponding list is fetched from the remote web server (named
;      RGET-LIST, located in the top directory). The two lists are then
;      compared, and any file/directory that is not in the local directory is
;      fetched using curl. For files there is also a comparison of the file
;      ctime, the size, and the execute bit (ignored on Windows). If any of
;      those do not match, the file is fetched.
;
; CATEGORY:
;      GENERAL/UTILITY
;
; CALLING SEQUENCE:
;      RGET_FETCH_FILES, url_to_remote_top_dir, path_to_local_top_dir
;
; INPUTS:
;      url_to_remote_top_dir: URL pointing to the remote top directory to be
;                             mirrored.
;
;      path_to_local_top_dir: Path to the local top directory (destination)
;
; OUTPUTS:
;      None.
;
; HISTORY:
;      Ver. 1, January 2021, Stein Haugan
;-

FUNCTION rget_fetch_files::init, top_url, top_dir, username=username, password=password, $
                                 debug=debug, verbose=verbose
  dprint = self.rget_dprint::init(debug=debug, verbose=verbose)
  
  IF n_elements(username) EQ 0 THEN username = ''
  IF n_elements(password) EQ 0 THEN password = ''
  
  self.d = dictionary()
  
  full_topdir = (file_search(top_dir, /fully_qualify_path, /test_directory, /mark_directory))[0]
  self.d.full_topdir = full_topdir.replace('\','/')
  is_directory = self.d.full_topdir.endswith('/')
  IF NOT is_directory THEN message, "Destination "  + top_dir + " is NOT a directory"
  
  IF NOT top_url.endswith('/') THEN top_url += '/'
  self.d.top_url = top_url
  
  self.d.username = username
  self.d.password = password
  
  self.make_fetch

  return, 1
END

;; IDL 8.5 messes up the (DY)LD_LIBRARY_PATHs - before spawning they
;; must be blanked to run normal programs reliably
;;
PRO rget_fetch_files::save_and_blank_library_paths
  self.d.ld_library_path = getenv("LD_LIBRARY_PATH")
  setenv, "LD_LIBRARY_PATH="
  self.d.dyld_library_path = getenv("DYLD_LIBRARY_PATH")
  setenv, "DYLD_LIBRARY_PATH="
END

PRO rget_fetch_files::restore_library_paths
  setenv, "LD_LIBRARY_PATH=" + self.d.ld_library_path
  setenv, "DYLD_LIBRARY_PATH=" + self.d.dyld_library_path
END
  
  
FUNCTION rget_fetch_files::curl_credentials
  credentials = self.d.username
  IF credentials THEN credentials += ':' + self.d.password
  IF credentials THEN BEGIN
     quotes = !version.os_family.tolower() eq "windows" ? '"' : "'"
     credentials = "--user " + quotes + credentials + quotes
  END
  return,credentials
end


PRO rget_fetch_files::create_file_if_necessary, temp_file, is_zero_length
  IF file_test(temp_file) THEN return
  IF NOT is_zero_length THEN BEGIN
     self.info, "** Non-zero-length source file did not arrive, but curl status was 0", level = -2
     self.info, "** Creating a zero-length file " + temp_file, level = -2
  END ELSE BEGIN
     self.info, "** Creating zero-length file " + temp_file, level = 1
  END
  openw, lun, temp_file, /get_lun
  free_lun, lun
END


FUNCTION rget_fetch_files::fetch_file, path, filename, is_zero_length
  url = self.d.top_url + path
  credentials = self.curl_credentials()
  temp_file = filename+'.rget-tmp'
  curl = "curl --fail --remote-time"
  curl += " -o " + temp_file
  curl += " " + credentials
  curl += " " + url
  self.dprint,"Executing: "+curl
  
  self.save_and_blank_library_paths
  spawn,curl,result,err,exit_status=exit_status, /null_stdin
  self.restore_library_paths
  
  IF exit_status EQ 0 THEN BEGIN
     self.create_file_if_necessary, temp_file, is_zero_length
     file_move, temp_file, filename, /overwrite
     return,1
  END
  ;; We don't want credentials to be piped to a log file:
  IF credentials THEN curl = curl.replace(credentials,'--user <username>:<password>')
  self.info, "** Error fetching "+url, level = -2
  self.info, "** "+curl, level = -2
  self.info, "** curl exit status: "+exit_status.toString(), level = -2
  self.info, "", level = -2
  return, 0
END


FUNCTION rget_fetch_files::file_is_ok, relative_path, remote_rget_file
  is_ok_so_far = self.d.local_hash.haskey(relative_path)
  IF is_ok_so_far THEN BEGIN
     local_rget_file = self.d.local_hash[relative_path]
     time_matches =  local_rget_file.time EQ remote_rget_file.time
     size_matches = local_rget_file.size EQ remote_rget_file.size
     exec_matches = local_rget_file.exec EQ remote_rget_file.exec
     ignore_exec = !version.os_family.tolower() eq "windows"
     IF NOT exec_matches AND ignore_exec THEN BEGIN
        self.info,"Ignoring exec diff on Windows: " + relative_path
     END
     exec_matches = exec_matches OR ignore_exec
     is_ok_so_far = time_matches AND time_matches AND exec_matches
  END
  return, is_ok_so_far
END

PRO rget_fetch_files::maybe_fetch_file, relative_path, remote_rget_file
  file_is_ok = self.file_is_ok(relative_path, remote_rget_file)
  IF file_is_ok THEN BEGIN  
     self.info, "Leave alone: " + relative_path, level = 1
     return
  END
  remote_size_string = " (" + remote_rget_file.size.tostring() + "b)"
  remote_exe_string = remote_rget_file.exec EQ "x" ? " (executable)" : ""
  self.info, "Fetching " + relative_path + remote_size_string + remote_exe_string
  is_zero_length = remote_rget_file.size EQ 0
  output_path = self.d.full_topdir + relative_path
  result = self.fetch_file(relative_path, output_path, is_zero_length)
  IF result THEN file_chmod, output_path, a_execute=remote_rget_file.exec EQ "x"
  file_chmod, output_path, /u_write
END


PRO rget_fetch_files::make_directory, directory
  self.info, "Making directory " + self.d.full_topdir + directory
  file_mkdir, self.d.full_topdir + directory
END


PRO rget_fetch_files::do_fetches
  foreach remote_entry, self.d.remote_hash, rem_key DO BEGIN
     remote_key = rem_key
     rem_key = rem_key + ''
     entry_type = typename(remote_entry)
     is_dir = remote_key.endswith('/') AND entry_type EQ "UNDEFINED"
     CASE 1 OF
        is_dir                            : self.make_directory, remote_key
        entry_type EQ "RGET_FILE_STRUCT"  : self.maybe_fetch_file, remote_key, remote_entry
     END
  END
END


PRO rget_fetch_files::do_deletes
  ;; Reverse order to ensure files are deleted before directories
  local_keys_reverse = reverse((self.d.local_hash.keys()).toarray())
  foreach key, local_keys_reverse DO BEGIN
     do_delete = ~ self.d.remote_hash.haskey(key)
     local_type = typename(self.d.local_hash[key])
     remote_type = do_delete ? "" : typename(self.d.remote_hash[key])
     do_delete = do_delete || (local_type NE remote_type)
     IF do_delete THEN BEGIN
        self.info, "Deleting " + self.d.full_topdir + key
        file_delete, self.d.full_topdir + key
     END
  END
END


FUNCTION rget_fetch_files::fetch_string_array,path
  url = self.d.top_url + path
  credentials = self.curl_credentials()
  curl = "curl " + credentials + " --fail " + url
  self.info, "Executing: " + curl,/level
  
  self.save_and_blank_library_paths
  spawn, curl, result, err_result, exit_status=exit_status, /null_stdin
  self.restore_library_paths
  
  if exit_status eq 0 then begin
     self.info,"RGET-LIST: " + result, format='(a)',/level
     return, result
  end 
  message,/continue,"curl error, exit status "+exit_status.toString()
  print,curl
  print,"Curl output:"
  print,"  : "+result,format='(a)'
  print,"Curl error output:"
  print,"  : "+err_result
  message,"Won't go on"
end


PRO rget_fetch_files::fetch_rget_list
  rget_list = self.fetch_string_array("RGET-LIST")
  IF rget_list[0] NE "#RGET-LIST" THEN BEGIN
     message,"Remote RGET-LIST corrupt?",/CONTINUE
     message,"First line is not '#RGET-LIST'",/CONTINUE
     print," : "+rget_list,format='(a)'
     message,"Can't continue"
  END
  self.d.remote_array = rget_list[1:*]
  self.d.remote_hash = RGET_MAKE_LIST.list_as_hash(self.d.remote_array)
END


PRO rget_fetch_files::make_fetch
  self.d.local_array = rget_make_list(self.d.full_topdir, debug=0)
  self.d.local_hash = rget_make_list.list_as_hash(self.d.local_array)
  self.fetch_rget_list
  self.dprint, "", "---", "", format='(a)'
  self.do_deletes
  self.dprint, "", "---", "", format='(a)'
  self.do_fetches
END


PRO rget_fetch_files__define
  ;; We use some static routines
  resolve_routine, "rget_make_list", /compile_full_file
  dummy = {rget_fetch_files, inherits rget_dprint, d: dictionary()}
END


PRO rget_fetch_files, url, top_dir, debug=debug, user=user, password=password, verbose=verbose
  o = obj_new('rget_fetch_files', url, top_dir, debug=debug, user=user, password=password, verbose=verbose)
END  


PRO rget_fetch_files_test,debug=debug,verbose=verbose,delete=delete
  !except = 2
  password = getenv("SPICE_PWD")
  user = 'spice'
  IF n_elements(delete) EQ 0 THEN delete=0
  
  url = 'http://astro-sdc-db.uio.no/vol/spice/rget-test/simple'
  top_dir = getenv("HOME")+"/rget-fetch-test-deleteme"
  IF delete EQ 1 then file_delete, top_dir, /recursive, /allow_nonexist
  IF delete EQ 2 then file_delete, top_dir+"/non-empty", /allow_nonexist
  IF delete EQ 3 then file_delete, top_dir+"/subdir2",/recursive, /allow_nonexist
  file_mkdir, top_dir
  file_chmod, top_dir, /u_write
  
  print, "**********************************************************************"
  print, "**********************************************************************"

  rget_fetch_files,url, top_dir, debug=debug, user = user, password = password,verbose=verbose
END

IF getenv("USER") EQ "steinhh" THEN rget_fetch_files_test, debug=debug, verbose=verbose,delete=delete

end
