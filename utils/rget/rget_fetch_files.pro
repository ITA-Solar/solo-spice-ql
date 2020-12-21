FUNCTION rget_fetch_files::init, top_url, top_dir, username=username, password=password, debug=debug, verbose=verbose
  dprint = self.dprint::init(debug=debug, verbose=verbose)
  
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


PRO rget_fetch_files::report_fetch_error, url
  print, "", "***********************************", format='(a)'
  print, "*** Error in fetching " + url, format='(a)'
  IF self.d.url_parts.username NE '' THEN print, "*** USERNAME: " + self.d.url_parts.username
  IF self.d.url_parts.username NE '' THEN print, "*** PASSWORD: " + self.d.url_parts.password
  self.d.neturl.getproperty, $
     response_code = code, response_header = header, response_filename = filename
  print, "***"
  print, "*** Response_code: ", code
  header = header.split(string([13b, 10b]))
  print, "*** Header: " + header, format='(a)'
  print, "***"
  print, "*** Filename: '" + filename + "'"
  print, "***************************************", "", format='(a)'
  help, (error_state = !error_state)
  print
  message, /reissue
END

function rget_fetch_files::fetch_string_array,url,credentials
  curl = "curl " + credentials + " --fail " + url
  self.info, "Executing: " + curl,/level
  spawn, curl, result, err_result, exit_status=exit_status
  if exit_status eq 0 then begin
     self.info,"RGET-LIST: " + result, format='(a)',/level
     return, result
  end 
  message,/continue,"curl error, exit status "+exit_status.toString()
  print,"Curl output:"
  print,"  : "+result,format='(a)'
  print,"Curl error output:"
  print,"  : "+err_result
  message,"Won't go on"
end


FUNCTION rget_fetch_files::fetch_file, path, filename=filename, string_array=string_array
  url = self.d.top_url + path
  credentials = self.d.username
  IF self.d.password THEN credentials = credentials + ':' + self.d.password
  IF credentials THEN credentials = "--user " + credentials
  IF keyword_set(string_array) THEN BEGIN
     curl = "curl " + credentials + " " + url
     print, "Executing: " + curl
     spawn, curl, result, err_result, exit_status=exit_status
     print, "Result: " + result, format='(a)'
     return, result
  END
  
;  output_file = filename + '.rget_tmp' : ""
  curl = "curl -o " + output_file + " " + path
  stop
  catch, err
  IF err NE 0 THEN BEGIN
     catch, /cancel
     self.report_fetch_error, path
     message, /reissue
  END
  
  result = self.safe_get(filename = output_file, string_array = string_array, response_code = response_code)
  IF response_code NE 200 THEN self.report_fetch_error, path
  catch, /cancel
  IF output_file THEN file_move, output_file, filename, /overwrite
  self.info, "Got: " + path, level = 2
  return, result
END


PRO rget_fetch_files::fetch_rget_list
  self.d.remote_array = self.fetch_file("RGET-LIST", /string_array)
  self.d.remote_hash = RGET_MAKE_LIST.list_as_hash(self.d.remote_array)
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


PRO rget_fetch_files::maybe_fetch_file, relative_path, remote_rget_file
  is_ok_so_far = self.d.local_hash.haskey(relative_path)
  IF is_ok_so_far THEN BEGIN
     local_rget_file = self.d.local_hash[relative_path]
     is_ok_so_far = is_ok_so_far AND (local_rget_file.time GE remote_rget_file.time)
     is_ok_so_far = is_ok_so_far AND (local_rget_file.size EQ remote_rget_file.size)
     is_ok_so_far = is_ok_so_far AND (local_rget_file.exec EQ remote_rget_file.exec)
  END
  IF is_ok_so_far THEN BEGIN  
     self.info, "Leave alone: " + relative_path, level = 1
     return
  END
  self.info, "Fetch " + relative_path
  output_path = self.d.full_topdir + relative_path
  result = self.fetch_file(relative_path, filename = output_path)
  file_chmod, output_path, a_execute=remote_rget_file.exec EQ "x"
END


PRO rget_fetch_files::make_directory, directory
  self.info, "Directory " + self.d.full_topdir + directory, level = 1
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
  dummy = {rget_fetch_files, inherits dprint, d: dictionary()}
END


;; For debugging purposes
FUNCTION rget_fetch_files::dict
  return, self.d
END

PRO rget_fetch_files, url, top_dir, dict_out, debug=debug, user=user, password=password, verbose=verbose
  o = obj_new('rget_fetch_files', url, top_dir, debug=debug, user=user, password=password, verbose=verbose)
  dict_out = o.dict()
END  


PRO rget_fetch_files_test
  !except = 2
  password = getenv("SPICE_PASSWD")
  user = 'spice'
  
  url = 'http://astro-sdc-db.uio.no/vol/spice/rget-test/simple'
  top_dir = getenv("HOME")+"/rget-fetch-test-deleteme"
;  file_delete, top_dir, /recursive, /allow_nonexist
  file_mkdir, top_dir
  
  print, "**********************************************************************"
  print, "**********************************************************************"

  rget_fetch_files,url, top_dir, dict, debug=0, user = user, password = password ;, /verbose
END

test = getenv("USER") EQ "steinhh"

IF test THEN rget_fetch_files_test

end
