FUNCTION rget_fetch_files::init, top_url, top_dir, username=username, password=password, debug=debug, verbose=verbose
  dprint = self.dprint::init(debug=debug, verbose=verbose)
  print
  
  IF n_elements(username) EQ 0 THEN username = ''
  IF n_elements(password) EQ 0 THEN password = ''
  
  self.d = dictionary()
  
  full_topdir = (file_search(top_dir, /fully_qualify_path, /test_directory, /mark_directory))[0]
  full_topdir = linux_path(full_topdir)
  is_directory = full_topdir.endswith('/')
  IF ~ is_directory THEN message, "Destination "  + top_dir + " is NOT a directory"
  self.d.full_topdir = full_topdir
  
  IF ~ top_url.endswith('/') THEN top_url += '/'
  self.d.top_url = top_url
  
  self.d.neturl = self.neturl_object(username = username, password = password)
  
  self.make_fetch
  return, 1
END


FUNCTION rget_fetch_files::neturl_object, top_url, username=username, password=password
  url_parts = parse_url(self.d.top_url)
  IF url_parts.query NE '' THEN message, "The URL can't have any query parts"
  
  url_parts.username = url_parts.username ? url_parts.username :  username
  url_parts.password = url_parts.password ? url_parts.password : password
  
  neturl = obj_new('IDLnetURL')
  
  neturl.SetProperty, url_username=url_parts.username, url_password=url_parts.password
  neturl.SetProperty, url_host=url_parts.host
  
  self.d.url_parts = url_parts
  
  neturl.SetProperty, headers = 'User-Agent: IDLnetURL/rget_fetch_files'
  neturl.SetProperty, verbose = !true
  
  neturl.SetProperty, url_path = url_parts.path
  return, neturl
END


FUNCTION rget_fetch_files::reverse_parse_url
  url_parts = self.d.url_parts
  url = url_parts.scheme + "://"
  IF url_parts.username NE '' THEN url += url_parts.username
  IF url_parts.password NE '' THEN url += ":" + url_parts.password
  IF url_parts.username NE '' THEN url += "@"
  url += self.d
  
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


;; STUPID IDLnetURL can't fetch empty files without throwing an error!
;;
;; We deal with those here, but reissue MESSAGE if the error is "something
;; else"
FUNCTION rget_fetch_files::safe_get, string_array=string_array, filename=filename, response_code=response_code
  catch, err
  IF err NE 0 THEN BEGIN
     catch, /cancel
     self.d.neturl.getproperty,response_code=response_code, response_header=response_header
     zero_length = 'Content-Length: 0' + string([13b, 10b])
     ok = response_code EQ 200 AND response_header.contains(zero_length)
     IF ok THEN self.dprint, "NETURL.GET zero-length file detected", format = '(A,$)'
     IF NOT ok THEN message, /reissue
     
     ;; EMPTY FILE!
     IF keyword_set(string_array) THEN return, ''
     ;; Make empty destination file:
     openw, lun, filename, /get_lun
     free_lun, lun
     return, filename
  END
  IF keyword_set(filename) THEN file_mkdir, file_dirname(filename)
  result = self.d.neturl.get(string_array = string_array, filename = filename)
  self.d.neturl.getproperty, response_code = response_code
  catch, /cancel
  return, result
END


FUNCTION rget_fetch_files::fetch_file, path, filename=filename, string_array=string_array
  path = self.d.url_parts.path + path
  self.d.neturl.SetProperty, url_path = path
  output_file = keyword_set(filename) ? filename + '.rget_fetch_files' : ''
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
  self.info, "Got: " + path, threshold = 1
  return, result
END


PRO rget_fetch_files::fetch_rget_list
  self.d.remote_array = self.fetch_file("RGET-LIST", /string_array)
  self.d.remote_hash = RGET_MAKE_LIST.list_as_hash(self.d.remote_array)
END

;; TODO: Windows: remove symlink entries
PRO rget_fetch_files::clean_hash_for_platform, hash
  compile_opt idl2
  remove_keys = []

  add_entries = hash()
  
  IF strlowcase(!version.os_family) EQ "unix" THEN BEGIN
     foreach entry, hash, key DO BEGIN
        IF key.contains('# ') THEN BEGIN
           self.dprint, "Cleaning Windows item: " + key
           remove_keys = [remove_keys, key]
        END
     END
  END ELSE BEGIN
     foreach entry, hash, key DO BEGIN
        IF typename(entry) EQ "STRING" THEN BEGIN
           self.dprint, "Cleaning symlink " + key + " -> " + entry
           remove_keys = [remove_keys, key]
           CONTINUE
        END
        IF key.contains('# ') THEN BEGIN
           new_key = key.substring(2)
           WHILE new_key.contains('# ') DO new_key = new_key.replace('# ', '')
           self.dprint, "Transforming key " + key + " => " + new_key
           add_entries[new_key] = entry
           remove_keys = [remove_keys, key]
           CONTINUE
        END
     END
  END
  foreach key, remove_keys DO hash.remove, key
  foreach entry, add_entries, key DO hash[key] = entry
  return
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
  do_fetch = ~ self.d.local_hash.haskey(relative_path)
  local_rget_file = do_fetch ? !null : self.d.local_hash[relative_path]
  do_fetch = do_fetch || local_rget_file.time LT remote_rget_file.time
  do_fetch = do_fetch || local_rget_file.size NE remote_rget_file.size
  do_fetch = do_fetch || local_rget_file.exec NE remote_rget_file.exec
  IF NOT do_fetch THEN BEGIN  
     self.dprint, "Leave alone: " + remote_key
     return
  END

  self.info, "Fetch " + relative_path
  output_path = self.d.full_topdir + relative_path
  result = self.fetch_file(relative_path, filename = output_path)
  file_chmod, output_path, a_execute=remote_rget_file.exec EQ "x"
END


FUNCTION rget_fetch_files::calculate_relative_link, link_name, link_target
  IF link_target.substring(0, 1) EQ '!!' THEN return, link_target
  name_parts   = link_name.split('/')
  target_parts = link_target.split('/')
  FOR num_common=0, n_elements(name_parts)-2 DO BEGIN
     IF target_parts[num_common] NE name_parts[num_common] THEN BREAK
  END
  ;; We need to go up the non-common path
  go_up = n_elements(name_parts) - num_common - 1
  self.dprint, "SOURCE: " + link_name
  self.dprint, "DEST  : " + link_target
  up_string = ""
  FOR i=0, go_up - 1 DO up_string += "../"  
  down_string = (target_parts[num_common:*]).join('/')
  relative_link = up_string + down_string
  self.dprint, "RELATIVE: " + relative_link, "", format = '(a)'
  return, relative_link
END



;; TODO: Move this into rget_make_list!
PRO rget_fetch_files::make_symlink, link_name, link_target
  self.dprint, "", "ln -s " + link_name + " -> " + link_target, format = '(a)'
  IF link_name.startswith('./') THEN link_name = link_name.substring(2)
  relative_link = self.calculate_relative_link(link_name, link_target)
  IF relative_link.startswith('!!') THEN BEGIN
     self.info, "Ignoring dangling symlink: " + link_name + " -> " + link_target, format = '(a)'
     return
  END
  verbose = self.debug() || self.verbose()
  file_link, relative_link, self.d.full_topdir + link_name, verbose=verbose, /noexpand_path
END


PRO rget_fetch_files::make_directory, directory
  self.info, "Directory " + self.d.full_topdir + directory
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
        entry_type EQ "STRING"            : self.make_symlink, remote_key, remote_entry
        entry_type EQ "RGET_FILE_STRUCT"  : self.maybe_fetch_file, remote_key, remote_entry
     END
  END
END


PRO rget_fetch_files::make_fetch
  self.d.local_array = rget_make_list(self.d.full_topdir, entry_hash=local_hash, debug=0)
  self.d.local_hash = local_hash
  self.fetch_rget_list
  self.clean_hash_for_platform, self.d.local_hash
  self.clean_hash_for_platform, self.d.remote_hash
  self.dprint, "", "---", "", format='(a)'
  self.do_deletes
  self.dprint, "", "---", "", format='(a)'
  self.do_fetches
END

PRO rget_fetch_files__define
  dummy = {rget_fetch_files, inherits dprint, d: dictionary()}
END

FUNCTION rget_fetch_files::dict
  return, self.d
END

PRO rget_fetch_files, url, top_dir, dict_out, debug=debug, user=user, password=password, verbose=verbose
  o = obj_new('rget_fetch_files', url, top_dir, debug=debug, user=user, password=password, verbose=verbose)
  dict_out = o.dict()
END  


PRO rget_fetch_files_test
;  rget_make_list_test
  
  !except = 2
  password = getenv("SPICE_PASSWD")
  user = 'spice'
  
  url = 'http://astro-sdc-db.uio.no/vol/spice/rget-test/simple'
  top_dir = conc(getenv("HOME"),"rget-test-deleteme")

;  spawn, "rsync -av --delete " + top_dir + "/../orig-dest/ " + top_dir
  
  print
  print, "**********************************************************************"
  print, "**********************************************************************"

  rget_fetch_files,url, top_dir, dict, debug=0, user = user, password = password, verbose=0
  
  print, "", "******** Testing *********", "", format='(a)'
  spawn, "diff -r  ~/tmp/rget-test/source/rget-test/  ~/tmp/rget-test/dest/|& grep -v 'recursive'"
END

test = getenv("USER") EQ "steinhh"

IF test THEN rget_fetch_files_test

end
