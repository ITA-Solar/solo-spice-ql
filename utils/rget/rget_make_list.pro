;; NOTE: The /fully_qualify_path flag does NOT mean "physical" path But we are
;;       safe: as long as a symlink ends up with a path that starts with the
;;       fully qualified topdir, it must be under that topdir anyway.
;;
;;       However, symlink chains that physically speaking end up inside the
;;       source directory through a different route will be handled as an
;;       externally symlinked directory. C'mon! Who would do that?
;;
;;       ALSO: we "follow" symlinks by swapping in the link contents,
;;       and if the link contents contain "../" then those elements
;;       are "resolved".
;;

;; TODO: Add
;; self.info, ... , level=level        ; Informative, only when threshold <= verbose

FUNCTION rget_make_list::init, topdir, debug=debug, verbose=verbose, recursion_list=recursion_list
  dprint = self.dprint::init(debug = debug)
  IF n_elements(recursion_list) EQ 0 THEN recursion_list = []
  IF NOT file_test(topdir, /directory) THEN message, "Not a directory: " + topdir
  self.d = dictionary()
  self.d.debug = keyword_set(debug)
  self.d.topdir = topdir
  self.d.full_topdir = file_search(topdir, /fully_qualify_path, /mark_directory)
  self.d.full_topdir = self.d.full_topdir.replace('\','/')
  self.d.clipped_topdir = self.d.full_topdir.substring(0, self.d.full_topdir.strlen()-2)
  self.d.recursion_list = recursion_list
  IF self.detect_recursion() THEN return, 0
  self.d.places_visited = [self.d.full_topdir]
  self.make_list
  return, 1
END


FUNCTION rget_make_list::relative_path, absolute_path
  IF absolute_path EQ !null THEN return, !null
  qualified_path = (file_search(absolute_path, /fully_qualify_path, /mark_directory))[0]
  qualified_path = qualified_path.replace('\','/')
  internal = qualified_path.startswith(self.d.full_topdir)
  IF NOT internal THEN return, absolute_path
  return, strmid(qualified_path, self.d.full_topdir.strlen(), 1000)
END


FUNCTION rget_make_list::list_as_array
  return, self.d.list.toArray()
END


FUNCTION rget_make_list::file_struct_entry, parts
  compile_opt static
  file_struct = {rget_file_struct, time:ulong(parts[1]), size:ulong(parts[2]), exec:parts[3]}
  return, file_struct
END

FUNCTION rget_make_list::list_as_hash, relative_list_or_arr
  compile_opt static
  IF typename(arr) EQ 'LIST' THEN arr = relative_list_or_arr.toArray() $
  ELSE                            arr = relative_list_or_arr
  hash = orderedhash()
  foreach entry, arr DO BEGIN
     parts = entry.split(' ` |  ->  ')
     parts[0] = parts[0].replace('\','/')
     is_dir = n_elements(parts) EQ 1 AND parts[0].endswith('/')
     is_file = n_elements(parts) EQ 4 AND entry.contains(' ` ')
     is_link = n_elements(parts) EQ 2 AND entry.contains('  ->  ')
     ;; Dirlinks end with / in listing for readability - remove for hash use
     IF is_link THEN BEGIN
        IF parts[0].endswith('/') THEN parts[0] = strmid(parts[0], 0, parts[0].strlen()-1)
        IF parts[1].endswith('/') THEN parts[1] = strmid(parts[1], 0, parts[1].strlen()-1)
     END
     CASE 1 OF
        is_file: hash[parts[0]] = rget_make_list.file_struct_entry(parts)
        is_dir: hash[parts[0]] = !null
        is_link: hash[parts[0]] = parts[1]
     END
  END
  return, hash
END


FUNCTION rget_make_list::detect_recursion
  IF n_elements(self.d.recursion_list) EQ 0 THEN return, 0
  
  foreach visited, self.d.recursion_list DO BEGIN
     info = file_info(visited)
     IF info.dangling_symlink THEN CONTINUE ;; Otherwise would trigger !error
     IF file_same(visited, self.d.full_topdir) THEN BEGIN
        self.info, "Recursion detected, these two are the same:"
        self.info, " -> " + visited 
        self.info, " -> " + self.d.full_topdir
        return, 1
     END
  END
  return, 0
END


FUNCTION rget_make_list::symlink_is_internal, file_info, link_destination
  ;; An absolute link may still be pointing inside the source directory, and
  ;; should be translated to a relative path
  IF link_destination.startswith('/') THEN BEGIN
     relative_destination = self.relative_path(link_destination)
     IF relative_destination.startswith('/') THEN return, 0
     link_destination = relative_destination
     return, 1b
  END
  
  ;; It's a relative link, but may of course still point outside the source
  ;; tree! Using the fully qualified (absolute) path (file_info.name) to the
  ;; *link*, "calculate" the final *destination*. Then *recurse*, pretending
  ;; that the link started out as an absolute link! Problem solved (above)
  ;;
  current_dir = file_dirname(file_info.name, /mark_directory)
  link_destination = current_dir + link_destination
  return, self.symlink_is_internal(file_info, link_destination)
END


;; External links: 
;
;; Create a new instance of ourselves, giving it the absolute path to the
;; external directory. We receive a list of the contents in the external
;; directory, as *relative* paths. Then we pretend it was lying right here in
;; the first place.
;;
;; To detect recursions, we add our *own* topdir (only) to the recursion list
;; and send that to the child object. That means the child object will detect
;; recursions directly to our top, *not* to other places inside "us". However,
;; if there *is* a loop, we will eventually end up examining this particular
;; link once more - and at *that* point, the recursion list we pass on to the
;; new child will already contain that path, so it refuses to go on.

PRO rget_make_list::handle_external_directory, file_info, relative_path, link_destination, windows=windows
  windows_mark = keyword_set(windows) ? "# " : ""
  self.dprint, windows_mark + "Descending into " + relative_path, format = '(a)', level = 2
  self.d.list.add, windows_mark + relative_path
  
  ;;
  new_recursion_list = [self.d.recursion_list, self.d.full_topdir]
  sublist_obj = obj_new('rget_make_list', file_info.name, recursion_list=new_recursion_list, $
                        debug=self.debug(/level), verbose=self.verbose(/level))
  
  IF sublist_obj EQ !null THEN return ; Recursion detected

  sublist_entries = sublist_obj.list_as_array()
  foreach sublist_entry, sublist_entries DO BEGIN
     sublist_relative_path = relative_path + sublist_entry
     self.d.list.add, windows_mark + sublist_relative_path
  END
  self.dprint, windows_mark + "Done descending", level = 2
END


FUNCTION rget_make_list::file_details, file_info
  compile_opt static
  details = file_info.mtime.toString()
  details += " ` " + file_info.size.toString()
  exec = (file_info.mode AND '0111'o) NE 0
  details += " ` " + (exec ? "x" : "-")
  return, details
END


PRO rget_make_list::handle_external_file, file_info, relative_path,  link_destination, $
                                          windows=windows
  windows_mark = keyword_set(windows) ? "# " : ""
  IF keyword_set(windows) THEN link_destination = self.d.full_topdir + link_destination
  destination_info = file_info(link_destination)
  entry = relative_path + " ` " + self.file_details(destination_info)
  self.dprint, windows_mark + "SYMLINK(ext):   " + entry + "(" + link_destination + ")", level = 2
  self.d.list.add, windows_mark + entry
END


PRO rget_make_list::handle_internal_symlink, file_info, relative_path, link_destination
  entry = relative_path + "  ->  " + "./" + link_destination
  self.dprint, "SYMLINK(int):   " + entry + (file_info.directory ? "/" : ""), level = 2
  self.d.list.add, entry
  IF file_info.regular THEN self.handle_external_file, file_info, relative_path, link_destination, /windows
  IF file_info.directory THEN self.handle_external_directory, file_info, relative_path, link_destination, /windows
END


;; FILE_SEARCH does not follow symlinked directories.
;;
;; 1. If symlink points below top_dir then list as symlink, no
;;    matter what the destination is (directory or file, or even
;;    nothing). The link has to be there, full stop, and we don't
;;    want to duplicate files unnecessarily. Return.
;;
;; EXTERNAL links:
;;
;; 2. If destination is regular file, list it as if the symlink
;;    is a regular file, but use DESTINATION file date!
;;    (TODO: What if destination is another link? Follow!!?)
;;
;; 3. If destination is a directory, traverse it and pretend
;;    the files are inside the tree, otherwise they are not
;;    visible.
;;
PRO rget_make_list::handle_ok_symlink, file_info, relative_path
  link_destination = file_readlink(file_info.name)
  internal = self.symlink_is_internal(file_info, link_destination)
  regular = file_info.regular
  self.dprint,relative_path, " => " + link_destination, "(internal: "+internal.tostring()+")", level = 2
  CASE 1 OF 
     internal: self.handle_internal_symlink, file_info, relative_path, link_destination
     regular:  self.handle_external_file, file_info, relative_path, link_destination
     ELSE:     self.handle_external_directory, file_info, relative_path, link_destination
  END
END


PRO rget_make_list::handle_dangling_symlink, file_info, relative_path
  link_destination = file_readlink(file_info.name)
  self.d.list.add, relative_path + "  ->  !!" + link_destination
END


PRO rget_make_list::handle_directory, file_info, relative_path
  new_relative_path = self.relative_path(file_info.name)
  IF new_relative_path NE relative_path THEN stop
  self.dprint, "DIRECTORY:      " + self.relative_path(file_info.name), level = 2
  self.d.list.add, relative_path
END


PRO rget_make_list::handle_regular_file, file_info, relative_path
  self.dprint, "REGULAR:        " + relative_path, level = 2
  self.d.list.add, relative_path + " ` " + self.file_details(file_info)
END


PRO rget_make_list::make_list
  files = file_search(self.d.full_topdir, "*", /expand_tilde, /expand_environment, /match_initial_dot,/mark_directory)
  files = files.replace('\','/')
  IF total(files.contains("`")) GT 0 THEN message, "Sorry, some file name(s) contain '`'"
  self.d.list = list()
  foreach file, files DO BEGIN
     file =  file.replace("$", "\$")
     file =  file.replace("[", "\[")
     IF file EQ '' THEN continue
     file_info = file_info(file)
     
     relative_path = self.relative_path(file_info.name)
     CASE 1 OF
        file_info.dangling_symlink: self.handle_dangling_symlink, file_info, relative_path
        file_info.symlink:          self.handle_ok_symlink, file_info, relative_path 
        file_info.directory:        self.handle_directory, file_info, relative_path
        file_info.regular:          self.handle_regular_file, file_info, relative_path
        ELSE: BEGIN
           message,"Ooops: Not sure what this is:", /continue
           help, file_info
           message, "Stopping"
        END 
     END
  END  
END


PRO rget_make_list::write_file, output_file
  IF typename(output_file) NE "STRING" THEN output_file = self.d.full_topdir + "RGET-LIST"
  self.info, "", "Will write list to file " + output_file, format = '(a)'
  openw, lun, output_file, /get_lun
  printf, lun, self.list_as_array(), format='(a)'
  free_lun, lun
END

PRO rget_make_list__define
  dummy = {rget_make_list, inherits dprint, $
           d: dictionary() $
          }
END


FUNCTION rget_make_list, path, write_file=write_file, entry_hash=entry_hash, debug=debug, verbose=verbose
  make_list_obj = obj_new('rget_make_list', path, debug=debug)
  
  entry_array = make_list_obj.list_as_array()
  
  IF arg_present(entry_hash) THEN BEGIN
     entry_hash = make_list_obj.list_as_hash(entry_array)
  END
  
  IF keyword_set(write_file) THEN make_list_obj.write_file, write_file
  return, entry_array
END


PRO rget_make_list_test
;  !except = 2
;  print, "", "", "***************************************", format='(a)'
;  setenv, "SPICE_DATA=~/tmp/rget-test/source/rget-test"
;  entries = rget_make_list("~/tmp/rget-test/source/rget-test", /write,
;  /debug, entry_hash=entry_hash)
;  entries = rget_make_list(getenv("SPICE_DATA"), /write)
  entries = rget_make_list(getenv("SPICE_DATA")+"/simple-rget-test")
  stop
;  
;  print, "", "----", format='(a)'
  
;  foreach entry, entries DO print, entry
  
;  print, "", "----", format='(a)'
;  print, "", "---", "RSYNCing results",format='(a)'
;  spawn, "rsync -av --delete ~/tmp/rget-test/source/ " + $
;         "osdcapps@astro-sdc-db:/astro/astro-sdc-fs/d1/sdc/roslo/vol/spice/rget-test/source/"
END

test = getenv("USER") EQ "steinhh"
IF test THEN rget_make_list_test

END
