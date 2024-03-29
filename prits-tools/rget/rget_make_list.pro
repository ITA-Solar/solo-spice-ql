;+
; NAME:
;      RGET_MAKE_LIST
;
; PURPOSE:
;
;      Given the path to a directory, it creates an RGET-LIST (see below). The
;      list may optionally be written to a file (default file name is
;      path/RGET-LIST).
;
;      Can be invoked both as a FUNCTION and as a PRO. When invoked as a
;      function, it returns the contents of the RGET-LIST.
;
;      This program is only meant/tested for unix/linux platforms. It follows
;      symlinks the same way a web browser would do, through recursion. The
;      possibility of symlink loops is handled by throwing an error if the
;      path depth exceeds the value of the keyword max_allowed_depth (default
;      value is 100), unless max_allowed_depth is set to 0, in which case
;      there is no handling of symlink loops.
;
; ADDITIONAL INFORMATION:
;
;      An RGET-LIST contains a list of all files and subdirectories in a
;      directory, one line per directory/file.
;
;      Each line contains the name of the file/directory, for files also the
;      UNIX ctime of the file, the file size, and a flag that is "x" for
;      executable files and "-" for non-executable files.
;
;      This allows a quick comparison between a remote web server's
;      directory and the corresponding local directory, so only files with
;      non-matching attributes need to be fetched.
;
;      The term "RGET" stems from a concatenation of rsync and wget. Never
;      mind that the implementation now actually uses curl, not wget :).
;
; KEYWORDS:
;      VERBOSE: Make more noise
;
;      QUIET: Make less noise
;
;      WRITE: Unless WRITE is set, the RGET-LIST file will not be written
;
;      MAX_ALLOWED_DEPTH: Default 100, to prevent endless symlink recursion. 
;   
;      INCLUDE: One or more regular expressions. When present, only paths
;               matching one or more of the expressions will be included.
;
;      EXCLUDE: One or more regular expressions. When present, paths matching
;               one or more of the expressions will be excluded.
;      
; CATEGORY:
;      GENERAL/UTILITY
;
; CALLING SEQUENCE:
;      RGET_MAKE_LIST,PATH [,/write_file] [,max_allowed_depth=N]
;      rget_list = RGET_MAKE_LIST(PATH [,/write_file] [,max_allowed_depth=N])
;
; INPUTS:
;      PATH: The path to a directory to be scanned for files that will be
;            included in the RGET-LIST
;
; OUTPUTS:
;      When called as a function, it returns the RGET-LIST
;
; HISTORY:
;      Ver. 1, January 2021, Stein Haugan
;-

FUNCTION rget_make_list::init, topdir, max_allowed_depth=max_allowed_depth, $
                               debug=debug, verbose=verbose, quiet=quiet, $
                               include=include, exclude=exclude
  
  dprint = self.rget_dprint::init(debug = debug, verbose=verbose, quiet=quiet)
  
  IF NOT file_test(topdir, /directory) THEN message, "TOPDIR is not a directory: " + topdir
  
  IF n_elements(max_allowed_depth) EQ 0 THEN max_allowed_depth = 100
  IF n_elements(include) EQ 0 THEN include = !null
  IF n_elements(exclude) EQ 0 THEN exclude = !null
  
  self.d = dictionary()
  self.d.debug = keyword_set(debug)
  self.d.topdir = topdir
  self.d.full_topdir = file_search(topdir, /fully_qualify_path, /mark_directory)
  self.d.full_topdir = self.d.full_topdir.replace('\','/')
  self.d.max_allowed_depth = max_allowed_depth
  IF self.detect_recursion() THEN return, 0
  self.d.places_visited = [self.d.full_topdir]
  
  self.d.include = include
  self.d.exclude = exclude
  
  self.make_list
  return, 1
END


FUNCTION rget_make_list::relative_path, absolute_path
  IF absolute_path EQ !null THEN return, !null
  qualified_path = (file_search(absolute_path, /fully_qualify_path, /mark_directory))[0]
  qualified_path = qualified_path.replace('\','/')
  is_internal = qualified_path.startswith(self.d.full_topdir)
  IF NOT is_internal THEN return, absolute_path
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
  IF self.d.max_allowed_depth EQ 0 THEN return, 0
  current_depth = n_elements(self.d.topdir.split("/"))
  IF current_depth LE self.d.max_allowed_depth THEN return, 0
  
  print, "** Possible recursive symlinking detected!"
  print, "** Current directory depth: " + current_depth.toString()
  print, "** Path: " + self.d.topdir
  print, "** max_allowed_depth = " + self.d.max_allowed_depth.tostring()
  print, "** Use rget_make_list(..., max_allowed_depth=N)"
  print, "** Setting max_allowed_depth to zero means no limit"
  print, "** IGNORING " + self.d.topdir
  return, 1
END


;; Symbolic links to directories: 
;;
;; Create a new instance of ourselves, giving it the absolute path to the
;; linked directory. We receive a list of the contents in the linked
;; directory, as *relative* paths. Then we pretend they were lying right here
;; in the first place.

PRO rget_make_list::handle_symlink_directory, file_info, relative_path, link_destination
  self.dprint, "Descending into " + relative_path, format = '(a)', level = 2
  self.d.list.add, relative_path
  
  sublist_obj = obj_new('rget_make_list', file_info.name, $
                        debug=self.debug(/level), verbose=self.verbose(/level), $
                        max_allowed_depth=max_allowed_depth)
  
  IF sublist_obj EQ !null THEN return ; Recursion detected

  sublist_entries = sublist_obj.list_as_array()
  foreach sublist_entry, sublist_entries DO BEGIN
     sublist_relative_path = relative_path + sublist_entry
     self.d.list.add, sublist_relative_path
  END
  self.dprint, "Done descending", level = 2
END


FUNCTION rget_make_list::file_details, file_info
  compile_opt static
  details = file_info.mtime.toString()
  details += " ` " + file_info.size.toString()
  exec = (file_info.mode AND '0111'o) NE 0
  details += " ` " + (exec ? "x" : "-")
  return, details
END


PRO rget_make_list::handle_symlink_file, file_info, relative_path,  link_destination
  link_is_absolute = link_destination.startswith('/')
  IF NOT link_is_absolute THEN BEGIN
     starting_location = self.d.full_topdir + file_dirname(relative_path) + "/"
     link_destination = starting_location + link_destination
  END 
  destination_info = file_info(link_destination)
  entry = relative_path + " ` " + self.file_details(destination_info)
  self.dprint, "SYMLINK(ext):   " + entry + "(" + link_destination + ")"
  self.d.list.add, entry
END


;; FILE_SEARCH does not follow symlinked directories.
;;
;; 1. If destination is regular file, list it as if the symlink
;;    is a regular file, but use DESTINATION file date!
;;    (TODO: What if destination is another link? Follow!!?)
;;
;; 2. If destination is a directory, traverse it and pretend
;;    the files are inside the tree
;;
PRO rget_make_list::handle_valid_symlink, file_info, relative_path
  link_destination = file_readlink(file_info.name)
  regular = file_info.regular
  self.dprint,relative_path, " =*> " + link_destination, level = 2
  CASE 1 OF 
     regular:  self.handle_symlink_file, file_info, relative_path, link_destination
     ELSE:     self.handle_symlink_directory, file_info, relative_path, link_destination
  END
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

PRO rget_make_list::prune_list
  IF n_elements(self.d.include) GT 0 THEN BEGIN
     includes = [self.d.include] 
     foreach include, includes DO BEGIN
        
     END
  END 
END

PRO rget_make_list::make_entry, file
  file =  file.replace("$", "\$")
  file =  file.replace("[", "\[")
  IF file EQ '' THEN return
  file_info = file_info(file)
  relative_path = self.relative_path(file_info.name)
  self.info, "Found: " + relative_path, /level
  CASE 1 OF
     file_info.dangling_symlink: self.info,"Ignoring dangling symlink: "+relative_path
     file_info.symlink:          self.handle_valid_symlink, file_info, relative_path 
     file_info.directory:        self.handle_directory, file_info, relative_path
     file_info.regular:          self.handle_regular_file, file_info, relative_path
     ELSE: BEGIN
        message,"Ooops: Not sure what this is:", /continue
        help, file_info
        message, "Stopping"
     END 
  END
END

PRO rget_make_list::make_list
  _extra = {expand_tilde:1, expand_environment:1, match_initial_dot:1, mark_directory:1}
  files = file_search(self.d.full_topdir, "*", _strict_extra=_extra)
  files = files.replace('\','/')
  IF total(files.contains("`")) GT 0 THEN message, "Sorry, some file name(s) contain '`'"
  self.d.list = list()
  
  foreach file, files DO self.make_entry, file
  
  self.prune_list
END


PRO rget_make_list::write_file, output_file
  IF typename(output_file) NE "STRING" THEN output_file = self.d.full_topdir + "RGET-LIST"
  self.info, "", "Will write list to file " + output_file, format = '(a)'
  openw, lun, output_file, /get_lun
  printf, lun, "#RGET-LIST"
  printf, lun, self.list_as_array(), format='(a)'
  free_lun, lun
END

PRO rget_make_list__define
  dummy = {rget_make_list, inherits rget_dprint, $
           d: dictionary() $
          }
END


;; Invoked as a function, e.g. rget_list = rget_make_list(path)
;;
FUNCTION rget_make_list, path, write_file=write_file, max_allowed_depth=max_allowed_depth, _extra=_extra
  
  make_list_obj = obj_new('rget_make_list', path, max_allowed_depth=max_allowed_depth, _extra=_extra)
  
  entry_array = make_list_obj.list_as_array()
  
  IF keyword_set(write_file) THEN make_list_obj.write_file, write_file
  return, entry_array
END

;; Invoked as a procedure, e.g. rget_make_list,path
;;
PRO rget_make_list, path, write_file=write_file, max_allowed_depth=max_allowed_depth, _extra=_extra
  list = rget_make_list(path, write_file=write_file, max_allowed_depth=max_allowed_depth, _extra=_extra)
END


;;;;;;;;;;;;;;;;; TESTING PURPOSES ;;;;;;;;;;;;;;;;;;;;;;

PRO rget_make_list_test
  path = getenv("HOME")+"/rget-test-deleteme
  entries = rget_make_list(path, include="subdir")
  
  print, "", "rget_make_list_test result:",": " + entries, "-", format='(a)'
  stop
END

IF getenv("USER") EQ "steinhh" THEN rget_make_list_test

END
