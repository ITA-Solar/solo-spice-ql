FUNCTION prits_tools::concat_dirs, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10
  compile_opt static
  result = concat_dir(x1, x2)
  IF exist(x3) THEN return, prits_tools.concat_dirs(result, x3, x4, x5, x6, x7, x8, x9, x10)
  return, result
END

;; from and two are absolute paths/file references, and we want to make "from"
;; be the shortest relative symlink to "to". E.g.:
;;
;; from = "/a/b/c/d/from"
;; to   = "/a/b/c/x/to"
;; 
;; Now "from" should be = "../x/to", i.e., up to c, down into x, then "to"
;;
;; from = "/a/b/c/d/from"
;; to   = "/a/x/y"
;;
;; from = "..[to c]/..[to b]/..[to a]]/x/y", i.e. "../../../x/y"
;;
;;
FUNCTION prits_tools::shorten_symlink, link_to_input, link_from_input, verbose=verbose
  link_to = file_expand_path(link_to_input)
  link_from = file_expand_path(link_from_input)
  
  IF link_to EQ '' OR link_from EQ '' THEN message, "Neither 'link_to' nor 'link_from' can be blank"
  IF link_to.startswith(link_from + "/") THEN message, link_from + " can't point below itself: " + link_to
  
  IF link_to EQ link_from THEN BEGIN
     return, file_basename(link_to)
  END

  link_to_bytarr = byte(link_to)
  link_from_bytarr = byte(link_from)
  
  differences_bytarr = link_from_bytarr - link_to_bytarr
  
  different_bytarr = differences_bytarr NE 0
  equal_bytarr = differences_bytarr EQ 0
  
  first_difference = (where(different_bytarr))[0]
  
  ;; No first difference & not equal => differ at first char beyond shortest
  IF first_difference EQ -1 THEN first_difference = n_elements(differences_bytarr)
  
  common_start = strmid(link_to, 0, first_difference)
  last_slash_ix = common_start.lastindexof('/')
  common_start_path = strmid(common_start, 0, last_slash_ix)
  
  way_down = link_to.replace(common_start_path + "/", "")

  way_to_climb_up = link_from.replace(common_start_path, "")
  way_to_climb_up_dir = file_dirname(way_to_climb_up)
  
  slash = (byte('/'))[0]
  slashes_in_way_up_dir = total(byte(way_to_climb_up_dir) EQ slash)
  
  
  way_up = strjoin(replicate('../', slashes_in_way_up_dir), '')
  IF way_to_climb_up_dir EQ '/' THEN way_up = './'
  
  link = concat_dir(way_up, way_down)
  IF keyword_set(verbose) THEN BEGIN 
     print, link_to
     print, link_from + ' -> ' + link
  END
  return, link
END


PRO prits_tools::gen_addons_init
  ; Nothing to do
END


PRO prits_tools__gen_addons__define
  compile_opt static
  vso = {prits_tools__gen_addons, $
         dummy:0b $
        }
END

IF getenv("USER") EQ 'steinhh' THEN BEGIN
   COMMON prits_tools, gen_addons_pt
   IF n_elements(gen_addons_pt) EQ 0 THEN gen_addons_pt = prits_tools()
   pt = gen_addons_pt
   cd, '$HOME/tmp/link-test'
   ff = pt.shorten_symlink('A/a', 'A/a', /verbose)
   ff = pt.shorten_symlink('A/a', 'A/b', /verbose)
   ff = pt.shorten_symlink('A/a', 'B/a', /verbose) ;; => '../A/a'
   ff = pt.shorten_symlink('A/a', 'B1/B2/B3/B4/a', /verbose) ;; => '../A/a'
   ff = pt.shorten_symlink('A/a', 'AA/a', /verbose)
   ff = pt.shorten_symlink('AA/a', 'A/a', /verbose)
END
END

;; /
;; /a/b/c/d
;; 0
