;;
;; Dump object contents and stop inside the scope of the object
;;
;; Use: pt.stop_for_inspection
;;
;; 
PRO prits_tools::stop_for_inspection
  t = tag_names(self)
  FOR i=0, n_elements(t)-1 DO BEGIN
     print, t[i]
     help, self.(i)
     print
     print
  END
  stop
END
