PRO shadow_check, file_hash, duplicate_hash, different_hash
  rm_path, "$HOME/sf", /expand
  rm_path, "$IDL_DIR/lib", /expand
  rm_path, "$IDL_DIR/lib/coyote", /expand
  rm_path, "$IDL_DIR/lib/astrolib", /expand
  rm_path, "$SSW/vobs/ontology/idl/gen_temp", /expand
  rm_path, "$SSW/soho/cds/idl/ops/data_anal/egse/ops", /expand
  paths = strsplit(!path, ":", /extract)
  paths = paths[sort(paths)]
  paths = paths[uniq(paths)]
  files = file_search(paths, "*.pro")
  file_hash = hash()
  foreach file, files DO BEGIN
     path_parts = strsplit(file, "/", /extract)
     pro_name = path_parts[-1]
     IF file_hash.haskey(pro_name) THEN BEGIN
        ;; We need to weed out duplicates b/c of multiple path levels:
        ;;
        ;; Both .../jp2gen/idl/hinode and .../jp2gen/idl/hinode/sot will
        ;; locate files  in .../jp2gen/idl/hinode/sot!
        ;;
        identical_entry = (where(file EQ file_hash[pro_name]))[0] GT -1
        IF identical_entry THEN CONTINUE

        file_hash[pro_name] = [file_hash[pro_name], file]
     END ELSE BEGIN
        file_hash[pro_name] = [file]
     END 
  END
  duplicate_hash = hash()
  print
  foreach entry, file_hash, key DO BEGIN
     IF n_elements(entry) EQ 1 THEN CONTINUE
     duplicate_hash[key] = entry
  END
  
  different_hash = hash()
  foreach duplicate_list, duplicate_hash, duplicate_proc DO BEGIN
     first = duplicate_list[0]
     different_files = [first]
     filename_has_been_printed = 0
     foreach next_duplicate, duplicate_list[1:*] DO BEGIN
        command = "diff -iwBZ "+first+" "+next_duplicate
        command += " | grep -vE '^[<>] ;'"
        command += " | grep -vE '^---$'"
        command += " | grep -vP '^\d+(,\d+)?[acd]\d'" ;; PERL REGEXP
        
        spawn, command, out
        IF n_elements(out) GT 1 OR out[0] NE "" THEN BEGIN
           IF filename_has_been_printed++ EQ 0 THEN BEGIN
              print, '', '', duplicate_proc, format='(a)'
           END
           print, "      "+[first,next_duplicate], format='(a)'
           print, "      "+command
           print, '      ' + out, format='(a)'
           different_files = [different_files, next_duplicate]
        END
     END
     IF n_elements(different_files) GT 1 THEN BEGIN
        print
        different_hash[duplicate_proc] = different_files
     END
  END
  
  stop
END

shadow_check, file_hash, duplicate_hash, different_hash
END
