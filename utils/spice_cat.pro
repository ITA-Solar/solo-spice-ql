FUNCTION spice_cat__read_fitslist,filename,headers=headers
  openr,lun,filename,/get_lun
  t = ''
  readf,lun,t
  fields = strsplit(/extract,t,",")
  list = []
  WHILE NOT eof(lun) DO BEGIN
     readf,lun,t
     values = strsplit(/extract,t,string(9b))
     struct = { }
     struct_name = ""
     foreach value,values,index DO BEGIN
        struct_name += fields[index]
        struct = create_struct(name=struct_name, struct,fields[index],value)
     END
     list = [list,struct]
  END
  headers = tag_names(list[0] )
  return,list
END 

FUNCTION spice_cat__values
  default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  default,listfiledir,spice_datadir
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  return,{$
         spice_datadir:spice_datadir, $
         listfiledir:listfiledir, $
         listfilename:listfilename $
         }
END

FUNCTION hash_to_struct,stash
  struct = {}
  foreach value,stash,key DO struct = create_struct(struct,key,value)
  return,struct
END

FUNCTION spice_cat__column_widths,headers
  h = "FILENAME,COMPRESS,STUDY_ID,OBS_ID,STUDYTYP,STUDYDES,STUDY,"
  w = "      20,       5,       2,    10,       5,      10,   10,"
  
  h += "AUTHOR,PURPOSE,READMODE,SOOPNAME,NWIN,NWIN_PRF,NWIN_DUM,"
  w += "    10,     10,       5,      10,   2,       2,        2,"
  
  h += "NWIN_INT"
  w += "       2"
  
  h = strsplit(h,",",/extract)
  w = fix(strsplit(w,",",/extract))
  
  stash = hash(h,w)
  widths = []
  foreach header,headers DO widths = [widths, stash[header]]
  return, widths * 12
END

PRO spice_cat__table_event,event
  help,event
END

PRO spice_cat
  v = spice_cat__values()
  p = hash()
  
  list = spice_cat__read_fitslist(v.listfilename,headers=headers)
  filter = list[0]
  foreach tag, tag_names(filter), index DO filter.(index) = ""
  list = [filter,list]
  
  ; /row_major table means every struct => one row
  ; Arrays like "editable" is [column,row], so [*,n] is all columns in row n
  
  num_columns = n_elements(headers)
  num_rows = n_elements(list)
  editable = bytarr(num_columns,num_rows)
  editable[*,0] = 1b
  background_color = replicate(230b,3,num_columns,num_rows)
  background_color[*,*,0] = 255b
  column_widths = spice_cat__column_widths(headers)
  
  base = widget_base(/column,xpad=0,ypad=0)
  
  headers_base = widget_base(base,frame=0b,xpad=0,ypad=0)
  
  table_base = widget_base(base,/column,/frame,event_pro="spice_cat__table_event",xpad=0,ypad=0)
  
  p = hash()
  p['value'] = list
  p['column_labels'] = headers
  ;p['no_headers'] = 1b
  p['editable'] = editable
  p['row_major'] = 1b
  p['background_color'] = background_color
  p['units'] = 0
  p['column_widths'] = column_widths
  p['all_events'] = 1b
  p['context_events'] = 1b
  
  table = widget_table(table_base,_extra=hash_to_struct(p))
  
  widget_control,base,/realize
  xmanager,"spice_cat",base
END

spice_cat
END
