FUNCTION spice_pickfile__read_fitslist,filename,headers=headers
  openr,lun,filename,/get_lun
  t = ''
  readf,lun,t
  fields = strsplit(/extract,t,",")
  list = []
  WHILE NOT eof(lun) DO BEGIN
     readf,lun,t
     values = strsplit(/extract,t,string(9b))
     struct = { }
     name = ""
     foreach value,values,index DO BEGIN
        name += fields[index]
        struct = create_struct(name=name, struct,fields[index],value)
     END
     list = [list,struct]
  END
  headers = tag_names(list[0] )
  return,list
END 

FUNCTION spice_p ickfile__values
  default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  default,listfiledir,spice_datadir
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  return,{$
         spice_datadir:spice_datadir, $
         listfiledir:listfiledir, $
         listfilename:listfilename $
         }
END

FUNCTION spice_pickfile__table,base,props
  extra = {}
  foreach value,props,key DO BEGIN
     extra = create_struct(extra,key,value)
  END
  
  table = widget_table(base,_strict_extra=extra)
  return,table
END

FUNCTION spice_pickfile__column_widths,headers
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

PRO spice_pickfile__table_event,event
  help,event
END

FUNCTION spice_pickfile__headers,base,headers
  widths = spice_pickfile__column_widths(headers)
  offset = 70
  foreach heading, headers, index DO BEGIN
;     text = widget_text(base,value=heading,scr_xsize=widths[index]+4,units=0b,frame=1b,$
;                        xoffset=offset,/editable)
     button = widget_button(base,value=heading,scr_xsize=widths[index]+4,xoffset=offset,frame=0,tooltip=heading)
     offset += widths[index]
  END
END

PRO spice_pickfile
  v = spice_pickfile__values()
  p = hash()
  
  list = spice_pickfile__read_fitslist(v.listfilename,headers=headers)
  
  ; /row_major table means every struct => one row
  ; Arrays like "editable" is [column,row], so [*,n] is all columns in row n
  
  num_columns = n_elements(headers)
  num_rows = n_elements(list)
  editable = bytarr(num_columns,num_rows)
  editable[*,0] = 1b
  background_color = replicate(230b,3,num_columns,num_rows)
  background_color[*,*,0] = 255b
  column_widths = spice_pickfile__column_widths(headers)
  
  base = widget_base(/column,xpad=0,ypad=0)
  
  headers_base = widget_base(base,frame=0b,xpad=0,ypad=0)
  
  table_base = widget_base(base,/column,/frame,event_pro="spice_pickfile__table_event",xpad=0,ypad=0)
  
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
  
  headers_widget = spice_pickfile__headers(headers_base,headers)
  table = spice_pickfile__table(table_base,p)
  
  widget_control,base,/realize
  xmanager,"spice_pickfile",base
END

spice_pickfile
END
