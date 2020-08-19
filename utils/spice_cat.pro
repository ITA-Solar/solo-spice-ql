FUNCTION spice_cat__read_fitslist,filename,headers=headers
  openr,lun,filename,/get_lun
  t = ''
  readf,lun,t
  keywords = strsplit(/extract,t,",")
  list = []
  keyword_info = spice_keyword_info(/all,/hash)
  WHILE NOT eof(lun) DO BEGIN
     readf,lun,t
     values = strsplit(/extract,t,string(9b))
     struct = { }
     struct_name = ""
     foreach value,values,index DO BEGIN
        struct_name += keywords[index]
        IF keyword_info[keywords[index]].type EQ "i" THEN value = long(value)
        IF keyword_info[keywords[index]].type EQ "d" THEN value = double(value)
        struct = create_struct(name=struct_name, struct,keywords[index],value)
     END
     list = [list,struct]
  END
  free_lun,lun
  headers = tag_names(list[0] )
  return,list
END 


PRO spice_cat__context_menu_event,event
  widget_control,event.id,get_uvalue=uvalue
  print,"UVALUE: "+uvalue
END


PRO spice_cat__make_heading_context_menu,base,column_name
  button = widget_button(base,value="Remove column",uvalue="REMOVE-COLUMN:"+column_name)
  button = widget_button(base,value="Sort ascending",uvalue="ASCENDING:"+column_name)
  button = widget_button(base,value="Sort descending",uvalue="DESCENDING:"+column_name)
END

PRO spice_cat__make_datacell_contet_menu,base,state,row,col,column_name
  column_name = (tag_names(state.displayed))[col]
  cell_value = trim(state.displayed[row].(col))
  filename = state.displayed[row].filename
  name_and_value = column_name+": "+cell_value
  button = widget_button(base,value=filename,uvalue="FILENAME")
  button = widget_button(base,value=name_and_value,uvalue="NAME-AND-VALUE")
END

FUNCTION spice_cat__table_make_context_menu,state,ev
  IF ev.row EQ 0 THEN return,-1 ; No context menu for filter row
  IF ev.col LT 0 THEN return,-1 ; No context menu for row labels
  
  
  base = widget_base(/CONTEXT_MENU,ev.id,event_pro="spice_cat__context_menu_event")
  
  IF ev.row EQ -1 THEN spice_cat__make_heading_context_menu, base, ev.col
  IF ev.row GE 1 THEN spice_cat__make_datacell_contet_menu, base, state, ev.row, ev.col

  return,base
END


PRO spice_cat__table_event,ev
  widget_control,ev.top,get_uvalue=uvalue
  type = tag_names(ev,/structure_name)
  IF type EQ "WIDGET_CONTEXT" THEN BEGIN
     help,ev,/structure 
     menu = spice_cat__table_make_context_menu(uvalue,ev)
     IF menu NE -1 THEN widget_displaycontextmenu,ev.id, ev.x,ev.y, menu
  END
  IF type EQ "WIDGET_TABLE_CELL_SEL" THEN BEGIN
     ; widget_control,ev.handler,set_table_select=[-1,-1,-1,-1]
  END
  help,ev
END


FUNCTION spice_cat__parameters
  default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  default,listfiledir,spice_datadir
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  return,{$
         spice_datadir:spice_datadir, $
         listfiledir:listfiledir, $
         listfilename:listfilename $
         }
END


PRO spice_cat__new_incarnation,new_incarnation
  COMMON spice_cat,previous_incarnation
  default,previous_incarnation,0L
  IF widget_info(previous_incarnation,/valid_id) THEN BEGIN
     widget_control,previous_incarnation,/destroy
  END
  previous_incarnation = new_incarnation
END


PRO spice_cat
  base = widget_base(/column,xpad=0,ypad=0)
  spice_cat__new_incarnation,base
  
  params = spice_cat__parameters()
  
  list = spice_cat__read_fitslist(params.listfilename,headers=headers)
  filter = list[0]
  foreach tag, tag_names(filter), index DO filter.(index) = ""
  displayed = [filter,list]
  
  ; Arrays like "editable" is [column,row], so [*,n] is all columns in row n
  
  num_columns = n_elements(headers)
  num_rows = n_elements(list)
  editable = bytarr(num_columns,num_rows)
  editable[*,0] = 1b
  background_color = replicate(230b,3,num_columns,num_rows)
  background_color[*,*,0] = 255b
  column_widths = (spice_keyword_info(headers)).display_width * 12
  
  table_base = widget_base(base,/column,/frame,xpad=0,ypad=0)
  
  t = hash()
  t['value'] = displayed
  t['column_labels'] = headers
  t['no_row_headers'] = 1b
  t['editable'] = editable
  t['row_major'] = 1b
  t['background_color'] = background_color
  t['column_widths'] = column_widths
  t['all_events'] = 1b
  t['context_events'] = 1b
  t['event_pro']="spice_cat__table_event"
  table = widget_table(table_base,_extra=hash_to_struct(t))
  
  data = { list: list, displayed: list, headers:headers }
  state = create_struct(params,data)
  widget_control,base,set_uvalue=state
  
  widget_control,base,/realize
  xmanager,"spice_cat",base,/no_block
END

xmanager,/cleanup
spice_cat
END
