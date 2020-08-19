FUNCTION spice_cat::read_fitslist,filename,headers=headers
  compile_opt static
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
     structure_name = ""
     foreach value,values,index DO BEGIN
        structure_name += keywords[index]
        IF keyword_info[keywords[index]].type EQ "i" THEN value = long(value)
        IF keyword_info[keywords[index]].type EQ "d" THEN value = double(value)
        struct = create_struct(name=structure_name, struct,keywords[index],value)
     END
     list = [list,struct]
  END
  free_lun,lun
  headers = tag_names(list[0] )
  return,list
END 


PRO spice_cat::context_menu_event,event
  widget_control,event.id,get_uvalue=uvalue
  print,"UVALUE: "+uvalue
END

PRO spice_cat::remove_column_event,event,parts
  print,"SORT on: ",parts[1]
END

PRO spice_cat::make_heading_context_menu,base,column_name
  button = widget_button(base,value="Remove column",uvalue="REMOVE-COLUMN:"+column_name)
  button = widget_button(base,value="Sort ascending",uvalue="SORT:ASCENDING:"+column_name)
  button = widget_button(base,value="Sort descending",uvalue="SORT:DESCENDING:"+column_name)
END

PRO spice_cat::make_datacell_context_menu,base,state,row,col,column_name
  state = *self.state
  column_name = (tag_names(state.displayed))[col]
  cell_value = trim(state.displayed[row].(col))
  filename = state.displayed[row].filename
  name_and_value = column_name+": "+cell_value
  button = widget_button(base,value=filename,uvalue="FILENAME")
  button = widget_button(base,value=name_and_value,uvalue="NAME-AND-VALUE")
END


PRO spice_cat::table_widget_context_event,ev, parts
  print,"Table context event detected"
  IF ev.row EQ 0 THEN return ; No context menu for filter row
  IF ev.col LT 0 THEN return ; No context menu for row labels
  
  base = widget_base(/CONTEXT_MENU, ev.id)
  
  IF ev.row EQ -1 THEN self.make_heading_context_menu, base, ev.col
  IF ev.row GE 1 THEN self.make_datacell_context_menu, base, state, ev.row, ev.col
  
  widget_displaycontextmenu,ev.id, ev.x, ev.y, base
END

PRO spice_cat::table_widget_table_cell_sel_event,ev,parts
  print,"Table cell selection detected"
END

PRO spice_cat::table_event, ev, parts
  print,"TABLE EVENT DETECTED"
  method = "table_" + tag_names(ev,/structure_name)+"_event"
  call_method, method, self, ev, parts
END

;; COMMAND BASE EVENTS -----------------------

PRO spice_cat::return_selection_event,event,parts
  print,"RETURN_SELECTION"
END

PRO spice_cat::regenerate_event,event,parts
  print,"REGENERATE"
END

; RESIZE TABLE ACCORDING TO TLB!
;
PRO spice_cat::tlb_event,event
  help,event
  IF tag_names(event,/structure_name) EQ "WIDGET_BASE" THEN BEGIN
     tablex = event.x
     tabley = event.y
;     widget_control,event.top,xsize=tablex,ysize=tabley
     widget_control,(*self.state).table_id,xsize=tablex,ysize=tabley
     widget_control,(*self.state).table_id,scr_xsize=tablex,scr_ysize=tabley
  END
END

;; THE CATCH-ALL EVENT HANDLER ----------------

PRO spice_cat__event,event
  widget_control,event.top,get_uvalue=self
  IF event.id EQ event.top THEN BEGIN
     self.tlb_event,event
     return
  END 
  widget_control,event.id,get_uvalue=uvalue
  
  parts = uvalue.split(':')
  method = parts[0]+"_event"
  
  call_method,method,self,event,parts
END

;; UTILITY TO KILL PREVIOUS INCARNATION -------

PRO spice_cat::new_incarnation,new_incarnation
  compile_opt static
  COMMON spice_cat,previous_incarnation
  default,previous_incarnation,0L
  IF widget_info(previous_incarnation,/valid_id) THEN BEGIN
     widget_control,previous_incarnation,/destroy
  END
  previous_incarnation = new_incarnation
END

;; Utility function to handle defaults etc -----

FUNCTION spice_cat::parameters, example_param1, example_param2, _extra=extra
  compile_opt static
  default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  default,listfiledir,spice_datadir
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  return,{$
         spice_datadir:spice_datadir, $
         listfiledir:listfiledir, $
         listfilename:listfilename $
         }
END

;; INIT: create, realize and register widget
function spice_cat::init,example_param1, example_param2,_extra=extra
  print,"spice_cat::init"
  base = widget_base(/column,xpad=0,ypad=0,uvalue=self,/tlb_size_events)
  spice_cat.new_incarnation,base
  command_base = widget_base(base,/row)
  command_button = widget_button(command_base,value="Return selection",uvalue="RETURN_SELECTION:")
  command_button = widget_button(command_base,value="Regenerate fits list",uvalue="REGENERATE:")
  params = spice_cat.parameters(example_param1,example_param2,_extra = extra)
  
  list = spice_cat.read_fitslist(params.listfilename,headers=headers)
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
  
  data = { list: list, displayed: list, headers:headers }
  self.state = ptr_new(create_struct(params,data,"top",base))
  
  t = hash()
  t['value'] = displayed
  t['scroll'] = 1b 
  t['column_labels'] = headers
  t['no_row_headers'] = 1b
  t['editable'] = editable
  t['row_major'] = 1b
  t['background_color'] = background_color
  t['column_widths'] = column_widths
  t['all_events'] = 1b
  t['context_events'] = 1b
  t['uvalue']="TABLE:"
  ;t['resizeable_columns'] = 1b
  table = widget_table(table_base,_extra=hash_to_struct(t))
  *self.state = create_struct(*self.state,'table_id',table)
  widget_control,base,/realize
  xmanager,"spice_cat",base,/no_block,event_handler="spice_cat__event"
  return,1
END

PRO spice_cat__define
  dummy = {spice_cat, state: ptr_new()}  
END

PRO spice_cat,o
  spice_cat__define
  o = obj_new('spice_cat')
END

spice_cat,o
END
