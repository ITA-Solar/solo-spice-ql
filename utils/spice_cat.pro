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


FUNCTION spice_cat::column_name,column,original=original
  IF NOT keyword_set(original) THEN BEGIN
     return, (tag_names(self.state.displayed))[column]
  END ELSE BEGIN 
     return, (tag_names(self.state.displayed))[column]
  END
END

PRO spice_cat::handle_remove_column,event,parts
  print,"SORT on: ",parts[1]
END

PRO spice_cat::make_heading_context_menu,base,ev
  column_name = self.column_name(ev.col)
  button = widget_button(base,value="Remove column",uvalue="REMOVE-COLUMN:"+column_name)
  button = widget_button(base,value="Sort ascending",uvalue="SORT:ASCENDING:"+column_name)
  button = widget_button(base,value="Sort descending",uvalue="SORT:DESCENDING:"+column_name)
END

PRO spice_cat::make_datacell_context_menu,base,ev
  column_name = self.column_name(ev.col)
  cell_value = trim(self.state.displayed[ev.row].(ev.col))
  filename = self.state.displayed[ev.row].filename
  name_and_value = column_name+": "+cell_value
  button = widget_button(base,value=filename,uvalue="FILENAME")
  button = widget_button(base,value=name_and_value,uvalue="NAME-AND-VALUE")
END


PRO spice_cat::handle_table_widget_context,ev
  print,"Table context event detected"
  IF ev.row EQ 0 THEN return ; No context menu for filter row
  IF ev.col LT 0 THEN return ; No context menu for row labels
  
  base = widget_base(/CONTEXT_MENU, ev.id)
  IF ev.row EQ -1 THEN self.make_heading_context_menu, base, ev
  IF ev.row GE 1 THEN self.make_datacell_context_menu, base, ev
  
  widget_displaycontextmenu,ev.id, ev.x, ev.y, base
END

PRO spice_cat::handle_table_widget_table_cell_sel,ev
  print,"Table cell selection detected"
  help,ev,/structure
END

PRO spice_cat::handle_table_widget_table_ch,ev
  print,"Table widget cell change event detected"
END  

PRO spice_cat::handle_table, ev, parts
  type = tag_names(ev,/structure_name)
  CASE type OF 
     "WIDGET_TABLE_COL_WIDTH": return
     ELSE:
  END
  
  method = "handle_table_" + tag_names(ev,/structure_name)
  call_method, method, self, ev
END

;; COMMAND BASE EVENTS -----------------------

PRO spice_cat::handle_return_selection,event,parts
  print,"RETURN_SELECTION"
END

PRO spice_cat::handle_regenerate,event,parts
  print,"REGENERATE"
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
  method = "handle_"+parts[0]
  
  call_method,method,self,event,parts
END

;; UTILITY TO KILL PREVIOUS INCARNATION -------

PRO spice_cat::new_incarnation
;  compile_opt static
  COMMON spice_cat,previous_incarnation
  default,previous_incarnation,0L
  IF widget_info(previous_incarnation,/valid_id) THEN BEGIN
     widget_control,previous_incarnation,/destroy
  END
  previous_incarnation = self.state.top_base
END

;; Utility function to handle defaults etc -----

PRO spice_cat::parameters, example_param1, example_param2, _extra=extra
  self.state = dictionary()
  
  default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  default,listfiledir,spice_datadir
  
  self.state.spice_datadir = spice_datadir
  self.state.listfiledir = listfiledir
  self.state.listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
END


PRO spice_cat::rebuild_table,scr_xsize,scr_ysize
  
END
; RESIZE TABLE ACCORDING TO TLB size change!
;
PRO spice_cat::tlb_event,event
  help,event
  IF tag_names(event,/structure_name) EQ "WIDGET_BASE" THEN BEGIN
     tablex = event.x
     tabley = event.y
     widget_control,self.state.table_id,scr_xsize=tablex,scr_ysize=tabley
   END
END

;; INIT: create, realize and register widget
function spice_cat::init,example_param1, example_param2,_extra=extra
  self.parameters, example_param1,example_param2,_extra = extra
  
  self.state.top_base = widget_base(/row,xpad=0,ypad=0,uvalue=self,/tlb_size_events)
  self.new_incarnation
  
  ysize_spacer_base = widget_base(self.state.top_base,/column)
  
  content_base = widget_base(self.state.top_base,/column)
  command_base = widget_base(content_base,/row)
  command_button = widget_button(command_base,value="Return selection",uvalue="RETURN_SELECTION:")
  command_button = widget_button(command_base,value="Regenerate fits list",uvalue="REGENERATE:")
  label = widget_label(command_base,value='                     ')
  label = widget_label(command_base,value='                     ')
  label = widget_label(command_base,value='                     ')
  label = widget_label(command_base,value='                     ')
  label = widget_label(command_base,value='                     ')
  label = widget_label(command_base,value='                     ')
  
  
  self.state.full_list = spice_cat.read_fitslist(self.state.listfilename,headers=headers)
  self.state.headers = headers
  
  filter = create_struct(name=tag_names(self.state.full_list,/structure_name))
  self.state.displayed = [filter,self.state.full_list]
  
  ; Arrays like "editable" is [column,row], so [*,n] is all columns in row n
  
  num_table_columns = n_elements(self.state.headers)
  num_table_rows = n_elements(self.state.full_list)+1
  editable = bytarr(num_table_columns,num_table_rows)
  editable[*,0] = 1b
  background_color = replicate(230b,3,num_table_columns,num_table_rows)
  background_color[*,*,0] = 255b
  column_widths = (spice_keyword_info(self.state.headers)).display_width * 12
  
  self.state.table_base = widget_base(content_base,/column,/frame,xpad=0,ypad=0)
    
  self.state.table_props = dictionary()
  self.state.table_props.value = self.state.displayed
  self.state.table_props.scroll = 1b 
  self.state.table_props.column_labels = self.state.headers
  self.state.table_props.no_row_headers = 1b
  self.state.table_props.editable = editable
  self.state.table_props.row_major = 1b
  self.state.table_props.background_color = background_color
  self.state.table_props.column_widths = column_widths
  self.state.table_props.all_events = 1b
  self.state.table_props.context_events = 1b
  self.state.table_props.uvalue="TABLE:"
  self.state.table_props.resizeable_columns = 1b
  
  props = hash_or_dict_to_struct(self.state.table_props)
  self.state.table_id = widget_table(self.state.table_base, _extra=props)
  
  widget_control,self.state.top_base,/realize
  
  ;; Make table fill available space despite /scroll
  widget_control,self.state.top_base,tlb_get_size=tlb_size
  base_resize_event = {widget_base}
  base_resize_event.x = tlb_size[0]
  base_resize_event.y = tlb_size[1]
  self.tlb_event, base_resize_event
  
  xmanager,"spice_cat",self.state.top_base,/no_block,event_handler="spice_cat__event"
  return,1
END

PRO spice_cat__define
  dummy = {spice_cat, state: dictionary()}  
END

PRO spice_cat,o
  spice_cat__define
  o = obj_new('spice_cat')
END

spice_cat,o
END
