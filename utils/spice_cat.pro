PRO spice_cat::default,var,default
  compile_opt static
  IF n_elements(var) EQ 0 THEN var = default
END 


PRO spice_cat::read_fitslist,filename,headers=headers
  openr,lun,self.state.listfilename,/get_lun
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
  
  self.state.full_list = list
  self.state.headers = tag_names(list[0])
END 


FUNCTION spice_cat::column_name,column,original=original
  IF NOT keyword_set(original) THEN BEGIN
     return, (tag_names(self.state.displayed))[column]
  END ELSE BEGIN 
     return, (tag_names(self.state.displayed))[column]
  END
END


PRO spice_cat::handle_remove_column,event,parts
  print,"Remove column: ",parts[1]
END


PRO spice_cat::handle_sort,event,parts
  print,"Handle sort: ",parts[1]
END


PRO spice_cat::handle_filter_event,event,parts
  print,"Handle filter event: "+parts[1]
  widget_control,event.id,get_value=filter_text
  print,"FILTER: "+filter_text
END

PRO spice_cat::handle_filter_flash_text,event,parts
  print,"Handle filter unselect_text: "+parts[1]
  iteration = parts[1].toInteger()
  widget_control,self.wid.filter_text,get_value=text
  text_len = text.strlen()
  IF (iteration MOD 2) THEN widget_control,self.wid.filter_text,set_text_select=[0,0] $
  ELSE                      widget_control,self.wid.filter_text,set_text_select=[0,text_len]
  IF iteration LT 5 THEN BEGIN
     iteration++
     widget_control,event.id,set_uvalue="FILTER_FLASH_TEXT:"+iteration.toString()
     widget_control,event.id,timer=0.1
  END
END

PRO spice_cat::handle_edit_filter,event,parts
  print,"Handle edit filter: ",(keyword = parts[1])
  widget_control,self.wid.top_base,update=0
  
  filter_base = self.wid.filter_base
  filter_base_children = widget_info(filter_base,/all_children)
  foreach child,filter_base_children DO widget_control,child,/destroy
  
  self.wid.filter_label = widget_label(filter_base, value="Filter on "+parts[1]+":")
  current_value = "<current-value>"
  self.wid.filter_text = widget_text(self.wid.filter_base, value=current_value,/editable,$
                                     /all_events,uvalue="FILTER_EVENT:"+parts[1])
  widget_control,self.wid.filter_text,/input_focus
  
  timer_base = widget_base(filter_base,uvalue="FILTER_FLASH_TEXT:1",map=0)
  widget_control,self.wid.top_base,update=1
  widget_control,self.wid.filter_text,set_text_select=[0,current_value.strlen()]
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
  widget_control,timer_base,timer=0.1
END


PRO spice_cat::make_heading_context_menu,base,ev
  column_name = self.column_name(ev.col)
  button = widget_button(base,value="Remove column",uvalue="REMOVE_COLUMN:"+column_name)
  button = widget_button(base,value="Sort ascending",uvalue="SORT:ASCENDING:"+column_name)
  button = widget_button(base,value="Sort descending",uvalue="SORT:DESCENDING:"+column_name)
END


PRO spice_cat::make_datacell_context_menu,base,ev
  column_name = self.column_name(ev.col)
  cell_value = self.state.displayed[ev.row].(ev.col).tostring()
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
  IF ev.row GE 1 THEN BEGIN
     self.make_datacell_context_menu, base, ev
  END
  
  widget_displaycontextmenu,ev.id, ev.x, ev.y, base
END


PRO spice_cat::handle_table_widget_table_cell_sel,ev
  e = {left:ev.sel_left,right:ev.sel_right,top:ev.sel_top,bottom:ev.sel_bottom}
  
  column_range = e.left.tostring() + ':' + e.right.tostring()
  row_range = e.top.tostring() + ':' + e.bottom.tostring()
  text = '[' + column_range + ', ' + row_range + ']'
  print,"Table cell selection detected: "+text
  IF (e.top NE e.bottom) OR (e.left NE e.right) OR (e.top NE 0) THEN return
  column_name = self.state.headers[e.left]
  
  self.handle_edit_filter,ev,["EDIT_FILTER",column_name]
END


PRO spice_cat::handle_table_widget_table_ch,ev
  print,"Table cell change"
  IF ev.x LT 0 OR ev.y LT 0 THEN BEGIN
     print,"Non-event!"
     return
  END
END  

PRO spice_cat::handle_table_widget_table_del,ev
  print,"Table cell text deletion"
END 

PRO spice_cat::handle_table_widget_table_text_sel
  print,"Table cell text selection"
END

PRO spice_cat::handle_all_table, ev, parts
  ;; We came here because all table events, anywhere,
  ;; results in uvalue="ALL_TABLE:"
  ;;
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
  COMMON spice_cat,previous_incarnation
  self.default,previous_incarnation,0L
  IF widget_info(previous_incarnation,/valid_id) THEN BEGIN
     widget_control,previous_incarnation,/destroy
  END
  previous_incarnation = self.wid.top_base
END

;; Utility function to handle defaults etc -----

PRO spice_cat::parameters, example_param1, example_param2, _extra=extra
  self.state = dictionary()
  
  IF getenv("SPICE_DATA") NE "" THEN spice_datadir = getenv("SPICE_DATA")
  self.default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  
  self.default,listfiledir,spice_datadir
  
  self.state.spice_datadir = spice_datadir
  self.state.listfiledir = listfiledir
  self.state.listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
END


; RESIZE TABLE ACCORDING TO TLB size change!
;
PRO spice_cat::tlb_event,event
  IF tag_names(event,/structure_name) EQ "WIDGET_BASE" THEN BEGIN
     widget_control,self.wid.xsize_spacer_base,xsize=event.x
     widget_control,self.wid.ysize_spacer_base,ysize=event.y
     tablex = event.x
     tabley = event.y - 50
     widget_control,self.wid.table_id,scr_xsize=tablex,scr_ysize=tabley
   END
END

PRO spice_cat::build_table
  ; Arrays like "editable" is [column,row], so [*,n] is all columns in row n
  
  num_table_columns = n_elements(self.state.headers)
  num_table_rows = n_elements(self.state.full_list)+1
  editable = bytarr(num_table_columns,num_table_rows)
  editable[*,0] = 1b
  background_color = replicate(230b,3,num_table_columns,num_table_rows)
  background_color[*,*,0] = 255b
  column_widths = (spice_keyword_info(self.state.headers)).display_width * 12
  
  self.wid.table_props = dictionary()
  self.wid.table_props.value = self.state.displayed
  self.wid.table_props.scroll = 1b 
  self.wid.table_props.column_labels = self.state.headers
  self.wid.table_props.no_row_headers = 1b
  self.wid.table_props.editable = editable
  self.wid.table_props.row_major = 1b
  self.wid.table_props.background_color = background_color
  self.wid.table_props.column_widths = column_widths
  self.wid.table_props.all_events = 1b
  self.wid.table_props.context_events = 1b
  self.wid.table_props.uvalue="ALL_TABLE:"
  self.wid.table_props.resizeable_columns = 1b
  
  props = self.wid.table_props.tostruct()
  self.wid.table_id = widget_table(self.wid.table_base, _extra=props)
END

PRO spice_cat::set_window_position
  base = widget_base()
  spacer = widget_base(base,xsize=5000,ysize=5000)
  widget_control,base,/realize
  widget_control,base,tlb_get_size=screen_size
  print,"SCREEN SIZE: ",screen_size
  widget_control,base,/destroy

  
  widget_control,self.wid.top_base,tlb_get_size=tlb_size
  print,tlb_size
  ;; Left edge offset from left edge of screen is...
  ;; middle of screen minus half our size
  offsets = screen_size/2 - tlb_size/2
  widget_control,self.wid.top_base,xoffset=offsets[0],yoffset=offsets[1]
END

PRO spice_cat::create_buttons
  buttons = $
     [ $
     { value: "Return selection", uvalue: "RETURN_SELECTION:", ALIGN_CENTER: 1b },$
     { value: "Regenerate list", uvalue: "REGENERATE:", ALIGN_CENTER: 1b },$
     { value: "Call <program>", uvalue: "CALL_PROGRAM:", ALIGN_CENTER: 1b } $
     ]
  button = widget_button(self.wid.button_base,_extra=buttons[0])
  button = widget_button(self.wid.button_base,value="Regenerate fits list",uvalue="REGENERATE:",/align_center)
END

PRO spice_cat::build_widget
  self.wid = dictionary()
  
  self.wid.top_base = widget_base(/row,xpad=0,ypad=0,uvalue=self,/tlb_size_events,title='SPICE-CAT')
  self.wid.ysize_spacer_base = widget_base(self.wid.top_base,/column,ysize=800)
  self.wid.content_base = widget_base(self.wid.top_base,/column)
  self.wid.xsize_spacer_base = widget_base(self.wid.content_base,/row,xsize=800)
  self.wid.top_row_base = widget_base(self.wid.content_base,/row)
  self.wid.button_base = widget_base(self.wid.top_row_base,/row,/ALIGN_CENTER)
  self.wid.filter_base = widget_base(self.wid.top_row_base,/row)
  self.wid.table_base = widget_base(self.wid.content_base,/column,/frame,xpad=0,ypad=0)
    
  self.create_buttons
  
  self.wid.filter_label = widget_label(self.wid.filter_base,value='Filter:')
  text = widget_text(self.wid.filter_base,value="right-click headers to edit")
  
  self.build_table
  
  self.new_incarnation
  widget_control,self.wid.top_base,/realize
  self.set_window_position
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
  
  ;; Make table fill available space despite /scroll
  widget_control,self.wid.top_base,tlb_get_size=tlb_size
  base_resize_event = {widget_base}
  base_resize_event.x = tlb_size[0]
  base_resize_event.y = tlb_size[1]
  self.tlb_event, base_resize_event
END

;; INIT: create, realize and register widget
function spice_cat::init,example_param1, example_param2,_extra=extra
  self.parameters, example_param1,example_param2,_extra = extra
  self.read_fitslist
  
  filter = create_struct(name=tag_names(self.state.full_list,/structure_name))
  self.state.displayed = [filter,self.state.full_list]
  
  self.build_widget
  
  xmanager,"spice_cat",self.wid.top_base,/no_block,event_handler="spice_cat__event"
  return,1
END

PRO spice_cat__define
  dummy = {spice_cat, state: dictionary(), wid:dictionary() }  
END

PRO spice_cat,o
  spice_cat__define
  o = obj_new('spice_cat')
END

spice_cat,o
END
