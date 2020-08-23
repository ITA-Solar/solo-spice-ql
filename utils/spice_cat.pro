PRO spice_cat::handle_remove_column,event,parts
  print,"Remove column: ",parts[1]
END


PRO spice_cat::handle_sort,event,parts
  print,"Handle sort: ",parts[1]
END


PRO spice_cat::default,var,default
  compile_opt static
  IF n_elements(var) EQ 0 THEN var = default
END 


PRO spice_cat::load_fitslist,filename
  openr,lun,self.state.listfilename,/get_lun
  t = ''
  readf,lun,t
  keywords = strsplit(/extract,t,",")
  list = []
  keyword_info = spice_keyword_info(/all,/return_as_hash)
  WHILE NOT eof(lun) DO BEGIN
     readf,lun,t
     values = strsplit(/extract,t,string(9b))
     struct = { }
     structure_name = ""
     foreach value,values,index DO BEGIN
        structure_name += keywords[index]
        struct = create_struct(name=structure_name, struct,keywords[index],value)
     END
     list = [list,struct]
  END
  free_lun,lun
  self.state.full_list = list
  self.state.column_names = tag_names(list[0])
END 


FUNCTION spice_cat::column_name,column,original=original
  IF NOT keyword_set(original) THEN BEGIN
     return, (tag_names(self.state.displayed))[column]
  END ELSE BEGIN 
     return, (tag_names(self.state.displayed))[column]
  END
END


FUNCTION spice_cat::filter_as_array,filter_as_text
  IF filter_as_text EQ "<filter>" THEN return,['']
  parts = (strsplit(filter_as_text,"<=",/extract,/preserve_null)).trim()
  IF n_elements(parts) EQ 1 THEN return, parts[0]
  min_max = ["*","*"]
  IF parts[0] NE '' THEN min_max[0] = parts[0]
  IF parts[1] NE '' THEN min_max[1] = parts[1]
  return,min_max
END


FUNCTION spice_cat::filter_as_text,filter_as_array
  IF n_elements(filter_as_array) EQ 1 THEN return,filter_as_array[0]
  return,filter_as_array.join(" <= ")
END


FUNCTION spice_cat::get_filter_as_array_by_column_name,column_name
  column_number = where(self.state.column_names EQ column_name)
  select = [column_number,0,column_number,0]
  widget_control,self.wid.table_id,get_value=filter_as_text,use_table_select=select
  return,self.filter_as_array(filter_as_text)
END


PRO spice_cat::set_filter_by_column_name,column_name,filter_as_array
  filter_as_text = self.filter_as_text(filter_as_array)
  column_number = (where(self.state.column_names EQ column_name))[0]
  select = [column_number,0,column_number,0]
  widget_control,self.wid.table_id,set_value=filter_as_text,use_table_select=select
END


PRO spice_cat::handle_text_filter_change_event,event,parts
  column_name = parts[1]
  print,"Handle filter event: " + column_name
  widget_control,event.id,get_value=new_filter_text_as_array
  print,"FILTER: "+new_filter_text_as_array[0]
  self.set_filter_by_column_name,column_name,new_filter_text_as_array
END


PRO spice_cat::handle_filter_flash_texts,event,parts
  print,"Handle filter unselect_text: "+parts[1]
  iteration = parts[1].toInteger()

  IF (iteration MOD 2)+1 THEN BEGIN 
     foreach text, self.wid.filter_flash_texts DO widget_control,text, /input_focus
  END ELSE BEGIN
     widget_control,self.wid.draw_focus,/input_focus
  END
  
  IF iteration LT 8 THEN BEGIN
     iteration++
     widget_control,event.id,set_uvalue="FILTER_FLASH_TEXTS:"+iteration.toString()
     widget_control,event.id,timer=0.05
  END ELSE BEGIN
     widget_control,self.wid.filter_flash_texts,get_value=value
     IF value EQ "<filter>" THEN widget_control,self.wid.filter_text,set_value=""
  END
END


PRO spice_cat::build_text_filter,column_name,current_filter_as_array
  print,"Building text filter: " + column_name + " : " + current_filter_as_array
  current_filter_as_text = current_filter_as_array[0]
  filter_text_uvalue = "TEXT_FILTER_CHANGE_EVENT:" + column_name
  self.wid.filter_label = widget_label(self.wid.filter_base, value=column_name+":")
  self.wid.filter_text = widget_text(self.wid.filter_base, value=current_filter_as_text,$
                                     /editable,/all_events,uvalue=filter_text_uvalue)
  self.wid.filter_flash_texts = self.wid.filter_text
  widget_control,self.wid.draw_focus,/input_focus
  widget_control,self.wid.filter_text,set_text_select=[0,current_filter_as_array.strlen()]
END


PRO spice_cat::build_range_filter,column_name,current_filter_as_array
  print,"Building range filter: " + column_name + " : " + current_filter_as_array
;  self.wid.filter_label = widget_label
END


PRO spice_cat::handle_rebuild_filter, dummy_event, parts
  column_name = parts[1]
  current_filter_as_array = parts[2:*]
  
  widget_control,self.wid.top_base,update=0
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
  
  filter_base_children = widget_info(self.wid.filter_base,/all_children)
  foreach child,filter_base_children DO widget_control,child,/destroy
  
  current_filter_type = n_elements(current_filter_as_array) EQ 1 ? "T" : "R"
  
  IF current_filter_type EQ "R" THEN self.build_range_filter, column_name, current_filter_as_array
  IF current_filter_type EQ "T" THEN self.build_text_filter, column_name, current_filter_as_array
  
  current_filter_as_text = self.filter_as_text(current_filter_as_array)
  widget_control,self.wid.filter_base, set_uvalue="FILTER_FLASH_TEXTS:1
  widget_control,self.wid.filter_base,timer=0.05
  
  widget_control,self.wid.top_base,update=1
END

; Rebuilding the filter-editing base contents can occur in *two* situations,
; 1) after a click in the filter row, *or* 2) after a click to chage filter
; type.
;
; Case 1: after click in filter row:
;
;       Current_filter_as_array value available through selection value
;
;       a) if no valid filter in place, set
;            R''    for numeric types
;            T'     for text types
;          and continue with b:
;
;       b) a valid filter is in place, build the corresponding base
;            R' min' max (range)  NOTE: This is allowed for *TEXTS* too!
;            T' regexp            NOTE: *NOT ALLOWED* for numeric column types
;
; Case 2: When *switching*, INVALIDATE ANY EXISTING FILTER! 
;         Set a blank filter of the indicated type, then 
;         GOTO b above!
; 

PRO spice_cat::handle_click_on_filter,column_name
  print,"Click on filter: "+column_name
  
  current_filter_as_array = self.get_filter_as_array_by_column_name(column_name)
  
  ;; Case 1a:
  IF current_filter_as_array[0] EQ "<filter>" THEN BEGIN
     column_type = self.state.keyword_info[column_name].type
     IF column_type EQ "t" THEN current_filter_as_array = ""
     IF column_type EQ "i" THEN current_filter_as_array = "* <= " + column_name + " <= *"
  END
  
  ;; Case 2b:
  self.set_filter_by_column_name, column_name, current_filter_as_array
  
  ;; Corresponds to uvalue="REBUILD_FILTER:column_name:type"
  self.handle_rebuild_filter,dummy_event,["REBUILD_FILTER", column_name, current_filter_as_array]
END


PRO spice_cat::make_heading_context_menu,base,ev
  column_name = self.column_name(ev.col)
  button = widget_button(base,value="Remove column",uvalue="REMOVE_COLUMN:"+column_name)
  button = widget_button(base,value="Sort ascending",uvalue="SORT:ASCENDING:"+column_name)
  button = widget_button(base,value="Sort descending",uvalue="SORT:DESCENDING:"+column_name)
END


PRO spice_cat::make_datacell_context_menu,base,ev
  print,"Make datacell context menu"
  column_name = self.column_name(ev.col)
  cell_value = self.state.displayed[ev.row].(ev.col).tostring()
  filename = self.state.displayed[ev.row].filename
  filter_on_value = "Filter on "+column_name+"='"+cell_value+"'"
  button = widget_button(base,value=filename,uvalue="CONTEXT_CLICK_ON_FILENAME:"+filename)
  button = widget_button(base,value=filter_on_value,uvalue="CONTEXT_CLICK_ON_NAME_AND_VALUE")
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

FUNCTION spice_cat::selection_range_string,ev
  column_range = ev.left.tostring() + ':' + ev.right.tostring()
  row_range = ev.top.tostring() + ':' + ev.bottom.tostring()
  text = '[' + column_range + ', ' + row_range + ']'
  return,text
END

PRO spice_cat::handle_table_widget_table_cell_sel,ev
  sel = {left:ev.sel_left,right:ev.sel_right,top:ev.sel_top,bottom:ev.sel_bottom}
  
  print,"Table cell selection detected: "+self.selection_range_string(sel)
  
  ;; Only meaningful action at this stage is if the user wants
  ;; to edit the filter (1st and only 1st row)
  
  IF (sel.top NE sel.bottom) OR (sel.left NE sel.right) OR (sel.top NE 0) THEN return
  
  column_name = self.state.column_names[sel.left]
  self.handle_click_on_filter,column_name
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
  self.state.keyword_info = spice_keyword_info(/all,/return_as_hash)
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

PRO spice_cat::build_table
  ; Arrays like "editable" is [column,row], so [*,n] is all columns in row n
  
  num_table_columns = n_elements(self.state.column_names)
  num_table_rows = n_elements(self.state.full_list)+1
  background_color = replicate(230b,3,num_table_columns,num_table_rows)
  background_color[1,*,0] = 255b
  keyword_info = spice_keyword_info(self.state.column_names)
  column_widths = keyword_info.display_width * 12
  self.wid.table_props = dictionary()
  self.wid.table_props.value = self.state.displayed
  self.wid.table_props.scroll = 1b 
  self.wid.table_props.column_labels = self.state.column_names
  self.wid.table_props.no_row_headers = 1b
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

PRO spice_cat::create_buttons
  buttons = $
     [ $
     { value: "Return selection", uvalue: "RETURN_SELECTION:", ALIGN_CENTER: 1b },$
     { value: "Regenerate list", uvalue: "REGENERATE:", ALIGN_CENTER: 1b },$
     { value: "Call <program>", uvalue: "CALL_PROGRAM:", ALIGN_CENTER: 1b } $
     ]
  foreach button_info, buttons DO button = widget_button(self.wid.button_base,_extra=buttons[0])
END

PRO spice_cat::build_widget
  self.wid = dictionary()
  self.wid.top_base = widget_base(/row,xpad=0,ypad=0,uvalue=self,/tlb_size_events,title='SPICE-CAT')
  self.wid.ysize_spacer_base = widget_base(self.wid.top_base,ysize=800,xpad=0,ypad=0)
  self.wid.content_base = widget_base(self.wid.top_base,/column)
  self.wid.xsize_spacer_base = widget_base(self.wid.content_base,/row,xsize=800)
  self.wid.top_row_base = widget_base(self.wid.content_base,/row)
  self.wid.button_base = widget_base(self.wid.top_row_base,/row,/ALIGN_CENTER)
  self.wid.filter_base = widget_base(self.wid.top_row_base,/row)
  self.wid.table_base = widget_base(self.wid.content_base,/column,/frame,xpad=0,ypad=0)
    
  self.wid.draw_focus = widget_text(self.wid.ysize_spacer_base,scr_xsize=1,scr_ysize=1)
  self.create_buttons
  
  self.wid.filter_label = widget_label(self.wid.filter_base,value='Filter:')
  text = widget_text(self.wid.filter_base,value="click on green line to edit")
  
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

PRO spice_cat::build_displayed_list
  ;; Ooops! We get a core dump if we first create self.state.filters, and
  ;; then try to manipulate it with e.g. self.state.filters.(i) = "<filter>",
  ;; probably b/c .filters is a DICTIONARY() entry
  
  filters = create_struct(name=tag_names(self.state.full_list,/structure_name))
  column_names = tag_names(self.state.full_list)
  FOR i=0,n_elements(column_names)-1 DO filters.(i) = "<filter>"
  self.state.filters = filters
  select_mask = replicate(1b,n_elements(self.state.full_list))
  ix = where(select_mask)
  self.state.displayed = [self.state.filters, self.state.full_list[ix]]
END

;; INIT: create, realize and register widget
function spice_cat::init,example_param1, example_param2,_extra=extra
  self.parameters, example_param1,example_param2,_extra = extra
  self.load_fitslist
  self.build_displayed_list
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
