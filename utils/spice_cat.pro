;;
;; UTILITY FUNCTIONS
;;

PRO spice_cat::handle_remove_column, event, parts
  print,"Handle "+parts[0]+" : "+parts[1]
END


PRO spice_cat::handle_sort, event, parts
  print,"Handle "+parts[0]+" : "+parts[1]
END


PRO spice_cat::default, var, default
  compile_opt static
  IF n_elements(var) EQ 0 THEN var = default
END 


FUNCTION spice_cat::remove_non_digits, text
  compile_opt static
  bytes = byte(text)
  byte0 = (byte('0'))[0]
  byte9 = (byte('9'))[0]
  ix = where(bytes GE byte0 AND bytes LE byte9, count)
  IF count EQ 0 THEN return,""
  return, string(bytes[ix])
END

;;
;; FILTER CONVERSION text <--> array
;;

FUNCTION spice_cat::filter_as_array, filter_as_text
  IF filter_as_text EQ "<filter>" THEN return, ["<filter>"]
  parts = filter_as_text.extract("^{ (.*) , (.*) }$", /subexpr)
  IF parts[0].strlen() EQ 0 THEN return, [filter_as_text]
  return, [parts[1], parts[2]]
END


FUNCTION spice_cat::filter_as_text, filter_as_array
  IF n_elements(filter_as_array) EQ 1 THEN return, filter_as_array[0]
  IF filter_as_array.join("") EQ "" THEN return,"<filter>"
  return,"{ " + filter_as_array[0] + " , " + filter_as_array[1] + " }"
END

;;
;; DATA LOADING & MANIPULATION
;;

PRO spice_cat::load_fitslist
  fitslist = spice_read_fitslist(self.state.listfilename)  ;; Array of orderedhashes()
  
  self.state.full_list = fitslist
  self.state.full_tag_names = tag_names(fitslist[0])
  self.state.full_column_names = self.state.full_tag_names.replace('$','-')
  
  self.state.current_tag_names = self.state.full_tag_names
  self.state.current_column_names = self.state.full_column_names
END 


FUNCTION spice_cat::apply_filter, filter, tag_index
  print,"APPLYING FILTER: "+filter
  filter_as_array = self.filter_as_array(filter)
  
  IF n_elements(filter_as_array) THEN BEGIN 
     mask = self.state.full_list[*].(tag_index).matches(filter_as_array[0])
  END ELSE BEGIN
     min = filter_as_array[0]
     max = filter_as_array[1]
     
     IF filter_as_array[0] NE "" THEN mask = self.state.full_list GE min $
     ELSE                             mask = replicate(1b,n_elements(self.state.full_list))
     
     IF filter_as_array[1] NE "" THEN mask = mask AND self.state.full_list LE max
  END
  print,TOTAL(mask)
  return,mask
END


FUNCTION spice_cat::filter_mask, filters
  mask = replicate(1b,n_elements(self.state.full_list))
  
  foreach tag_name, tag_names(filters), tag_ix DO BEGIN
     filter = filters.(tag_ix)
     IF filter EQ "<filter>" THEN CONTINUE
     mask = mask AND self.apply_filter(filter, tag_ix)
  END
  print,"APPLIED ALL FILTERS: ",TOTAL(mask)
  return,mask
END


PRO spice_cat::create_displayed_list, use_columns = use_columns
  
  empty_filters_as_text = {}
  foreach tag_name, self.state.full_tag_names DO BEGIN
     empty_filters_as_text = create_struct(empty_filters_as_text, tag_name, '<filter>')
  END
  
  IF self.state.haskey("current_filters_as_text") THEN BEGIN
     current_filters_as_text = self.state.current_filters_as_text
  END ELSE BEGIN
     current_filters_as_text = empty_filters_as_text
  END
  
  new_filters_as_text = current_filters_as_text
  IF keyword_set(use_columns) THEN BEGIN
     new_filters_as_text = use_columns
     struct_assign,new_filters_as_text,empty_filters_as_text    ;; All tags = <filter>
     struct_assign,new_filters_as_text,current_filters_as_text  ;; Override with current filters
  END
  self.state.current_filters_as_text = new_filters_as_text
  self.state.current_tag_names = tag_names(new_filters_as_text)
  
  filter_mask = self.filter_mask(new_filters_as_text)
  
  ix = where(filter_mask,count)
  IF count EQ 0 THEN self.state.displayed = [new_filters_as_text] $
  ELSE               self.state.displayed = [new_filters_as_text, self.state.full_list[ix]]
END


PRO spice_cat::remake_displayed_list
  self.create_displayed_list
  widget_control,self.wid.table_id,set_value=self.state.displayed,$
                 table_ysize=n_elements(self.state.displayed)
END

;;
;; EVENT HANDLING HELPERS
;;

FUNCTION spice_cat::get_filter_by_column_name, column_name
  column_number = where(self.state.current_column_names EQ column_name)
  select = [column_number, 0, column_number, 0]
  widget_control, self.wid.table_id, get_value=filter_as_text, use_table_select=select
  return, self.filter_as_array(filter_as_text)
END


PRO spice_cat::set_filter_by_column_name, column_name, filter_as_array
  filter_as_text = self.filter_as_text(filter_as_array)
  
  IF filter_as_text EQ "" THEN filter_as_text = "<filter>"
  column_number = (where(self.state.current_column_names EQ column_name))[0]
  
  select = [column_number, 0, column_number, 0]
  widget_control, self.wid.table_id, set_value=filter_as_text, use_table_select=select
  
  ;; DON'T do "current_filters_as_text.(column_number)" directly (CORE DUMP!),
  ;; use the parentheses!
  ;; 
  current_filters_as_text = self.state.current_filters_as_text
  IF filter_as_text NE current_filters_as_text.(column_number) THEN BEGIN
     current_filters_as_text.(column_number) = filter_as_text
     self.state.current_filters_as_text = current_filters_as_text
     self.remake_displayed_list
  END
END


PRO spice_cat::set_filter_edit_color, column_name, clear=clear
  num_columns = n_elements(tag_names(self.state.displayed))
  table_select = [0, 0, num_columns-1, 0]
  widget_control, self.wid.table_id, background_color=[230b,255b,230b], use_table_select=table_select
  
  IF keyword_set(clear) THEN return
  
  column_number = (where(self.state.current_column_names EQ column_name))[0]
  table_select = [column_number, 0, column_number, 0]
  color = [150b, 255b, 150b]
  widget_control, self.wid.table_id, background_color=color, use_table_select=table_select
END


FUNCTION spice_cat::pseudo_handler_filter_focus_change_ok, event, column_name
  IF tag_names(event,/structure_name) NE "WIDGET_KBRD_FOCUS" THEN BEGIN
     print,"pseudo_handle_filter_focus_change_ok: not ok, not a focus event"
     return, 0
  END
  
  ;; We get here more often than we should, but... that's IDL's fault for
  ;; creating weird extra events (and because we can't send any extra info in
  ;; the event, whether to ignore or not).
  IF self.state.ignore_next_focus_change EQ 0 THEN BEGIN 
     IF event.enter GE 0 THEN self.set_filter_edit_color, column_name
     IF event.enter EQ 0 THEN self.set_filter_edit_color, /clear
  END
  return,1 ; It's OK, dealt with!
END

;;
;; EVENT HANDLERS
;;

PRO spice_cat::handle_text_filter_change, event, parts
  column_name = parts[1]
  
  IF self.pseudo_handler_filter_focus_change_ok(event, column_name) THEN return
  
  widget_control, event.id, get_value=new_text_filter_as_singular_array
  self.set_filter_by_column_name, column_name, new_text_filter_as_singular_array
END


PRO spice_cat::handle_range_filter_change, event, parts
  print,"Handle "+parts[0]+" : "+parts[1]
  
  min_or_max = parts[1]
  column_name = parts[2]
  
  IF self.pseudo_handler_filter_focus_change_ok(event, column_name) THEN return

  widget_control, self.wid.min_filter_text, get_value=min_value
  widget_control, self.wid.max_filter_text, get_value=max_value
  
  new_min_value = min_value
  new_max_value = max_value
  
  ;; Remove non-digit chars and adjust cursor position for numeric columns
  ;;
  IF self.state.keyword_info[column_name].type NE "t" THEN BEGIN
     new_min_value = self.remove_non_digits(min_value)
     new_max_value = self.remove_non_digits(max_value)
  
     min_text_select = widget_info(self.wid.min_filter_text, /text_select)
     max_text_select = widget_info(self.wid.max_filter_text, /text_select)
     
     min_text_select[0] = min_text_select[0] - (min_value.strlen() - new_min_value.strlen())
     max_text_select[0] = max_text_select[0] - (max_value.strlen() - new_max_value.strlen())
     
     widget_control, self.wid.min_filter_text, $
                     set_value=new_min_value, set_text_select=min_text_select 
     widget_control, self.wid.max_filter_text, $
                     set_value=new_max_value, set_text_select=max_text_select
  END
  
  new_range_filter_as_array = [new_min_value, new_max_value]
  self.set_filter_by_column_name, column_name, new_range_filter_as_array
END


PRO spice_cat::handle_flash_filter_focus, event, parts
  iteration = parts[1].toInteger()

  IF (iteration MOD 2)+1 THEN widget_control,self.wid.filter_focus_flash_text,/input_focus $
  ELSE                        widget_control, self.wid.draw_focus_away, /input_focus
  
  
  IF iteration LT 4 THEN BEGIN
     self.state.ignore_next_focus_change = 1
     iteration++
     widget_control, event.id, set_uvalue="FLASH_FILTER_FOCUS`"+iteration.toString()
     widget_control, event.id, timer=0.05
  END ELSE BEGIN
     self.state.ignore_next_focus_change = 0
  END
END

;;
;; WIDGET BUILDERS
;;

PRO spice_cat::build_text_filter, column_name, current_filter_as_array
  print,"Building text filter: " + column_name + " : " + current_filter_as_array
  current_filter_as_text = current_filter_as_array[0]
  filter_text_uvalue = "TEXT_FILTER_CHANGE`" + column_name
  self.wid.filter_label = widget_label(self.wid.filter_base, value=column_name+":")
  text_props = {editable:1b, all_events:1b, kbrd_focus_events: 1b}
  self.wid.filter_text = widget_text(self.wid.filter_base, value=current_filter_as_text,$
                                     _extra=text_props, uvalue=filter_text_uvalue)
  button_uvalue = "REBUILD_FILTER`"+column_name+"``"
  button = widget_button(self.wid.filter_base, value="Use alphabetical range", uvalue=button_uvalue)
  self.wid.filter_focus_flash_text = self.wid.filter_text
  widget_control, self.wid.filter_text, set_text_select=[0, current_filter_as_array.strlen()]
END


PRO spice_cat::build_range_filter, column_name, current_filter_as_array
  print,"Building range filter: " + column_name + " : " + current_filter_as_array.join(' - ')
  min_value = current_filter_as_array[0]
  max_value = current_filter_as_array[1]
  
  min_text_uvalue = "RANGE_FILTER_CHANGE`MIN`" + column_name
  max_text_uvalue = "RANGE_FILTER_CHANGE`MAX`" + column_name
  
  extra = {editable: 1b, all_events: 1b, kbrd_focus_events: 1b}
  min_text = widget_text(self.wid.filter_base, value=min_value, uvalue=min_text_uvalue, _extra=extra)
  label = widget_label(self.wid.filter_base, value="<= " + column_name + " <=")
  max_text = widget_text(self.wid.filter_base, value=max_value, uvalue=max_text_uvalue, _extra=extra)
  
  self.wid.min_filter_text = min_text
  self.wid.max_filter_text = max_text
  
  IF self.state.keyword_info[column_name].type EQ "t" THEN BEGIN
     button_uvalue = "REBUILD_FILTER`" + column_name + "`"
     button = widget_button(self.wid.filter_base, value="Use regexp", uvalue=button_uvalue)
  END
  
  self.wid.filter_focus_flash_text = min_text
END


PRO spice_cat::handle_rebuild_filter, dummy_event, parts
  print,"Handle "+parts[0]+" : "+parts[1]
  column_name = parts[1]
  new_filter_as_array = parts[2:*]
  
  self.set_filter_by_column_name, column_name, new_filter_as_array
  widget_control, self.wid.top_base, update=0
  widget_control, self.wid.table_id, set_table_select=[-1, -1, -1, -1]
  
  filter_base_children = widget_info(self.wid.filter_base, /all_children)
  foreach child, filter_base_children DO widget_control, child, /destroy
  
  text = n_elements(new_filter_as_array) EQ 1
  range = n_elements(new_filter_as_array) EQ 2
  
  IF text THEN self.build_text_filter, column_name, new_filter_as_array
  IF range THEN self.build_range_filter, column_name, new_filter_as_array
  
  new_filter_as_text = self.filter_as_text(new_filter_as_array)
  widget_control, self.wid.filter_base, set_uvalue="FLASH_FILTER_FOCUS`1
  widget_control, self.wid.filter_base, timer=0.05
  
  widget_control, self.wid.top_base, update=1
END


PRO spice_cat::deal_with_click_on_filter,column_name
  print,"Handle click on filter : " + column_name

  current_filter_as_array = self.get_filter_by_column_name(column_name)
  self.set_filter_edit_color,column_name
  
  IF current_filter_as_array[0] EQ "<filter>" THEN BEGIN
     column_type = self.state.keyword_info[column_name].type
     IF column_type EQ "t" THEN current_filter_as_array = [""]
     IF column_type EQ "i" THEN current_filter_as_array = ["", ""] ; range
  END
  
  self.set_filter_by_column_name, column_name, current_filter_as_array
  filter_as_uvalue_text = current_filter_as_array.join("`")
  
  ;; Simulates event with UVALUE = "REBUILD_FILTER`column_name`min`max"
  ;;                 or   UVALUE = "REBUILD_FILTER`column_name`text_filter"
  ;;
  self.handle_rebuild_filter, dummy_event, ["REBUILD_FILTER", column_name, current_filter_as_array]
END


PRO spice_cat::make_heading_context_menu, base, ev
  print,"Make header context menu"
  column_name = (tag_names(self.state.displayed))[ev.col]
  button = widget_button(base, value="Remove column", uvalue="REMOVE_COLUMN`"+column_name)
  button = widget_button(base, value="Sort ascending", uvalue="SORT`ASCENDING`"+column_name)
  button = widget_button(base, value="Sort descending", uvalue="SORT`DESCENDING`"+column_name)
  button = widget_button(base, value="Move left", uvalue="MOVE`LEFT`"+column_name)
  button = widget_button(base, value="Move right", uvalue="MOVE`RIGHT`"+column_name)
END


PRO spice_cat::make_datacell_context_menu, base, ev
  print,"Make datacell context menu"
  column_name = (tag_names(self.state.displayed))[ev.col]
  cell_value = self.state.displayed[ev.row].(ev.col).tostring()
  
  filename = self.state.displayed[ev.row].filename
  filename_uvalue = "CONTEXT_CLICK_ON_FILENAME`"+filename
  
  filter_on_value = "Filter on "+column_name+"='"+cell_value+"'"
  filter_on_value_uvalue = "CONTEXT_CLICK_ON_FULL_VALUE`+column_name"
  
  button = widget_button(base, value=filename, uvalue=filename_uvalue)
  button = widget_button(base, value=filter_on_value, uvalue=filter_on_value_uvalue)
END


PRO spice_cat::handle_context, ev
  print,"Handle : "+tag_names(ev, /structure_name)
  IF ev.row EQ 0 THEN return ; No context menu for filter row
  IF ev.col LT 0 THEN return ; No context menu for row labels
  
  base = widget_base(/CONTEXT_MENU, ev.id)
  IF ev.row EQ -1 THEN self.make_heading_context_menu, base, ev
  IF ev.row GE 1 THEN self.make_datacell_context_menu, base, ev
  
  widget_displaycontextmenu, ev.id, ev.x, ev.y, base
END

FUNCTION spice_cat::format_selection_range_string, ev
  column_range = ev.left.tostring() + ':' + ev.right.tostring()
  row_range = ev.top.tostring() + ':' + ev.bottom.tostring()
  text = '[' + column_range + ', ' + row_range + ']'
  return, text
END

PRO spice_cat::handle_table_cell_sel, ev
  sel = {left:ev.sel_left, right:ev.sel_right, top:ev.sel_top, bottom:ev.sel_bottom}
  
  ;; Ignore nonsensical [-1, -1, -1, -1] events:
  IF total([sel.left, sel.top, sel.right, sel.bottom] EQ -1) EQ 4 THEN return
  
  print,"Handle "+tag_names(ev, /structure_name) + " : " + self.format_selection_range_string(sel)
  
  ;; Only meaningful action at this stage is if the user wants
  ;; to edit the filter (1st and only 1st row)
  
  IF (sel.top NE sel.bottom) OR (sel.left NE sel.right) OR (sel.top NE 0) THEN return
  
  column_name = self.state.current_column_names[sel.left]
  self.deal_with_click_on_filter,column_name
END


PRO spice_cat::handle_all_table_events, ev, parts
  ;; We came here because all table events, anywhere,
  ;; results in uvalue="ALL_TABLE_EVENTS`"
  ;;
  type = tag_names(ev, /structure_name)
  CASE type OF 
     "WIDGET_TABLE_COL_WIDTH": return
     "WIDGET_TABLE_CH": return ;; Doh! Typing into non-editable cells triggers this!!!
     ELSE:
  END
  
  short_event_name = strmid(tag_names(ev, /structure_name), 7, 1000)
  method = "handle_" + short_event_name
  call_method, method, self, ev
END

;; COMMAND BASE EVENTS -----------------------

PRO spice_cat::handle_return_selection, event, parts
  print,"RETURN_SELECTION"
END

PRO spice_cat::handle_regenerate, event, parts
  print,"REGENERATE"
END

;; THE CATCH-ALL EVENT HANDLER ----------------

PRO spice_cat__event, event
  widget_control, event.top, get_uvalue=self
  IF event.id EQ event.top THEN BEGIN
     self.tlb_event,event
     return
  END 
  widget_control, event.id, get_uvalue=uvalue
  
  parts = uvalue.split('`')
  method = "handle_"+parts[0]
  
  call_method,method, self, event, parts
END

;; UTILITY TO KILL PREVIOUS INCARNATION -------

PRO spice_cat::new_incarnation
  COMMON spice_cat, previous_incarnation
  self.default,previous_incarnation, 0L
  IF widget_info(previous_incarnation, /valid_id) THEN BEGIN
     widget_control,previous_incarnation, /destroy
  END
  previous_incarnation = self.wid.top_base
END

;; Utility function to handle defaults etc -----

; RESIZE TABLE ACCORDING TO TLB size change!
;
PRO spice_cat::tlb_event,event
  IF tag_names(event, /structure_name) EQ "WIDGET_BASE" THEN BEGIN
     widget_control,self.wid.xsize_spacer_base, xsize=event.x
     widget_control,self.wid.ysize_spacer_base, ysize=event.y
     tablex = event.x
     tabley = event.y - 50
     widget_control,self.wid.table_id, scr_xsize=tablex, scr_ysize=tabley
   END
END


PRO spice_cat::create_buttons
  buttons = $
     [ $
     { value: "Return selection", uvalue: "RETURN_SELECTION`", ALIGN_CENTER: 1b },$
     { value: "Regenerate list", uvalue: "REGENERATE`", ALIGN_CENTER: 1b },$
     { value: "Call <program>", uvalue: "CALL_PROGRAM`", ALIGN_CENTER: 1b } $
     ]
  foreach button, buttons DO button = widget_button(self.wid.button_base, _extra=button)
END


PRO spice_cat::build_table
  ; Arrays like "editable" is [column, row], so [*, n] is all columns in row n
  
  num_table_columns = n_elements(self.state.current_column_names)
  num_table_rows = n_elements(self.state.full_list)+1
  background_color = replicate(230b, 3, num_table_columns, num_table_rows)
  background_color[1, *, 0] = 255b
  relative_column_widths = ((self.state.keyword_info.values()).toarray()).display_width
  self.wid.table_props = dictionary()
  self.wid.table_props.value = self.state.displayed
  self.wid.table_props.scroll = 1b 
  self.wid.table_props.column_labels = self.state.current_column_names
  self.wid.table_props.no_row_headers = 1b
  self.wid.table_props.row_major = 1b
  self.wid.table_props.background_color = background_color
  self.wid.table_props.column_widths = relative_column_widths * 12
  self.wid.table_props.all_events = 1b
  self.wid.table_props.context_events = 1b
  self.wid.table_props.uvalue="ALL_TABLE_EVENTS`"
  self.wid.table_props.resizeable_columns = 1b
  
  props = self.wid.table_props.tostruct()
  self.wid.table_id = widget_table(self.wid.table_base, _extra=props)
END


PRO spice_cat::build_widget
  self.wid = dictionary()
  top_props = {row: 1b, xpad: 0b, ypad: 0b, uvalue: self, tlb_size_events: 1b}
  self.wid.top_base = widget_base(title='SPICE-CAT', _extra=top_props)
  self.wid.ysize_spacer_base = widget_base(self.wid.top_base, ysize=800, xpad=0, ypad=0)
  self.wid.content_base = widget_base(self.wid.top_base, /column)
  self.wid.xsize_spacer_base = widget_base(self.wid.content_base, /row, xsize=800)
  self.wid.top_row_base = widget_base(self.wid.content_base, /row)
  self.wid.button_base = widget_base(self.wid.top_row_base, /row, /align_center, xpad=0, ypad=0)
  self.wid.filter_base = widget_base(self.wid.top_row_base, /row, xpad=0, ypad=0)
  self.wid.table_base = widget_base(self.wid.content_base, /column, /frame, xpad=0, ypad=0)
    
  self.wid.draw_focus_away = widget_text(self.wid.ysize_spacer_base, scr_xsize=1, scr_ysize=1)
  self.create_buttons
  
  self.wid.filter_label = widget_label(self.wid.filter_base, value='Filter:')
  text = widget_text(self.wid.filter_base, value="click on green line to edit")
  
  self.build_table
  
  self.new_incarnation
  widget_control,self.wid.top_base, /realize
  spice_center_window, self.wid.top_base
  widget_control,self.wid.table_id, set_table_select=[-1, -1, -1, -1]
  
  ;; Make table fill available space despite /scroll
  widget_control,self.wid.top_base, tlb_get_size=tlb_size
  base_resize_event = {widget_base}
  base_resize_event.x = tlb_size[0]
  base_resize_event.y = tlb_size[1]
  self.tlb_event, base_resize_event
END


PRO spice_cat::parameters, example_param1, example_param2, _extra=extra
  self.state = dictionary()
  self.state.keyword_info = spice_keyword_info(/all)
  spice_datadir = getenv("SPICE_DATA")
  IF spice_datadir EQ "" THEN message,"Environment variable SPICE_DATADIR is blank or not set"
  
  self.default,listfiledir,spice_datadir
  
  self.state.spice_datadir = spice_datadir
  self.state.listfiledir = listfiledir
  self.state.listfilename = concat_dir(listfiledir, 'spice_fitslist.txt')
END


;; INIT: create, realize and register widget
function spice_cat::init, example_param1,  example_param2, _extra=extra
  self.parameters, example_param1, example_param2, _extra = extra
  self.load_fitslist
  self.create_displayed_list
  self.build_widget
  
  xmanager,"spice_cat", self.wid.top_base, /no_block, event_handler="spice_cat__event"
  return,1
END

PRO spice_cat__define
  dummy = {spice_cat, state: dictionary(), wid:dictionary(), x: ''}
END

PRO spice_cat, o
  spice_cat__define
  o = obj_new('spice_cat')
END

spice_cat, o
END
