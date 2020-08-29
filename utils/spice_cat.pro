;;
PRO _____________________UTILITY_FUNCTIONS & END
;;
PRO spice_cat::modal_message,message,timer=timer
  spice_modal_message, self.wid.top_base, message, timer=timer
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


FUNCTION spice_cat::format_selection_range_string, ev
  column_range = ev.left.tostring() + ':' + ev.right.tostring()
  row_range = ev.top.tostring() + ':' + ev.bottom.tostring()
  text = '[' + column_range + ', ' + row_range + ']'
  return, text
END


FUNCTION spice_cat::empty_filters_as_text, tag_names
  filters_as_text = {}
  foreach tag, tag_names DO filters_as_text = create_struct(filters_as_text, tag, '<filter>')
  return, filters_as_text
END

PRO spice_cat::destroy_children, parent
  children = widget_info(parent, /all_children)
  IF children[0] EQ 0 THEN return
  foreach child, children DO widget_control, child, /destroy
END

;;
PRO _____________________FILTER_CONVERSION___TEXT_vs_ARRAY       & END
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
PRO _____________________DATA_LOADING_AND_MANIPULATION           & END
;;

PRO spice_cat::load_fitslist
  fitslist = spice_read_fitslist(self.state.listfilename)  ;; Array of orderedhashes()
  
  self.state.full_list = fitslist
  self.state.full_tag_names = tag_names(fitslist[0])
  self.state.full_column_names = self.state.full_tag_names.replace('$','-')
  
  self.state.current_tag_names = self.state.full_tag_names
  self.state.current_column_names = self.state.full_column_names
  self.state.current_sort_column = 'DATE-BEG'
  self.state.current_sort_order = "ascending"
END 


FUNCTION spice_cat::apply_filter, filter, full_list_tag_index
  filter_as_array = self.filter_as_array(filter)
  
  IF n_elements(filter_as_array) EQ 1 THEN BEGIN 
     mask = self.state.full_list[*].(full_list_tag_index).matches(filter_as_array[0],/fold_case)
  END ELSE BEGIN
     min = filter_as_array[0]
     max = filter_as_array[1]
     
     column_name = self.state.full_column_names[full_list_tag_index]
     
     IF (self.state.keyword_info[column_name].type) EQ "i" THEN BEGIN
        IF min NE "" THEN min = min + 0.0d
        IF max NE "" THEN max = max + 0.0d
     END
     
     IF filter_as_array[0] EQ "" THEN BEGIN
        mask = replicate(1b,n_elements(self.state.full_list))
     END ELSE BEGIN
        mask = self.state.full_list[*].(full_list_tag_index) GE min
     END
     
     IF filter_as_array[1] NE "" THEN BEGIN
        mask = mask AND self.state.full_list[*].(full_list_tag_index) LE max
     END
  END
  print,TOTAL(mask)
  return,mask
END


FUNCTION spice_cat::apply_filters, filters
  mask = replicate(1b,n_elements(self.state.full_list))
  
  foreach current_filter_tag_name, tag_names(filters), current_filter_tag_ix DO BEGIN
     current_filter = filters.(current_filter_tag_ix)
     IF current_filter EQ "<filter>" THEN CONTINUE
     
     ; apply_filter() operates on full_list, we must supply
     ; full_list_tag_index, not filter_tag_index

     full_list_tag_index = (where(self.state.full_tag_names EQ current_filter_tag_name))[0]
     
     mask = mask AND self.apply_filter(current_filter, full_list_tag_index)
  END
  print,"APPLIED ALL CURRENT_FILTERS: ",long(TOTAL(mask))
  return,mask
END


PRO spice_cat::update_current_filter, keywords
  spice_default, keywords, self.state.current_column_names
  
  IF self.state.haskey("current_filters_as_text") THEN BEGIN
     current_filters_as_text = self.state.current_filters_as_text
  END ELSE BEGIN
     current_filters_as_text = self.empty_filters_as_text(self.state.current_tag_names)
  END
  
  new_filters_as_text = self.empty_filters_as_text(keywords.replace('-','$'))
  struct_assign,current_filters_as_text, new_filters_as_text
  
  self.state.current_filters_as_text = new_filters_as_text
END


FUNCTION spice_cat::sort,list
  IF n_elements(list) EQ 0 THEN return, list
  
  tag_names = tag_names(list)
  current_sort_tag = self.state.current_sort_column.replace('-','$')
  
  sort_tag_ix = (where(tag_names EQ current_sort_tag, count))[0]
  
  IF self.state.keyword_info[self.state.current_sort_column].type EQ "i" THEN BEGIN
     sort_values = 0.0d + list[*].(sort_tag_ix)
     sortix = sort(sort_values)
  END ELSE BEGIN
     sortix = sort(list.(sort_tag_ix))
  END
  
  IF self.state.current_sort_order EQ "descending" THEN sortix = reverse(sortix)
  
  return,list[sortix]
END


PRO spice_cat::create_displayed_list, keywords
  self.update_current_filter, keywords
  
  new_list_without_filter = []
  
  filter_mask = self.apply_filters(self.state.current_filters_as_text)
  selected_ix = where(filter_mask,count)
  
  IF count GT 0 THEN BEGIN
     new_list_without_filter = replicate(self.state.current_filters_as_text, count)
     struct_assign, self.state.full_list[selected_ix], new_list_without_filter
  END
  
  new_sorted_list_without_filter = self.sort(new_list_without_filter)
  
  new_list = [self.state.current_filters_as_text, temporary(new_sorted_list_without_filter)]
  
  self.state.displayed = temporary(new_list)
  self.state.current_tag_names = tag_names(self.state.displayed)
  self.state.current_column_names = (tag_names(self.state.displayed)).replace('$','-')
END


;;
PRO _____________________TABLE_WIDGET_UTILITIES                          & END
;;

FUNCTION spice_cat::cell_alignments
  num_cols = n_elements(self.state.current_column_names)
  num_rows = n_elements(self.state.displayed)
  
  cell_alignments = replicate(0b,num_cols, num_rows)
  foreach column_name, self.state.current_column_names, columnix DO BEGIN
     align = self.state.keyword_info[column_name].type EQ "i"
     cell_alignments[columnix,*] = align * 2
  END
  return,cell_alignments
END


FUNCTION spice_cat::background_colors
  num_table_columns = n_elements(self.state.current_column_names)
  num_table_rows = n_elements(self.state.displayed)
  background_colors = replicate(230b, 3, num_table_columns, num_table_rows)
  background_colors[1, *, 0] = 255b
  return, background_colors
END

PRO spice_cat::capture_column_widths
  COMMON spice_cat_column_widths, widths_by_column_name
  widths = widget_info(self.wid.table_id, /column_widths)
  foreach column_name, self.state.current_column_names, index DO BEGIN
     widths_by_column_name[column_name] = widths[index]
  END
END

FUNCTION spice_cat::current_column_widths
  COMMON spice_cat_column_widths, widths_by_column_name
  IF n_elements(widths_by_column_name) EQ 0 THEN BEGIN
     widths_by_column_name = hash()
     foreach info, self.state.keyword_info, column_name DO BEGIN
        widths_by_column_name[column_name] = info.display_width * 12
     END
  END
  
  widths = []
  foreach column_name, self.state.current_column_names DO BEGIN
     widths = [widths, widths_by_column_name[column_name]]
  END
  return, widths
END


PRO spice_cat::display_displayed_list
;  relative_column_widths = ((self.state.keyword_info.values()).toarray()).display_width
;  table_props.column_widths = relative_column_widths * 12
  
  column_widths = self.current_column_widths()
  
  widget_control,self.wid.table_id, set_value=self.state.displayed
  widget_control,self.wid.table_id, table_ysize=n_elements(self.state.displayed)
  widget_control,self.wid.table_id, alignment=self.cell_alignments()
  widget_control,self.wid.table_id, background_color=self.background_colors()
  widget_control, self.wid.table_id, column_labels = self.state.current_column_names
  widget_control, self.wid.table_id, column_widths=column_widths
END

;;
PRO _____________________EVENT_HANDLING_HELPERS                      & END
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
     self.create_displayed_list
     self.display_displayed_list
     self.set_filter_edit_color,column_name
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


FUNCTION spice_cat::deal_with_filter_focus_change, event, column_name
  IF tag_names(event,/structure_name) EQ "WIDGET_KBRD_FOCUS" THEN BEGIN
     IF self.state.ignore_next_focus_change EQ 0 THEN BEGIN 
        IF event.enter GE 0 THEN self.set_filter_edit_color, column_name
        IF event.enter EQ 0 THEN self.set_filter_edit_color, /clear
     END
     return,1 ; It's OK, dealt with!
  END
  return,0 ; It wasn't a keyboard focus event, not ok
END


PRO spice_cat::deal_with_click_on_filter,column_name
  current_filter_as_array = self.get_filter_by_column_name(column_name)
  self.set_filter_edit_color,column_name
  
  IF current_filter_as_array[0] EQ "<filter>" THEN BEGIN
     column_type = self.state.keyword_info[column_name].type
     IF column_type EQ "t" THEN current_filter_as_array = [""]
     IF column_type EQ "i" THEN current_filter_as_array = ["", ""] ; range
  END
  
  ;; TODO: can't be necessary, right?
  self.set_filter_by_column_name, column_name, current_filter_as_array
  filter_as_uvalue_text = current_filter_as_array.join("`")
  
  ;; Simulates event with UVALUE = "REBUILD_FILTER`column_name`min`max"
  ;;                 or   UVALUE = "REBUILD_FILTER`column_name`text_filter"
  ;;
  self.handle_rebuild_filter, dummy_event, ["REBUILD_FILTER", column_name, current_filter_as_array]
END

;;
PRO _____________________EVENT_HANDLERS                     & END
;;

; RESIZE TABLE ACCORDING TO TLB size change!
; Only called when event.id eq event.top
;
PRO spice_cat::handle_tlb,event
  IF tag_names(event, /structure_name) NE "WIDGET_BASE" THEN return
  
  widget_control,self.wid.xsize_spacer_base, xsize=event.x
  widget_control,self.wid.ysize_spacer_base, ysize=event.y
  tablex = event.x
  tabley = event.y - 50
  widget_control,self.wid.table_id, scr_xsize=tablex, scr_ysize=tabley
END

PRO spice_cat::handle_remove_column, event, parts
  print,"Handle "+parts[0]+" : "+parts[1]
  screen_size = spice_get_screen_size()
END


PRO spice_cat::handle_sort, event, parts
  self.state.current_sort_order = parts[1].tolower()
  self.state.current_sort_column = parts[2]
  self.build_sort_pulldown
  self.create_displayed_list
  self.display_displayed_list
END


PRO spice_cat::handle_text_filter_change, event, parts
  column_name = parts[1]
  IF self.deal_with_filter_focus_change(event, column_name) THEN return
  
  widget_control, event.id, get_value=new_text_filter_as_singular_array
  self.set_filter_by_column_name, column_name, new_text_filter_as_singular_array
END


PRO spice_cat::handle_range_filter_change, event, parts
  min_or_max = parts[1]
  column_name = parts[2]
  
  IF self.deal_with_filter_focus_change(event, column_name) THEN return

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


PRO spice_cat::handle_filter_flash_timer, event, parts
  iteration = parts[1].toInteger()

  IF (iteration MOD 2)+1 THEN widget_control,self.wid.filter_focus_flash_text,/input_focus $
  ELSE                        widget_control, self.wid.draw_focus_away, /input_focus
  
  
  IF iteration LT 4 THEN BEGIN
     self.state.ignore_next_focus_change = 1
     iteration++
     widget_control, event.id, set_uvalue="FILTER_FLASH_TIMER`"+iteration.toString()
     widget_control, event.id, timer=0.05
  END ELSE BEGIN
     self.state.ignore_next_focus_change = 0
  END
END


PRO spice_cat::handle_rebuild_filter, dummy_event, parts
  widget_control, self.wid.top_base, update=0
  
  column_name = parts[1]
  new_filter_as_array = parts[2:*]
  
  self.destroy_children, self.wid.filter_base
  
  self.set_filter_by_column_name, column_name, new_filter_as_array
  
  text = n_elements(new_filter_as_array) EQ 1
  range = n_elements(new_filter_as_array) EQ 2
  
  IF text THEN self.build_text_filter, column_name, new_filter_as_array
  IF range THEN self.build_range_filter, column_name, new_filter_as_array
  
  new_filter_as_text = self.filter_as_text(new_filter_as_array)
  widget_control, self.wid.filter_base, set_uvalue="FILTER_FLASH_TIMER`1
  widget_control, self.wid.filter_base, timer=0.05
  
  widget_control, self.wid.top_base, update=1
END


PRO spice_cat::handle_table_context, ev
  IF ev.row EQ 0 THEN return ; No context menu for filter row
  IF ev.col LT 0 THEN return ; No context menu for row labels
  
  base = widget_base(/CONTEXT_MENU, ev.id)
  
  IF ev.row EQ -1 THEN self.make_heading_context_menu, base, ev
  IF ev.row GE 1 THEN self.make_datacell_context_menu, base, ev
  
  widget_displaycontextmenu, ev.id, ev.x, ev.y, base
END

PRO spice_cat::handle_table_cell_sel, ev
  sel = {left:ev.sel_left, right:ev.sel_right, top:ev.sel_top, bottom:ev.sel_bottom}
  
  ;; Ignore nonsensical [-1, -1, -1, -1] events:
  IF total([sel.left, sel.top, sel.right, sel.bottom] EQ -1) EQ 4 THEN return
  
  
  ;; Only meaningful action is if the user wants to edit the filter:
  ;; a) single cell from first row
  ;; b) header click (selects entire column)
  
  num_displayed = n_elements(self.state.displayed)
  header_click = sel.left EQ sel.right AND sel.top EQ 0 AND sel.bottom EQ num_displayed - 1
  filter_click = (sel.top EQ 0) AND (sel.bottom EQ 0) AND (sel.left EQ sel.right)
  
  IF (NOT header_click) AND (NOT filter_click) THEN return 
  
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
  column_name = self.state.current_column_names[sel.left]
  self.deal_with_click_on_filter,column_name
END


PRO spice_cat::handle_all_table_events, ev, parts
  ;; We came here because of any table event (uvalue="ALL_TABLE_EVENTS`")
  ;;
  type = tag_names(ev, /structure_name)
  
  IF type EQ "WIDGET_TABLE_COL_WIDTH" THEN BEGIN
     self.capture_column_widths
     return
  END
  
  IF type EQ "WIDGET_TABLE_CH" THEN return   ;; Doh! Typing into non-editable cells triggers this!!!
  
  short_event_name = strmid(tag_names(ev, /structure_name), 7, 1000)
  
  ;; Note that context events within the table come as "WIDGET_CONTEXT"
  ;; events, not WIDGET_TABLE_CONTEXT. So we "fix" that:
  
  IF short_event_name EQ "CONTEXT" THEN short_event_name = "TABLE_CONTEXT"
  
  method = "handle_" + short_event_name
  call_method, method, self, ev
END

;;
PRO _____________________COMMAND_BASE_EVENTS                            & END
;;

PRO spice_cat::handle_call_program, event, parts
  self.modal_message,"Call program with selection not implemented yet", timer = 2
END


PRO spice_cat::handle_return_selection, event, parts
  self.modal_message, "Returning selection is not implemented yet", timer=2
END


PRO spice_cat::handle_regenerate, event, parts
  self.modal_message, ["Regenerate list with:","IDL> spice_fitslist"], timer=2
END

;;
PRO _____________________CATCH_ALL_EVENT_HANDLER                         & END
;;
  
PRO spice_cat__event, event
  widget_control, event.top, get_uvalue=self
  
  ;; The TLB's UVALUE is pointing to *self*, so it can't be used for directing
  ;; the event to the right handler. We must detect that as a separate thing,
  ;; and pretend everything was normal:
  IF event.id EQ event.top THEN BEGIN
     self.handle_tlb,event
     return
  END 
  widget_control, event.id, get_uvalue=uvalue
  
  parts = uvalue.split('`')
  method = "handle_"+parts[0]
  
  call_method,method, self, event, parts
END

;;
PRO _____________________WIDGET_BUILDERS                   & END
;;

PRO spice_cat::build_text_filter, column_name, filter_as_array
  filter_as_text = filter_as_array[0]
  filter_text_uvalue = "TEXT_FILTER_CHANGE`" + column_name
  self.wid.filter_label = widget_label(self.wid.filter_base, value=column_name+":")
  text_props = {editable:1b, all_events:1b, kbrd_focus_events: 1b}
  self.wid.filter_text = widget_text(self.wid.filter_base, value=filter_as_text,$
                                     _extra=text_props, uvalue=filter_text_uvalue)
  button_uvalue = "REBUILD_FILTER`"+column_name+"``"
  button = widget_button(self.wid.filter_base, value="Use alphabetical range", uvalue=button_uvalue)
  self.wid.filter_focus_flash_text = self.wid.filter_text
  widget_control, self.wid.filter_text, set_text_select=[0, filter_as_array.strlen()]
END


PRO spice_cat::build_range_filter, column_name, filter_as_array
  min_value = filter_as_array[0]
  max_value = filter_as_array[1]
  
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


PRO spice_cat::build_sort_pulldown
  widget_control, self.wid.top_base, update=0
  
  self.destroy_children, self.wid.sort_base
  
  direction = self.state.current_sort_order EQ "ascending" ? "(incr.)" : "(decr.)"
  
  menu_text = "Sort: " + self.state.current_sort_column + " " + direction
  menu = widget_button(self.wid.sort_base, /menu, value=menu_text)
  
  foreach column_name, self.state.current_column_names DO BEGIN
     mx = widget_button(menu, value=column_name, /menu)
     button = widget_button(mx, value="increasing", $
                            uvalue="SORT`ASCENDING`"+column_name)
     button = widget_button(mx, value="decreasing", $
                            uvalue="SORT`DESCENDING`"+column_name)
  END
  widget_control, self.wid.top_base, update=1
END

PRO spice_cat::make_heading_context_menu, base, ev
  column_name = (tag_names(self.state.displayed))[ev.col].replace('$', '-')
  button = widget_button(base, value="Remove column", uvalue="REMOVE_COLUMN`"+column_name)
  button = widget_button(base, value="Sort increasing", uvalue="SORT`ASCENDING`"+column_name)
  button = widget_button(base, value="Sort decreasing", uvalue="SORT`DESCENDING`"+column_name)
  button = widget_button(base, value="Move left", uvalue="MOVE`LEFT`"+column_name)
  button = widget_button(base, value="Move right", uvalue="MOVE`RIGHT`"+column_name)
END


PRO spice_cat::make_datacell_context_menu, base, ev
  column_name = (tag_names(self.state.displayed))[ev.col]
  cell_value = self.state.displayed[ev.row].(ev.col).tostring()
  
  filename = self.state.displayed[ev.row].filename
  filename_uvalue = "CONTEXT_CLICK_ON_FILENAME`"+filename
  
  filter_on_value = "Filter on "+column_name+"='"+cell_value+"'"
  filter_on_value_uvalue = "CONTEXT_CLICK_ON_FULL_VALUE`+column_name"
  
  button = widget_button(base, value=filename, uvalue=filename_uvalue)
  button = widget_button(base, value=filter_on_value, uvalue=filter_on_value_uvalue)
END


;; UTILITY TO KILL PREVIOUS INCARNATION -------

PRO spice_cat::new_incarnation
  COMMON spice_cat, previous_incarnation
  spice_default,previous_incarnation, 0L
  IF widget_info(previous_incarnation, /valid_id) THEN BEGIN
     widget_control,previous_incarnation, /destroy
  END
  previous_incarnation = self.wid.top_base
END

PRO spice_cat::create_command_buttons
  buttons = $
     [ $
     { value: "Return selection", uvalue: "RETURN_SELECTION`", ALIGN_CENTER: 1b },$
     { value: "Regenerate list", uvalue: "REGENERATE`", ALIGN_CENTER: 1b },$
     { value: "Call <program>", uvalue: "CALL_PROGRAM`", ALIGN_CENTER: 1b } $
     ]
  foreach button, buttons DO button = widget_button(self.wid.button_base, _extra=button)
END


PRO spice_cat::build_table
  table_props = dictionary()
  table_props.scroll = 1b 
  table_props.no_row_headers = 1b
  table_props.row_major = 1b
  table_props.all_events = 1b
  table_props.context_events = 1b
  table_props.uvalue="ALL_TABLE_EVENTS`"
  table_props.resizeable_columns = 1b
  
  props = table_props.tostruct()
  self.wid.table_id = widget_table(self.wid.table_base, value=self.state.displayed, _extra=props)
  self.display_displayed_list
  
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
END


PRO spice_cat::build_widget
  self.wid = dictionary()
  
  top_props = {row: 1b, xpad: 0b, ypad: 0b, uvalue: self, tlb_size_events: 1b}
  
  self.wid.top_base = widget_base(title='SPICE-CAT', _extra=top_props)
  self.new_incarnation

  self.wid.ysize_spacer_base = widget_base(self.wid.top_base, ysize=800, xpad=0, ypad=0)
  self.wid.content_base = widget_base(self.wid.top_base, /column)
  self.wid.xsize_spacer_base = widget_base(self.wid.content_base, /row, xsize=800)
  self.wid.top_row_base = widget_base(self.wid.content_base, /row)
  self.wid.button_base = widget_base(self.wid.top_row_base, /row, /align_center, xpad=0, ypad=0)
  spacer = widget_base(self.wid.top_row_base,xsize=5)
  self.wid.sort_base = widget_base(self.wid.top_row_base, /row, xpad=0, ypad=0, /align_center)
  self.wid.filter_base = widget_base(self.wid.top_row_base, /row, xpad=0, ypad=0, /align_center)
  self.wid.table_base = widget_base(self.wid.content_base, /column, /frame, xpad=0, ypad=0)
    
  self.wid.draw_focus_away = widget_text(self.wid.ysize_spacer_base, scr_xsize=1, scr_ysize=1)
  
  self.create_command_buttons
  
  self.build_sort_pulldown
  
  t = "Click on green line or the heading to edit filter"
  text = widget_label(self.wid.filter_base, frame=2,value=t)
  
  self.build_table
  
  widget_control,self.wid.top_base, /realize
  spice_center_window, self.wid.top_base
  
  ;; Make table fill available space despite /scroll
  widget_control,self.wid.top_base, tlb_get_size=tlb_size
  resize_event = {widget_base,id:0L,top:0L,handler:0L, x:tlb_size[0], y:tlb_size[1] }
  self.handle_tlb, resize_event
END


PRO spice_cat::parameters, example_param1, example_param2, _extra=extra
  self.state = dictionary()
  self.state.keyword_info = spice_keyword_info(/all)
  spice_datadir = getenv("SPICE_DATA")
  IF spice_datadir EQ "" THEN message,"Environment variable SPICE_DATADIR is blank or not set"
  
  spice_default,listfiledir,spice_datadir
  
  self.state.spice_datadir = spice_datadir
  self.state.listfiledir = listfiledir
  self.state.listfilename = concat_dir(listfiledir, 'spice_fitslist.txt')
END


;; INIT: create, realize and register widget
function spice_cat::init, example_param1,  example_param2, _extra=extra
  self.parameters, example_param1, example_param2, _extra = extra
  self.load_fitslist
  
  keywords = getenv("SPICE_CAT_KEYWORDS")
  IF keywords GT "" THEN BEGIN
     keywords = keywords.split(",")
     keywords = keywords.trim()
     IF total(keywords EQ "DATE-BEG") EQ 0 THEN keywords = ["DATE-BEG", keywords]
  END ELSE BEGIN
     keywords = []
  END
  self.create_displayed_list,keywords
  
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

setenv,"SPICE_CAT_KEYWORDS=FILENAME,DATE-BEG,COMPRESS,OBS_ID,NWIN"
spice_cat, o
END
