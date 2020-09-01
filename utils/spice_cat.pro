;;
PRO spice_cat::_____________UTILITY_FUNCTIONS & END
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


PRO spice_cat::set_message, label, message, select=select
  message_text = message.tostring()
  widget_control, self.wid.message_label, set_value=label.tostring()
  widget_control, self.wid.message_text, set_value=message_text
  IF keyword_set(select) THEN BEGIN
     text_length = strlen(message_text)
     widget_control, self.wid.message_text, set_text_select=[0, text_length]
  END
END


PRO spice_cat::register_selection, sel, blank=blank
  IF keyword_set(blank) THEN BEGIN
     self.curr.selection = []
     self.curr.selection_beg = -1
     self.curr.selection_end = -1
     return
  END
  
  self.curr.selection = self.curr.displayed[sel.top:sel.bottom].filename
  self.curr.selection_beg = sel.top
  self.curr.selection_end = sel.bottom
  widget_control, self.wid.table_id, background_color=self.background_colors()
END


FUNCTION spice_cat::selection
  IF NOT self.curr.haskey("selection") THEN return, !null
  return, self.curr.selection
END


PRO spice_cat::replace_previous_incarnation
  COMMON spice_cat, previous_incarnation
  spice_default,previous_incarnation, 0L
  IF widget_info(previous_incarnation, /valid_id) THEN BEGIN
     widget_control,previous_incarnation, /destroy
  END
  previous_incarnation = self.wid.top_base
END


PRO spice_cat::safe_struct_assign, source, destination
  ;; Builtin STRUCT_ASSIGN BLANKS OUT tags that do not exist in source!!!
  source_tag_names = tag_names(source)
  foreach destination_tag_name, tag_names(destination), destination_tag_ix DO BEGIN
     source_tag_ix = (where(source_tag_names EQ destination_tag_name))[0]
     IF source_tag_ix NE -1 THEN destination.(destination_tag_ix) = source.(source_tag_ix)
  END
END
  
;;
PRO spice_cat::_____________FILTER_CONVERSION___TEXT_vs_ARRAY       & END
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
PRO spice_cat::_____________DATA_LOADING_AND_MANIPULATION           & END
;;

PRO spice_cat::load_fitslist
  fitslist = spice_read_fitslist(self.d.listfilename)  ;; Array of orderedhashes()
  
  self.d.full_list = fitslist
  self.d.full_tag_names = tag_names(fitslist[0])
  self.d.full_column_names = (tag_names(fitslist[0])).replace('$','-')
  
  self.curr.column_names = self.d.full_column_names
  self.curr.sort_column = 'DATE-BEG'
  self.curr.sort_order = "INCREASING"
END 


FUNCTION spice_cat::apply_filter, filter, full_list_tag_index
  filter_as_array = self.filter_as_array(filter)
  
  IF n_elements(filter_as_array) EQ 1 THEN BEGIN 
     mask = self.d.full_list[*].(full_list_tag_index).matches(filter_as_array[0],/fold_case)
  END ELSE BEGIN
     min = filter_as_array[0]
     max = filter_as_array[1]
     
     column_name = self.d.full_column_names[full_list_tag_index]
     
     IF (self.d.keyword_info[column_name].type) EQ "i" THEN BEGIN
        IF min NE "" THEN min = min + 0.0d
        IF max NE "" THEN max = max + 0.0d
     END
     
     IF filter_as_array[0] EQ "" THEN BEGIN
        mask = replicate(1b,n_elements(self.d.full_list))
     END ELSE BEGIN
        mask = self.d.full_list[*].(full_list_tag_index) GE min
     END
     
     IF filter_as_array[1] NE "" THEN BEGIN
        mask = mask AND self.d.full_list[*].(full_list_tag_index) LE max
     END
  END
  return,mask
END


FUNCTION spice_cat::apply_filters, filters
  mask = replicate(1b,n_elements(self.d.full_list))
  
  foreach current_filter_tag_name, tag_names(filters), current_filter_tag_ix DO BEGIN
     current_filter = filters.(current_filter_tag_ix)
     IF current_filter EQ "<filter>" THEN CONTINUE
     
     ; apply_filter() operates on full_list, so we must supply
     ; full_list_tag_index, not filter_tag_index
     full_tag_names = self.d.full_column_names.replace('-', '$')
     full_list_tag_index = (where(full_tag_names EQ current_filter_tag_name))[0]
     
     mask = mask AND self.apply_filter(current_filter, full_list_tag_index)
  END
  return,mask
END


PRO spice_cat::update_current_filters, keywords
  spice_default, keywords, self.curr.column_names
  
  IF self.curr.haskey("filters_as_text") THEN BEGIN
     inited_current_filters_as_text = self.curr.filters_as_text
  END ELSE BEGIN
     current_tag_names = self.curr.column_names.replace('-', '$')
     inited_current_filters_as_text = self.empty_filters_as_text(current_tag_names)
  END
  
  new_filters_as_text = self.empty_filters_as_text(keywords.replace('-','$'))
  
  self.safe_struct_assign, inited_current_filters_as_text, new_filters_as_text
  
  self.curr.filters_as_text = new_filters_as_text
END


FUNCTION spice_cat::sort,list
  IF n_elements(list) EQ 0 THEN return, list
  
  tag_names = tag_names(list)
  current_sort_tag = self.curr.sort_column.replace('-','$')
  
  sort_tag_ix = (where(tag_names EQ current_sort_tag, count))[0]
  
  IF self.d.keyword_info[self.curr.sort_column].type EQ "i" THEN BEGIN
     sort_values = 0.0d + list[*].(sort_tag_ix)
     sortix = sort(sort_values)
  END ELSE BEGIN
     sortix = sort(list.(sort_tag_ix))
  END
  
  IF self.curr.sort_order EQ "DECREASING" THEN sortix = reverse(sortix)
  
  return,list[sortix]
END


PRO spice_cat::create_displayed_list, column_names
  self.update_current_filters, column_names
  
  new_list_without_filter = []
  
  filter_mask = self.apply_filters(self.curr.filters_as_text)
  selected_ix = where(filter_mask,count)
  
  IF count GT 0 THEN BEGIN
     new_list_without_filter = replicate(self.curr.filters_as_text, count)
     struct_assign, self.d.full_list[selected_ix], new_list_without_filter
  END
  
  new_sorted_list_without_filter = self.sort(new_list_without_filter)
  
  new_list = [self.curr.filters_as_text, temporary(new_sorted_list_without_filter)]
  
  self.curr.displayed = temporary(new_list)
  self.curr.column_names = (tag_names(self.curr.displayed)).replace('$','-')
END


;;
PRO spice_cat::_____________TABLE_WIDGET_UTILITIES                          & END
;;

FUNCTION spice_cat::cell_alignments
  num_cols = n_elements(self.curr.column_names)
  num_rows = n_elements(self.curr.displayed)
  
  cell_alignments = replicate(0b,num_cols, num_rows)
  foreach column_name, self.curr.column_names, columnix DO BEGIN
     align = self.d.keyword_info[column_name].type EQ "i"
     cell_alignments[columnix,*] = align * 2
  END
  return,cell_alignments
END


FUNCTION spice_cat::background_colors
  num_table_columns = n_elements(self.curr.column_names)
  num_table_rows = n_elements(self.curr.displayed)
  background_colors = replicate(230b, 3, num_table_columns, num_table_rows)
  background_colors[1, *, 0] = 255b
  IF self.curr.haskey("selection_beg") && self.curr.selection_beg GT 0 THEN BEGIN
     background_colors[*, *, self.curr.selection_beg : self.curr.selection_end] = 200b
     background_colors[2, *, self.curr.selection_beg : self.curr.selection_end] = 255b
  END
  sort_column_ix = (where(self.curr.sort_column EQ self.curr.column_names))[0]
  background_colors[*, sort_column_ix, *] = (background_colors[*, sort_column_ix, *] + 40) < 255
  return, background_colors
END


PRO spice_cat::capture_column_widths
  COMMON spice_cat_column_widths, widths_by_column_name
  widths = widget_info(self.wid.table_id, /column_widths)
  foreach column_name, self.curr.column_names, index DO BEGIN
     widths_by_column_name[column_name] = fix(widths[index])
  END
  env_widths = []
  foreach width, widths_by_column_name, column_name DO BEGIN
     env_widths = [env_widths, column_name + ":" + width.tostring()]
  END
  env_widths = env_widths.join(",")
  print, "** Put this in your IDL-startup to set column widhts permanently:"
  print, "SETENV,'SPICE_CAT_KEYWORD_WIDTHS=" + env_widths + "'"
END


PRO spice_cat::init_column_widths
  COMMON spice_cat_column_widths, widths_by_column_name
  
  widths_by_column_name = hash()
  foreach info, self.d.keyword_info, column_name DO BEGIN
     widths_by_column_name[column_name] = info.display_width * 12
  END
  
  ; Override with SPICE_CAT_KEYWORD_WIDTHS when set
  spice_cat_keyword_widths = getenv("SPICE_CAT_KEYWORD_WIDTHS")
  IF spice_cat_keyword_widths EQ "" THEN return
  spice_cat_keyword_widths = spice_cat_keyword_widths.split(',')
  foreach width_setting, spice_cat_keyword_widths DO BEGIN
     parts = width_setting.split(':')
     widths_by_column_name[parts[0]] = parts[1].toInteger()
  END
END
  

FUNCTION spice_cat::current_column_widths
  COMMON spice_cat_column_widths, widths_by_column_name
  IF n_elements(widths_by_column_name) EQ 0 THEN self.init_column_widths
  widths = []
  foreach column_name, self.curr.column_names DO BEGIN
     widths = [widths, widths_by_column_name[column_name]]
  END
  return, widths
END


FUNCTION spice_cat::current_column_labels
  up_or_down = self.curr.sort_order EQ "INCREASING" ? "(incr)" : "(decr)"
  labels = self.curr.column_names
  foreach label, labels, index DO BEGIN
     IF label EQ self.curr.sort_column THEN labels[index] += " " + up_or_down
  END
  return, labels
END


PRO spice_cat::display_displayed_list, keep_selection=keep_selection
  widget_control, self.wid.table_id, update=1
  
  IF NOT keyword_set(keep_selection) THEN self.register_selection, /blank
  
  widget_control,self.wid.table_id, set_value=self.curr.displayed
  widget_control,self.wid.table_id, table_ysize=n_elements(self.curr.displayed)
  widget_control, self.wid.table_id, table_xsize=n_elements(tag_names(self.curr.displayed))
  widget_control,self.wid.table_id, alignment=self.cell_alignments()
  widget_control,self.wid.table_id, background_color=self.background_colors()
  widget_control, self.wid.table_id, column_labels = self.current_column_labels()
  
  self.set_message, "Files found:", " "+(n_elements(self.curr.displayed)-1).tostring()
  
  widget_control, self.wid.table_id, update=1
  
  widget_control, self.wid.table_id, column_widths=self.current_column_widths()
END

;;
PRO spice_cat::_____________EVENT_HANDLING_HELPERS                      & END
;;

FUNCTION spice_cat::get_filter_by_column_name, column_name
  column_number = where(self.curr.column_names EQ column_name)
  select = [column_number, 0, column_number, 0]
  widget_control, self.wid.table_id, get_value=filter_as_text, use_table_select=select
  return, self.filter_as_array(filter_as_text)
END


PRO spice_cat::set_filter_by_column_name, column_name, filter_as_array
  filter_as_text = self.filter_as_text(filter_as_array)
  
  IF filter_as_text EQ "" THEN filter_as_text = "<filter>"
  column_number = (where(self.curr.column_names EQ column_name))[0]
  
  select = [column_number, 0, column_number, 0]
  widget_control, self.wid.table_id, set_value=filter_as_text, use_table_select=select
  
  ;; DON'T CHANGE! Using "self.curr.filters_as_text" DIRECTLY => core dump!
  current_filters_as_text = self.curr.filters_as_text
  
  filters_as_text = self.curr.filters_as_text ;; CORE DUMP unless we do this!!
  IF filter_as_text NE filters_as_text.(column_number) THEN BEGIN
     current_filters_as_text.(column_number) = filter_as_text
     self.curr.filters_as_text = current_filters_as_text
     self.create_displayed_list
     self.display_displayed_list
     self.set_filter_cell_to_edit_color,column_name
  END
END


PRO spice_cat::set_filter_cell_to_edit_color, column_name, clear=clear
  num_columns = n_elements(tag_names(self.curr.displayed))
  table_select = [0, 0, num_columns-1, 0]
  widget_control, self.wid.table_id, background_color=[230b,255b,230b], use_table_select=table_select
  
  IF keyword_set(clear) THEN return
  
  column_number = (where(self.curr.column_names EQ column_name))[0]
  table_select = [column_number, 0, column_number, 0]
  color = [150b, 255b, 150b]
  widget_control, self.wid.table_id, background_color=color, use_table_select=table_select
END


FUNCTION spice_cat::absorb_filter_focus_change, event, column_name
  IF tag_names(event,/structure_name) EQ "WIDGET_KBRD_FOCUS" THEN BEGIN
     IF self.curr.ignore_next_focus_change EQ 0 THEN BEGIN 
        IF event.enter GE 0 THEN self.set_filter_cell_to_edit_color, column_name
        IF event.enter EQ 0 THEN self.set_filter_cell_to_edit_color, /clear
     END
     return,1 ; It's OK, dealt with!
  END
  return,0 ; It wasn't a keyboard focus event, not ok
END


PRO spice_cat::deal_with_click_on_filter_cell,column_name
  current_filter_as_array = self.get_filter_by_column_name(column_name)
  self.set_filter_cell_to_edit_color,column_name
  
  IF current_filter_as_array[0] EQ "<filter>" THEN BEGIN
     column_type = self.d.keyword_info[column_name].type
     IF column_type EQ "t" THEN current_filter_as_array = [""]
     IF column_type EQ "i" THEN current_filter_as_array = ["", ""] ; range
  END
  
  filter_as_uvalue_text = current_filter_as_array.join("`")
  
  ;; Simulates event with UVALUE = "REBUILD_FILTER`column_name`min`max"
  ;;                 or   UVALUE = "REBUILD_FILTER`column_name`text_filter"
  ;;
  self.handle_rebuild_filter, dummy_event, ["REBUILD_FILTER", column_name, current_filter_as_array]
END


PRO spice_cat::set_message_to_full_content, sel
  column_name = self.curr.column_names[sel.left]
  full_content = self.curr.displayed[sel.top].(sel.left)
  self.set_message, column_name+":", full_content, /select
END


;;
PRO spice_cat::_____________EVENT_HANDLERS                     & END
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
  column_name = parts[1]
  
  goodix = where(self.curr.column_names NE column_name, count)
  IF count EQ 0 THEN self.modal_message, "You can't remove the last column!"
  
  new_column_names = self.curr.column_names[goodix]
  
  IF column_name EQ self.curr.sort_column THEN BEGIN
     newsort = self.curr.column_names[0]
     IF (where(new_column_names EQ 'DATE-BEG'))[0] NE -1 THEN newsort = 'DATE-BEG'
     self.curr.sort_column = newsort
  END
  
  self.create_displayed_list, new_column_names
  self.display_displayed_list, /keep_selection
END


PRO spice_cat::handle_add_column, event, parts
  left_or_right = parts[1]
  left_or_right_of = parts[2]
  add_column_name = parts[3]
  
  print, "ADD_COLUMN " + add_column_name + " " + left_or_right + " of " + left_or_right_of
  
  ref_col_ix = (where(left_or_right_of EQ self.curr.column_names))[0]
  
  IF left_or_right EQ "RIGHT" THEN BEGIN
     ;; What goes on the left is everything up to and *including* ref_col_ix
     ;; What goes on the right is the rest... *IF* ANY!
     ;;
     left = self.curr.column_names[0:ref_col_ix]
     right = []
     inside_bounds = ref_col_ix LT n_elements(self.curr.column_names)-1
     IF inside_bounds THEN right = self.curr.column_names[ref_col_ix + 1:*]
  END
  IF left_or_right EQ "LEFT" THEN BEGIN
     ;; What goes on the left, is up to but *excluding* ref_col_ix, *IF* ANY
     ;; What goes on the right is the rest, *including* ref_col_ix
     left = []
     inside_bounds = ref_col_ix GT 0
     IF inside_bounds THEN left = self.curr.column_names[0:ref_col_ix-1]
     right = self.curr.column_names[ref_col_ix:*]
  END
  
  new_column_names = [left, add_column_name, right]
  setenv_command = "SPICE_CAT_KEYWORDS=" + new_column_names.join(",")
  setenv, setenv_command
  print, "** Put this in your IDL-startup to set keyword list permanently:"
  print, "setenv," + setenv_command
  self.create_displayed_list, new_column_names
  self.display_displayed_list
END


PRO spice_cat::handle_move, event, parts
  direction_to_move = parts[1]
  column_to_move = parts[2]
  
  original_ix = (where(column_to_move EQ self.curr.column_names))[0]
  purged_list_ix = where(self.curr.column_names NE column_to_move)
  purged_list = self.curr.column_names[purged_list_ix]
  
  IF direction_to_move EQ "LEFT" THEN BEGIN
     left = []
     IF original_ix GE 2 THEN left = purged_list[0:original_ix-2]
     right = purged_list[original_ix-1:*]
  END
  IF direction_to_move EQ "RIGHT" THEN BEGIN
     left = purged_list[0: original_ix]
     right = []
     IF original_ix LT n_elements(purged_list)-1 THEN BEGIN
        right = purged_list[original_ix + 1:*]
     END
  END
  new_column_names = [left, column_to_move, right]
  self.create_displayed_list, new_column_names
  self.display_displayed_list
END


PRO spice_cat::handle_sort, event, parts
  self.curr.sort_order = parts[1]
  self.curr.sort_column = parts[2]
  self.build_sort_pulldown
  self.create_displayed_list
  self.display_displayed_list
END


PRO spice_cat::handle_text_filter_change, event, parts
  column_name = parts[1]
  IF self.absorb_filter_focus_change(event, column_name) THEN return
  
  widget_control, event.id, get_value=new_text_filter_as_singular_array
  self.set_filter_by_column_name, column_name, new_text_filter_as_singular_array
END


FUNCTION spice_cat::handle_range_single_filter_change, text_id, column_name
  widget_control, text_id, get_value=value
  
  new_value = value
  
  ;; Remove non-digit chars for numeric columns
  ;;
  IF self.d.keyword_info[column_name].type NE "t" THEN BEGIN
     new_value = self.remove_non_digits(value)
     
     ;; Correct cursor position due to deletion of non-digit content:
     ;;
     text_select = widget_info(text_id, /text_select)
     text_select = text_select[0] - (value.strlen() - new_value.strlen())
     widget_control, text_id, set_value=new_value, set_text_select=text_select 
  END
  
  return, new_value
END


PRO spice_cat::handle_range_filter_change, event, parts
  column_name = parts[1]
  
  IF self.absorb_filter_focus_change(event, column_name) THEN return
  
  new_min_value = self.handle_range_single_filter_change(self.wid.min_filter_text, column_name)
  new_max_value = self.handle_range_single_filter_change(self.wid.max_filter_text, column_name)
  
  new_range_filter_as_array = [new_min_value, new_max_value]
  self.set_filter_by_column_name, column_name, new_range_filter_as_array
END


PRO spice_cat::handle_filter_cell_flash_timer, event, parts
  iteration = parts[1].toInteger()

  IF (iteration MOD 2)+1 THEN widget_control,self.wid.filter_focus_flash_text,/input_focus $
  ELSE                        widget_control, self.wid.draw_focus_away, /input_focus
  
  IF iteration LT 4 THEN BEGIN
     self.curr.ignore_next_focus_change = 1
     iteration++
     widget_control, event.id, set_uvalue="FILTER_CELL_FLASH_TIMER`"+iteration.toString()
     widget_control, event.id, timer=0.05
  END ELSE BEGIN
     self.curr.ignore_next_focus_change = 0
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
  
  widget_control, self.wid.filter_base, set_uvalue="FILTER_CELL_FLASH_TIMER`1
  widget_control, self.wid.filter_base, timer=0.05
  
  widget_control, self.wid.top_base, update=1
END


PRO spice_cat::handle_table_context, ev
  IF ev.row EQ 0 THEN return ; No context menu for filter row
  
  ;; It's tempting to mess with table_select here since IDL doesn't blank out
  ;; the previous selection even if the context click was on a different cell,
  ;; BUT THIS CAUSES A SCROLL so the new selection (or top-row) is on top
  ;; (even though it was already visible)
  ;;
  context_menu_base = widget_base(/CONTEXT_MENU, ev.id)
  
  IF ev.row EQ -1 THEN self.build_context_menu_heading, context_menu_base, ev
  IF ev.row GE 1 THEN self.build_context_menu_datacell, context_menu_base, ev
  
  widget_displaycontextmenu, ev.id, ev.x, ev.y, context_menu_base
END


PRO spice_cat::handle_table_cell_sel, ev
  sel = {left:ev.sel_left, right:ev.sel_right, top:ev.sel_top, bottom:ev.sel_bottom}
  
  ;; Ignore nonsensical [-1, -1, -1, -1] events:
  IF total([sel.left, sel.top, sel.right, sel.bottom] EQ -1) EQ 4 THEN return
  
  IF sel.top GE 1 THEN self.register_selection, sel
  
  ;; Only meaningful actions are:
  ;; EITHER to edit the filter:
  ;;   a) single cell from first row
  ;;   b) header click (selects entire column)
  ;; OR just get the full text in the message window by 
  ;;   c) selecting a single cell anywhere else
  
  single_cell = (sel.top EQ sel.bottom) AND (sel.left EQ sel.right)
  
  IF single_cell AND (sel.top GE 1) THEN BEGIN
     self.set_message_to_full_content, sel
     return
  END
  
  num_displayed = n_elements(self.curr.displayed)
  header_click = sel.left EQ sel.right AND sel.top EQ 0 AND sel.bottom EQ num_displayed - 1
  filter_click = single_cell AND sel.top EQ 0
  
  IF (NOT header_click) AND (NOT filter_click) THEN return 
  
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
  column_name = self.curr.column_names[sel.left]
  self.deal_with_click_on_filter_cell,column_name
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


PRO spice_cat::handle_call_program, event, parts
  call_procedure, parts[1], self.selection()
END


PRO spice_cat::handle_exit, event, parts
  widget_control, event.top, /destroy
END



;;
PRO spice_cat::_____________WIDGET_BUILDERS                   & END
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
  widget_control, self.wid.filter_text, set_text_select=strlen(filter_as_text)
END


FUNCTION spice_cat::build_range_filter_text, base, column_name, minmax, value
  extra = {editable: 1b, all_events: 1b, kbrd_focus_events: 1b}
  
  uvalue = "RANGE_FILTER_CHANGE`" + column_name
  text_id = widget_text(base, value=value, uvalue=uvalue, _extra=extra)
  widget_control, text_id, set_text_select=strlen(value)
  
  return, text_id
END


PRO spice_cat::build_range_filter, column_name, filter_as_array
  base = self.wid.filter_base
  
  min_text = self.build_range_filter_text(base, column_name, "MIN", filter_as_array[0])
  
  label = widget_label(self.wid.filter_base, value="<= " + column_name + " <=")
  
  max_text = self.build_range_filter_text(base, column_name, "MAX", filter_as_array[1])
    
  self.wid.min_filter_text = min_text
  self.wid.max_filter_text = max_text
  
  IF self.d.keyword_info[column_name].type EQ "t" THEN BEGIN
     button_uvalue = "REBUILD_FILTER`" + column_name + "`"
     button = widget_button(self.wid.filter_base, value="Use regexp", uvalue=button_uvalue)
  END
  
  self.wid.filter_focus_flash_text = min_text
END


PRO spice_cat::build_sort_choices_for_column, base, sort_column_name
  column_name_base = widget_button(base, value=sort_column_name, /menu)
  
  foreach sort_order, ['INCREASING', 'DECREASING'] DO BEGIN
     uvalue = "SORT`" + sort_order.toupper() + "`" + sort_column_name
     sensitive = (sort_column_name NE self.curr.sort_column) $
                 OR (sort_order NE self.curr.sort_order)
     value = sort_order.tolower()
     button = widget_button(column_name_base, uvalue=uvalue,  value=value, sensitive=sensitive)
  END
  
END


PRO spice_cat::build_sort_pulldown
  widget_control, self.wid.top_base, update=0
  self.destroy_children, self.wid.sort_base
  
  incr_or_decr = self.curr.sort_order EQ "INCREASING" ? "(incr)" : "(decr)"
  
  menu_text = "Sort: " + self.curr.sort_column + " " + incr_or_decr
  bbase = widget_base(self.wid.sort_base, xpad=0, ypad=0, frame=2)
  menu = widget_button(bbase, /menu, value=menu_text)
  
  foreach column_name, self.curr.column_names DO BEGIN
     self.build_sort_choices_for_column, menu, column_name
  END
  
  widget_control, self.wid.top_base, update=1
END



PRO spice_cat::build_add_column_menu, base, uvalue
  left_or_right_of = (uvalue.split("`"))[2]
  foreach column_name, self.d.full_column_names DO BEGIN
     IF total(column_name EQ self.curr.column_names) GT 0 THEN CONTINUE
     full_uvalue = uvalue + "`" + column_name
     print, "BUILD: " + full_uvalue
     b = widget_button(base, value=column_name, uvalue=full_uvalue)
  END
END

PRO spice_cat::build_context_menu_heading, base, ev
  tag_name = (tag_names(self.curr.displayed))[ev.col]
  column_name = tag_name.replace('$', '-')
  
  buttons = [ {value:"Sort increasing",  uvalue:"SORT`INCREASING`",  sensitive:1 }, $
              {value:"Sort decreasing",  uvalue:"SORT`DECREASING`",  sensitive:1 }, $
              {value:"Move left",        uvalue:"MOVE`LEFT`",        sensitive:1 }, $
              {value:"Move right",       uvalue:"MOVE`RIGHT`",       sensitive:1 }, $
              {value:"Add column left",  uvalue:"ADD_COLUMN`LEFT`",  sensitive:1 }, $
              {value:"Add column right", uvalue:"ADD_COLUMN`RIGHT`", sensitive:1 }, $
              {value:"Remove column",    uvalue:"REMOVE_COLUMN`",    sensitive:1 } $
            ]
  
  buttons[*].uvalue = buttons[*].uvalue + column_name

  uvalue_for_current_sorting = "SORT`" + self.curr.sort_order + "`" + self.curr.sort_column
  
  possible_matching_uvalue_ix = (where(buttons.uvalue EQ uvalue_for_current_sorting))[0]
  IF possible_matching_uvalue_ix NE -1 THEN buttons[possible_matching_uvalue_ix].sensitive = 0
  
  column_position = (where(column_name EQ self.curr.column_names))[0]
  move_left_insensitive = column_position EQ 0
  move_right_insensitive = column_position EQ (n_elements(self.curr.column_names)-1)
  IF move_left_insensitive THEN buttons[2].sensitive = 0
  IF move_right_insensitive THEN buttons[3].sensitive = 0
  
  foreach button, buttons DO BEGIN
     add_column_menu = strmid(button.uvalue, 0, 10) EQ "ADD_COLUMN"
     b = widget_button(base, _extra=button, menu=add_column_menu)
     IF add_column_menu THEN self.build_add_column_menu, b, button.uvalue
  END 
END


PRO spice_cat::handle_filter_on_full_value, ev, parts
  column_name = parts[1]
  full_value = parts[2]
  IF self.d.keyword_info[column_name].type EQ "t" THEN BEGIN 
     self.set_filter_by_column_name, column_name, [full_value]
  END
END


PRO spice_cat::build_context_menu_datacell, base, ev
  column_name = (tag_names(self.curr.displayed))[ev.col].replace('$', '-')
  cell_value = self.curr.displayed[ev.row].(ev.col).tostring()
  
;  filename = self.curr.displayed[ev.row].filename
;  filename_uvalue = "CONTEXT_CLICK_ON_FILENAME`"+filename
  
  filter_on_value = "Filter on " + column_name + "='" + cell_value + "'"
  filter_on_value_uvalue = "FILTER_ON_FULL_VALUE`" + column_name + "`" + cell_value
  
  button = widget_button(base, value=filter_on_value, uvalue=filter_on_value_uvalue)
END


PRO spice_cat::build_command_buttons
  base = self.wid.button_base
  
  return_or_select = self.d.modal ? "Return selection" : "Exit"
  b = widget_button(base, value=return_or_select, uvalue='EXIT', /align_center)
  
  bbase = widget_base(base, xpad=0, ypad=0, frame=2)
  call = widget_button(bbase, value="Call procedure", /menu)

  foreach program, self.d.programs DO BEGIN
     b = widget_button(call, value=program, uvalue="CALL_PROGRAM`"+program)
  END
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
  self.wid.table_id = widget_table(self.wid.table_base, value=self.curr.displayed, _extra=props)
  self.display_displayed_list
  
  widget_control,self.wid.table_id,set_table_select=[-1,-1,-1,-1]
END


PRO spice_cat::build_widget
  w = (self.wid = dictionary()) ; What happens to W also happens to self.wid
  
  w.top_base = widget_base(title='SPICE-CAT', uvalue=self, /row, /tlb_size_events, xpad=0, ypad=0)
  
  self.replace_previous_incarnation

  w.ysize_spacer_base = widget_base(w.top_base, ysize=800, xpad=0, ypad=0)
  w.content_base = widget_base(w.top_base, /column)
  
  w.xsize_spacer_base = widget_base(w.content_base, /row, xsize=800)  
  w.top_row_base = widget_base(w.content_base, /row, xpad=0, ypad=0)
  w.message_base = widget_base(w.content_base, /row, xpad=0, ypad=0)
  w.table_base = widget_base(w.content_base, /column, xpad=0, ypad=0)
    
  w.draw_focus_away = widget_text(w.ysize_spacer_base, scr_xsize=1, scr_ysize=1)
  w.button_base = widget_base(w.top_row_base, /row, /align_center, xpad=0, ypad=0)
  w.sort_base = widget_base(w.top_row_base, /row, xpad=0, ypad=0, /align_center)
  button_spacer = widget_base(w.top_row_base, xsize=1, xpad=0, ypad=0)
  w.filter_base = widget_base(w.top_row_base, /row, xpad=0, ypad=0, /align_center)
  
  w.message_label = widget_label(w.message_base, value='     STATUS:', /align_right)
  w.message_text = widget_text(w.message_base, scr_xsize=700)
  
  self.build_command_buttons
  
  self.build_sort_pulldown
  
  t = "Click on green line or the heading to edit filter"
  text = widget_label(w.filter_base, frame=2,value=t)
  
  self.build_table
  
  widget_control,w.top_base, /realize
  spice_center_window, w.top_base
  
  ;; Make table fill available space despite /scroll
  widget_control,w.top_base, tlb_get_size=tlb_size
  resize_event = {widget_base,id:0L,top:0L,handler:0L, x:tlb_size[0], y:tlb_size[1] }
  self.handle_tlb, resize_event
END


PRO spice_cat_______________CATCH_ALL_EVENT_HANDLER, event
  
  widget_control, event.top, get_uvalue=self
  
  IF event.id EQ event.top THEN BEGIN ;; TLB event has ID = TOP
     self.handle_tlb,event
     return
  END 
  
  widget_control, event.id, get_uvalue=uvalue
  
  parts = uvalue.split('`')
  method = "handle_"+parts[0]
  
  call_method,method, self, event, parts
END


PRO spice_cat::parameters, modal=modal
  self.d = dictionary() ;; Data/dict
  self.curr = dictionary() ;; <name is wrong if a comment is necessary...>
  self.d.modal = keyword_set(modal)
  self.d.programs = ["help", "print"] ;; TODO: plug in Martin's routines
  self.d.keyword_info = spice_keyword_info(/all)
  spice_datadir = getenv("SPICE_DATA")
  IF spice_datadir EQ "" THEN message,"Environment variable SPICE_DATADIR is blank or not set"
  
  spice_default,listfiledir,spice_datadir
  
  self.d.spice_datadir = spice_datadir
  self.d.listfiledir = listfiledir
  self.d.listfilename = concat_dir(listfiledir, 'spice_fitslist.txt')
END


PRO spice_cat::send_event, uvalue
  widget_control, self.wid.draw_focus_away, set_uvalue=uvalue
  event = {id:self.wid.draw_focus_away, top:self.wid.top_base}
  spice_cat_______________catch_all_event_handler, event
END

function spice_cat::init, modal=modal
  self.parameters, modal = modal
  self.load_fitslist
  
  column_names = getenv("SPICE_CAT_KEYWORDS")
  IF column_names GT "" THEN BEGIN
     column_names = column_names.split(",")
     column_names = column_names.trim()
     IF total(column_names EQ "DATE-BEG") EQ 0 THEN column_names = ["DATE-BEG", column_names]
     IF total(column_names EQ "FILENAME") EQ 0 THEN column_names = ["FILENAME", column_names]
  END ELSE BEGIN
     column_names = []
  END
  self.create_displayed_list,column_names
  
  self.build_widget
  
  no_block = keyword_set(self.d.modal) ? 0 : 1
  event_handler = "spice_cat_______________catch_all_event_handler"
  xmanager,"spice_cat", self.wid.top_base, no_block=no_block, event_handler=event_handler
  
  return,1
END


PRO spice_cat_define_structure
  dummy = {spice_cat, d: dictionary(), wid:dictionary(), curr: dictionary()}
END


FUNCTION spice_cat                   ;; IDL> selection = spice_cat()
  spice_cat_define_structure
  o = obj_new('spice_cat',/modal)
  return, o.selection()
END


PRO spice_cat, o                     ;; IDL> spice_cat
  spice_cat_define_structure
  o = obj_new('spice_cat')
END

setenv,"SPICE_CAT_KEYWORDS=FILENAME,DATE-BEG,COMPRESS,OBS_ID,NWIN"
;setenv, "SPICE_CAT_KEYWORD_WIDTHS=FILENAME:50,DATE-BEG:20"

spice_cat, o
;
; The beginnings of unit testing! Can also be used for compoud widgets in
; isolation!
;
uvals = ["ADD_COLUMN`LEFT`DATE-BEG`STUDY_ID", $
         "ADD_COLUMN`LEFT`FILENAME`STUDYTYP", $
         "ADD_COLUMN`RIGHT`NWIN`NWIN_INT", $
         "REMOVE_COLUMN`STUDY_ID", $
         "REMOVE_COLUMN`STUDYTYP", $
         "REMOVE_COLUMN`NWIN_INT", $
         "MOVE`LEFT`DATE-BEG", $
         "MOVE`RIGHT`DATE-BEG", $
         "MOVE`RIGHT`DATE-BEG", $
         "MOVE`RIGHT`DATE-BEG", $
         "MOVE`RIGHT`DATE-BEG" $
        ]
foreach uval, uvals DO o.send_event, uval
END
