;+
; Project     : SOLAR ORBITER - SPICE     
;                   
; Name        : SPICE_CAT
;               
; Purpose     : Interactively search list of files from spice_catalog.csv
;               
; Explanation : This program allows filtering/sorting/selection of the
;               contents in spice_catalog.csv in the $SPICE_DATA/ directory
;               (but other paths can be specified, see CATALOG below),
;               generated with SPICE_GEN_CAT.
;
;               See spice_cat_readme.txt for details
;
; Use         : spice_cat  (procedure)  OR  filelist=spice_cat()  (function)
;    
; Inputs      : None required.
;               
; Opt. Inputs : CATALOG: Path to the spice_catalog.csv file to be used
;               
; Outputs     : When used as a function, returns list of selected files if any
;               
; Opt. Outputs: None.
;               
; Keywords    : KEYWORDS: Comma-separated list of keywords to be displayed in
;                         the table. If not supplied, the list is taken from
;                         $SPICE_CAT_KEYWORDS. If that is not defined, all
;                         keywords present in spice_catalog.csv are
;                         shown. Note that the application can be quite
;                         sluggish if many keywords are shown for a long list
;                         of files (>1000 files).
;               WIDTHS: Comma-separated list of column width specifications in
;                       the form of KEYWORD:width (in pixels). Normally taken
;                       from $SPICE_CAT_KEYWORD_WIDTHS
;
; Category    : SPICE_UTILITY
;               
; Prev. Hist. : Concept based on cfitslist/sfitslist for CDS/SUMER files
;
; Written     : Stein Vidar H. Haugan, UiO, 9 August 2020
;               
; Modified    : Version 1, SVHH, 9 August 2020
;                          Initial version based on sfitslist.pro
;               Version 2, SVHH, 9 September 2020
;                          Rewritten from scratch
;
; Version     : Version 2, SVHH, 9 September 2020
;
;
; $Id: 2024-02-13 13:50 CET $
;-            
;;
PRO spice_cat::_____________UTILITY_FUNCTIONS & END
;;
  
PRO spice_cat::setenv_commands_info
  compile_opt static
  print
  print, "* Put these commands in your IDL startup to save the current column setup"
  print, "* Equivalent commands may also be put in your shell initialization file"
  print
  print, 'setenv, "SPICE_CAT_KEYWORDS=' + getenv("SPICE_CAT_KEYWORDS") + '"'
  print
  print, 'setenv, "SPICE_CAT_KEYWORD_WIDTHS=' + getenv("SPICE_CAT_KEYWORD_WIDTHS") + '"'
  print
END


PRO spice_cat::save_column_widths
  foreach width, self.d.widths_by_column_name, column_name DO BEGIN
     IF n_elements(keyword_width_assignments) EQ 0 THEN keyword_width_assignments = []
     keyword_width_assignments = [keyword_width_assignments, column_name + ":" + width.toString()]
  END
  
  setenv, "SPICE_CAT_KEYWORD_WIDTHS=" + keyword_width_assignments.join(",")
END


PRO spice_cat::cleanup
  print
  print, "Column setup saved for this IDL session"
  print, "To see how to preserve it for future sessions, type spice_cat.setenv_commands_info"
  IF widget_info(self.wid.top_base, /valid_id) THEN widget_control, self.wid.top_base, /destroy
END


PRO spice_cat::modal_message,message,timer=timer
  spice_modal_message, self.wid.top_base, message, timer=timer
END


FUNCTION spice_cat::remove_non_digits_or_points, text
  compile_opt static
  bytes = byte(text)
  byte0 = (byte('0'))[0]
  byte9 = (byte('9'))[0]
  byte_point = (byte('.'))[0]
  ix = where(((bytes GE byte0) AND (bytes LE byte9)) OR (bytes EQ byte_point), count)
  IF count EQ 0 THEN return,""
  ok_bytes = bytes[ix]
  IF ok_bytes[0] EQ byte_point THEN BEGIN
     ix = where(ok_bytes NE byte_point, count)
     IF count EQ 0 THEN return, ""
     ok_bytes = ok_bytes[ix]
  END 
  return, string(ok_bytes)
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


PRO spice_cat::register_rows_selection, sel, clear=clear
  IF keyword_set(clear) THEN BEGIN
     self.curr.selection = []
     self.curr.selection_beg = -1
     self.curr.selection_end = -1
     return
  END
  
  ;; STUPID IDL gives selection events based on previous selection,
  ;; even after table has been updated (i.e. the selected rows may be
  ;; gone!!!!!!)
  sel.top = sel.top < n_elements(self.curr.displayed)
  sel.bottom = sel.bottom < n_elements(self.curr.displayed)

  self.curr.selection = self.curr.displayed[sel.top:sel.bottom].filename
  self.curr.selection_beg = sel.top
  self.curr.selection_end = sel.bottom
  widget_control, self.wid.table_id, background_color=self.background_colors()
END


FUNCTION spice_cat::selection
  IF NOT self.curr.haskey("selection") THEN return, !null
  return, self.curr.selection
END


PRO spice_cat::safe_struct_assign, source, destination
  ;; Builtin STRUCT_ASSIGN BLANKS OUT tags that do not exist in source!!!
  source_tag_names = tag_names(source)
  foreach destination_tag_name, tag_names(destination), destination_tag_ix DO BEGIN
     source_tag_ix = (where(source_tag_names EQ destination_tag_name))[0]
     IF source_tag_ix NE -1 THEN destination.(destination_tag_ix) = source.(source_tag_ix)
  END
END
  

PRO spice_cat::send_event, uvalue
  widget_control, self.wid.draw_focus_away, set_uvalue=uvalue
  event = {id:self.wid.draw_focus_away, top:self.wid.top_base}
  spice_cat_______________catch_all_event_handler, event
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

PRO spice_cat::load_catalog
  catalog = spice_read_cat(self.d.cat_filename)  ;; Array of orderedhashes()
  
  self.d.full_list = catalog
  self.d.full_tag_names = tag_names(catalog[0])
  self.d.full_column_names = (tag_names(catalog[0])).replace('$','-')
END 


FUNCTION spice_cat::apply_filter, filter, full_list_tag_index
  filter = filter.tolower()
  filter_as_array = self.filter_as_array(filter)
  
  IF n_elements(filter_as_array) EQ 1 THEN BEGIN
     regex_from_glob = filter_as_array.replace("*", ".*")
     regex_from_glob = regex_from_glob.replace("?", ".")
     mask = self.d.full_list[*].(full_list_tag_index).matches(regex_from_glob,/fold_case)
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
        mask = self.d.full_list[*].(full_list_tag_index).tolower() GE min
     END
     
     IF filter_as_array[1] NE "" THEN BEGIN
        mask = mask AND self.d.full_list[*].(full_list_tag_index).tolower() LE max
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


PRO spice_cat::update_current_filters, keywords
  prits_tools.default, keywords, self.curr.column_names
  
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


PRO spice_cat::create_displayed_list, column_names
  IF NOT self.curr.haskey("column_names") THEN self.curr.column_names = self.d.full_column_names
  
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
  setenv, "SPICE_CAT_KEYWORDS=" + self.curr.column_names.join(',')
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

;
; Input "filter_colors" may be none or an array (3,cols,rows) where the rows
; dimension is optional
;
FUNCTION spice_cat::baseline_filter_colors, filter_colors, table_selection=table_selection
  num_columns = n_elements(self.curr.column_names)
  regular_filter_colors = rebin(self.d.color_filter, 3, num_columns)
  
  IF n_elements(filter_colors) EQ 0 THEN BEGIN
     filter_colors = regular_filter_colors
  END ELSE BEGIN
     filter_colors[*, *, 0] = regular_filter_colors
  END
  
  foreach column_name, self.curr.column_names, index DO BEGIN
     filter_as_text = self.filter_as_text(self.get_filter_by_column_name(column_name))
     IF (filter_as_text NE "<filter>") AND (filter_as_text NE "") THEN BEGIN
        filter_colors[*, index, 0] = self.d.color_filter_in_use
     END
  END
  table_selection = [0, 0, n_elements(self.curr.column_names)-1, 0]
  return, filter_colors
END

;; TODO: TODO: Only redisplay when list is *actually* changed (keep list of FILENAME?)
;;

FUNCTION spice_cat::background_colors
  num_table_columns = n_elements(self.curr.column_names)
  num_table_rows = n_elements(self.curr.displayed)
  
  ;; Basic:
  background_colors = rebin(self.d.color_table, 3, num_table_columns, num_table_rows)
  
  ;; Light green filters:
  background_colors = self.baseline_filter_colors(background_colors)
  
  ;; Selected rows:
  IF self.curr.haskey("selection_beg") && self.curr.selection_beg GT 0 THEN BEGIN
     selection_size = self.curr.selection_end - self.curr.selection_beg + 1
     selection_colors = rebin(self.d.color_selection, 3, num_table_columns, selection_size)
     background_colors[*, *, self.curr.selection_beg : self.curr.selection_end] = selection_colors
  END
  
  ;; Sort column. NOTE: Just a brightened version of current colors!!
  sort_column_ix = (where(self.curr.sort_column EQ self.curr.column_names))[0]
  background_colors[*, sort_column_ix, *] = (background_colors[*, sort_column_ix, *] + 40) < 255
  
  return, background_colors
END


PRO spice_cat::init_column_widths
  self.d.widths_by_column_name = hash()
  
  foreach info, self.d.keyword_info, column_name DO BEGIN
     self.d.widths_by_column_name[column_name] = info.display_width * 12
  END
  
  ; Override with SPICE_CAT_KEYWORD_WIDTHS when set
  spice_cat_keyword_widths = getenv("SPICE_CAT_KEYWORD_WIDTHS")
  IF spice_cat_keyword_widths EQ "" THEN BEGIN
     self.save_column_widths
     return
  END
  
  spice_cat_keyword_widths = spice_cat_keyword_widths.split(',')
  foreach width_setting, spice_cat_keyword_widths DO BEGIN
     parts = width_setting.split(':')
     self.d.widths_by_column_name[parts[0]] = parts[1].toInteger()
  END
END
  

FUNCTION spice_cat::current_column_widths
  widths = []
  foreach column_name, self.curr.column_names DO BEGIN
     widths = [widths, self.d.widths_by_column_name[column_name]]
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


PRO spice_cat::scroll_to_top_without_select
  old_view = widget_info(self.wid.table_id, /table_view)
  widget_control, self.wid.table_id, set_table_select=[-1, -1, -1, -1]
  widget_control, self.wid.table_id, set_table_view=[old_view[0], 0]
END


PRO spice_cat::display_displayed_list
  widget_control, self.wid.table_id, update=1
  
  widget_control, self.wid.table_id, $
                  set_value=self.curr.displayed, $
                  table_ysize=n_elements(self.curr.displayed), $
                  table_xsize=n_elements(tag_names(self.curr.displayed)), $
                  alignment=self.cell_alignments(), $
                  column_labels=self.current_column_labels()
  
  widget_control, self.wid.table_id, column_widths=self.current_column_widths()
  
  self.set_message, "Files found:", " "+(n_elements(self.curr.displayed)-1).tostring()
  
  widget_control,self.wid.table_id, background_color=self.background_colors()

  widget_control, self.wid.table_id, update=1
  self.scroll_to_top_without_select
END

;;
PRO spice_cat::_____________EVENT_HANDLING_HELPERS                      & END
;;

FUNCTION spice_cat::get_filter_by_column_name, column_name
  column_number = where(self.curr.column_names EQ column_name)
  filters_as_text = self.curr.filters_as_text ;; Core-dump if not
  filter_as_text = filters_as_text.(column_number)
  return, self.filter_as_array(filter_as_text)
END


PRO spice_cat::set_filter_by_column_name, column_name, filter_as_array
  filter_as_text = self.filter_as_text(filter_as_array)
  
  IF filter_as_text EQ "" THEN filter_as_text = "<filter>"
  column_number = (where(self.curr.column_names EQ column_name))[0]
  
  select = [column_number, 0, column_number, 0]
  widget_control, self.wid.table_id, set_value=filter_as_text, use_table_select=select
  
  ;; DON'T CHANGE THIS LINE! Using "self.curr.filters_as_text" DIRECTLY => core dump!
  current_filters_as_text = self.curr.filters_as_text

  IF filter_as_text NE current_filters_as_text.(column_number) THEN BEGIN
     self.register_rows_selection, /clear
     current_filters_as_text.(column_number) = filter_as_text
     self.curr.filters_as_text = current_filters_as_text
     self.create_displayed_list
     self.display_displayed_list
     self.set_filter_cell_to_edit_color,column_name
  END
END


PRO spice_cat::set_filter_cell_to_edit_color, column_name, clear=clear
  num_columns = n_elements(tag_names(self.curr.displayed))
  filter_colors = self.baseline_filter_colors(table_selection = table_selection)
  
  IF NOT keyword_set(clear) THEN BEGIN 
     column_number = (where(self.curr.column_names EQ column_name))[0]
     filter_colors[*, column_number, 0] = self.d.color_editing_filter
  END
  widget_control, self.wid.table_id, background_color=filter_colors, use_table_select=table_selection
END


FUNCTION spice_cat::absorb_filter_focus_change, event, column_name
  IF tag_names(event,/structure_name) EQ "WIDGET_KBRD_FOCUS" THEN BEGIN
     IF event.enter GE 0 THEN self.set_filter_cell_to_edit_color, column_name
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


PRO spice_cat::capture_column_widths
  widths = widget_info(self.wid.table_id, /column_widths)
  foreach column_name, self.curr.column_names, index DO BEGIN
     self.d.widths_by_column_name[column_name] = fix(widths[index])
  END
  self.save_column_widths
END

;;
PRO spice_cat::_____________EVENT_HANDLERS                     & END
;;

; RESIZE TABLE ACCORDING TO TLB size change!
; Only called when event.id eq event.top
;
PRO spice_cat::handle_tlb,event
  IF tag_names(event, /structure_name) EQ "WIDGET_KILL_REQUEST" THEN BEGIN
     IF NOT self.d.modal THEN obj_destroy, self $
     ELSE                     widget_control, self.wid.top_base, /destroy
     return
  END
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
     IF (where(new_column_names EQ 'DATE-BEG'))[0] NE -1 THEN newsort = 'DATE-BEG' $
     ELSE IF (where(new_column_names EQ 'FILENAME'))[0] NE -1 THEN newsort = 'FILENAME' $
     ELSE newsort = new_column_names[0]
     self.curr.sort_column = newsort
     self.register_rows_selection, /clear
  END
  
  removed_filter = self.filter_as_text(self.get_filter_by_column_name(column_name))
  IF removed_filter NE "<filter>" THEN self.register_rows_selection, /clear

  self.create_displayed_list, new_column_names
  start_time = systime(1)
  self.display_displayed_list
END


PRO spice_cat::handle_add_column, event, parts
  left_or_right = parts[1]
  left_or_right_of = parts[2]
  add_column_name = parts[3]
  
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
  self.register_rows_selection, /clear
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
     new_value = self.remove_non_digits_or_points(value)
     
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
  
  widget_control, self.wid.filter_focus_text, /input_focus
  
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
  
  IF sel.top GE 1 THEN self.register_rows_selection, sel
  
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
  
  handle_as_filter_click = header_click OR filter_click
  IF handle_as_filter_click THEN BEGIN
     column_name = self.curr.column_names[sel.left]
     self.deal_with_click_on_filter_cell,column_name
     self.scroll_to_top_without_select
  END
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
  IF NOT self.d.modal THEN obj_destroy, self
END



;;
PRO spice_cat::_____________WIDGET_BUILDERS                   & END
;;

PRO spice_cat::build_text_filter, column_name, filter_as_array
  widget_control, self.wid.filter_base, update=0
  filter_as_text = filter_as_array[0]
  filter_text_uvalue = "TEXT_FILTER_CHANGE`" + column_name
  self.wid.filter_label = widget_label(self.wid.filter_base, value=column_name+":")
  text_props = {editable:1b, all_events:1b, kbrd_focus_events: 1b}
  self.wid.filter_text = widget_text(self.wid.filter_base, value=filter_as_text,$
                                     _extra=text_props, uvalue=filter_text_uvalue)
  button_uvalue = "REBUILD_FILTER`"+column_name+"``"
  button = widget_button(self.wid.filter_base, value="Use alphabetical range", uvalue=button_uvalue)
  self.wid.filter_focus_text = self.wid.filter_text
  widget_control, self.wid.filter_text, set_text_select=strlen(filter_as_text)
  widget_control, self.wid.filter_base, update=1
END


FUNCTION spice_cat::build_range_filter_text, base, column_name, minmax, value
  extra = {editable: 1b, all_events: 1b, kbrd_focus_events: 1b}
  uvalue = "RANGE_FILTER_CHANGE`" + column_name
  text_id = widget_text(base, value=value, uvalue=uvalue, _extra=extra)
  widget_control, text_id, set_text_select=strlen(value)
  return, text_id
END


PRO spice_cat::build_range_filter, column_name, filter_as_array
  widget_control, self.wid.filter_base, update=0
  base = self.wid.filter_base
  
  min_text = self.build_range_filter_text(base, column_name, "MIN", filter_as_array[0])
  
  label = widget_label(self.wid.filter_base, value="<= " + column_name + " <=")
  
  max_text = self.build_range_filter_text(base, column_name, "MAX", filter_as_array[1])
    
  self.wid.min_filter_text = min_text
  self.wid.max_filter_text = max_text
  
  IF self.d.keyword_info[column_name].type EQ "t" THEN BEGIN
     button_uvalue = "REBUILD_FILTER`" + column_name + "`"
     button = widget_button(self.wid.filter_base, value="Use glob pattern", uvalue=button_uvalue)
  END
  
  self.wid.filter_focus_text = min_text
  widget_control, self.wid.filter_base, update=1
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
     b = widget_button(base, value=column_name, uvalue=full_uvalue)
  END
END


PRO spice_cat::desensitize_button_by_uvalue, struct_arr, uvalue
  matching_uvalue_ix = (where(struct_arr[*].uvalue.startswith(uvalue)))[0]
  IF matching_uvalue_ix NE -1 THEN struct_arr[matching_uvalue_ix].sensitive = 0
END

PRO spice_cat::build_context_menu_heading, base, ev
  column_name = ((tag_names(self.curr.displayed))[ev.col]).replace('$', '-')
  num_columns = n_elements(self.curr.displayed)
  
  ;; TODO: Add button showing full header name
  buttons = [ {value: "Column: " + column_name, uvalue: "NULL`", sensitive:1}, $
              {value:"Sort increasing",  uvalue:"SORT`INCREASING`",  sensitive:1 }, $
              {value:"Sort decreasing",  uvalue:"SORT`DECREASING`",  sensitive:1 }, $
              {value:"Move left",        uvalue:"MOVE`LEFT`",        sensitive:1 }, $
              {value:"Move right",       uvalue:"MOVE`RIGHT`",       sensitive:1 }, $
              {value:"Add column left",  uvalue:"ADD_COLUMN`LEFT`",  sensitive:1 }, $
              {value:"Add column right", uvalue:"ADD_COLUMN`RIGHT`", sensitive:1 }, $
              {value:"Remove column",    uvalue:"REMOVE_COLUMN`",    sensitive:1 } $
            ]
  
  buttons[*].uvalue += column_name
  
  current_sort_uvalue = "SORT`" + self.curr.sort_order + "`" + self.curr.sort_column
  self.desensitize_button_by_uvalue, buttons, current_sort_uvalue
  
  IF ev.col EQ 0 THEN self.desensitize_button_by_uvalue, buttons, "MOVE`LEFT"
  IF ev.col EQ num_columns THEN self.desensitize_button_by_uvalue, buttons, "MOVE`RIGHT"
  
  foreach button, buttons DO BEGIN
     add_column_menu = strmid(button.uvalue, 0, 10) EQ "ADD_COLUMN"
     b = widget_button(base, _extra=button, menu=add_column_menu)
     IF add_column_menu THEN self.build_add_column_menu, b, button.uvalue
  END 
END


PRO spice_cat::handle_filter_on_precise_cell_value, ev, parts
  column_name = parts[1]
  full_value = parts[2]
  IF self.d.keyword_info[column_name].type EQ "t" THEN BEGIN 
     self.set_filter_by_column_name, column_name, [full_value]
  END
  IF self.d.keyword_info[column_name].type EQ "i" THEN BEGIN
     self.set_filter_by_column_name, column_name, [full_value, full_value]
  END
  self.scroll_to_top_without_select
END


PRO spice_cat::build_context_menu_datacell, base, ev
  column_name = (tag_names(self.curr.displayed))[ev.col].replace('$', '-')
  cell_value = self.curr.displayed[ev.row].(ev.col).tostring()
  
  filter_on_value = "Filter on " + column_name + "='" + cell_value + "'"
  filter_on_value_uvalue = "FILTER_ON_PRECISE_CELL_VALUE`" + column_name + "`" + cell_value
  
  button = widget_button(base, value=filter_on_value, uvalue=filter_on_value_uvalue)
END


PRO spice_cat::handle_spice_gen_cat, ev, parts
  spice_gen_cat
  spice_cat, self.d.cat_filename
END


PRO spice_cat::build_command_buttons
  base = self.wid.button_base
  
  b = widget_button(base, value="SPICE_GEN_CAT", uvalue="SPICE_GEN_CAT")
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
END


PRO spice_cat::build_widget
  w = (self.wid = dictionary()) ; What happens to W also happens to self.wid
  
  props = {row:1b, xpad:0, ypad:0, tlb_size_events:1, tlb_kill_request_events:1}
  base = widget_base(title='SPICE-CAT', uvalue=self, _extra=props)
  w.top_base = base
  widget_control, w.top_base, set_uvalue=self
  
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
  prits_tools.center_window, w.top_base
  
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
  IF parts[0] EQ "NULL" THEN return
  
  method = "handle_"+parts[0]
  
  call_method,method, self, event, parts
END


PRO spice_cat::set_background_colors
  self.d.color_table = replicate(230b, 3)
  self.d.color_filter = [230b, 255b, 230b]
  self.d.color_editing_filter = [150b, 255b, 150b]
  self.d.color_filter_in_use = [255b, 255b, 120b]
  self.d.color_selection = [200b, 200b, 255b]
END


FUNCTION spice_cat::parameters, modal=modal, catalog=catalog
  self.d = dictionary()    ;; "Data"
  self.curr = dictionary() ;; Current values
  self.last = dictionary() ;; Last values
  
  IF NOT keyword_set(catalog) THEN BEGIN
     spice_datadir = getenv("SPICE_DATA")
     IF spice_datadir EQ "" THEN message,"Environment variable SPICE_DATA is blank or not set"
     catalog = concat_dir(spice_datadir, 'spice_catalog.csv')
  END
  
  IF NOT file_test(catalog, /regular) THEN BEGIN
    message, "No spice_catalog.csv file: " + catalog, /informational
    message, 'Please run spice_gen_cat first to generate the catalog.', /informational
    return, 0
  ENDIF
  self.d.cat_filename = catalog
  
  self.d.modal = keyword_set(modal)
  self.d.programs = ["help", "print"] ;; TODO: plug in Martin's routines
  self.d.keyword_info = spice_keyword_info(/all)
  
  column_names = getenv("SPICE_CAT_KEYWORDS")
  IF column_names GT "" THEN BEGIN
     column_names = column_names.split(",")
     column_names = column_names.trim()
     IF total(column_names EQ "DATE-BEG") EQ 0 THEN column_names = ["DATE-BEG", column_names]
     IF total(column_names EQ "FILENAME") EQ 0 THEN column_names = ["FILENAME", column_names]
     IF total(column_names EQ "OBT_BEG") EQ 0 THEN column_names = ["OBT_BEG", column_names]
     self.curr.column_names = column_names
  END
  
  self.curr.sort_column = 'OBT_BEG'
  self.curr.sort_order = "INCREASING"
  
  self.init_column_widths
  
  self.set_background_colors
  return, 1
END


PRO spice_cat::start
  event_handler = "spice_cat_______________catch_all_event_handler"
  no_block = keyword_set(self.d.modal) ? 0 : 1
  xmanager,"spice_cat", self.wid.top_base, no_block=no_block, event_handler=event_handler
END

PRO spice_cat::halt
  stop
END

PRO spice_cat::replace_previous_incarnation
  COMMON spice_cat, previous_incarnation
  IF obj_valid(previous_incarnation) THEN obj_destroy, previous_incarnation
  previous_incarnation = self
END


function spice_cat::init, catalog, modal=modal, keywords=keywords, widths=widths
  self.replace_previous_incarnation
  IF n_elements(keywords) GT 0 THEN setenv, "SPICE_CAT_KEYWORDS=" + keywords
  IF n_elements(widths) GT 0 THEN setenv, "SPICE_CAT_KEYWORD_WIDTHS=" + widths
  IF self.parameters(modal = modal, catalog = catalog) THEN BEGIN
    self.load_catalog
    self.create_displayed_list
    self.build_widget

    return,1    
  ENDIF ELSE return,0
END


PRO spice_cat_define_structure
  dummy = {spice_cat, d: dictionary(), wid:dictionary(), curr: dictionary(), last:dictionary()}
END


FUNCTION spice_cat, catalog, keywords=keywords, widths=widths ;; IDL> selection = spice_cat()
  spice_cat_define_structure
  cat = obj_new('spice_cat',catalog, /modal, keywords=keywords, widths=widths)
  IF cat eq !NULL then return,!NULL
  cat.start ; Blocking
  selection = cat.selection()
  obj_destroy, cat
  return, selection
END


PRO spice_cat, catalog, output_object=object, keywords=keywords, widths=widths ;; IDL> spice_cat
  spice_cat_define_structure
  object = obj_new('spice_cat', catalog, keywords=keywords, widths=widths)
  object.start
END

spice_cat_development = 0
spice_cat_run_tests = spice_cat_development AND 0

IF spice_cat_development THEN BEGIN
   IF spice_cat_run_tests THEN setenv, "SPICE_CAT_KEYWORDS=FILENAME,DATE-BEG,COMPRESS,OBS_ID,NWIN" $
   ELSE                        setenv, "SPICE_CAT_KEYWORDS="
   
   spice_cat, output_object=o ;; , keywords="FILENAME,DATE-BEG"
   
   ;;
   ;; The beginnings of unit testing! Can also be used for compoud widgets in
   ;; isolation!
   ;;
   ;; Would be nice with utility functions for creating dummy events (with e.g.
   ;; row begin/end for table selection
   ;;
   IF spice_cat_run_tests THEN BEGIN 
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
END
END
