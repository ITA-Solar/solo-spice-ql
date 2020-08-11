

function spice_pickfile__label,base,text
  return, widget_label(base,value=text,frame=5)  
END



PRO spice_pickfile
  default,spice_datadir,'/mn/acubens/u1/steinhh/tmp/spice_data/level2'
  default,listfiledir,spice_datadir
  listfilename = concat_dir(listfiledir,'spice_fitslist.txt')
  
  table_contents = replicate({a:"a",b:3.5},10)
  base = widget_base(/row,/base_align_bottom)
  base1 = widget_base(base,/column,/frame)
  table = widget_table(base1,value=table_contents,/row_major)
  base2 = widget_base(base,/column,/frame)
  table = widget_table(base2,value=table_contents,/column_major)
  widget_control,base,/realize
  
  
END

spice_pickfile
END
