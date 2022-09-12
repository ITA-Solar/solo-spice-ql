; $Id: 2022-08-11 15:08 CEST $
; SPICE_CAT User Manual
;
; SPICE_CAT reads a SPICE fits file catalog with metadata about each fits
; file, generated with SPICE_GEN_CAT, and displays the information in a
; searchable and sortable table.
;
; When called as a FUNCTION, it returns a list of file names corresponding
; to the selected rows in the table. When called as a PROCEDURE it returns
; immediately after creating the widget. So, invoke as one of these two:
;
;    IDL> files = spice_cat()     OR      IDL> spice_cat
;
; If your data directory does not already contain a spice_catalog.csv file,
; generate one by using SPICE_GEN_CAT.
;
; TIP 1: If you have multiple data levels in your SPICE_DATA directory and
;        only want to work with one level, set the filter for the LEVEL
;        keyword (third column by default) to e.g. "L2".
;
; TIP 2: Upon reading the catalog, the application generates a synthetic
;        keyword called FIRST_RASTER (shown as the first column by
;        default). This is set to "x" for files that are the first file
;        (chronologically) which has this file's value of SPIOBSID. Since
;        repeated rasters have the same SPIOBSID, this means that by filtering
;        on this column being equal to "x", you will only see one line for
;        each raster with multiple repetitions. This works even if some files
;        are missing at the beginning of a series of raster repetitions.
;
; NOTE: Since L0 files do not have DATE-BEG, the default sort order is set to
;       DATE_OBT (the on-board time of DATE-BEG).
;
; ****** FUNCTIONALITIES OF BUTTONS ETC *******
;
;; * Return selection
;  : Present only when called as a function. Returns a list of file names
;    according to the selected rows in the table
;
;; * Exit
; : Present only when called as a procedure - exits application
;
;; * Call procedure
;  : Select a procedure to be called with the file names corresponding to the
;    selected rows in the table as an argument.
;
;; * Sort: <keyword> <direction>
;  : Pulldown menu to select sorting on a specific keyword, ascending or
;    descending. More easily available through context menu by right-clicking
;    on column heading.
;
;; * Filter section (after filtering is invoked, see below)
;  : For numeric columns, range filtering can be done by entering numbers in
;    the min/max fields. 
;
;  : For text columns, the default is to use case-insensitive glob patterns
;    such as "solo_*_exp". The glob pattern will match substrings (there is an
;    implied asterisk at the beginning and end of the pattern). 
;
;  : Alphabetical range filtering can also be used. Note that to select all
;    files from March (only), you should use the range { 2020-03, 2020-04 },
;    since e.g. 2020-04-01 is lexically larger than just 2020-04
;
;; * Status line
;  : When selecting a cell in the table, the full value of the cell is shown,
;    which can be handy since texts can often be longer than the width of a
;    column. Also, the text is automatically selected. On some platforms, this
;    puts the value directly into the cut buffer for pasting somewhere
;    else. On other platforms, it may be necessary to press Ctrl-/Cmd-C.
;
;  : When editing filters, the status line will show the number of files that
;    match the current filters.
;
;; * Table column headers
;  : A regular click on a column header initiates editing of the filter for
;    that column.
;
;  : A right-click on a column header reveals a context menu. The first
;    "choice" does nothing, it's just there to show the full name of the
;    column (in case the column is too narrow to display it). The other
;    choices are hopefully self-explanatory - you can sort ascending or
;    descending on that column, move the column left or right, add another
;    column to the left or right, or remove the column.
;
;; * First table row
;  : Clicking into a cell in the first row of the table initiates editing of
;    the filter for that column - in the filter section at the top of the
;    window.
;
;  : Filters that are in effect are highlighted in yellow. The filter being
;    edited is colored green.
;
;; * Other table cells
;  : A cell is selected by being clicked on, and multiple cells can be
;    selected by clicking and dragging. When a cell is selected, the file to
;    which the cell belongs is selected, and the entire row is colored
;    purple. Also, when selecting a single cell, the status line is set to
;    contain the full contents of the cell (see "Status line" above).
;
;  : A right-click on a regular table cell gives the option to set the filter
;    for this column to the precise value of the cell.
;
;  : The right-click to filter on a precise value can be useful when you want
;    to see only files in one specific series of rasters. Right-click on the
;    file name of one of them, then clip the -nnn.fits part of the file name
;    in the filter.
;                                                        
;
; ******* ENVIRONMENT VARIABLES *******
;
;  SPICE_DATA points to the top of the data directory hierarchy (where the
;  spice_catalog.csv catalog should reside)
;
;  SPICE_CAT_KEYWORDS is a comma-separated list of the keywords to be shown in
;  the table. The environment variable is updated according to 
