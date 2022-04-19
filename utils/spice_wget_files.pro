;+
; Project     :	Solar Orbiter - SPICE
;
; Name        :	SPICE_WGET_FILES
;
; Purpose     :	Mirror SPICE FITS files from Oslo
;
; Explanation : This routine mirrors SPICE FITS files from Oslo.  Files are
;               written to either a "level1" or "level2" subdirectory of the
;               $SPICE_DATA directory.  Below each directory, the files are
;               organized by date, e.g. "level2/2021/10/20".  Files no longer
;               on the server are removed from the local copy.  The
;               spice_catalog.txt files are also mirrored.
;
; Examples    :	spice_wget_files, '2021/10/20', level=2
;
; Inputs      :	SUBPATH = Subdirectory path to mirror.  If not passed, the
;                         entire directory tree is mirrored.  Possible formats
;                         are:
;
;                               '2021/10/20'    Day
;                               '2021/10'       Month
;                               '2021'          Year
;
; Keywords    :	LEVEL   = Data level, either 1 or 2.  Default=2.
;
; Env. Vars.  : SPICE_DATA = Location of top of SPICE data tree.  Below this
;                            are the "level*" directories.
;
;               SPICE_PWD  = Password for the SPICE website.
;
; Restrictions:	The environment variables SPICE_DATA and SPICE_PWD are
;               required.
;
; History     :	Version 1, 11-Apr-2022, William Thompson, GSFC
;
; Contact     :	WTHOMPSON
;-
;
pro spice_wget_files, p_subpath, level=k_level
;
if n_elements(p_subpath) eq 0 then subpath = '' else subpath = p_subpath
if datatype(subpath) ne 'STR' then subpath = ntrim(subpath)
if n_elements(k_level) eq 0 then level=2 else level=k_level
level = ntrim(level)
;
;  First, get the top catalog file.
;
spice_data = getenv('SPICE_DATA')
remote_dir = 'http://astro-sdc-db.uio.no/vol/spice/fits/'
pwd = getenv('SPICE_PWD')
command0 = 'wget -nH -L -m -erobots=off --user=spice --password=' + pwd + $
           ' --no-check-certificate --cut-dirs=3 -P ' + spice_data + ' '
command = command0 + remote_dir + 'spice_catalog.txt'
print, command
spawn, command
;
;  Next get the catalog file for the current level.
;
remote_dir = remote_dir + 'level' + level + '/'
command = command0 + remote_dir + 'spice_catalog.txt'
print, command
spawn, command
;
;  Get a list of currently held FITS files.
;
local_dir = concat_dir(spice_data, 'level' + level + '/' + subpath)
old_files = file_search(local_dir, '*.fits')
;
;  Mirror over the selected data, and cleanup any old files no longer on the
;  server.
;
remote_dir = remote_dir + subpath
command = command0 + remote_dir
if strmid(command, strlen(command)-1, 1) ne '/' then command = command + '/'
print, command
spawn, command
ssw_wget_cleanup2, local_dir, remote_dir
;
;  Determine which files are new.
;
new_files = file_search(local_dir, '*.fits')
new = bytarr(n_elements(new_files))
n_new = 0
for i=0,n_elements(new_files)-1 do begin
    w = where(new_files[i] eq old_files, count)
    if count eq 0 then begin
        new[i] = 1
        n_new = n_new + 1
    endif
endfor
if n_new gt 0 then begin
    print
    print, 'New files:'
    print
    w = where(new)
    hprint, new_files[w]
endif
;
end
