# SPICE Quicklook and Data Analysis Software

See [Data analysis user's manual](https://spice-wiki.ias.u-psud.fr/doku.php/data:data_analysis_manual)
for a general description of the SPICE data and its use. 
You'll also find a link to the Data Products Description Document (DPDD).

For more detailed information on the software described within this README, 
see [our wiki page](https://github.com/ITA-Solar/solo-spice-ql/wiki/).

If you find any bugs, please (preferably) [raise a new issue](https://github.com/ITA-Solar/solo-spice-ql/issues/new/choose)
or send an email to prits-group@astro.uio.no

## Table of Content

- [SPICE Quicklook and Data Analysis Software](#spice-quicklook-and-data-analysis-software)
  * [Table of Content](#table-of-content)
  * [Setup](#setup)
  * [Download SPICE FITS files](#download-spice-fits-files)
  * [Quicklook Software](#quicklook-software)
  * [SPICE Data Object](#spice-data-object)
  * [Level 3 Data Products](#level-3-data-products)
    + [Creating level 3 FITS files](#creating-level-3-fits-files)
      - [Using a GUI](#using-a-gui)
      - [Programmatically](#programmatically)
    + [Reading a level 3 FITS file](#reading-a-level-3-fits-file)
      - [Using a GUI](#using-a-gui-1)
      - [Programmatically](#programmatically-1)
  * [File Management Utilities](#file-management-utilities)
  * [For Developers](#for-developers)


## Setup
In your shell startup configuration file or in IDL_STARTUP, include SPICE in the `SSW_INSTR` environment variable
and define the `SPICE_DATA` environment variable, and to be able to download all SPICE FITS files you need to define
`SPICE_PWD` environment variable:
```
setenv SSW_INSTR "gen so spice"
setenv SPICE_DATA "~/spice_data"
setenv SPICE_PWD "*******"
```
Update SolarSoft to include the SPICE tools:
```
ssw_upgrade, /so, /spice, /iris, /spawn
```
Some of the software from the IRIS branch is used in the SPICE software and is therefore required.
It is not necessary to set it as an instrument, since it will be included implicitly when SPICE is
included in SSW_INSTR.


## Download SPICE FITS files

There are two tools in the repository that download data for you:
```
spice_wget_files [, sub_path=sub_path] [, level=level]
```
SUBPATH = Subdirectory path to mirror.  If not passed, the entire directory tree is mirrored.  Possible formats are:
- '2021/10/20'    Day
- '2021/10'       Month
- '2021'          Year

This first tool is very user-friendly, but it requires the environment variable SPICE_DATA 
to be set. It also requires that 'wget' is installed on the computer.
If the environment variable SPICE_PWD is set, then this tool will allow the download of all
SPICE FITS files, otherwise only publicly available files can be downloaded.

The second tool is more general:
```
RGET_FETCH_FILES, url_to_remote_top_dir, path_to_local_top_dir [, username=username] [, password=password]
[, /debug] [, /verbose]
```
It doesn't require 'wget', but you will still need the username and password, as well as the correct url.
See 'rget_fetch_files.pro' for more details.

Third, the files can also be downloaded manually from this website:
http://astro-sdc-db.uio.no/vol/spice/fits/

Fourth, the publicly available files can also be downloaded manually from the 
Solar Orbiter Archive:
https://soar.esac.esa.int/soar/#home


## Quicklook Software

All quicklook software can be accessed through the GUI SPICE_XFILES. You can call it using this command:
```
spice_xfiles
```
This will open a window, which lets you search your local hard disk for spice files. Selecting a level 2 file
will call SPICE_XCONTROL, which in turn opens a new window. In this window you'll get an overview of the content
of the selected FITS file, and you can access the different quicklook software, which are:

* SPICE_XDETECTOR: This tool displays the windows superimposed on the whole SPICE detector.
* SPICE_RASTER_BROWSER: This routine is used to browse 3D SPICE data-cubes.
* SPICE_XRASTER: This tool displays all exposures of the selected windows in a row.
* SPICE_XWHISKER: This tool is used to display 2-D spectroscopic data as whisker plots (images), i.e. 
Intensity[wavelength, y]. FITS file must contain more than one exposure.
* SPICE_XMAP: This tool is used to display 2-D (or higher) data. FITS file must contain more than one exposure.

Selecting a level 3 file will call SPICE_XCONTROL_L23, which in turn opens a new window. This gives you an overview
of existing level 2 and level 3 data of the selected file.


## SPICE Data Object

We defined a SPICE data object, called SPICE_DATA, which can be initiated with a SPICE FITS file. This object 
contains many methods to handle the data and the header contained in the given FITS file. It can be created 
using one of these commands:
```
spice_object = spice_data(spice_file)            ; short form
spice_object = obj_new('spice_data', spice_file) ; long form
```
Then, to print out all available methods of the object:
```
spice_object->help
```

Getting data from the object:
```
data_one_window = spice_object->get_one_image(0, 0)  ; returns the data for window 0, exposure 0
data_all = spice_object->get_window_data(0)          ; returns the data for window 0, all exposures
```

Opening quicklook software with the object:
```
spice_raster_browser, spice_object   ; opens spice_raster with the data from the object
spice_xwhisker, spice_object, 0      ; opens spice_xwhisker with the data of window 0
ana = spice_object->xcfit_block(0)   ; opens xcfit_block with the data of window 0
```

## Level 3 Data Products

Level 3 data products will be physical quantities derived from integrated emission line intensities. For example, 
temperature, density and FIP bias maps. The first step in generating such maps will be automatic Gaussian fitting of 
the observed emission lines. This will be done with the CFIT software in Solarsoft.

### Creating level 3 FITS files

#### Using a GUI

Level 3 SPICE FITS files can be generated using a GUI, either through SPICE_XFILES or SPICE_XCONTROL.

In SPICE_XFILES you can click on the button 'Open file in XControl_L23', which will open SPICE_XCONTROL_L23. 
This GUI lets you create level 3 data for single windows at a time or for several or all windows of a level 2 
file in one go.

In SPICE_XCONTROL, there's the button 'Create level 3 files', which opens the GUI SPICE_CREATE_L3_WIDGET.
This GUI let's you set the different options before creating the level 3 file.

#### Programmatically

- Single file

A level 3 SPICE FITS file can be generated automatically using the following SPICE_DATA method:
```
spice_object->create_l3_file(window_index)
```
The procedure opens XCFIT_BLOCK with the result. The FIT components and its parameters can be edited/adjusted by 
the user in XCFIT_BLOCK. The level 3 file is then saved into the directory $SPICE_DATA/user/level3/.

- Multiple files

Multiple level 3 files can be created by using the procedure SPICE_CREATE_L3_DRIVER. This tool allows to select
all level 2 files within a certain time window, and create level 3 files for all of them. Additionally, one can
also create overview images from the level 3 data.

- Level 3 images

The level 3 data can be saved as images. These images are created from level 3 FITS files with the tool
SPICE_CREATE_L3_IMAGES. These images can also be created with the above-mentioned SPICE_CREATE_L3_DRIVER.

### Reading a level 3 FITS file

#### Using a GUI

The easiest way to read and view a level 3 SPICE FITS file is by using SPICE_XFILES. You can search for level 3
files and by selecting one, SPICE_XCONTROL_L23 is opened.

#### Programmatically

Alternatively, you can read a file also with the function FITS2ANA which returns an array of analysis structures,
one per window.
```
anas = fits2ana(spice_l3_file, headers_results=headers_results)
XCFIT_BLOCK, ana=anas[0]
```
'Headers_results' will be a pointer array, containing the headers of the results extensions as string arrays.


## File Management Utilities

There are several useful tools to manage SPICE files:
* SPICE_WGET_FILES: This tool mirrors SPICE FITS files from Oslo and cleans up any old files no longer on the server.
* SPICE_INGEST: This tool will move your downloaded SPICE FITS files to the appropriate location in your $SPICE_DATA path.
* SPICE_FIND_FILE: This program is also used by SPICE_XFILES. It is a very flexible tool to search for files on your local hard drive.
* SPICE_FIND_OBSERVATIONS: This routine searches the SPICE catalog to find observations where the specified pointing falls within the SPICE field-of-view at the specified date/time or range.
* SPICE_CAT: This program allows filtering/sorting/selection of the contents in spice_catalog.csv which contains information about all available (not necessarily locally) SPICE FITS files.
* SPICE_GEN_CAT: This program creates files called spice_catalog.txt, spice_catalog.csv and keyword_info.json in the $SPICE_DATA/ directory (but other paths can be specified), with various information on the content of the files found in the directory hierarchy below that path.
* SPICE_FILE2INFO: This routine takes as input a SPICE filename (or list of files) and extracts information from the filename.
* SPICE_REMOVE_OLD_FILES: Remove earlier versions of SPICE fits files.


## For Developers

This repository includes a pre-commit git hook, that updates a specific line of each modified file with the current 
date and time. The line with this format will be edited:
```
; $Id: 2024-02-09 14:12 CET $
```
If the file you modified, does not contain this line yet, please add it, preferably append it to the procedure 
description at the beginning of the file. 

To make git aware of this hook, run this command after cloning the repository:
```
cd path_of_repository
git config --local core.hooksPath .githooks/
chmod +x .githooks/*
```
Git will then run the script _./githooks/pre-commit_ every time you commit something. This script will check 
each modified and staged file whether there is a line with the above format, and if yes, updates date and time 
and adds these changes to the commit.
