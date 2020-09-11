function SPICE_xfiles_appReadme
;This function initializes some readme-files for the user, which are saved in the hidden .idl folder in the home 
;directory of the user
;It returns the path to that directory, which is used to save the default parameters for SPICE_xfiles
; $Id: 11.09.2020 13:04 CEST $  ;

    AuthorDirname = 'mwiesmann'
    AuthorDesc = 'Martin Wiesmann, Institute of Theoretical Astrophysics, University of Oslo'
    AppDirname = 'spice_xfiles'
    AppDesc = 'SPICE Xfiles'
    AppReadmeText = 'SPICE Xfiles, written by Martin Wiesmann, ITA, University of Oslo (martin.wiesmann@astro.uio.no)' + $
      '/n these files can be deleted without restrictions.'
    AppReadmeVersion=1
    return, APP_USER_DIR(AuthorDirname, AuthorDesc, AppDirname, AppDesc, AppReadmeText, AppReadmeVersion)
end