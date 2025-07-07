Dynamically Loadable Modules (DLMs), or "plugins" for IDL
*********************************************************

DLMs are dynamically loadable modules that can be used to add native,
compiled functions or procedures to  IDL.

They are written in C and compiled into shared libraries that IDL can
load at runtime. This allows for faster execution of certain functions,
as they can be executed natively rather than interpreted.

These instructions will help you set up the DLMs included in SSW 
(currently, only fmedian() is available).

First, print the values of !version.os and !version.arch.

    IDL> PRINT,!version.os
    IDL> PRINT,!version.arch

Then, in your .tcshrc or .cshrc, set the following environment variable:

MacOS users:

    setenv SSW_COMPILE_TYPE <!version.os>.<!version.arch>

Non-MacOS users:

    setenv SSW_COMPILE_TYPE <!version.os>

Then, set the following environment variables (SSW_BINARY_TYPE should
always contain !version.arch):

    setenv SSW_BINARY_TYPE <!version.os>.<!version.arch>
    setenv SSW_DLM_TOP $SSW/gen/dlm
    setenv IDL_DLM_PATH "<IDL_DEFAULT>:$SSW_DLM_TOP/$SSW_BINARY_TYPE"

NOTE: If you're using tcsh, it will first look for .tcshrc and execute
that. If AND ONLY IF it doesn't find that, it will look for .cshrc and
execute that. So if you have both (and are using tcsh), you should put
the above lines in .tcshrc, not .cshrc.

Now make sure you start a new shell (tcsh or csh) so the new
environment variables are set. You can check this by typing e.g.:

    echo $SSW_COMPILE_TYPE

Now try:

    IDL> help,fmedian(findgen(10,10),3,3)
    % Loaded DLM: FMEDIAN.
    <Expression>    FLOAT     = Array[10, 10]   

If that's the output, you're ok. Congratulations, you're now using
a native version of fmedian() that's about 20 times faster than
the .pro version!

If that's not the output, something's wrong. First, try replacing
"gen" with "site" in the SSW_DLM_TOP variable, i.e. set it to:

    setenv SSW_DLM_TOP $SSW/site/dlm

Then try again. If that works, leave it like that.

If it still doesn't work, you may need to compile the DLMs yourself.
Keep the change in SSW_DLM_TOP.

Change directory to $SSW/gen/dlm, and execute this command:

    make rounds

If that works (there may be warnings, but there should be no errors),
try the test command again (help,fmedian(findgen(10,10),3,3)). If it
works, you're done.

If it doesn't, consult your local IT support. If they can't figure it
out, contact prits-group@astro.uio.no. We may or may not be able to
help you.
