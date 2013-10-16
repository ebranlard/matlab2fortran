# DOCUMENTATION : matlab2fortran

## Brief description:

* matlab2fortran(matlab-to-fortran) : converting matlab code to fortran
* Usage: matlab2fortran(filename);
* Author:Emmanuel Branlard
* Creation Date  : December 2012
* Version: VERSIONNUMBER 
* Web-Sites: 
    - http://github.com/elmanuelito/matlab2fortran
    - http://emmanuel.branlard.free.fr/work/programming/
* License: None. Thank you for sharing your improvements to me by email.


## DESCRIPTION

matlab2fortran(matlab-to-fortran) is a code that performs an automatic conversion of one or multiple matlab files to fortran format. The script translate some of the main features of matlab to fortran. matlab2fortran performs a quick and dirty conversion on a line-by-line basis (but still supports conditional loops, subroutine). The script allows multiple matlab command per line if these commands are separated by ";", but not by ",".

matlab2fortran does not intend to produce ready-to-compile code, but performs several necessary conversions. The generated code keeps the structure, variables names and comments of the original code. 


## INSTALLATION AND REQUIREMENTS

The script is located in the release directory of the repository.
The script matlab2fortran (matlab-to-fortran) is a single script file written in matlab. It does not require a particular installation other than maybe adding this script to your current folder or matlab path. 


The script has been tested on linux and windows.
The script has been tested on matlab and octave. 
(Octave generates some warnings. Ouputs are the same expect for some replacement of "...")



## REVISIONS

* 14/12/12 : first release

* 15/12/12 : added declaration of variables at beginning of script or subroutines
         added handling of intent(inout), 
         corrected small bug for do loops and number of ":" since they can be in length(a(:,1)) 

* 18/12/12 :
        - Get the function arguments do the decl_stack
        - Handling of [] for array constructor and string concanenation
        - support for transpose ' 
        - When allocation for zeros, and ones, don't add the line x=x if no operation if present
        - when removing duplicates from the stack, don't loose data
        - Solved Bug for parenthesis in case like: if (a) && b which needs to give ((a) .and. b)
                     
* 16/10/13 : From now on, revisions will only be listed vith git commits



## FEATURES AND TODOs

### Main Features of matlab2fortran:

- conversion of nested structure: do, if, switch (select), while
- conversion of function to subroutine with recognition of output and input arguments, (output arguments are put at the end)
- perform subroutine list of arguments with intent(in), intent(out), intent(inout)
- make a declaration list for simple variable , sort them by Name or Type 
- determines type based on variable name (function fgetVarTypeFromVarName) and some syntax
- does its best to determine array size with things like :, [], zeros
- recognize simple function call and convert them to subroutine calls (syntax [y] = f(x) );
- performs allocation when converting matlab function zeros, ones (linspace possible)
- splitting of lines that contain multiple matlab command separated by ;
- comments
- small support for sprintf and fprintf
- small support for string concatenation
- small support for transpose when written as '
- misc conversions such as logical operators
- few intrinsic functions conversion
- replaces end by size(XXX,XXX)
- Provides Doxygen compatible comments for subroutine and arguments

### TODOs:

- easy: replace also & and | after && and ||
- better printf handling
- better handling of function calls and nested function calls..
- inline ifs?
- isequal


### Features that are not intented to be supported

- Ready-to-compile output and perfection...
- Parsing of line and tree-like syntax detection
- Full type detection
- Type detection based on Matlab workspace output
- Detecting all variables


## RUN INSTRUCTIONS and PARAMETERS

The script should run in few seconds for a file a 1000 lines (say 2s). If it takes longer, activate the Debug flag bDebug=1; to see where the script is stuck...
Several parameters are found at the beginning of the script.


## EXAMPLES

example 1: one file
matlab2fortran('tests/test1.m');

example 2: list of files
matlab2fortran('tests/test1.m','tests/test2.m');

example 3: all files in current directory (does not work with subfolders with dir..)
FileList=dir('*.m');
matlab2fortran(FileList.name);





