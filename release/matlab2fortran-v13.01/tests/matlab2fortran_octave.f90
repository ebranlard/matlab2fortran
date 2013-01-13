! DOCUMENTATION : matlab2fortran 
! --------------------------------------------------- 
! Brief description: 
! matlab2fortran(matlab-to-fortran) : converting matlab code to fortran 
! Usage: matlab2fortran(filename); 
! 
! 
! Author:Emmanuel Branlard 
! Creation Date : December 2012 
! Version: 13.01 (year.month) 
! Git-revision: 37ce392 
! 
! Web-Site: http://emmanuel.branlard.free.fr/work/programming/ 
! License: None. Thank you for sharing your improvements to me by email. 
! 
! 
! DESCRIPTION 
! ---------------------------------------------------- 
! matlab2fortran(matlab-to-fortran) is a code that performs an automatic conversion of one or multiple matlab files to fortran format. The script translate some of the main features of matlab to fortran. matlab2fortran performs a quick and dirty conversion on a line-by-line basis (but still supports conditional loops, subroutine). The script allows multiple matlab command per line if these commands are separated by ";", but not by ",". 
! 
! matlab2fortran does not intend to produce ready-to-compile code, but performs several necessary conversions. The generated code keeps the structure, variables names and comments of the original code. 
! 
! 
! INSTALLATION AND REQUIREMENTS 
! ---------------------------------------------------- 
! The script is located in the release directory of the repository. 
! The script matlab2fortran (matlab-to-fortran) is a single script file written in matlab. It does not require a particular installation other than maybe adding this script to your current folder or matlab path. 
! 
! 
! The script has been tested on linux and windows. 
! The script has been tested on matlab and octave. 
! (Octave generates some warnings. Ouputs are the same expect for some replacement of "...") 
! 
! 
! 
! REVISIONS 
! ---------------------------------------------------- 
! 14/12/12 : first release 
! 
! 15/12/12 : added declaration of variables at beginning of script or subroutines 
! added handling of intent(inout), 
! corrected small bug for do loops and number of ":" since they can be in length(a(:,1)) 
! 18/12/12 : 
! - Get the function arguments do the decl_stack 
! - Handling of [] for array constructor and string concanenation 
! - support for transpose ' 
! - When allocation for zeros, and ones, don't add the line x=x if no operation if present 
! - when removing duplicates from the stack, don't loose data 
! - Solved Bug for parenthesis in case like: if (a) && b which needs to give ((a) .and. b) 
! 
! 
! 
! 
! 
! FEATURES AND TODOs 
! ---------------------------------------------------- 
! Main Features of matlab2fortran: 
! - conversion of nested structure: do, if, switch (select), while 
! - conversion of function to subroutine with recognition of output and input arguments, (output arguments are put at the end) 
! - perform subroutine list of arguments with intent(in), intent(out), intent(inout) 
! - make a declaration list for simple variable , sort them by Name or Type 
! - determines type based on variable name (function fgetVarTypeFromVarName) and some syntax 
! - does its best to determine array size with things like :, [], zeros 
! - recognize simple function call and convert them to subroutine calls (syntax [y] = f(x) ); 
! - performs allocation when converting matlab function zeros, ones (linspace possible) 
! - splitting of lines that contain multiple matlab command separated by ; 
! - comments 
! - small support for sprintf and fprintf 
! - small support for string concatenation 
! - small support for transpose when written as ' 
! - misc conversions such as logical operators 
! - few intrinsic functions conversion 
! - replaces end by size(XXX,XXX) 
! - Provides Doxygen compatible comments for subroutine and arguments 
! 
! TODOs: 
! - better printf handling 
! - better handling of function calls and nested function calls.. 
! - inline ifs? 
! - isequal 
! 
! Features that are not intented to be supported 
! - Ready-to-compile output and perfection... 
! - Parsing of line and tree-like syntax detection 
! - Full type detection 
! - Type detection based on Matlab workspace output 
! - Detecting all variables 
! 
! 
! RUN INSTRUCTIONS and PARAMETERS 
! ---------------------------------------------------- 
! The script should run in few seconds for a file a 1000 lines (say 2s). If it takes longer, activate the Debug flag bDebug=1; to see where the script is stuck... 
! Several parameters are found at the beginning of the script. 
! 
! 
! EXAMPLES 
! ---------------------------------------------------- 
! example 1: one file 
! matlab2fortran('tests/test1.m'); 
! 
! example 2: list of files 
! matlab2fortran('tests/test1.m','tests/test2.m'); 
! 
! example 3: all files in current directory (does not work with subfolders with dir..) 
! FileList=dir('*.m'); 
! matlab2fortran(FileList.name); 
! 
! 
! 
! 
! 
!> 
subroutine matlab2fortran( varargin )
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, dimension(:), intent(in) :: varargin  !>  
! Variable declarations 
logical :: bDebug  !>  
logical :: bPipe  !>  
logical :: bSortDeclarationbyNames  !>  
logical :: bSortDeclarationbyTypes  !>  
real*8 :: Declarations  !>  
real*8 :: f  !>  
real*8 :: fidf  !>  
real*8 :: fidftmp  !>  
real*8 :: fidm  !>  
real*8 :: file  !>  
real*8 :: filef  !>  
real*8 :: fileftmp  !>  
! 

!! Parameters 
bDebug=0 ! show input in stdout
bPipe=0 ! pipe output to stdout as well as in the file
bSortDeclarationbyNames=1 !
bSortDeclarationbyTypes=0 !

!! 
if (nargin==0) then 
error('Empty file list')
end if 

!! dealing with multiple input files "recursively".. 
if (size(varargin)>1) then 
do ifile=1,size(varargin) 
f=varargin{ifile}
matlab2fortran(f)
end do 
else
!! One file call 
file=varargin{1}
fidm=fopen(file,'r')
if (fidm==-1) then 
error(['Unable to open input file: ' file])
end if 


filef=regexprep(file,'\.m$','.f90')
fileftmp=regexprep(file,'\.m$','_tmp.f90')
!m2f: fprintf('Converting to file:  write(unitnumber,*) ' ' 
! warning off 
if (.not.(bDebug)) then 
fidftmp=fopen(fileftmp,'w')
if (fidftmp==-1) then 
error(['Unable to open temporary output file: ' fileftmp])
end if 
fidf=fopen(filef,'w')
if (fidf==-1) then 
error(['Unable to open output file: ' filef])
end if 
else
fidftmp=1 ! standard output
fidf=1 ! standard output
end if 
!! Conversion line by line and ouput to file 
if (.not.(bDebug)) then 
warning off !WATCH OUT
end if 
call fconvertlinebyline( fidm,fidftmp,bDebug,bPipe , Declarations) 
warning on
fclose(fidm)
if (.not.(bDebug)) then 
fclose(fidftmp)
end if 


!! Sorting Declarations for each subroutine and main program 
Declarations=fSortDeclarations(Declarations,bSortDeclarationbyNames,bSortDeclarationbyTypes,bDebug)


!! Reading temp file, outputting to final file with declarations , only possible without Debug 
if (.not.(bDebug)) then 
fidftmp=fopen(fileftmp,'r')
if (fidftmp==-1) then 
error(['Unable to re open temporary output file: ' fileftmp])
end if 
fWriteDeclarationsAtCorrectLocation(fidftmp,fidf,Declarations)
fclose(fidftmp)
delete(fileftmp)
fclose(fidf)
end if ! end output of declaration if not debug mode
end if ! end switch between one or two files

end subroutine matlab2fortran !function



!> 
subroutine fconvertlinebyline(fidm, fidf, bDebug, bPipe, Declarations)
implicit none 
! Use somemodule 
! Arguments declarations 
logical, intent(in) :: bDebug  !>  
logical, intent(in) :: bPipe  !>  
matlabcell, intent(out) :: Declarations  !> !m2f: check dim(:) 
real*8, intent(in) :: fidf  !>  
real*8, intent(in) :: fidm  !>  
! Variable declarations 
real*8 :: current_unit  !>  
real*8, dimension(1) :: decl_stack  !>  
real*8 :: decl_stack_new  !>  
real*8, dimension(1) :: end_stack  !>  
real*8 :: pref_in  !>  
real*8 :: pref_out  !>  
character(len=*) :: s  !>  
character(len=*) :: sf  !>  
character(len=*) :: sLine  !> !m2f: check dim(:)!m 
character(len=*) :: sout  !>  
real*8 :: unit_switch  !>  
! 
sLine = fgets(fidm)
end_stack=[ ] 
current_unit=1
Declarations{current_unit}=[ ] ! contains all variables declaration
decl_stack=[ ] ! contains variable declaration for current subroutine or main..
pref_in=''
pref_out=''
if (bDebug) then 
pref_in='In :'
pref_out='Out:'
end if 
do while (sLine /= -1) 
! remove useless spaces 
sLine=trim(sLine)
sLine=fremovespaces(sLine)

if (bDebug) then 
!m2f: fprintf(' write(unitnumber,*) ' ' 
end if 

! Simple case: emptyline 
if (isempty(sLine)) then 
sf=sLine // '\n'
!m2f: fprintf(fidf,[pref_out,sf]);
 write(unitnumber,*) ' ' 
!m2f: if bPipe ; fprintf(1,[pref_out,sf]); end
 write(unitnumber,*) ' ' 
! Simple case: start with a comment, still needs small handling 
else if (sLine(1)==') then !'
! kbd 
sLine(1)='!'
sLine=strrep(sLine,'!','!')
! sLine=strrep(sLine,'!','!'); 
sLine=strrep(sLine,'\n','NewLine')
sLine=strrep(sLine,'\','\\')
sf=sLine // ' \n'
! sf=strrep(sf,'!','!!'); 
!m2f: fprintf(fidf,[pref_out,sf]);
 write(unitnumber,*) ' ' 
!m2f: if bPipe ; fprintf(1,[pref_out,sf]); end
 write(unitnumber,*) ' ' 
else
! the problematic printf case is dealt then 
if (.not.(isempty(regexp(sLine,'[fs]+printf\(')))) then 
call freplaceprintf( sLine , sf) 
!m2f: fprintf(fidf,[pref_out,sf]);
 write(unitnumber,*) ' ' 
!m2f: if bPipe ; fprintf(1,[pref_out,sf]); end
 write(unitnumber,*) ' ' 
else
! comments replacement 
sLine=strrep(sLine,'!','!')
! splitting matlab lines if commands on the same line 
call fsplitmatlablines( sLine,[] , sout) 
do isplit=1,size(sout) 
s=sout{isplit}
! --------------------------------- 
! The Main function 
! --------------------------------- 
call fconvertline( s,end_stack,decl_stack , sf,end_stack,decl_stack,unit_switch,decl_stack_new) 
if (unit_switch/=0) then 
! storing the stack of declaration of previous unit 
if (bDebug) then 
disp('Entering new unit')
end if 
Declarations{current_unit}=decl_stack
current_unit=current_unit+unit_switch
if (unit_switch==1) then 
Declarations{current_unit}=[ ] 
decl_stack=decl_stack_new
end if 
end if 
! --------------------------------- 
sf=fremovespaces(sf)
!m2f: fprintf(fidf,[pref_out,sf]);
 write(unitnumber,*) ' ' 
!m2f: if bPipe ; fprintf(1,[pref_out,sf]); end
 write(unitnumber,*) ' ' 
end do 
end if 
end if 
sLine = fgets(fidm)
end do 

Declarations{current_unit}=decl_stack

! if at the end we still have some end, output them 
do ie=1,size(end_stack) 
!m2f: fprintf(fidf,'end  write(unitnumber,*) ' ' 
end do 

end subroutine fconvertlinebyline !function


!> 
subroutine fconvertline(s, end_stack, decl_stack, sf, unit_switch, decl_stack_new)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: decl_stack  !>  
real*8, dimension(1), intent(out) :: decl_stack_new  !>  
real*8, intent(inout) :: end_stack  !>  
character(len=*), intent(in) :: s  !> !m2f: check dim(:)!m 
character(len=*), intent(out) :: sf  !>  
real*8, intent(out) :: unit_switch  !>  
! Variable declarations 
integer, dimension(:) :: I  !> !m2f: check dim(:)!m 
character(len=*) :: sold  !>  
! 
unit_switch=0
decl_stack_new=[ ] 
! function replace 
I=strfind(s,'function')
if (.not.(isempty(I) .and. I(1)==1)) then 
call freplacefunction( s , end_stack , sf,end_stack,decl_stack_new) 
unit_switch=1 !we are entering a new unit
return
end if 

!! Simple regexprep or rep 
! logical 
s=regexprep(s,'/=','/=')
s=regexprep(s,'.not.((.*)','.not.($1)'))
s=strrep(s,'.and.','.and.')
s=strrep(s,'.or.','.or.')
s=strrep(s,'exit','exit')
s=strrep(s,'.true.','..true..')
s=strrep(s,'.false.','..false..')

! intrisic 
s=strrep(s,'trim(','trim(')
s=strrep(s,'norm2(','norm2(')
s=strrep(s,'ceiling(','ceiling(')
s=strrep(s,'anint(','anint(') ! or just int?
s=strrep(s,'floor(','floor(')
s=strrep(s,'modulo(','modulo(') !mod exists but return negative values
! array stuff 
s=regexprep(s,'size\(([ ** ] *)\)','shape($1)')!replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'size(','shape(') ! or count maybe..

! math 
s=strrep(s,'dot_product','dot_product_product') !replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'**','**')

! strings 
! s=strrep(s,'&','\\'); 
s=strrep(s,'\','\\')

!! Things that are easily recognizable by their first position on the line 
! case 
s=regexprep(s,'**case([ **!, ] *)','case ($1) !')
s=regexprep(s,'**otherwise*','case default ')
sold=s
s=regexprep(sold,'**switch([ **!, ] *)','select case ($1) !')
if (size(s)/=size(sold)) then ! we did replace something
! stacking 
end_stack=fstack_push(end_stack,'select')
end if 

! for replace 
I=strfind(s,'for')
if (.not.(isempty(I) .and. I(1)==1)) then 
call freplacefor( s , end_stack , sf,end_stack) 
return
end if 
! while replace 
I=strfind(s,'while')
if (.not.(isempty(I) .and. I(1)==1)) then 
call freplacewhile( s , end_stack , sf,end_stack) 
return
end if 

! if replace 
I=regexp(s,'elseif|else if|if\ |if\(') ! watch out for the order here
if (.not.(isempty(I) .and. min(I)==1)) then 
call freplaceif( s , end_stack , sf,end_stack) 
return
end if 

! end replace 
I=strfind(s,'size(XXX,XXX)')
if (.not.(isempty(I))) then 
if (I(1)==1) then 
!let's check that this is not a variable 
if (size(s)==3 .or. s(4)==' ' .or. s(4)==') then ;' .or.s(4)=='!'
call freplaceend( s , end_stack , sf,end_stack) 
return
end if 
else
! Dealing with end in the middle 
s=regexprep(s,'([ **a-zA-Z0-9_ ] +)size(XXX,XXX)([**a-zA-Z0-9_]+)','$1size(XXX,XXX)$2')
end if 
end if 

! function calls 
I=strfind(s,'[')
if (.not.(isempty(I) .and. I(1)==1)) then 
call freplacefunctioncall( s, decl_stack , sf,decl_stack) 

return
end if 

! multiline 
! s=strrep(s,'...','\&'); !char(8) not enough.. !HACK : the escape sequence is not recognize and hence it merges with the next line... 
s=regexprep(s,'(?<!!.*)\.\.\.','&') ! this line requires some explanation below
! If uses a negative lookbehind "(?<!)" It looks if there is no !.* before (ie, no comment) 
! If this is not a commen then we replace the ... by &. This regexp will not work with octave, but it doesnt matter since it's a comment. 
! Octabe will replace by & all the time 
! If one want, & can be replace by \& so that matlab will not recognize this bad escpae character and hence will merge the current line with the next.. but that's a HACK 


!! More tricky things that should require syntax parsing and recursion but that we'll not do!.. 
! zeros and ones 
! I=regexp(s,'(zeros\(|ones\(|linspace\()'); 
I=regexp(s,'(zeros\(|ones\()')
if (.not.(isempty(I))) then 
call freplacezeros( s ,decl_stack , s,decl_stack) 
else
! stacking assignements so that they can be used for declarations 
call fassignement( s ,decl_stack , s,decl_stack) ! calls freplacesbracket
end if 


!! brackets 
! I=regexp(s,'(zeros\(|ones\(|linspace\()'); 
! if ~isempty(I) && mod(length(I),2)==0 
! else 
! TODO with better string handling 
! s=strrep(s,'[','(/'); 
! s=strrep(s,']','/)'); 


! default return 
sf=s // '\n'
end subroutine fconvertline !function






!> 
subroutine fWriteDeclarationsAtCorrectLocation(fidftmp, fidf, Declarations)
implicit none 
! Use somemodule 
! Arguments declarations 
matlabcell, dimension(1), intent(in) :: Declarations  !>  
real*8, intent(in) :: fidf  !>  
real*8, intent(in) :: fidftmp  !>  
! Variable declarations 
real*8 :: current_unit  !>  
integer, dimension(:) :: I  !> !m2f: check dim(:)!m 
character(len=*) :: sread  !>  
! 
! kbd 
current_unit=1
sread = fgets(fidftmp)
do while (sread /= -1) 
if (current_unit==1) then 
! wait for the first non comment empty to output the declaration 
if (.not.(isempty(Declarations{1,1}))) then 
if (.not.(isempty(sread) .and. sread(1)/=') then !')
!m2f: fprintf(fidf,'! Variable declarations \n');
 write(unitnumber,*) ' ' 
fwrite_Declarations(fidf,Declarations{current_unit,1})
!m2f: fprintf(fidf,'! \n');
 write(unitnumber,*) ' ' 
Declarations{1}=[ ] 
end if 
end if 
end if 
I=strfind(sread,'subroutine')
if (.not.(isempty(I) .and. I(1)==1)) then 
! if current_unit==1 && ~isempty(Declarations{current_unit}) 
! ! case of first unit, i.e. main program 
! ! ! we need to output the declaration here 
! fwrite_Declarations(fidf,Declarations{current_unit}); 
! end 
current_unit=current_unit+1 !
end if 
I=strfind(sread,'!M2F-HERE-XXX')
if (.not.(isempty(I) .and. I(1)==1)) then 
if (.not.(isempty(Declarations{current_unit,2}))) then 
!m2f: fprintf(fidf,'! Arguments declarations \n');
 write(unitnumber,*) ' ' 
fwrite_Declarations(fidf,Declarations{current_unit,2})
end if 
if (.not.(isempty(Declarations{current_unit,1}))) then 
!m2f: fprintf(fidf,'! Variable declarations \n');
 write(unitnumber,*) ' ' 
fwrite_Declarations(fidf,Declarations{current_unit,1})
end if 
sread=''
end if 
sread=strrep(sread,'\','\\') ! required
!m2f: fprintf(fidf,sread);
 write(unitnumber,*) ' ' 
sread = fgets(fidftmp)
end do !while reading file tmp
end subroutine fWriteDeclarationsAtCorrectLocation !function fWriteDeclarationsAtCorrectLocation


!> 
subroutine fSortDeclarations(Declarations, bSortDeclarationbyNames, bSortDeclarationbyTypes, bDebug, DeclarationsOut)
implicit none 
! Use somemodule 
! Arguments declarations 
logical, intent(in) :: bDebug  !>  
logical, intent(in) :: bSortDeclarationbyNames  !>  
logical, intent(in) :: bSortDeclarationbyTypes  !>  
real*8, intent(in) :: Declarations  !>  
matlabcell, intent(out) :: DeclarationsOut  !>  
! Variable declarations 
real*8 :: .not.(  !>  
real*8 :: D  !>  
matlabcell :: Decl  !>  
real*8, dimension(1) :: DeclArguments  !>  
integer :: iD  !>  
integer, dimension(:) :: Iintent  !> !m2f: check dim(:) 
integer, dimension(:) :: Isort  !> !m2f: check dim(:)!m 
integer, dimension(:) :: Isort2  !>  
integer, dimension(:) :: IsortA  !> !m2f: check dim(:)!m 
real*8 :: Pintent  !>  
real*8 :: VarNames  !>  
real*8 :: VarNamesA  !>  
real*8 :: VarProp  !>  
real*8 :: VarTypes  !>  
real*8 :: VarTypesA  !>  
! 
do iud=1,size(Declarations) 
if (bDebug) then 
!m2f: fprintf(['-------------Declaration for unit ' num2str(iud) '\n']);
 write(unitnumber,*) ' ' 
end if 
! fprintf(2,Declarations{iud}{id}); 
Decl=Declarations{iud}
DeclArguments=[ ] 
if (size(Decl)>0) then 
VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',.false.)
! sort by name first for convenience 
call sort( lower(VarNames) , .not.(,Isort) ! NOTE: the lower here is a matter of choice)
Decl=Decl(Isort)

!! Removing duplicates declaration entry (now by merging) 
iD=1
do while (iD+1<=size(Decl)) 
if (.not.(isequal(Decl{iD}.name,Decl{iD+1}.name))) then 
! we just increment 
iD=iD+1
else
! we merge 
D=fmergeDeclarations(Decl{iD},Decl{iD+1})
Decl{iD}=D
Decl=fstack_pop_element(Decl,iD+1)
end if 
end do 
!! Check for same variable different case 
call unique( VarNames , .not.(,Isort) ! NOTE: case ins-sensitive so that the user sees the possible clash since fortran is case insensitive)
call unique( lower(VarNames) , .not.(,Isort2) )
if (size(Isort)/=size(Isort2)) then 
warning('Be careful, there are variables that have the same characters but different cases.')
end if 

!! Dealing with intent first 
VarProp = cellfun(@(x)x.prop, Decl, 'UniformOutput',.false.)
Pintent=strfind(VarProp,'intent')
Iintent = cellfun(@(x).not.(isempty(x), Pintent, 'UniformOutput',.true.))
DeclArguments=Decl(Iintent)
Decl=Decl(.not.(logical(Iintent)))
! Decl=Decl(Isort); 
VarNamesA = cellfun(@(x)x.name, DeclArguments, 'UniformOutput',.false.)
VarTypesA = cellfun(@(x)x.name, DeclArguments, 'UniformOutput',.false.)
VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',.false.)
VarTypes = cellfun(@(x)x.name, Decl, 'UniformOutput',.false.)

! sorting 
if (bSortDeclarationbyNames) then 
call sort( lower(VarNamesA) , .not.(,IsortA) ! NOTE: the lower here is a matter of choice)
call sort( lower(VarNames) , .not.(,Isort) ! NOTE: the lower here is a matter of choice)
else if (bSortDeclarationbyTypes) then 
call sort( lower(VarTypesA) , .not.(,IsortA) )
call sort( lower(VarTypes) , .not.(,Isort) )
else
IsortA=1:size(VarNamesA)
Isort=1:size(VarNames)
end if 
DeclArguments=DeclArguments(IsortA)
Decl=Decl(Isort)

! Printing 
if (bDebug) then 
fwrite_Declarations(1,Decl)
end if 
end if 
DeclarationsOut{iud,1}=Decl
DeclarationsOut{iud,2}=DeclArguments
end do 
end subroutine fSortDeclarations !function format Declarations


!> 
subroutine fwrite_Declarations(fidout, Decl)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(in) :: Decl  !>  
real*8, intent(in) :: fidout  !>  
! Variable declarations 
character(len=*) :: sf  !>  
! 
do id=1,size(Decl) 
call fgetDeclaration( Decl{id} , sf) 
!m2f: fprintf(fidout,sf);
 write(unitnumber,*) ' ' 
end do 
end subroutine fwrite_Declarations ! function

!> 
subroutine fassignement( s , decl_stack )
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: decl_stack  !>  
character(len=*), dimension(:), intent(inout) :: s  !>  
! Variable declarations 
real*8 :: .not.(  !>  
logical :: bFirstDimMayBeDetermined  !>  
logical :: bSecondDimMayBeDetermined  !>  
logical :: bwithin  !>  
integer, dimension(:) :: Ic  !> !m2f: check dim(:) 
integer, dimension(:) :: Icurl  !> !m2f: check dim(:) 
integer, dimension(:) :: Ieq  !> !m2f: check dim(:) 
integer, dimension(:) :: Ipar  !> !m2f: check dim(:) 
integer :: is  !>  
integer, dimension(1) :: Isimple  !> !m2f: check dim(:) 
integer :: ncol  !>  
integer :: ncommas  !>  
integer :: nlines  !>  
character(len=*) :: sCOM  !>  
character(len=*) :: sLHS  !>  
character(len=*) :: sRHS  !>  
character(len=*), dimension(1) :: sRHSb  !>  
real*8, dimension(:) :: v  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: v.comment  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.name  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: v.prop  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.shape  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.type  !> !m2f: check dim(:) 
real*8, dimension(:) :: vb.comment  !> !m2f: check dim(:) 
real*8, dimension(:) :: vb.name  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: vb.prop  !> !m2f: check dim(:)!m 
character(len=*), dimension(:) :: vb.shape  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: vb.type  !> !m2f: check dim(:)!m 
! 

! extract the LHS 
Ieq=strfind(s,'=')
if (isempty(Ieq)) then 
return
end if 
sLHS=s(1:Ieq(1)) ! !!! includes the equal for now!
! extract the RHS and comment 
Ic=strfind(s,'!')
if (.not.(isempty(Ic)) then ! could be smarter)
bwithin=0
is=Ieq(1)+1
do while (is<=size(s)) 
if (s(is)=='''') then 
if (.not.(bwithin)) then 
bwithin=1
else
bwithin=0
end if 
end if 
if (s(is)==') then !' .and. .not.(bwithin)
exit
end if 
is=is+1
end do 
sRHS=s((Ieq(1)+1):(is-1))
sCOM=s(is:size(XXX,XXX))
else
sRHS=s((Ieq(1)+1):size(XXX,XXX))
sCOM=''
end if 
! if ~isempty(Ic) ! could be smarter 
! ! make sure the comment is not surrounded by apostroph 
! [b p1 p2]=fissurroundedby(s,Ic(1),'''',''''); 
! if b 
! 
! else 
! end 

! Initialization of variable properties 
v.comment=''
v.type=''
v.shape=''
v.name='TODO'
v.prop=''

vb.comment=''
vb.type=''
vb.shape=''
vb.name='TODO'
vb.prop=''

! hack for matrices assignemnt: in the form A(1,:)=[a a] , then the shape can be determined in one dimension 
bFirstDimMayBeDetermined=0
bSecondDimMayBeDetermined=0


!! Finding variable name and properties if possible in LHS 
! test simple case pure variable 
! only simple assignments are allowed x1_var =.. , I.element = 
! It is chosen to keep the dot in the variable name.. 
Isimple=regexp(sLHS,'[ a-z.A-Z0-9_ ] +=')
if (.not.(isempty(Isimple) .and. Isimple(1)==1)) then 
! that's the simple case 
vb.name=trim(sLHS(1:(size(XXX,XXX)-1)))
call fgetVarTypeFromName( vb.name , vb.type,vb.shape,vb.prop) 
else
! if an assignment is of the form a(:,1) we dont cons 
! is there is some : and (, try to find the dimension 
Ipar=strfind(sLHS,'(')
Icurl=strfind(sLHS,'{')
if (.not.(isempty(Icurl)) then ! has to be first)
vb.name=trim(sLHS(1:(Icurl(1)-1)))
vb.type='matlabcell'
else if (.not.(isempty(Ipar))) then 
vb.name=trim(sLHS(1:(Ipar(1)-1)))
call fgetVarTypeFromName( vb.name , vb.type,.not.(,vb.prop) )
ncommas=size(strfind(sLHS,','))+1
vb.shape=':'
do is=2,ncommas 
vb.shape=vb.shape,// ',:'
end do 
! let's a simple hack for matrices 
bFirstDimMayBeDetermined=.not.(isempty(strfind(fremovesinglespace(sLHS),':,')))
bSecondDimMayBeDetermined=.not.(isempty(strfind(fremovesinglespace(sLHS),',:')))
else
!TODO 
end if 
end if 

!! merging info 
v.name=vb.name
v=fmergeDeclarations(v,vb)


!! Dealing with squared brackets in RHS 
if (.not.(isempty(strfind(sRHS,'[')))) then 
call freplacesbracket( sRHS , sRHS,vb.type,ncol,nlines) 
if (min(ncol,nlines)==1) then 
!m2f: vb.shape=sprintf(' write(s,*) ' '  
! hack 
if (bFirstDimMayBeDetermined) then 
!m2f: vb.shape=sprintf(' write(s,*) ' '  
else if (bSecondDimMayBeDetermined) then 
!m2f: vb.shape=sprintf(':, write(s,*) ' '  
end if 
else if (ncol*nlines/=0) then 
! TODO more than matrices... 
!m2f: vb.shape=sprintf(' write(s,*) ' '  
end if 
! TODO possibility to handle some types here 
! elseif ~isempty(regexp(sLHS,'[0-9]+')) 
! v.type= 
end if 
v.name=vb.name
v=fmergeDeclarations(v,vb)



!! Dealing with transpose as apostroph 
if (modulo(size(strfind(sRHS,'''')),2)==1) then ! clearly this will fail if there is an even number of transpose on the line..
s='!m2f: ' // sLHS // sRHS // sCOM // '\n'
sRHSb=regexprep(sRHS,'([ a-zA-Z\(\)\_0-9: ] *)''','transpose($1)')
s=[ s,sLHS,sRHSb,sCOM ] 
else
! default 
s=[ sLHS,sRHS,sCOM ] 
end if 
decl_stack=fstack_push(decl_stack,v)

! if isequal(v.name,'TODO') 
! disp(s); 
! end 

end subroutine fassignement !function

!> 
subroutine ffindmatching(s, pstart, co, cc, bforward, b, po, pc)
implicit none 
! Use somemodule 
! Arguments declarations 
logical, intent(out) :: b  !>  
logical, intent(in) :: bforward  !>  
real*8, intent(in) :: cc  !>  
real*8, intent(in) :: co  !>  
real*8, intent(out) :: pc  !>  
real*8, intent(out) :: po  !>  
real*8, intent(in) :: pstart  !>  
character(len=*), intent(in) :: s  !>  
! Variable declarations 
real*8 :: found  !>  
integer :: i  !>  
integer, dimension(:) :: Iopen  !> !m2f: check dim(:)!m 
integer :: nopen  !>  
! 
! string, start position, character open, character close 
! 
! [b po pc]=ffindmatching('1(3(5)7()df) d',2,'(',')',1 ) 
! 
b=0
po=0
pc=0


if (.not.(bforward)) then 
error('not done')
end if 
found=.false.
nopen=0
Iopen=[ ] 
i=pstart
do while (.not.(found .and. i<=size(s))) 
if (s(i)==co) then 
nopen=nopen+1
Iopen=[ Iopen,i ] 
end if 
if (s(i)==cc) then 
if (nopen>=1) then !NOTE: this is a choice, if a closing character is found, before an opened one, we don't care..
nopen=nopen-1
if (nopen==0) then 
found=.true.
pc=i
po=Iopen(1)
exit
end if 
end if 
end if 
i=i+1
end do 

b=found

end subroutine ffindmatching 
!> 
subroutine fgetDeclaration( v , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
character(len=*), intent(out) :: sf  !>  
real*8, dimension(:), intent(in) :: v  !>  
! Variable declarations 
character(len=*), dimension(:) :: varcomment  !> !m2f: check dim(:) 
real*8, dimension(:) :: varname  !> !m2f: check dim(:)!m 
real*8, dimension(1) :: varprop  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: varshape  !> !m2f: check dim(:)!m 
character(len=*), dimension(:) :: vartype  !> !m2f: check dim(:)!m 
! 
if (isempty(v.type)) then 
vartype='real*8'
else
vartype=v.type
end if 
if (.not.(isempty(v.shape))) then 
varshape='dimension(' // v.shape // ')'
if (.not.(isempty(v.prop))) then 
varshape=varshape // ', '
end if 
else
varshape=''
end if 
if (.not.(isempty(v.prop))) then 
varprop=[ v.prop ] 
else
varprop=''
end if 
if (.not.(isempty(v.name))) then 
varname=v.name
else
varname='TODO'
end if 
if (.not.(isempty(varprop).or. ~isempty(varshape))) then 
vartype=vartype // ', '
end if 

! varcomment=[v.comment ' !> ']; 
varcomment=' !> ' // v.comment(1:min(20,// size(v.comment)))


!m2f: sf=sprintf(' write(s,*) ' '  

end subroutine fgetDeclaration 

!> 
subroutine fgetVarTypeFromName( varname , vartype, varshape, varprop)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, dimension(:), intent(in) :: varname  !>  
real*8, dimension(:), intent(out) :: varprop  !> !m2f: check dim(:)!m 
real*8, dimension(:), intent(out) :: varshape  !> !m2f: check dim(:)!m 
real*8, dimension(:), intent(out) :: vartype  !> !m2f: check dim(:)!m 
! 
! default type 
vartype=''
varshape=''
varprop=''
! attempt a type conversion based on first character, this is my own convention, comment if needed 
if (varname(1)=='i' .or. varname(1)=='n') then 
vartype='integer'
else if (varname(1)=='I') then 
vartype='integer'
varshape=':'
else if (varname(1)=='b') then 
vartype='logical'
else if (varname(1)=='s') then 
vartype='character(len=*)'
else if (size(varname)>=3 & isequal(varname(1:3),'cpt')) then 
vartype='integer'
else if (varname(1)=='v') then 
vartype='real*8'
varshape=':'
else
vartype='' ! this is postponed to the writting
end if 

end subroutine fgetVarTypeFromName 

!> 
subroutine fissurroundedby( s, p, c1, c2 , b, p1, p2)
implicit none 
! Use somemodule 
! Arguments declarations 
logical, intent(out) :: b  !>  
real*8, intent(in) :: c1  !>  
real*8, intent(in) :: c2  !>  
real*8, intent(in) :: p  !>  
real*8, intent(out) :: p1  !>  
real*8, intent(out) :: p2  !>  
character(len=*), intent(in) :: s  !>  
! Variable declarations 
integer :: i  !>  
integer :: notfound  !>  
! 
! looks if character s(p) is surrounded by the characters c1 and c2 
! returns a boolean , and the postion of these charactesrs 
! [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',10,'[',']') 
! [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',11,'[',']') 
! [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',10,'[',']') 
! [b p1 p2]=fissurroundedby('function [''a b c'']=issur([,])',10,'''','''') 
! [b p1 p2]=fissurroundedby('function [''a b c'']=issur([,])',12,'''','''') 
! [b p1 p2]=fissurroundedby('(),()',4,'(',')') 
p1=0
p2=0
!! stupid whiles 
! look backward 
notfound=1
i=p-1
do while (i>=1 .and. notfound) 
if (c1/=c2) then 
! then if c2 is encountered we should break 
if (s(i)==c2) then 
exit
end if 
end if 
if (s(i)==c1) then 
notfound=0
p1=i
end if 
i=i-1
end do 

! look forward 
notfound=1
i=p+1
do while (i<=size(s) .and. notfound) 
if (c1/=c2) then 
! then if c1 is encountered we should break 
if (s(i)==c1) then 
exit
end if 
end if 
if (s(i)==c2) then 
notfound=0
p2=i
end if 
i=i+1
end do 


if (p1==0 .or. p2==0) then 
b=0 ! not surrounded
else
b=1
end if 
end subroutine fissurroundedby 
!> 
subroutine fmergeDeclarations( v1, v2 , decl)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(out) :: decl  !>  
real*8, dimension(:), intent(in) :: v1  !>  
real*8, dimension(:), intent(in) :: v2  !>  
! Variable declarations 
real*8 :: .not.(  !>  
logical, dimension(1) :: b1  !>  
logical, dimension(1) :: b2  !>  
real*8 :: c  !>  
real*8, dimension(2) :: decl.comment  !>  
real*8 :: decl.name  !>  
real*8 :: decl.prop  !>  
real*8 :: decl.shape  !>  
real*8 :: decl.type  !>  
integer, dimension(:) :: I  !> !m2f: check dim(:) 
integer :: n1  !>  
integer :: n2  !>  
character(len=*) :: s_prop  !>  
character(len=*) :: s_shape  !>  
real*8, dimension(:) :: v1.name  !> !m2f: check dim(:) 
real*8, dimension(:) :: v2.name  !> !m2f: check dim(:) 
real*8, dimension(:) :: vartype  !> !m2f: check dim(:) 
real*8 :: ~  !>  
! 
v1.name=trim(v1.name)
v2.name=trim(v2.name)
if (.not.(isequal(v1.name ,v2.name))) then 
error('Can''t merge if name different')
end if 
decl.name=v1.name




!! comment 
decl.comment=[ v1.comment,v2.comment ] 

!! dimension 
b1=isempty(regexp(v1.shape,'[ 0-9 ] +'))
b2=isempty(regexp(v2.shape,'[ 0-9 ] +'))
! give priority to numbers 
if (.not.(b1 .and. ~b2)) then 
n1=size(strfind(v1.shape,':'))
n2=size(strfind(v2.shape,':'))
s_shape=':'
do is=2,max(n1,n2) 
s_shape=s_shape,// ',:'
end do 
else if (b1) then 
s_shape=v2.shape
if (.not.(isempty(v1.shape))) then 
decl.comment=decl.comment // '!m2f: check dim(' // v1.shape // ')'
end if 
else
s_shape=v1.shape
if (.not.(isempty(v2.shape))) then 
decl.comment=decl.comment // '!m2f: check dim(' // v2.shape // ')'
end if 
end if 
decl.shape=s_shape

!! prop 
if (isempty(v1.prop)) then 
decl.prop=v2.prop
else if (isempty(v2.prop)) then 
decl.prop=v1.prop
else
! none of them are empty; 
s_prop=v1.prop,// ',' // v2.prop
call regexp( s_prop,',' , .not.(,~,~,~,~,~,c) )
c=trim(c)
c=sort(c)
! cannot be allocatable and intent 
if (.not.(isempty(strfind(s_prop,'allocatable')) .and. ~isempty(s_prop,'intent'))) then 
! remove allocatable prop 
I=cellfun(@(x).not.(isequal(x,'allocatable'),c))
c=c(I)
end if 
s_shape=c{1}
do ic=2,size(c) 
s_prop=', ' // c{ic}
end do 
end if 
!! type. NOTE: a more advanced merging could be thought 
if (.not.(isempty(strfind([v1.type v2.type],'matlabcell')))) then 
decl.type='matlabcell'
else
vartype = fgetVarTypeFromName( v1.name )
if (isempty(v1.type)) then 
decl.type=v2.type
else if (isempty(v2.type)) then 
decl.type=v1.type
else if (isequal(v1.type, vartype)) then 
decl.type=v2.type
else if (isequal(v2.type, vartype)) then 
decl.type=v1.type
else
decl.type=''; ! don't add information you don't have
end if 
end if 



end subroutine fmergeDeclarations 

!> 
subroutine fremovesinglespace( s )
implicit none 
! Use somemodule 
! Arguments declarations 
character(len=*), dimension(1), intent(inout) :: s  !>  
! 
! remove multiple spaces 
s=regexprep(s,'[ \ ] +','')

end subroutine fremovesinglespace !function

!> 
subroutine fremovespaces( s )
implicit none 
! Use somemodule 
! Arguments declarations 
character(len=*), dimension(1), intent(inout) :: s  !>  
! 
! remove multiple spaces 
s=regexprep(s,'[ \ ] +',' ')

end subroutine fremovespaces 

!> 
subroutine freplaceend( s, end_stack , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
matlabcell, intent(inout) :: end_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
real*8 :: elmt  !>  
! 
! freplaceend('end !(ksdjf)',[]) 
! freplaceend('end ; ! why not?',[]) 

if (isempty(end_stack)) then 
warning('Too much ends in the file.. Some stuff have not been accounted for?')
end_stack{1}=''
end if 

call fstack_pop( end_stack , end_stack,elmt) 
sf='size(XXX,XXX) ' // elmt // ' ' // s(4:size(XXX,// XXX)) // '\n'! could add a security comment..


end subroutine freplaceend !function

!> 
subroutine freplacefor( s, end_stack , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: end_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), dimension(3), intent(out) :: sf  !>  
! Variable declarations 
real*8 :: .not.(  !>  
integer, dimension(:) :: Icol  !> !m2f: check dim(:) 
integer, dimension(:) :: Ieq  !> !m2f: check dim(:) 
real*8 :: loopvar  !>  
integer :: nc  !>  
integer :: ncol  !>  
integer :: no  !>  
character(len=*) :: sloopextent  !>  
character(len=*) :: split  !>  
matlabcell, dimension(1) :: splitb  !>  
character(len=*) :: ssplit  !>  
real*8 :: ~  !>  
! 

! freplacefor('for i=I',[]) 
! freplacefor('for i=1:0.2:10 ; ! why not?',[]) 
! freplacefor('for iarg=1:length(c_all_args(:,1)',[]) 

Icol=strfind(s,':')
Ieq=strfind(s,'=')
ncol=size(Icol)
loopvar=trim(s(4:(Ieq(1)-1)))
sf='do ' // loopvar // '='
sloopextent=s((Ieq(1)+1):size(XXX,XXX))
call regexp( sloopextent,':' , .not.(,~,~,~,~,~,split) )

! nasty handling of colum that are used for index ranges.. 
splitb=[ ] 
do is=1,size(split) 
ssplit=split{is}
no=size(strfind(ssplit,'('))
nc=size(strfind(ssplit,')'))
if (no>nc) then 
if ((is+1)<=size(split)) then 
splitb{size(XXX,XXX)+1}=split{is},// ':' // split{is+1}
end if 
else if (nc>no) then 
! should have been accounted by previous step 
else
splitb{size(XXX,XXX)+1}=split{is}
end if 
end do 

split=splitb
ncol=size(split)-1
select case ( ncol) !
case ( 0) !
sf=[ sf,s((Ieq(1)+1):size(XXX,XXX)) ] 
case ( 1) !
sf=sf // trim(split{1}) // ',' // trim(split{2})
case ( 2) !
sf=sf // trim(split{1}) // ',' // trim(split{3}) // ',' // trim(split{2})
end select 
sf=sf // ' \n'
! stacking 
end_stack=fstack_push(end_stack,'do')

end subroutine freplacefor 

!> 
subroutine freplacefunctioncall( s, decl_stack , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: decl_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
real*8 :: .not.(  !>  
logical :: b  !>  
real*8 :: c_argout  !>  
real*8 :: fname  !>  
integer :: icb  !>  
integer :: icp  !>  
integer :: ieq  !>  
integer :: iob  !>  
integer :: iop  !>  
integer :: isf  !>  
character(len=*) :: s_argin  !>  
character(len=*) :: s_argout  !>  
character(len=*) :: s_args  !>  
character(len=*) :: scomm  !>  
real*8, dimension(:) :: v.comment  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.name  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.prop  !>  
real*8, dimension(:) :: v.shape  !>  
real*8, dimension(:) :: v.type  !>  
real*8 :: ~  !>  
! 
! this function assume the form: [ ]= kdsfkdsfj() 
! 
! 
! freplacefunctioncall('[a , b]=ffunctionreplace( s,end_stack ) !',[]) 


ieq=strfind(s,'=')
iob=strfind(s,'[')
icb=strfind(s,']')

call ffindmatching( s,ieq(1),'(',')',1 , b,iop,icp) 
! output arguments 
isf=ieq+1
s_argout=trim(s((iob+1):(icb-1)))
s_argout=strrep(s_argout,', ',',')
s_argout=strrep(s_argout,' ,',',')
s_argout=fremovespaces(s_argout)
s_argout=strrep(s_argout,' ',',')
! [~, ~, ~, ~, ~, ~, c_argin]=regexp(s_argin,','); 
call regexp( s_argout,',' , .not.(,~,~,~,~,~,c_argout) )

! function name 
fname=trim(s(isf:(iop-1)))
! arguments 
s_argin=s((iop+1):(icp-1))

! concatenation, don't care about inout for now... 
if (.not.(isempty(s_argout) .and. ~isempty(s_argin))) then 
s_args = s_argin // ' , ' // s_argout
else if (isempty(s_argout) .and. .not.(isempty(s_argin))) then 
s_args= s_argin
else if (isempty(s_argout) .and. isempty(s_argin)) then 
s_args= s_argout
else
s_args=''
end if 

scomm=s((icp+1):size(XXX,XXX))

! To tring 
sf='call ' // fname // '( ' // s_args // ') ' // scomm // '\n'

! Declarations 
do iout=1,size(c_argout) 
v.name=trim(c_argout{iout})
call fgetVarTypeFromName( v.name , v.type,v.shape,v.prop) 
v.comment=''
decl_stack=fstack_push(decl_stack,v)
end do 


end subroutine freplacefunctioncall !function

!> 
subroutine freplacefunction( s, end_stack, sf, decl_stack)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, dimension(1), intent(out) :: decl_stack  !>  
character(len=*), intent(inout) :: end_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
real*8 :: a  !>  
logical :: b  !>  
real*8 :: c  !>  
matlabcell, dimension(:,:) :: c_all_args  !> !m2f: check dim(:,:) 
real*8, dimension(1) :: c_argin  !>  
matlabcell :: c_argout  !> !m2f: check dim(:) 
real*8 :: d  !>  
real*8 :: e  !>  
real*8 :: f  !>  
real*8 :: fname  !>  
real*8 :: found  !>  
integer, dimension(:) :: I  !> !m2f: check dim(:) 
integer :: icb  !>  
integer :: icp  !>  
integer :: ieq  !>  
integer :: iob  !>  
integer :: iop  !>  
integer :: isf  !>  
character(len=*) :: s_argin  !>  
character(len=*), dimension(1) :: s_argout  !>  
character(len=*) :: s_args  !>  
real*8, dimension(:) :: v.comment  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.name  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.prop  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: v.shape  !>  
real*8, dimension(:) :: v.type  !>  
! 
decl_stack=[ ] 
! freplacefunction('function [ sf, end_stack ] = ffunctionreplace( s,end_stack )',[]) 


ieq=strfind(s,'=')
iob=strfind(s,'[')
icb=strfind(s,']')
iop=strfind(s,'(')
icp=strfind(s,')')
! output arguments 
if (isempty(ieq)) then 
c_argout=[ ] 
s_argout=[ ] 
isf=9
else
isf=ieq+1
if (isempty(iob)) then 
c_argout{1}=trim(s(9:(ieq-1)))
s_argout=trim(s(9:(ieq-1)))
else
s_argout=s((iob+1):(icb-1))
s_argout=trim(s_argout)
s_argout=fremovespaces(s_argout) ! remove double spaces
if (.not.(isempty(strfind(s_argout,','))) then ! argout separated by commas)
call regexp( s_argout,',' , a,b,c,d,e,f,c_argout) 
else !argout separated by spaces
call regexp( s_argout,'[\ ]*' , a,b,c,d,e,f,c_argout) 
end if 
end if 
end if 

! function name 
fname=s(isf:(iop-1))
! arguments 
s_argin=s((iop+1):(icp-1))
call regexp( s_argin,',' , a,b,c,d,e,f,c_argin) 
if (size(c_argin)==1 .and. size(trim(c_argin{1}))==0) then 
c_argin=[ ] 
end if 
if (size(c_argout)==1 .and. size(trim(c_argout{1}))==0) then 
c_argout=[ ] 
end if 
!! Let's see who is inout 
c_all_args=cell(0,2)
do iin=1,size(c_argin) 
found=0
do iout=1,size(c_argout) 
if (isequal(trim(c_argin{iin}),trim(c_argout{iout}))) then 
found=iout
end if 
end do 
if (found==0) then 
c_all_args{iin,1}=c_argin{iin}
c_all_args{iin,2}='in'
else
c_all_args{iin,1}=c_argin{iin}
c_all_args{iin,2}='inout'
! deletion of output 
c_argout=c_argout(setdiff(1:size(c_argout),found))
end if 
end do 
if (size(c_argout)>0) then 
I=size(c_argin)+(1:size(c_argout))
c_all_args(I,1)=c_argout
c_all_args(I,2)={'out'}
end if 
s_args=''
if (.not.(isempty(c_all_args))) then 
s_args=c_all_args{1,1}
do iarg=2,size(c_all_args(:,1)) 
s_args=s_args // ', ' // c_all_args{iarg,// 1}
end do 
end if 
! 
! 
!! Output 
sf='!> \n'
sf=sf // 'subroutine ' // fname // '(' // s_args // ')\n'
sf=sf // 'implicit none \n'
sf=sf // '! Use somemodule \n'
if (.not.(isempty(c_all_args))) then 
! sf=[sf '! Input/output arguments NewLine']; 
do iarg=1,size(c_all_args(:,1)) 
v.name=trim(c_all_args{iarg,1})
call fgetVarTypeFromName( v.name , v.type,v.shape,v.prop) 
if (.not.(isempty(v.prop))) then 
v.prop=v.prop // ','
end if 
v.comment=''
v.prop=v.prop // 'intent(' // c_all_args{iarg,// 2} // ')'
! [ sfdecl ] = fgetDeclaration( v ); 
! sf=[sf sfdecl];! ', intent(' c_all_args{iarg,2} ') :: ' ' !> NewLine']; 
decl_stack=fstack_push(decl_stack,v)
end do 
end if 
! sf=[sf '! Variables declaration NewLine']; 
sf=sf // '!M2F-HERE-XXX \n'
sf=sf // '! \n'



! stacking 
end_stack=fstack_push(end_stack,'subroutine ' // fname)

end subroutine freplacefunction 

!> 
subroutine freplaceif( s, end_stack , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: end_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
logical :: bMatched  !>  
integer, dimension(:) :: Icomm  !> !m2f: check dim(:) 
integer, dimension(:) :: Ielse  !> !m2f: check dim(:) 
integer, dimension(:) :: Iif  !> !m2f: check dim(:) 
integer, dimension(2) :: imin  !>  
integer, dimension(:) :: Iterm  !> !m2f: check dim(:) 
real*8 :: pc  !>  
real*8 :: po  !>  
character(len=*) :: s_comm  !>  
character(len=*) :: s_cond  !>  
! 

! freplaceif('if (ksdjf)',[]) 
! freplaceif('if i==1 ; ! why not?',[]) 
! freplaceif('else if i==1 ; ! why not?',[]) 


Ielse=strfind(s,'else')
Iif=strfind(s,'if')

! splitting the string in substrings 
s_cond=trim(s((Iif(1)+2):size(XXX,XXX))) ! the conditional string
s_comm=''
Iterm=strfind(s_cond,';') !is there a termination..
Icomm=strfind(s_cond,'!') !is there a comment


if (.not.(isempty(Icomm) .or. ~isempty(Iterm))) then 
imin=min([ Iterm,Icomm ] )
s_comm=s_cond(imin:size(XXX,XXX))
s_cond=trim(s_cond(1:(imin-1)))
end if 

!Adding parenthesis if needed 
call ffindmatching( s_cond,1,'(',')',1 , bMatched,po,pc) 
if (.not.((pc==size(s_cond) .and. po==1 .and. bMatched))) then 
s_cond='(' // trim(s_cond) // ')'
! if (s_cond(1)==' ' && s_cond(2) ~='(' ) || (s_cond(1)~=' ' && s_cond(1)~='(' ) !of course could be smarter 
end if 

sf='if ' // s_cond // ' then ' // s_comm // '\n'
if (.not.(isempty(Ielse) .and. Ielse(1)==1)) then 
! else if 
sf='else ' // sf
else
! if not an elseif then we stack 
! stacking 
end_stack=fstack_push(end_stack,'if')
end if 
end subroutine freplaceif ! function

!> 
subroutine freplaceprintf( s , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
real*8 :: .not.(  !>  
integer, dimension(1) :: I  !> !m2f: check dim(:) 
integer :: ip  !>  
real*8 :: p1  !>  
real*8 :: p2  !>  
character(len=*) :: s_assign  !>  
character(len=*) :: s_post  !>  
character(len=*) :: s_print  !>  
character(len=*) :: scmd  !>  
! 
sf=s
I=regexp(s,'[ sf ] +printf\(')
if (.not.(isempty(I))) then 
ip=I(1) ! NOTE:only deals with one printf..
if (s(ip)=='s') then 
scmd='sprintf'
else if (s(ip)=='f') then 
scmd='fprintf'
else
scmd='printf'
end if 
! find parenthesis extent 
call ffindmatching( s,ip+3,'(',')',1 , .not.(,p1,p2) )
s=strrep(s,'\','\\')
s=strrep(s,'!','!!')

s_print=s((p1+1):(p2-1)) ! not safe and will keep the !
s_assign=s(1:(ip-1))
s_post=s((p2+1):size(XXX,XXX))
! s_print=regexprep(s_print,'!![sdf.0-9]*',''', XX ,'''); !TODO improve me 
sf='!m2f: ' // s // '\n '
select case ((scmd)) !
case ( 'printf') !
! sf=[sf 'write(*,*) ' s_print 'NewLine !m2f: ' s_post 'NewLine'] ; 
sf=sf // 'write(*,*) '' '' \n'
case ( 'sprintf') !
! sf=[sf 'write(s,*) ' s_print 'NewLine !m2f: ' s_post 'NewLine'] ; 
sf=sf // 'write(s,*) '' '' \n'
case ( 'fprintf') !
! sf=[sf 'write(unitnumber,*) ' s_print 'NewLine !m2f: ' s_post 'NewLine'] ; 
sf=sf // 'write(unitnumber,*) '' '' \n'
end select 

end if 



end subroutine freplaceprintf 

!> 
subroutine freplacesbracket( s, vtype, ncol, nlines)
implicit none 
! Use somemodule 
! Arguments declarations 
integer, intent(out) :: ncol  !>  
integer, intent(out) :: nlines  !>  
character(len=*), dimension(3), intent(inout) :: s  !>  
real*8, dimension(:), intent(out) :: vtype  !> !m2f: check dim(:)!m 
! Variable declarations 
logical :: b  !>  
logical :: bwithin  !>  
integer :: is  !>  
integer :: napostroph  !>  
integer :: nc  !>  
integer :: nsc  !>  
real*8 :: pc  !>  
real*8 :: po  !>  
character(len=*) :: sb  !>  
character(len=*) :: spost  !>  
character(len=*) :: spre  !>  
! 
! [ s , var1,ncol,nlines] = freplacesbracket( 'aaaa=[]!' ); 
! 
! 
! 
vtype=''
ncol=0
nlines=0

call ffindmatching( s,1,'[',']',1 , b,po,pc) 
if (b) then 
! pre and post strings without bracket 
spre=s(1:(po-1))
spost=trim(s((pc+1):size(XXX,XXX)))
! inside bracket string 
sb=trim(s((po+1):(pc-1)))
napostroph=size(strfind(sb,''''))
if (napostroph>0 .and. modulo(napostroph,2)==0) then 
! deal with string, in an ugly way 
sb=trim(fremovespaces(sb))
is=1
bwithin=0
do while (is<=size(sb)) 
if (sb(is)=='''') then 
if (.not.(bwithin)) then 
bwithin=1
else
bwithin=0
end if 
end if 
if ((sb(is)==' ' .or. sb(is)==',' .or. sb(is)==') then ;') .and. .not.(bwithin)
! concatenate strings 
sb=sb(1:(is)) // '// ' // sb((is+1):size(XXX,// XXX))
is=is+4
else
is=is+1
end if 
end do 
vtype='character(len=*)'


else
! matching brackets have been found.. Assumed not including furtger brackets TODO 
! some really ugly stuff below to accept spaces, commas and semi-column as separator. A good regexp would help 
sb=fremovespaces(trim(sb)) ! remove double spaces

sb=strrep(sb,'; ', ';') !
sb=strrep(sb,' ;', ';') !

sb=strrep(sb,',', ' ') ! replacing commas
sb=fremovespaces(trim(sb)) ! remove double spaces again
! Now spaces have meaning, so we replace them by , 
sb=strrep(sb,' ', ',') ! replacing spaces

nc=size(strfind(sb,','))
nsc=size(strfind(sb,';'))
nlines=nsc+1
! if nlines==1 
ncol=floor(nc/nlines)+1
! else 
! ncol= 
! end 
if (min(ncol,nlines)==1) then 
! no need to reshape, I think... 
!m2f: sb=sprintf('[  write(s,*) ' '  
else
!m2f: sb=sprintf('reshape([  write(s,*) ' '  
end if 

end if 

s=[ spre,sb,spost ] 

end if 


end subroutine freplacesbracket 

!> 
subroutine freplacewhile( s, end_stack , sf)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: end_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
integer, dimension(:) :: Icol  !> !m2f: check dim(:) 
integer, dimension(:) :: Icomm  !> !m2f: check dim(:) 
integer, dimension(:) :: Ieq  !> !m2f: check dim(:) 
integer, dimension(2) :: imin  !>  
integer, dimension(:) :: Iterm  !> !m2f: check dim(:) 
integer :: ncol  !>  
character(len=*) :: s_comm  !>  
character(len=*) :: s_cond  !>  
! 

! freplacewhile('while (i<I)!cool',[]) 
! freplacefor('while i>0;!why not?',[]) 


Icol=strfind(s,':')
Ieq=strfind(s,'=')
ncol=size(Icol)
s_cond=trim(s(6:size(XXX,XXX)))
s_comm=''

Iterm=strfind(s_cond,';') !is there a termination..
Icomm=strfind(s_cond,'!') !is there a comment
if (.not.(isempty(Icomm) .or. ~isempty(Iterm))) then 
imin=min([ Iterm,Icomm ] )
s_comm=s_cond(imin:size(XXX,XXX))
s_cond=s_cond(1:(imin-1))
end if 
if ((s_cond(1)==' ' .and. s_cond(2) /='(' ) .or. (s_cond(1)/=' ' .and. s_cond(1)/='(' )) then !of course could be smarter
s_cond='(' // trim(s_cond) // ')'
end if 
sf='do while ' // s_cond // ' ' // s_comm
sf=sf // ' \n'
! stacking 
end_stack=fstack_push(end_stack,'do')

end subroutine freplacewhile 

!> 
subroutine freplacezeros( s, decl_stack, sf)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: decl_stack  !>  
character(len=*), intent(in) :: s  !>  
character(len=*), intent(out) :: sf  !>  
! Variable declarations 
real*8 :: .not.(  !>  
logical :: b  !>  
integer, dimension(:) :: I  !> !m2f: check dim(:) 
integer, dimension(:) :: Ieq  !> !m2f: check dim(:) 
integer, dimension(1) :: Is  !> !m2f: check dim(:) 
integer, dimension(1) :: Is2  !> !m2f: check dim(:) 
integer :: n_comma  !>  
real*8 :: pc  !>  
real*8 :: po  !>  
real*8 :: post  !>  
real*8 :: posttrim  !>  
real*8 :: pre  !>  
character(len=*) :: s_end  !>  
character(len=*) :: s_length  !>  
character(len=*) :: s_linsp  !>  
character(len=*) :: s_shape  !>  
character(len=*) :: s_size  !>  
character(len=*) :: s_start  !>  
character(len=*) :: split  !>  
character(len=*), dimension(:) :: v.comment  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.name  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.prop  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.shape  !> !m2f: check dim(:) 
real*8, dimension(:) :: v.type  !> !m2f: check dim(:) 
real*8, dimension(:) :: value  !> !m2f: check dim(:)!m 
real*8, dimension(:) :: varname  !> !m2f: check dim(:)!m 
real*8 :: ~  !>  
! 

! freplacezeros('aaa zeros(1,10) ; ones(size(1)) !sdkjskf') 


! I=regexp(s,'(zeros\(|ones\(|linspace\()'); 
I=regexp(s,'(zeros\(|ones\()')
if (s(I(1))=='z') then 
value=0
else if (s(I(1))=='o') then 
value=1
else
value=-1 !linspace
end if 

call ffindmatching( s,I(1),'(',')',1 , b,po,pc) 
s_size=s((po+1):(pc-1))

! linspace specific stuff 
if (value==-1) then 
s_linsp=s((po+1):(pc-1))
call regexp( s_linsp,',' , .not.(,~,~,~,~,~,split) )
s_start = split{1}
s_end = split{2}
s_length = split{3}

s_size=s_length
end if 


Ieq=strfind(s,'=')


! default value 
! if value>=0 
sf= 'allocate(TODO(' // s_size // ')) \n'
!m2f: sf= sprintf(' write(s,*) ' '  
! else 
! a = 1.5 ! Initial value 
! y = (/((i*a),i=1,100)/) ! m2f: kind of linspace 
! sf= [s 'NewLine']; !we can leave the linspace, and make a function that does that. 
! sf= sprintf('!sTODO = (/( (i*a),i=1,length(,cc (!s) NewLine',sf,value); 
! end 
sf= sf // '!m2f: ' // s // '\n'
if (.not.(isempty(Ieq))) then 
! default 
sf= '!m2f: ' // s // '\n'
! ok, there is an assignment somewhere 
pre=s((Ieq(1)+1):(I(1)-1))
post=s((pc+1):size(XXX,XXX))

! Let's see if it's a simple assigment: var= operation zeros( 
Is=regexp(s,'[ a-z.A-Z0-9_ ] +=[() ***.+0-9]*(zeros\(|ones\()')
if (.not.(isempty(Is) .and. Is(1)==1)) then 
! Cool 
varname=s(1:(Ieq(1)-1))
else
Is2=regexp(s,'[ a-z.A-Z0-9_ ] +=')
if (.not.(isempty(Is2) .and. Is2(1)==1)) then 
varname=s(1:(Ieq(1)-1))
else
varname='TODO'
end if 
end if 
n_comma=size(strfind(s_size,','))
s_shape=':'
do is=2,n_comma+1 
s_shape=s_shape,// ',:'
end do 
varname=trim(varname)
v.type='real*8'
v.name=trim(varname)
v.shape=s_shape
v.prop=''
v.comment='! m2f:check dim(' // s_size // ')'

decl_stack=fstack_push(decl_stack,v)

sf= sf // 'if (allocated(' // varname // ')) deallocate(' // varname // ')\n'
sf= sf // 'allocate(' // varname // '(' // s_size // '))\n'
!m2f: sf= sprintf(' write(s,*) ' '  

posttrim=trim(post)
if (isempty(trim(pre)) .and. (isempty(posttrim) .or. posttrim(1)==') then !')
! ayaya... 
else
sf= sf // varname // ' =' // pre // varname // post // '\n'
end if 
end if 
end subroutine freplacezeros !function
!> 
subroutine fsplitmatlablines( s, sout )
implicit none 
! Use somemodule 
! Arguments declarations 
character(len=*), dimension(:), intent(in) :: s  !> !m2f: check dim(:) 
matlabcell, intent(inout) :: sout  !>  
! Variable declarations 
logical :: b  !>  
logical :: b1  !>  
logical :: b2  !>  
logical :: b3  !>  
real*8 :: foundacomment  !>  
integer, dimension(2) :: ifound  !>  
integer :: ii  !>  
integer :: isplit  !>  
integer :: itry  !>  
integer :: notmeaningful  !>  
real*8 :: p1  !>  
real*8 :: p3  !>  
character(len=*) :: send  !>  
! 
! recursive function that splits s into 
! s=fsplitmatlablines('',[]) 
! s=fsplitmatlablines('a',[]) 
! s=fsplitmatlablines('a;',[]) 
! s=fsplitmatlablines('a ;',[]) 
! s=fsplitmatlablines('a ; bhjhj',[]) 
! s=fsplitmatlablines('a ; bhjhj ; sdifsidfj ;',[]) 
! s=fsplitmatlablines('a ; bhjhj '';'' sdifsidfj ;',[]) 


! ifound=strfind(s,';') 
ifound=regexp(s,'[ , ] ')! the case with , requires more care and is dropped for now..
! ifound=regexp(s,'[;,]'); 
if (.not.(isempty(ifound))) then 
notmeaningful=1
itry=1
do while (notmeaningful .and. itry<=size(ifound)) 
if (s(ifound(itry))==') then ;'
call fissurroundedby( s,ifound(itry),'''','''' , b1,p1,p3) 
call fissurroundedby( s,ifound(itry),'[',']' , b2,p1,p3) 
b=b1+b2>0
! dealing with the case where what follows is a comment.. this is nasty but well 
if (.not.(b)) then 
ii=ifound(itry)
foundacomment=0
do while (ii<=size(s) .and. .not.(foundacomment)) 
if (s(ii)==') then !'
foundacomment=1
exit
end if 
if (s(ii)==' ' .or. s(ii)==') then ;'
ii=ii+1
else
exit
end if 
end do 
if (foundacomment) then 
s(ifound(itry))=' '; ! removing the ';'
exit
end if 
end if 
else
call fissurroundedby( s,ifound(itry),'[',']' , b1) 
call fissurroundedby( s,ifound(itry),'(',')' , b2) 
call fissurroundedby( s,ifound(itry),'''','''' , b3) 
b=b1+b2+b3>0
end if 
if (.not.(b)) then 
notmeaningful=0
isplit=ifound(itry)
end if 
itry=itry+1
end do 
if (.not.(notmeaningful)) then 
sout{size(XXX,XXX)+1}=trim(s(1:(isplit-1)))
if (itry<size(s)) then 
send=trim(s((isplit+1):size(s)))
if (.not.(isempty(send))) then 
sout=fsplitmatlablines(send,sout)
end if 
end if 
else
sout{size(XXX,XXX)+1}=s
end if 

else
sout{size(XXX,XXX)+1}=s
end if 
end subroutine fsplitmatlablines 

!> 
subroutine fstack_pop_element( stack, n , element)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(out) :: element  !>  
integer, intent(in) :: n  !>  
character(len=*), intent(inout) :: stack  !>  
! 
! remove an element at given location 
element=stack(n)
stack=stack(setdiff(1:size(stack),n))
end subroutine fstack_pop_element !function

!> 
subroutine fstack_pop( stack , element)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(out) :: element  !>  
character(len=*), intent(inout) :: stack  !>  
! 
! remove the head 
element=stack{size(XXX,XXX)}
! shortening the stack 
stack=stack(1:(size(stack)-1))
end subroutine fstack_pop !function

!> 
subroutine fstack_push( stack, addme )
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(in) :: addme  !>  
matlabcell, dimension(1), intent(inout) :: stack  !>  
! 
if (isempty(stack)) then 
stack=[ ] 
end if 
! add an element at the tail of variable stack 
stack{size(XXX,XXX)+1}=addme

end subroutine fstack_push !function

