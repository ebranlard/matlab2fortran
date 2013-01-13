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
! Git-revision: 
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
!M2F-HERE-XXX 
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
!m2f: fprintf('Converting to file: %s\n',filef);
 write(unitnumber,*) ' ' 
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
!M2F-HERE-XXX 
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
!m2f: fprintf('%s%s \n',pref_in,sLine);
 write(unitnumber,*) ' ' 
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
!m2f: fprintf(fidf,'end %s \n',end_stack{ie});
 write(unitnumber,*) ' ' 
end do 

end subroutine fconvertlinebyline !function


!> 
subroutine fconvertline(s, end_stack, decl_stack, sf, unit_switch, decl_stack_new)
implicit none 
! Use somemodule 
!M2F-HERE-XXX 
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
!M2F-HERE-XXX 
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
!M2F-HERE-XXX 
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
!M2F-HERE-XXX 
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
!M2F-HERE-XXX 
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
