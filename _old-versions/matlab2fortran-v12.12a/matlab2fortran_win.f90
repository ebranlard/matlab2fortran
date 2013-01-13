! DOCUMENTATION : matlab2fortran 
! --------------------------------------------------- 
! Brief description: 
! matlab2fortran(matlab to fortran) : converting matlab code to fortran 
! Usage: matlab2fortran(filename); 
! 
! 
! Author:Emmanuel Branlard 
! Date : December 2012 
! Version: 12.12a 
! 
! License: None. Thank you for sharing your improvements to me by email. 
! 
! Revision: 
! 15/12/12 : added declaration of variables at beginning of script or subroutines 
! added handling of intent(inout), 
! corrected small bug for do loops and number of ":" since they can be in length(a(:,1)) 
! 
! 
! 
! 
! matlab2fortran(matlab to fortran) is a code that performs an automatic conversion of one or multiple matlab files to fortran format.The script translate some of the main features of matlab to fortran. matlab2fortran performs a quick and dirty conversion based on a line-by-line basis (but still supports conditional loops and subroutine). The script allows multiple matlab command per line if these commands are separated by ";", but not by ",". 
! 
! matlab2fortran does not intend to produce ready-to-compile code, but performs several necessary conversions. The generated code keeps the structure, variables names and comments of the original code. 
! 
! The script matlab2fortran (matlab to fortran) is a single script file written in matlab and does not require a particular installation other than maybe adding this script to your current folder or matlab path. The script has been tested on linux and windows. 
! 
! Main Features of matlab2fortran: 
! - conversion of nested structure: do, if, switch (select), while 
! - conversion of function to subroutine with recognition of output and input arguments 
! - perform subroutine list of arguments with intent(in) and intent(out) 
! - make a declaration list for simple variable , sort them by Name or Type 
! - determines type based on variable name (function fgetVarTypeFromVarName) 
! - recognize simple function call and convert them to subroutine calls (syntax [y] = f(x) ); 
! - performs allocation when converting matlab function zeros, ones (linspace possible) 
! - splitting of lines that contain multiple matlab command separated by ; 
! - comments 
! - small support for sprintf and fprintf 
! - misc conversions such as logical operators 
! - few intrinsic functions conversion 
! 
! TODOs: 
! - better printf handling 
! - Handling of [] and string concanenation 
! - better handling of function calls and nested function calls.. 
! 
! 
! 
! Examples of calls: 
! ---------------------------------------------------- 
! example 1: 
! matlab2fortran('tests/test1.m'); 
! matlab2fortran('tests/test1.m','tests/test2.m'); 
! 
! example 2: all files in current directory (does not work with subfolders with dir..) 
! FileList=dir('*.m'); 
! matlab2fortran(FileList.name); 
!> 
subroutine matlab2fortran( varargin )
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: varargin !> 
! Variables declaration 
logical :: bDebug      !>  
logical :: bPipe      !>  
logical :: bSortDeclarationbyNames      !>  
logical :: bSortDeclarationbyTypes      !>  
real*8 :: Declarations      !>  
real*8 :: f      !>  
real*8 :: fidf      !>  
real*8 :: fidftmp      !>  
real*8 :: fidm      !>  
real*8 :: file      !>  
real*8 :: filef      !>  
real*8 :: fileftmp      !>  
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

!! dealing with multiple input files "recursively..." 
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
Declarations=fFormatDeclarations(Declarations,bSortDeclarationbyNames,bSortDeclarationbyTypes,bDebug)


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
! Input/output arguments 
real*8, intent(in), :: fidm !> 
real*8, intent(in), :: fidf !> 
logical, intent(in), :: bDebug !> 
logical, intent(in), :: bPipe !> 
real*8, intent(out), :: Declarations !> 
! Variables declaration 
real*8 :: current_unit      !>  
real*8 :: decl_stack      !>  
real*8 :: end_stack      !>  
real*8 :: pref_in      !>  
real*8 :: pref_out      !>  
character(len=:) :: s      !>  
character(len=:) :: sf      !>  
character(len=:) :: sLine      !>  
real*8 :: TODO      !>  
! 
sLine = fgets(fidm)
end_stack=[]
current_unit=1
Declarations{current_unit}=[] ! contains all variables declaration
decl_stack=[] ! contains variable declaration for current subroutine or main..
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
sf=[sLine '\n']
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
sf=[sLine ' \n']
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
call fconvertline( s,end_stack,decl_stack , sf,end_stack,decl_stack,unit_switch) 
if (unit_switch/=0) then 
! storing the stack of declaration of previous unit 
if (bDebug) then 
disp('Entering new unit')
end if 
Declarations{current_unit}=decl_stack
current_unit=current_unit+unit_switch
if (unit_switch==1) then 
Declarations{current_unit}=[]
decl_stack=[]
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
subroutine fconvertline(s, end_stack, decl_stack, sf, unit_switch)
implicit none 
! Use somemodule 
! Input/output arguments 
character(len=:), intent(in), :: s !> 
real*8, intent(inout), :: end_stack !> 
real*8, intent(inout), :: decl_stack !> 
character(len=:), intent(out), :: sf !> 
real*8, intent(out), :: unit_switch !> 
! Variables declaration 
real*8 :: end_stack      !>  
integer, dimension(:) :: I      !>  
character(len=:) :: s      !>  
character(len=:) :: sf      !>  
character(len=:) :: sold      !>  
real*8 :: unit_switch      !>  
! 
unit_switch=0
! function replace 
I=strfind(s,'function')
if (.not.(isempty(I) .and. I(1)==1)) then 
call freplacefunction( s , end_stack , sf,end_stack) 
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
s=strrep(s,'ceiling(','ceiling(')
s=strrep(s,'anint(','anint(') ! or just int?
s=strrep(s,'floor(','floor(')
s=strrep(s,'modulo(','modulo(') !mod exists but return negative values
! array stuff 
s=regexprep(s,'size\(([**,]*)\)','shape($1)') !replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'size(','shape(') ! or count...

! math 
s=strrep(s,'dot_product','dot_product_product') !replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'**','**')

! strings 
! s=strrep(s,'&','\\'); 
s=strrep(s,'\','\\')

!! Things that are easily recognizable by their first position on the line 
! case 
s=regexprep(s,'**case([**!;]*)','case ($1) !')
s=regexprep(s,'**otherwise*','case default ')
sold=s
s=regexprep(sold,'**switch([**!;]*)','select case ($1) !')
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
I=strfind(s,'end')
if (.not.(isempty(I) .and. I(1)==1)) then 
!let's check that this is not a variable 
if (size(s)==3 .or. s(4)==' ' .or. s(4)==') then ;' .or.s(4)=='!'
call freplaceend( s , end_stack , sf,end_stack) 
return
end if 
end if 

! function calls 
I=strfind(s,'[')
if (.not.(isempty(I) .and. I(1)==1)) then 
call freplacefunctioncall( s , sf) 
return
end if 

! brackets 
! TODO with better string handling 
! s=strrep(s,'[','(/'); 
! s=strrep(s,']','/)'); 
! multiline 
! s=strrep(s,'...','\&'); !char(8) not enough.. !HACK : the escape sequence is not recognize and hence it merges with the next line... 
s=regexprep(s,'(?<!!.*)\.\.\.','&') ! this line requires some explanation below
! If uses a negative lookbehind "(?<!)" It looks if there is no !.* before (ie, no comment) 
! If this is not a commen then we replace the ... by &. This regexp will not work with octave, but it doesnt matter since it's a comment. 
! Octabe will replace by & all the time 
! If one want, & can be replace by \& so that matlab will not recognize this bad escpae character and hence will merge the current line with the next.. but that's a HACK 


!! More tricky things that should require syntax parsing and recursion but that we'll not do!.. 
! zeros and ones 
I=regexp(s,'(zeros\(|ones\(|linspace\()')
if (.not.(isempty(I))) then 
call freplacezeros( s ,decl_stack , s,decl_stack) 
else
! stacking assignements so that they can be used for declarations 
call fassignement( s ,decl_stack , s,decl_stack) 
end if 

! default return 
sf=[s '\n']
end subroutine fconvertline !function






!> 
subroutine fWriteDeclarationsAtCorrectLocation(fidftmp, fidf, Declarations)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: fidftmp !> 
real*8, intent(in), :: fidf !> 
real*8, intent(in), :: Declarations !> 
! Variables declaration 
real*8 :: current_unit      !>  
integer, dimension(:) :: I      !>  
character(len=:) :: sread      !>  
real*8 :: TODO      !>  
! 
current_unit=1
sread = fgets(fidftmp)
do while (sread /= -1) 
if (current_unit==1) then 
! wait for the first non comment empty to output the declaration 
if (.not.(isempty(Declarations{1}))) then 
if (.not.(isempty(sread) .and. sread(1)/=') then !')
!m2f: fprintf(fidf,'! Variable declarations \n');
 write(unitnumber,*) ' ' 
fwrite_Declarations(fidf,Declarations{current_unit})
!m2f: fprintf(fidf,'! \n');
 write(unitnumber,*) ' ' 
Declarations{1}=[]
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
fwrite_Declarations(fidf,Declarations{current_unit})
sread=''
end if 
sread=strrep(sread,'\','\\') ! required
!m2f: fprintf(fidf,sread);
 write(unitnumber,*) ' ' 
sread = fgets(fidftmp)
end do !while reading file tmp
end subroutine fWriteDeclarationsAtCorrectLocation !function fWriteDeclarationsAtCorrectLocation


!> 
subroutine fFormatDeclarations(Declarations, bSortDeclarationbyNames, bSortDeclarationbyTypes, bDebug)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(inout), :: Declarations !> 
logical, intent(in), :: bSortDeclarationbyNames !> 
logical, intent(in), :: bSortDeclarationbyTypes !> 
logical, intent(in), :: bDebug !> 
! Variables declaration 
real*8 :: Decl      !>  
integer, dimension(:) :: Isort      !>  
real*8 :: TODO      !>  
real*8 :: VarNames      !>  
real*8 :: VarTypes      !>  
! 
do iud=1,size(Declarations) 
if (bDebug) then 
!m2f: fprintf(['-------------Declaration for unit ' num2str(iud) '\n']);
 write(unitnumber,*) ' ' 
end if 
! fprintf(2,Declarations{iud}{id}); 
Decl=Declarations{iud}
if (size(Decl)>0) then 
VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',.false.)
! unique 
call unique( VarNames , .not.(,Isort) ! NOTE: case ins-sensitive so that the user sees the possible clash since fortran is case insensitive)
call unique( lower(VarNames) , .not.(,Isort2) )
if (size(Isort)/=size(Isort2)) then 
warning('Be careful, there are variables that have the same characters but different cases.')
end if 
Decl=Decl(Isort)
VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',.false.)
VarTypes = cellfun(@(x)x.name, Decl, 'UniformOutput',.false.)

! sorting 
if (bSortDeclarationbyNames) then 
call sort( lower(VarNames) , .not.(,Isort) ! NOTE: the lower here is a matter of choice)
else if (bSortDeclarationbyTypes) then 
call sort( lower(VarTypes) , .not.(,Isort) )
else
Isort=1:size(VarNames)
end if 
Decl=Decl(Isort)

! Printing 
if (bDebug) then 
fwrite_Declarations(1,Decl)
end if 
end if 
Declarations{iud}=Decl
end do 
end subroutine fFormatDeclarations !function format Declarations


!> 
subroutine fwrite_Declarations(fidout, Decl)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: fidout !> 
real*8, intent(in), :: Decl !> 
! Variables declaration 
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
! Input/output arguments 
real*8, intent(inout), :: s !> 
real*8, intent(inout), :: decl_stack !> 
! Variables declaration 
real*8 :: decl_stack      !>  
integer, dimension(:) :: Ieq      !>  
integer, dimension(:) :: Is      !>  
real*8, dimension(:) :: v.comment      !>  
real*8, dimension(:) :: v.name      !>  
! 
! only simple assignments are allowed x1_var =... , I.element = 
! It is chosen to keep the dot in the variable name... 
! if an assignment is of the form a(:,1) we dont cons 
! 

Ieq=strfind(s,'=')
if (.not.(isempty(Ieq))) then 
Is=regexp(s,'[a-z.A-Z0-9_ ]+=')
if (.not.(isempty(Is) .and. Is(1)==1)) then 
v.name=trim(s(1:(Ieq(1)-1)))
else
v.name='TODO'
end if 
call fgetVarTypeFromName( v.name , v.type,v.shape,v.prop) 
v.comment=''
decl_stack=fstack_push(decl_stack,v)
end if 


end subroutine fassignement !function

!> 
subroutine ffindmatching(s, pstart, co, cc, bforward, b, po, pc)
implicit none 
! Use somemodule 
! Input/output arguments 
character(len=:), intent(in), :: s !> 
real*8, intent(in), :: pstart !> 
real*8, intent(in), :: co !> 
real*8, intent(in), :: cc !> 
logical, intent(in), :: bforward !> 
logical, intent(out), :: b !> 
real*8, intent(out), :: po !> 
real*8, intent(out), :: pc !> 
! Variables declaration 
logical :: b      !>  
real*8 :: found      !>  
integer :: i      !>  
integer, dimension(:) :: Iopen      !>  
integer :: nopen      !>  
real*8 :: pc      !>  
real*8 :: po      !>  
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
found=0
nopen=0
Iopen=[]
i=pstart
do while (.not.(found .and. i<=size(s))) 
if (s(i)==co) then 
nopen=nopen+1
Iopen=[Iopen i]
end if 
if (s(i)==cc) then 
if (nopen>=1) then ! this is a choice, if a closing cgaracter is found, before an opened one, we don't care...
nopen=nopen-1
if (nopen==0) then 
found=1
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
! Input/output arguments 
real*8, intent(in), :: v !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8, dimension(:) :: varcomment      !>  
real*8, dimension(:) :: varname      !>  
real*8, dimension(:) :: varprop      !>  
real*8, dimension(:) :: varshape      !>  
real*8, dimension(:) :: vartype      !>  
! 
if (isempty(v.type)) then 
vartype='real*8'
else
vartype=v.type
end if 
if (.not.(isempty(v.shape))) then 
varshape=['dimension(' v.shape ')']
if (.not.(isempty(v.prop))) then 
varshape=[ varshape ', ']
end if 
else
varshape=''
end if 
if (.not.(isempty(v.prop))) then 
varprop=[v.prop ', ']
else
varprop=''
end if 
if (.not.(isempty(v.name))) then 
varname=v.name
else
varname='TODO'
end if 
if (.not.(isempty(varprop).or. ~isempty(varshape))) then 
vartype=[ vartype ', ']
end if 

varcomment=[v.comment ' !> ']


!m2f: sf=sprintf(' write(s,*) ' '  

end subroutine fgetDeclaration 

!> 
subroutine fgetVarTypeFromName( varname , vartype, varshape, varprop)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: varname !> 
real*8, dimension(:), intent(out), :: vartype !> 
real*8, intent(out), :: varshape !> 
real*8, dimension(:), intent(out), :: varprop !> 
! Variables declaration 
real*8, dimension(:) :: varprop      !>  
real*8, dimension(:) :: varshape      !>  
real*8, dimension(:) :: vartype      !>  
! 
! default type 
vartype='real*8'
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
vartype='character(len=:)'
else if (varname(1)=='v') then 
vartype='real*8'
varshape=':'
else
vartype='real*8'
end if 

end subroutine fgetVarTypeFromName 

!> 
subroutine fissurroundedby( s, p, c1, c2 , b, p1, p2)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(in), :: p !> 
real*8, intent(in), :: c1 !> 
real*8, intent(in), :: c2 !> 
logical, intent(out), :: b !> 
real*8, intent(out), :: p1 !> 
real*8, intent(out), :: p2 !> 
! Variables declaration 
logical :: b      !>  
integer :: i      !>  
integer :: notfound      !>  
real*8 :: p1      !>  
real*8 :: p2      !>  
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
subroutine fremovesinglespace( s )
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(inout), :: s !> 
! Variables declaration 
character(len=:) :: s      !>  
! 
! remove multiple spaces 
s=regexprep(s,'[\ ]+','')

end subroutine fremovesinglespace !function

!> 
subroutine fremovespaces( s )
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(inout), :: s !> 
! Variables declaration 
character(len=:) :: s      !>  
! 
! remove multiple spaces 
s=regexprep(s,'[\ ]+',' ')

end subroutine fremovespaces 

!> 
subroutine freplaceend( s, end_stack , sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(inout), :: end_stack !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
character(len=:) :: sf      !>  
real*8 :: TODO      !>  
! 
! freplaceend('end !(ksdjf)',[]) 
! freplaceend('end ; ! why not?',[]) 

if (isempty(end_stack)) then 
warning('Too much ends in the file.. Some stuff have not been accounted for?')
end_stack{1}=''
end if 

call fstack_pop( end_stack , end_stack,elmt) 
sf=['end ' elmt ' ' s(4:end) '\n'] ! could add a security comment..


end subroutine freplaceend !function

!> 
subroutine freplacefor( s, end_stack , sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(inout), :: end_stack !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8 :: end_stack      !>  
integer, dimension(:) :: Icol      !>  
integer, dimension(:) :: Ieq      !>  
real*8 :: loopvar      !>  
integer :: nc      !>  
integer :: ncol      !>  
integer :: no      !>  
character(len=:) :: sf      !>  
character(len=:) :: sloopextent      !>  
character(len=:) :: split      !>  
character(len=:) :: splitb      !>  
character(len=:) :: ssplit      !>  
real*8 :: TODO      !>  
! 

! freplacefor('for i=I',[]) 
! freplacefor('for i=1:0.2:10 ; ! why not?',[]) 
! freplacefor('for iarg=1:length(c_all_args(:,1)',[]) 

Icol=strfind(s,':')
Ieq=strfind(s,'=')
ncol=size(Icol)
loopvar=trim(s(4:(Ieq(1)-1)))
sf=['do ' loopvar '=']
sloopextent=s((Ieq(1)+1):end)
call regexp( sloopextent,':' , .not.(,~,~,~,~,~,split) )

! nasty handling of colum that are used for index ranges.. 
splitb=[]
do is=1,size(split) 
ssplit=split{is}
no=size(strfind(ssplit,'('))
nc=size(strfind(ssplit,')'))
if (no>nc) then 
if (is+1)<=size(split) then 
splitb{end+1}=[split{is},':' split{is+1}]
end if 
else if (nc>no) then 
! should have been accounted by previous step 
else
splitb{end+1}=split{is}
end if 
end do 

split=splitb
ncol=size(split)-1
select case ( ncol) !
case ( 0) !
sf=[sf s((Ieq(1)+1):end)]
case ( 1) !
sf=[sf trim(split{1}) ',' trim(split{2})]
case ( 2) !
sf=[sf trim(split{1}) ',' trim(split{3}) ',' trim(split{2})]
end select 
sf=[sf ' \n']
! stacking 
end_stack=fstack_push(end_stack,'do')

end subroutine freplacefor 

!> 
subroutine freplacefunctioncall( s , sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8 :: fname      !>  
integer :: icb      !>  
integer :: ieq      !>  
integer :: iob      !>  
integer :: isf      !>  
character(len=:) :: s_argin      !>  
character(len=:) :: s_argout      !>  
character(len=:) :: s_args      !>  
character(len=:) :: scomm      !>  
character(len=:) :: sf      !>  
! 
! this function assume the form: [ ]= kdsfkdsfj() 
! 
! 
! freplacefunctioncall('[a , b]=ffunctionreplace( s,end_stack ) !',[]) 


ieq=strfind(s,'=')
iob=strfind(s,'[')
icb=strfind(s,']')

call ffindmatching( s,ieq,'(',')',1 , b,iop,icp) 
! output arguments 
isf=ieq+1
s_argout=trim(s((iob+1):(icb-1)))
s_argout=strrep(s_argout,', ',',')
s_argout=strrep(s_argout,' ,',',')
s_argout=fremovespaces(s_argout)
s_argout=strrep(s_argout,' ',',')
! [a b c d e f c_argout]=regexp(s_argout,','); 

! function name 
fname=trim(s(isf:(iop-1)))
! arguments 
s_argin=s((iop+1):(icp-1))

! concatenation, don't care about inout for now... 
if (.not.(isempty(s_argout) .and. ~isempty(s_argin))) then 
s_args = [s_argin ' , ' s_argout]
else if (isempty(s_argout) .and. .not.(isempty(s_argin))) then 
s_args= s_argin
else if (isempty(s_argout) .and. isempty(s_argin)) then 
s_args= s_argout
else
s_args=''
end if 

scomm=s((icp+1):end)

! and output 
sf=['call ' fname '( ' s_args ') ' scomm '\n']


end subroutine freplacefunctioncall 

!> 
subroutine freplacefunction( s, end_stack , sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(inout), :: end_stack !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8 :: c_all_args      !>  
real*8 :: c_argin      !>  
real*8 :: c_argout      !>  
real*8 :: end_stack      !>  
real*8 :: fname      !>  
real*8 :: found      !>  
integer, dimension(:) :: I      !>  
integer :: icb      !>  
integer :: icp      !>  
integer :: ieq      !>  
integer :: iob      !>  
integer :: iop      !>  
integer :: isf      !>  
character(len=:) :: s_argin      !>  
character(len=:) :: s_argout      !>  
character(len=:) :: s_args      !>  
character(len=:) :: sf      !>  
real*8 :: TODO      !>  
real*8, dimension(:) :: v.comment      !>  
real*8, dimension(:) :: v.name      !>  
real*8, dimension(:) :: v.prop      !>  
! 

! freplacefunction('function [ sf, end_stack ] = ffunctionreplace( s,end_stack )',[]) 


ieq=strfind(s,'=')
iob=strfind(s,'[')
icb=strfind(s,']')
iop=strfind(s,'(')
icp=strfind(s,')')
! output arguments 
if (isempty(ieq)) then 
c_argout=[]
s_argout=[]
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
c_argin=[]
end if 
if (size(c_argout)==1 .and. size(trim(c_argout{1}))==0) then 
c_argout=[]
end if 
!! Let's see who is inout 
c_all_args=[]
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
s_args=[s_args ', ' c_all_args{iarg,1}]
end do 
end if 
! 
! 
!! Output 
sf=['!> \n']
sf=[sf 'subroutine ' fname '(' s_args ')\n']
sf=[sf 'implicit none \n']
sf=[sf '! Use somemodule \n']
if (.not.(isempty(c_all_args))) then 
sf=[sf '! Input/output arguments \n']
do iarg=1,size(c_all_args(:,1)) 
v.name=c_all_args{iarg,1}
call fgetVarTypeFromName( v.name , v.type,v.shape,v.prop) 
if (.not.(isempty(v.prop))) then 
v.prop=[v.prop ',']
end if 
v.comment=''
v.prop=[v.prop 'intent(' c_all_args{iarg,2} ')']
call fgetDeclaration( v , sfdecl) 
sf=[sf sfdecl] ! ', intent(' c_all_args{iarg,2} ') :: ' ' !> \n'];
end do 
end if 
sf=[sf '! Variables declaration \n']
sf=[sf '!M2F-HERE-XXX \n']
sf=[sf '! \n']



! stacking 
end_stack=fstack_push(end_stack,['subroutine ' fname])

end subroutine freplacefunction 

!> 
subroutine freplaceif( s, end_stack , sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(inout), :: end_stack !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8 :: end_stack      !>  
integer, dimension(:) :: Icomm      !>  
integer, dimension(:) :: Ielse      !>  
integer, dimension(:) :: Iif      !>  
integer :: imin      !>  
integer, dimension(:) :: Iterm      !>  
character(len=:) :: s_comm      !>  
character(len=:) :: s_cond      !>  
character(len=:) :: sf      !>  
! 

! freplaceif('if (ksdjf)',[]) 
! freplaceif('if i==1 ; ! why not?',[]) 
! freplaceif('else if i==1 ; ! why not?',[]) 


Ielse=strfind(s,'else')
Iif=strfind(s,'if')

s_cond=trim(s((Iif(1)+2):end)) ! the conditional string
s_comm=''
Iterm=strfind(s_cond,';') !is there a termination..
Icomm=strfind(s_cond,'!') !is there a comment


if (.not.(isempty(Icomm) .or. ~isempty(Iterm))) then 
imin=min([Iterm Icomm])
s_comm=s_cond(imin:end)
s_cond=s_cond(1:(imin-1))
end if 

if (s_cond(1)==' ' .and. s_cond(2) /='(' ) .or. (s_cond(1)/=' ' .and. s_cond(1)/='(' ) then !of course could be smarter
s_cond=['(' trim(s_cond) ')']
end if 

sf=['if ' s_cond ' then ' s_comm '\n']
if (.not.(isempty(Ielse) .and. Ielse(1)==1)) then 
! else if 
sf=['else ' sf]
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
! Input/output arguments 
real*8, intent(in), :: s !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
integer, dimension(:) :: I      !>  
integer :: ip      !>  
character(len=:) :: s      !>  
character(len=:) :: s_assign      !>  
character(len=:) :: s_post      !>  
character(len=:) :: s_print      !>  
character(len=:) :: scmd      !>  
character(len=:) :: sf      !>  
! 
sf=s
I=regexp(s,'[sf]+printf\(')
if (.not.(isempty(I))) then 
ip=I(1) !only deals with one printf...
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

s_print=s((p1+1):(p2-1)) ! not safe and will keep the !...
s_assign=s(1:(ip-1))
s_post=s((p2+1):end)
! s_print=regexprep(s_print,'!![sdf.0-9]*',''', XX ,'''); !TODO improve me 
sf=['!m2f: ' s '\n ']
select case ((scmd)) !
case ( 'printf') !
! sf=[sf 'write(*,*) ' s_print 'NewLine !m2f: ' s_post 'NewLine'] ; 
sf=[sf 'write(*,*) '' '' \n']
case ( 'sprintf') !
! sf=[sf 'write(s,*) ' s_print 'NewLine !m2f: ' s_post 'NewLine'] ; 
sf=[sf 'write(s,*) '' '' \n']
case ( 'fprintf') !
! sf=[sf 'write(unitnumber,*) ' s_print 'NewLine !m2f: ' s_post 'NewLine'] ; 
sf=[sf 'write(unitnumber,*) '' '' \n']
end select 

end if 



end subroutine freplaceprintf 

!> 
subroutine freplacewhile( s, end_stack , sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(inout), :: end_stack !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8 :: end_stack      !>  
integer, dimension(:) :: Icol      !>  
integer, dimension(:) :: Icomm      !>  
integer, dimension(:) :: Ieq      !>  
integer :: imin      !>  
integer, dimension(:) :: Iterm      !>  
integer :: ncol      !>  
character(len=:) :: s_comm      !>  
character(len=:) :: s_cond      !>  
character(len=:) :: sf      !>  
! 

! freplacewhile('while (i<I)!cool',[]) 
! freplacefor('while i>0;!why not?',[]) 


Icol=strfind(s,':')
Ieq=strfind(s,'=')
ncol=size(Icol)
s_cond=trim(s(6:end))
s_comm=''

Iterm=strfind(s_cond,';') !is there a termination..
Icomm=strfind(s_cond,'!') !is there a comment
if (.not.(isempty(Icomm) .or. ~isempty(Iterm))) then 
imin=min([Iterm Icomm])
s_comm=s_cond(imin:end)
s_cond=s_cond(1:(imin-1))
end if 
if (s_cond(1)==' ' .and. s_cond(2) /='(' ) .or. (s_cond(1)/=' ' .and. s_cond(1)/='(' ) then !of course could be smarter
s_cond=['(' trim(s_cond) ')']
end if 
sf=['do while ' s_cond ' ' s_comm]
sf=[sf ' \n']
! stacking 
end_stack=fstack_push(end_stack,'do')

end subroutine freplacewhile 

!> 
subroutine freplacezeros( s, decl_stack, sf)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
real*8, intent(inout), :: decl_stack !> 
character(len=:), intent(out), :: sf !> 
! Variables declaration 
real*8 :: decl_stack      !>  
integer, dimension(:) :: I      !>  
integer, dimension(:) :: Ieq      !>  
integer, dimension(:) :: Is      !>  
integer, dimension(:) :: Is2      !>  
integer :: n_comma      !>  
real*8 :: post      !>  
real*8 :: pre      !>  
character(len=:) :: s_end      !>  
character(len=:) :: s_length      !>  
character(len=:) :: s_linsp      !>  
character(len=:) :: s_shape      !>  
character(len=:) :: s_size      !>  
character(len=:) :: s_start      !>  
character(len=:) :: sf      !>  
real*8, dimension(:) :: v.comment      !>  
real*8, dimension(:) :: v.name      !>  
real*8, dimension(:) :: v.prop      !>  
real*8, dimension(:) :: v.shape      !>  
real*8, dimension(:) :: v.type      !>  
real*8, dimension(:) :: value      !>  
real*8, dimension(:) :: varname      !>  
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
sf= ['allocate(TODO(' s_size ')) \n']
!m2f: sf= sprintf(' write(s,*) ' '  
! else 
! a = 1.5 ! Initial value 
! y = (/((i*a),i=1,100)/) ! m2f: kind of linspace 
! sf= [s 'NewLine']; !we can leave the linspace, and make a function that does that... 
! sf= sprintf('!sTODO = (/( (i*a),i=1,length(,cc (!s) NewLine',sf,value); 
! end 
sf= [sf '!m2f: ' s '\n']
if (.not.(isempty(Ieq))) then 
! default 
sf= ['!m2f: ' s '\n']
! ok, there is an assignment somewhere 
pre=s((Ieq(1)+1):(I(1)-1))
post=s((pc+1):end)

! Let's see if it's a simple assigment: var= operation zeros( 
Is=regexp(s,'[a-z.A-Z0-9_ ]+=[() ***.+0-9]*(zeros\(|ones\()')
if (.not.(isempty(Is) .and. Is(1)==1)) then 
! Cool 
varname=s(1:(Ieq(1)-1))
else
Is2=regexp(s,'[a-z.A-Z0-9_ ]+=')
if (.not.(isempty(Is2) .and. Is2(1)==1)) then 
varname=s(1:(Ieq(1)-1))
else
varname='TODO'
end if 
end if 
n_comma=size(strfind(s_size,','))
s_shape=':'
do is=2,n_comma+1 
s_shape=[s_shape,',:']
end do 

v.type='real*8'
v.name=trim(varname)
v.shape=s_shape
v.prop=''
v.comment=['! m2f:check dimension(' s_size ')']

decl_stack=fstack_push(decl_stack,v)

sf= [sf 'if (allocated(' varname ')) deallocate(' varname ') !m2f: check if wanted\n']
sf= [sf 'allocate(' varname '(' s_size '))\n']
!m2f: sf= sprintf(' write(s,*) ' '  
sf= [sf varname ' =' pre varname post '\n']
end if 
end subroutine freplacezeros !function
!> 
subroutine fsplitmatlablines( s, sout )
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(in), :: s !> 
character(len=:), intent(inout), :: sout !> 
! Variables declaration 
logical :: b      !>  
real*8 :: foundacomment      !>  
integer :: ifound      !>  
integer :: ii      !>  
integer :: isplit      !>  
integer :: itry      !>  
integer :: notmeaningful      !>  
character(len=:) :: send      !>  
character(len=:) :: sout      !>  
real*8 :: TODO      !>  
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
ifound=regexp(s,'[;]') ! the case with , requires more care and is dropped for now...
! ifound=regexp(s,'[;,]'); 
if (.not.(isempty(ifound))) then 
notmeaningful=1
itry=1
do while (notmeaningful .and. itry<=size(ifound)) 
if (s(ifound(itry))==') then ;'
call fissurroundedby( s,ifound(itry),'''','''' , b1,p1,p3) 
call fissurroundedby( s,ifound(itry),'[',']' , b2,p1,p3) 
b=b1+b2>0
! dealing with the case where what follows is a comment... this is nasty but well 
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
sout{end+1}=trim(s(1:(isplit-1)))
if (itry<size(s)) then 
send=trim(s((isplit+1):size(s)))
if (.not.(isempty(send))) then 
sout=fsplitmatlablines(send,sout)
end if 
end if 
else
sout{end+1}=s
end if 

else
sout{end+1}=s
end if 
end subroutine fsplitmatlablines 

!> 
subroutine fstack_pop_element( stack, n , element)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(inout), :: stack !> 
real*8, intent(in), :: n !> 
real*8, intent(out), :: element !> 
! Variables declaration 
real*8 :: element      !>  
character(len=:) :: stack      !>  
! 
! remove an element at given location 
element=stack(n)
stack=stack(setdiff(1:size(stack),n))
end subroutine fstack_pop_element !function

!> 
subroutine fstack_pop( stack , element)
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(inout), :: stack !> 
real*8, intent(out), :: element !> 
! Variables declaration 
real*8 :: element      !>  
character(len=:) :: stack      !>  
! 
! remove the head 
element=stack{end}
! shortening the stack 
stack=stack(1:(size(stack)-1))
end subroutine fstack_pop !function

!> 
subroutine fstack_push( stack, addme )
implicit none 
! Use somemodule 
! Input/output arguments 
real*8, intent(inout), :: stack !> 
real*8, intent(in), :: addme !> 
! Variables declaration 
real*8 :: TODO      !>  
! 
! add an element at the tail of variable stack 
stack{end+1}=addme

end subroutine fstack_push !function

