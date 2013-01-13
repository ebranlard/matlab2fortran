!> 
subroutine fassignement( s , decl_stack )
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: decl_stack      !>  
character(len=:), dimension(:), intent(inout) :: s      !>  
! Variable declarations 
real*8 :: .not.(      !>  
logical :: bFirstDimMayBeDetermined      !>  
logical :: bSecondDimMayBeDetermined      !>  
logical :: bwithin      !>  
integer, dimension(:) :: Ic !m2f: check dim(:)     !>  
integer, dimension(:) :: Icurl !m2f: check dim(:)     !>  
integer, dimension(:) :: Ieq !m2f: check dim(:)     !>  
integer, dimension(:) :: Ipar !m2f: check dim(:)     !>  
integer :: is      !>  
integer, dimension(1) :: Isimple !m2f: check dim(:)     !>  
integer :: ncol      !>  
integer :: ncommas      !>  
integer :: nlines      !>  
character(len=:) :: sCOM      !>  
character(len=:) :: sLHS      !>  
character(len=:) :: sRHS      !>  
character(len=:), dimension(1) :: sRHSb      !>  
real*8, dimension(:) :: v !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
real*8, dimension(:) :: v.comment !m2f: check dim(:)     !>  
real*8, dimension(:) :: v.name !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
real*8, dimension(:) :: v.prop !m2f: check dim(:)     !>  
real*8, dimension(:) :: v.shape !m2f: check dim(:)     !>  
real*8, dimension(:) :: v.type !m2f: check dim(:)     !>  
real*8, dimension(:) :: vb.comment !m2f: check dim(:)     !>  
real*8, dimension(:) :: vb.name !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
real*8, dimension(:) :: vb.prop !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
character(len=:), dimension(:) :: vb.shape !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
real*8, dimension(:) :: vb.type !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
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

