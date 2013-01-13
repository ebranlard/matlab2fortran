! Now the part where we add the incomplete Shed Segment, Gamma=0 at the "TE" 
! Variable declarations 
real*8, dimension(:,:) :: A ! m2f:check dim(1,n)     !>  
real*8, dimension(:) :: B ! m2f:check dim(n)     !>  
logical :: bThick      !>  
real*8, dimension(:,:,:) :: C ! m2f:check dim(1,3,4)     !>  
real*8 :: cpt_SegShed      !>  
integer :: imap1      !>  
integer :: imap2      !>  
integer :: iNewSegShed      !>  
integer :: iNW      !>  
integer :: ip      !>  
integer, dimension(:) :: IR !m2f: check dim(:)     !>  
real*8, dimension(:) :: IR.IBufferNewSegShed ! m2f:check dim(1,Data1.nTotSideEmit)!m2f: check dim(:)!m2f: check dim(:,:)     !>  
real*8, dimension(:,:) :: IR.IBufferNewSegTrailed ! m2f:check dim(1,Data1.nTotSideEmit+1)     !>  
integer :: isp2      !>  
real*8 :: X      !>  
! 
if (isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP')) then 
imap1=4
else if (isequal(EmissionMethod,'CreateTEBeforeSolving')) then 
imap1=4
else if (isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel')) then 
imap2=2
end if 

X=[1 22 &
3 + 12 &
3]

!m2f: A=zeros(1,n)
if (allocated(A)) deallocate(A) !m2f: check if wanted
allocate(A(1,n))
A = 0.D0 

!m2f: B=zeros(n)
if (allocated(B)) deallocate(B) !m2f: check if wanted
allocate(B(n))
B = 0.D0 

!m2f: C=zeros(1,3,4)
if (allocated(C)) deallocate(C) !m2f: check if wanted
allocate(C(1,3,4))
C = 0.D0 


!m2f: IR.IBufferNewSegShed =zeros(1,Data1.nTotSideEmit) ! Index of Shed Sgmt created along the TE, part of the buffer ring
if (allocated(IR.IBufferNewSegShed )) deallocate(IR.IBufferNewSegShed ) !m2f: check if wanted
allocate(IR.IBufferNewSegShed (1,Data1.nTotSideEmit))
IR.IBufferNewSegShed = 0.D0 

!m2f: IR.IBufferNewSegTrailed=zeros(1,Data1.nTotSideEmit+1) ! Index of Trailed Sgmt created along the TE, part of the buffer ring
if (allocated(IR.IBufferNewSegTrailed)) deallocate(IR.IBufferNewSegTrailed) !m2f: check if wanted
allocate(IR.IBufferNewSegTrailed(1,Data1.nTotSideEmit+1))
IR.IBufferNewSegTrailed = 0.D0 


! keyboard 
do iw=1,size(IR.Wings) 
do ipatch=1,size(IR.Wings(iw).Patches) ! loop on Patches 
! TODO leading edge and TE emission 

if (IR.Wings(iw).Patches(ipatch).bEmit) then ! Ok will emit
bThick=IR.Wings(iw).Patches(ipatch).bThick
do is=1,IR.Wings(iw).Patches(ipatch).nSpan !loop on span for TE panels 
! ----------------------- PANEL index ip and Panel Points iPs ---------------------------------------- 
if (isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP')) then 
! In this case the panel used is the last panel of the stripe 
ip=IR.Wings(iw).Patches(ipatch).FirstPanel+is*(IR.Wings(iw).Patches(ipatch).nChord)-1 ! last panel next to TE (top one if Thick)
else if (isequal(EmissionMethod,'CreateTEBeforeSolving') .or. isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel')) then 
! we use a NW panel 
iNW=iNW+1
ip=IR.IpanelsNW(iNW)
end if 
call fAddSegmentPoints( IR,P2,1,ispanCum,timeManager.itime , IR,isp2) !1 for convection

! Otherwise : it's the ShedVorticityConcentrationPoint on the TE trajectory 
call fAddSegment( IR, isp2,isp1,Gamma21,timeManager.itime , IR,iNewSegShed) 
cpt_SegShed=cpt_SegShed+1 ! counter of Shed segments
IR.IBufferNewSegShed(cpt_SegShed)=iNewSegShed
end do ! end loop on span/stripe
end if !if emit
end do ! loop on patches
end do 




!> 
subroutine fnull(x, a, i, h, f, s, b)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(inout) :: a      !>  
real*8, dimension(:,:), intent(out) :: b ! m2f:check dim(1,2)     !>  
real*8, intent(inout) :: f      !>  
real*8, dimension(:), intent(in) :: h ! m2f:check dim(1)     !>  
integer, dimension(3), intent(inout) :: i      !>  
character(len=:), intent(out) :: s      !>  
real*8, intent(in) :: x      !>  
! 
i=1
!m2f: b=zeros(1,2)
if (allocated(b)) deallocate(b) !m2f: check if wanted
allocate(b(1,2))
b = 0.D0 

i=[ 1,1,1 ] 
!m2f: h=zeros(1)
if (allocated(h)) deallocate(h) !m2f: check if wanted
allocate(h(1))
h = 0.D0 

end subroutine fnull 




!> 
subroutine fWriteDeclarationsAtCorrectLocation(fidftmp, fidf, Declarations)
implicit none 
! Use somemodule 
! Arguments declarations 
real*8, intent(in) :: Declarations      !>  
real*8, intent(in) :: fidf      !>  
real*8, intent(in) :: fidftmp      !>  
! Variable declarations 
real*8 :: current_unit      !>  
integer, dimension(:) :: I !m2f: check dim(:)!m2f: check dim(:)!m2f: check dim(:)     !>  
character(len=:) :: sread      !>  
! 
current_unit=1
sread = fgets(fidftmp)
do while (sread /= -1) 
if (size(Declarations)==1) then 
! no subroutines in file -> wait for the first non comment empty to output the declaration 
if (.not.(isempty(sread) .and. sread(1)/=') then !')
fwrite_Declarations(fidf,Declarations{current_unit})
end if 
else ! we have some subroutines
I=strfind(sread,'subroutine')
if (.not.(isempty(I) .and. I(1)==1)) then 
if (current_unit==1 .and. .not.(isempty(Declarations{current_unit}))) then 
! case of first unit, i.e. main program 
! ! we need to output the declaration here 
fwrite_Declarations(fidf,Declarations{current_unit})
end if 
current_unit=current_unit+1 !
end if 
!m2f: I=strfind(sread,'!M2F-HERE-XXX')
I=strfind(sread,transpose()!M2F-HERE-XXX')
if (.not.(isempty(I) .and. I(1)==1)) then 
fwrite_Declarations(fidf,Declarations{current_unit})
sread=''
end if 
end if 
sread=strrep(sread,'\','\\') ! required
!m2f: fprintf(fidf,sread);
 write(unitnumber,*) ' ' 
sread = fgets(fidftmp)
end do !while reading file tmp
end subroutine fWriteDeclarationsAtCorrectLocation !function fWriteDeclarationsAtCorrectLocation

