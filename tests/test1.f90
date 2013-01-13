! Now the part where we add the incomplete Shed Segment, Gamma=0 at the "TE" 
if ( isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP')) then 
imap1=4
else if ( isequal(EmissionMethod,'CreateTEBeforeSolving')) then 
imap1=4
else if ( isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel')) then 
imap2=2
end if 
 
!m2f: IR.IBufferNewSegShed =zeros(1,Data1.nTotSideEmit) ! Index of Shed Sgmt created along the TE, part of the buffer ring
real*8, allocatable, dimension(1,Data1.nTotSideEmit) :: TODO ! m2f:check >! 
if (allocated(TODO)) deallocate(TODO) !m2f: check if wanted
allocate(TODO(1,Data1.nTotSideEmit))
TODO = 0.0D 
TODO =TODO ! Index of Shed Sgmt created along the TE, part of the buffer ring

!m2f: IR.IBufferNewSegTrailed=zeros(1,Data1.nTotSideEmit+1) ! Index of Trailed Sgmt created along the TE, part of the buffer ring
real*8, allocatable, dimension(1,Data1.nTotSideEmit+1) :: TODO ! m2f:check >! 
if (allocated(TODO)) deallocate(TODO) !m2f: check if wanted
allocate(TODO(1,Data1.nTotSideEmit+1))
TODO = 0.0D 
TODO =TODO ! Index of Trailed Sgmt created along the TE, part of the buffer ring

 
! keyboard 
do iw=1,size(IR.Wings) 
do ipatch=1,size(IR.Wings(iw).Patches) ! loop on Patches 
! TODO leading edge and TE emission 
 
if ( IR.Wings(iw).Patches(ipatch).bEmit ) then ! Ok will emit
bThick=IR.Wings(iw).Patches(ipatch).bThick
do is=1,IR.Wings(iw).Patches(ipatch).nSpan !loop on span for TE panels 
! ----------------------- PANEL index ip and Panel Points iPs ---------------------------------------- 
if ( isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP')) then 
! In this case the panel used is the last panel of the stripe 
ip=IR.Wings(iw).Patches(ipatch).FirstPanel+is*(IR.Wings(iw).Patches(ipatch).nChord)-1 ! last panel next to TE (top one if Thick)
else if ( isequal(EmissionMethod,'CreateTEBeforeSolving') .or. isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel')) then 
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
 
