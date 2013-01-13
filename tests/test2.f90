! Now the part where we add the incomplete Shed Segment, Gamma=0 at the "TE" 
! The interest of adding those segments is that they will be convected. 
! The index of those segments are stored in IBufferNewSegShed, these segments are attributed an updated value later, so that there are not incomplete anymore. 
 
 
! TODO: the code below is quite redundant with Part_Emit... Maybe with a little if t=0 they could be combined.. 
 
 
! The imap are convenient index that allows the method to be dealt with the same way 
if ( isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP')) then 
! The future points 1 and 2 of the "Buffer vortex rings" are actually points 4 and 3 of the TE panel 
imap1=4
imap2=3
else if ( isequal(EmissionMethod,'CreateTEBeforeSolving')) then 
! The future points 1 and 2 of the "Buffer vortex rings" are points 4 and 3 of the NW panel.. 
imap1=4
imap2=3
else if ( isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel')) then 
! The future points 1 and 2 of the "Buffer vortex rings" are points 1 and 2 of the NW panel.. 
imap1=1
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

! IppForTEconv=zeros(1,Data1.nTotSideEmit); ! IcpForTEconv=zeros(1,Data1.nTotSideEmit); ! cpt_PP_te=0; ! cpt_CP_te=0; cpt_SegShed=0 ! counter of TE segments
ispanCum=0 ! increased index in "x" direction, almost compatible with former lattice. Before for instance X(the coordinates of the segmetns points) would have dimensions nx*nz, can be removed in the future
iNW=0 ! accumulation over "NW elements", either a NW panels or a NW incomplete segment from last time step.
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
iPs=IR.Panl(ip,cpanCoord) ! the four points indexes of either the last panel or the NW panel
 
! ------------------------ DETERMINATION OF GAMMA ---------------------------------------- 
Gamma21=0
 
! ------------------------ POINTS DEFINITIONS - ADDING Point 2 (and point 1 at beginning of stripe) of Buffer Ring---------- 
if (is==1) then 
ispanCum=ispanCum+1
! Point 1 of the "buffer vortex ring" 
! If ReplaceByRing : P1=P1_NW_panel (remeber this NW panel is replace by rings at this stage, the convection has not occured) 
! If CreateBefore : P1=P4_NW_panel ( the convection should have occured on the segShedBufferOld) 
! If ConvectTE : P1=P4_last_wing_panel (the convection should have occured) 
P1=IR.PanlP(iPs(imap1),1:3)
call fAddSegmentPoints( IR,P1,1,ispanCum,timeManager.itime , IR,isp1) !1 for convection
end if 
ispanCum=ispanCum+1
 
! Point 2 of the "buffer vortex ring": see comments for point 1 above 
P2=IR.PanlP(iPs(imap2),1:3)
call fAddSegmentPoints( IR,P2,1,ispanCum,timeManager.itime , IR,isp2) !1 for convection
 
! -------------------------- CREATING SEGMENTS of the Buffer Ring ---------------------------------------- 
! Adding the incomplete shed segment, NewSegShed (future segment 34 with intensity Gamma (will be GammaOld then)) 
! If ReplaceByRing : then this segment is at the TE 
! Otherwise : it's the ShedVorticityConcentrationPoint on the TE trajectory 
call fAddSegment( IR, isp2,isp1,Gamma21,timeManager.itime , IR,iNewSegShed) 
cpt_SegShed=cpt_SegShed+1 ! counter of Shed segments
IR.IBufferNewSegShed(cpt_SegShed)=iNewSegShed
 
! PanlP as Control points 
! cpt_PP_te=cpt_PP_te+1; 
! IppForTEconv(cpt_PP_te)=iPs(4); 
 
! This is required for CreateTEAfter and for ConvectTEBefore 
 
! Propagation for next stripe 
P1=P2
isp1=isp2
end do ! end loop on span/stripe
! cpt_PP_te=cpt_PP_te+1; 
! IppForTEconv(cpt_PP_te)=iPs(3); 
end if !if emit
end do ! loop on patches
end do 
 
 
! Unused / old... 
! IR.IppForTEconv=IppForTEconv(1:cpt_PP_te); 
! IR.IcpForTEconv=IcpForTEconv(1:cpt_CP_te); 
! IR.IppFutureNW =IppForTEconv  
