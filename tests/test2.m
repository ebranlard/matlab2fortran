% Now the part where we add the incomplete Shed Segment, Gamma=0 at the "TE"
% The interest of adding those segments is that they will be convected.  
% The index of those segments are stored in IBufferNewSegShed, these segments are attributed an updated value later, so that there are not incomplete anymore.


% TODO: the code below is quite redundant with Part_Emit... Maybe with a little if t=0 they could be combined..


% The imap are convenient index that allows the method to be dealt with the same way
if isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP') 
    % The future points 1 and 2 of the "Buffer vortex rings" are actually points 4 and 3 of the TE panel
    imap1=4;
    imap2=3;
elseif isequal(EmissionMethod,'CreateTEBeforeSolving');
    % The future points 1 and 2 of the "Buffer vortex rings" are points 4 and 3 of the NW panel..
    imap1=4;
    imap2=3;
elseif isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel');
    % The future points 1 and 2 of the "Buffer vortex rings" are points 1 and 2 of the NW panel..
    imap1=1;
    imap2=2;
end

IR.IBufferNewSegShed   =zeros(1,Data1.nTotSideEmit);    % Index of Shed Sgmt created along the TE, part of the buffer ring
IR.IBufferNewSegTrailed=zeros(1,Data1.nTotSideEmit+1);  % Index of Trailed Sgmt created along the TE, part of the buffer ring
%     IppForTEconv=zeros(1,Data1.nTotSideEmit);  % Index of panel  | points used as control points for convecting the TE , pp=panel points
%     IcpForTEconv=zeros(1,Data1.nTotSideEmit);  % Index of control|                                                     , cp=control points 
%     cpt_PP_te=0;   % counter of the number of panel   points that will be used for computing the convection of the TE
%     cpt_CP_te=0;   % counter of the number of control points that will be used for computing the convection of the TE
cpt_SegShed=0;   % counter of TE segments
ispanCum=0;     % increased index in "x" direction, almost compatible with former lattice. Before for instance X(the coordinates of the segmetns points) would have dimensions nx*nz, can be removed in the future
iNW=0;           % accumulation over "NW elements", either a NW panels or a NW incomplete segment from last time step.
% keyboard
for iw=1:length(IR.Wings)
    for ipatch=1:length(IR.Wings(iw).Patches); % loop on Patches
        % TODO leading edge and TE emission
        if IR.Wings(iw).Patches(ipatch).bEmit % Ok will emit
            bThick=IR.Wings(iw).Patches(ipatch).bThick;
            for is=1:IR.Wings(iw).Patches(ipatch).nSpan %loop on span for TE panels

                % -----------------------  PANEL index ip and Panel Points iPs ----------------------------------------
                if isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP') 
                    % In this case the panel used is the last panel of the stripe
                    ip=IR.Wings(iw).Patches(ipatch).FirstPanel+is*(IR.Wings(iw).Patches(ipatch).nChord)-1;% last panel next to TE (top one if Thick)
                elseif isequal(EmissionMethod,'CreateTEBeforeSolving') || isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel')
                    % we use a NW panel
                    iNW=iNW+1;
                    ip=IR.IpanelsNW(iNW);
                end
                iPs=IR.Panl(ip,cpanCoord); % the four points indexes of either the last panel or the NW panel

                % ------------------------ DETERMINATION OF GAMMA ----------------------------------------
                Gamma21=0;

                % ------------------------ POINTS DEFINITIONS - ADDING Point 2 (and point 1 at beginning of stripe) of Buffer Ring----------
                if(is==1)
                    ispanCum=ispanCum+1;
                    % Point 1 of the "buffer vortex ring"
                    % If ReplaceByRing : P1=P1_NW_panel (remeber this NW panel is replace by rings at this stage, the convection has not occured)
                    % If CreateBefore :  P1=P4_NW_panel ( the convection should have occured on the segShedBufferOld)
                    % If ConvectTE    :  P1=P4_last_wing_panel (the convection should have occured)
                    P1=IR.PanlP(iPs(imap1),1:3); 
                    [IR isp1]=fAddSegmentPoints(IR,P1,1,ispanCum,timeManager.itime); %1 for convection
                end 
                ispanCum=ispanCum+1;

                % Point 2 of the "buffer vortex ring": see comments for point 1 above
                P2=IR.PanlP(iPs(imap2),1:3); 
                [IR isp2]=fAddSegmentPoints(IR,P2,1,ispanCum,timeManager.itime); %1 for convection

                % --------------------------  CREATING SEGMENTS  of the Buffer Ring ----------------------------------------
                % Adding the incomplete shed segment, NewSegShed (future segment 34 with intensity Gamma (will be GammaOld then))
                % If ReplaceByRing : then this segment is at the TE
                % Otherwise :        it's the ShedVorticityConcentrationPoint on the TE trajectory
                [IR iNewSegShed]=fAddSegment(IR, isp2,isp1,Gamma21,timeManager.itime);
                cpt_SegShed=cpt_SegShed+1;   % counter of Shed segments
                IR.IBufferNewSegShed(cpt_SegShed)=iNewSegShed;

                % PanlP as Control points 
                %                 cpt_PP_te=cpt_PP_te+1;
                %                 IppForTEconv(cpt_PP_te)=iPs(4);

                % This is required for CreateTEAfter and for ConvectTEBefore

                % Propagation for next stripe 
                P1=P2;
                isp1=isp2;
            end % end loop on span/stripe
            %             cpt_PP_te=cpt_PP_te+1;
            %             IppForTEconv(cpt_PP_te)=iPs(3);
        end %if emit
    end % loop on patches
end


%     Unused / old...
%     IR.IppForTEconv=IppForTEconv(1:cpt_PP_te);
%     IR.IcpForTEconv=IcpForTEconv(1:cpt_CP_te);
%     IR.IppFutureNW =IppForTEconv  % Index of panel points that represent the TE and that will be convected to form the near wake (NW)

