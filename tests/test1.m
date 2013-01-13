% Now the part where we add the incomplete Shed Segment, Gamma=0 at the "TE"
if isequal(EmissionMethod,'ConvectTEAfterSolving_WithPanlP') 
    imap1=4;
elseif isequal(EmissionMethod,'CreateTEBeforeSolving');
    imap1=4;
elseif isequal(EmissionMethod,'CreateTEBeforeSolvingAndReplacePanel');
    imap2=2;
end

IR.IBufferNewSegShed   =zeros(1,Data1.nTotSideEmit);    % Index of Shed Sgmt created along the TE, part of the buffer ring
IR.IBufferNewSegTrailed=zeros(1,Data1.nTotSideEmit+1);  % Index of Trailed Sgmt created along the TE, part of the buffer ring

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
                [IR isp2]=fAddSegmentPoints(IR,P2,1,ispanCum,timeManager.itime); %1 for convection

                % Otherwise :        it's the ShedVorticityConcentrationPoint on the TE trajectory
                [IR iNewSegShed]=fAddSegment(IR, isp2,isp1,Gamma21,timeManager.itime);
                cpt_SegShed=cpt_SegShed+1;   % counter of Shed segments
                IR.IBufferNewSegShed(cpt_SegShed)=iNewSegShed;
            end % end loop on span/stripe
        end %if emit
    end % loop on patches
end

