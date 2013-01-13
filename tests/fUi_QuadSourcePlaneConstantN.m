function [ ui vi wi grad] = fUi_QuadSourcePlaneConstantN(Xcp,Ycp,Zcp,Sigmas,xi,eta,RefPoint,TransfoMat,I,Viscous,bComputeGrad)
% low level implementation
% takes flat vectors and returns flat vectors

% [ ui vi wi] = fUi_QuadSourcePlaneConstantN(1:4,4:7,7:10,0:5,0:5,0:5,0:5,0:5,0:5,0) 


% TODO I and viscous are not used yet


ncp=length(Xcp); %number of control points
np=size(xi,1); %number of panels


Ui=zeros(ncp,3); % columns for each component of velocity
V=zeros(ncp,3);  % Velocity for one panel on all CP
if bComputeGrad
    grad=zeros(ncp,9);
   %TODO  !!!
else
    grad=[];
end



%% This loop is vectorial for many control points..
for ip=1:np %loop on panels


    %param that are constant for each panel, distances and slopes - The slopes can be if divided by zero NaN => security required
    d12=sqrt((xi(ip,2)-xi(ip,1))^2+(eta(ip,2)-eta(ip,1))^2) ;
    d23=sqrt((xi(ip,3)-xi(ip,2))^2+(eta(ip,3)-eta(ip,2))^2) ;
    d34=sqrt((xi(ip,4)-xi(ip,3))^2+(eta(ip,4)-eta(ip,3))^2) ;
    d41=sqrt((xi(ip,1)-xi(ip,4))^2+(eta(ip,1)-eta(ip,4))^2) ;

    m12=(eta(ip,2)-eta(ip,1))/(xi(ip,2)-xi(ip,1));
    m23=(eta(ip,3)-eta(ip,2))/(xi(ip,3)-xi(ip,2));
    m34=(eta(ip,4)-eta(ip,3))/(xi(ip,4)-xi(ip,3));
    m41=(eta(ip,1)-eta(ip,4))/(xi(ip,1)-xi(ip,4));


    % transform control points in panel coordinate system using matrix - vectorial way
    A=squeeze(TransfoMat(ip,:,:));
    P0=RefPoint(ip,:); %coordinate of panel origin expressed in reference coordinates
    MP=[Xcp(:)-P0(1) Ycp(:)-P0(2) Zcp(:)-P0(3)];  % Pcp-P0 
    MPe = MP*A'; % transfo in element coordinate system, noted x,y,z, but in fact xi eta zeta
    
    % case where z is too small, jumps may occur
    eps_quadsource=10^-6; % !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
    Ierror=abs(MPe(:,3))<eps_quadsource ;

    % vectorial variables - Column vectors!
    r1=sqrt((MPe(:,1)-xi(ip,1)).^2 + (MPe(:,2)-eta(ip,1)).^2 + MPe(:,3).^2);
    r2=sqrt((MPe(:,1)-xi(ip,2)).^2 + (MPe(:,2)-eta(ip,2)).^2 + MPe(:,3).^2);
    r3=sqrt((MPe(:,1)-xi(ip,3)).^2 + (MPe(:,2)-eta(ip,3)).^2 + MPe(:,3).^2);
    r4=sqrt((MPe(:,1)-xi(ip,4)).^2 + (MPe(:,2)-eta(ip,4)).^2 + MPe(:,3).^2);

    e1=MPe(:,3).^2 + (MPe(:,1)-xi(ip,1)).^2; 
    e2=MPe(:,3).^2 + (MPe(:,1)-xi(ip,2)).^2; 
    e3=MPe(:,3).^2 + (MPe(:,1)-xi(ip,3)).^2; 
    e4=MPe(:,3).^2 + (MPe(:,1)-xi(ip,4)).^2; 
    
    h1=(MPe(:,2)-eta(ip,1)).*(MPe(:,1)-xi(ip,1));
    h2=(MPe(:,2)-eta(ip,2)).*(MPe(:,1)-xi(ip,2));
    h3=(MPe(:,2)-eta(ip,3)).*(MPe(:,1)-xi(ip,3));
    h4=(MPe(:,2)-eta(ip,4)).*(MPe(:,1)-xi(ip,4));

    % Velocities in element frame
    
    % log term 
    RJ12=1/d12 * log((r1+r2-d12)./(r1+r2+d12));
    RJ23=1/d23 * log((r2+r3-d23)./(r2+r3+d23));
    RJ34=1/d34 * log((r3+r4-d34)./(r3+r4+d34));
    RJ41=1/d41 * log((r4+r1-d41)./(r4+r1+d41));
    % Security - Katz Plotkin page 608 appendix D Code 11
    Ibad12= ( r1+r2-d12<=0  | d12 <=0 );
    Ibad23= ( r2+r3-d23<=0  | d23 <=0 );
    Ibad34= ( r3+r4-d34<=0  | d34 <=0 );
    Ibad41= ( r4+r1-d41<=0  | d41 <=0 );

    RJ12(Ibad12)=0;
    RJ23(Ibad23)=0;
    RJ34(Ibad34)=0;
    RJ41(Ibad41)=0;
%     
    % tan term
    if xi(ip,2)==xi(ip,1) % Security - Hess 1962 - page 47 - bottom
        TAN12=0; 
    else
        TAN12= atan((m12*e1-h1)./(MPe(:,3).*r1)) -  atan((m12*e2-h2)./(MPe(:,3).*r2)) ;
        TAN12(Ierror)= pi*fix((sign((m12*e1(Ierror)-h1(Ierror))) -  sign((m12*e2(Ierror)-h2(Ierror))))/2); % Security - Hess 1962 - page 47 - top
    end
    if xi(ip,3)==xi(ip,2)  % Security - Hess 1962 - page 47 - bottom
        TAN23=0; 
    else
        TAN23= atan((m23*e2-h2)./(MPe(:,3).*r2)) -  atan((m23*e3-h3)./(MPe(:,3).*r3)) ;
        TAN23(Ierror)= pi*fix((sign((m23*e2(Ierror)-h2(Ierror))) -  sign((m23*e3(Ierror)-h3(Ierror))))/2); % Security - Hess 1962 - page 47 - top
    end
    if xi(ip,4)==xi(ip,3) % Security - Hess 1962 - page 47 - bottom
        TAN34=0;
    else
        TAN34= atan((m34*e3-h3)./(MPe(:,3).*r3)) -  atan((m34*e4-h4)./(MPe(:,3).*r4)) ;
        TAN34(Ierror)= pi*fix((sign((m34*e3(Ierror)-h3(Ierror))) -  sign((m34*e4(Ierror)-h4(Ierror))))/2); % Security - Hess 1962 - page 47 - top
    end
    if xi(ip,1)==xi(ip,4)  % Security - Hess 1962 - page 47 - bottom
        TAN41=0;
    else
        TAN41= atan((m41*e4-h4)./(MPe(:,3).*r4)) -  atan((m41*e1-h1)./(MPe(:,3).*r1)) ;
        TAN41(Ierror)= pi*fix((sign((m41*e4(Ierror)-h4(Ierror))) -  sign((m41*e1(Ierror)-h1(Ierror))))/2); % Security - Hess 1962 - page 47 - top
    end
%     V(:,1)= Sigmas(ip)* (eta2(ip)-eta1(ip))/d12 * log((r1+r2-d12)./(r1+r2+d12)) + ... 
%                         (eta3(ip)-eta2(ip))/d23 * log((r2+r3-d23)./(r2+r3+d23)) + ... 
%                         (eta4(ip)-eta3(ip))/d34 * log((r3+r4-d34)./(r3+r4+d34)) + ... 
%                         (eta1(ip)-eta4(ip))/d41 * log((r4+r1-d41)./(r4+r1+d41)) ;
%     % Vy
%     V(:,2)= Sigmas(ip)* (xi2(ip)-xi1(ip))/d12 * log((r1+r2-d12)./(r1+r2+d12)) + ... 
%                         (xi3(ip)-xi2(ip))/d23 * log((r2+r3-d23)./(r2+r3+d23)) + ... 
%                         (xi4(ip)-xi3(ip))/d34 * log((r3+r4-d34)./(r3+r4+d34)) + ... 
%                         (xi1(ip)-xi4(ip))/d41 * log((r4+r1-d41)./(r4+r1+d41)) ;
%     % Vz
%     V(:,3)= Sigmas(ip)* (  atan((m12*e1-h1)./(MPe(:,3).*r1)) -  atan((m12*e2-h2)./(MPe(:,3).*r2)) ) + ...
%                         (  atan((m23*e2-h2)./(MPe(:,3).*r2)) -  atan((m23*e3-h3)./(MPe(:,3).*r3)) ) + ...
%                         (  atan((m34*e3-h3)./(MPe(:,3).*r3)) -  atan((m34*e4-h4)./(MPe(:,3).*r4)) ) + ...
%                         (  atan((m41*e4-h4)./(MPe(:,3).*r4)) -  atan((m41*e1-h1)./(MPe(:,3).*r1)) ) ;

    % Vx
    V(:,1)= Sigmas(ip)/(4*pi)*( (eta(ip,2)-eta(ip,1))*RJ12 + ... 
                                (eta(ip,3)-eta(ip,2))*RJ23 + ... 
                                (eta(ip,4)-eta(ip,3))*RJ34 + ... 
                                (eta(ip,1)-eta(ip,4))*RJ41   );
    % vy
    V(:,2)= Sigmas(ip)/(4*pi)*( (xi(ip,1)-xi(ip,2))  *RJ12 + ... 
                                (xi(ip,2)-xi(ip,3))  *RJ23 + ... 
                                (xi(ip,3)-xi(ip,4))  *RJ34 + ... 
                                (xi(ip,4)-xi(ip,1))  *RJ41   );
    % good hack
     Inear=( abs(Xcp-RefPoint(ip,1))<eps_quadsource & abs(Ycp-RefPoint(ip,2))<eps_quadsource &  abs(Zcp-RefPoint(ip,3))<eps_quadsource );
%     V(Inear,1)=0;
%     V(Inear,2)=0;

    % Vz
    V(:,3)= Sigmas(ip)/(4*pi)*( ( TAN12 ) + ...
                                ( TAN23 ) + ...
                                ( TAN34 ) + ...
                                ( TAN41 )   );
    % Going back to reference frame
    Ui=Ui+V*A; % summation for all panels
end %end panel loop

ui=Ui(:,1)';
vi=Ui(:,2)';
wi=Ui(:,3)';
% %% The for loop way
% ui=zeros(1,ncp);
% vi=zeros(1,ncp);
% wi=zeros(1,ncp);
% for icp=1:ncp
%     x0=[Xcp(icp) Ycp(icp) Zcp(icp)];
%     for ip=1:np
%         xp=[Xp(ip) Yp(ip) Zp(ip)];
%         U=cross([OmX(ip) OmY(ip) OmZ(ip)],x0-xp)/(4*pi*norm(x0-xp)^3);
%         ui(icp)=ui(icp)+U(1);
%         vi(icp)=vi(icp)+U(2);
%         wi(icp)=wi(icp)+U(3);
%     end
% end
