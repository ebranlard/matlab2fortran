function [ ui vi wi grad] = fUi_QuadDoubletConstantN(Xcp,Ycp,Zcp,Mu,Panl,PanlP,Viscous,bComputeGrad)
% A Quad doublet with constnt strength is equivlant to a vortex filament of intensity Gamma=Mu all around the boundary of the quad

% low level implementation
% takes flat vectors and returns flat vectors

% [ ui vi wi] = fUi_QuadDoubletPlaneConstantN(1:4,4:7,7:10,0:5,0:5,0:5,0:5,0:5,0:5,0) 

% -----------------------------
include_ColumnsAndTypes;
% -----------------------------

ncp=length(Xcp); %number of control points
np=size(Panl,1); %number of panels


UI=zeros(ncp,3); % columns for each component of velocity
V=zeros(ncp,3);  % Velocity for one panel on all CP
if bComputeGrad
    Grad=zeros(ncp,9);
else
    Grad=[];
end

SmoothModel=Viscous.PanlSmoothModel;
SmoothParam=Viscous.PanlSmoothParam;


% we'll do a ring by ring computation...
nz=2;
nx=2;

bTrailedOnly=0;
%% Loop optimized/ vectorialized for many CPs but few panels.. Not efficient for influence matrix computation
% Optimization for matrix computation should be done somewhere else
% This function can be further improved by maybe looping on wings and patches and use LatticeAllCp 
% Maybe I could use the xi and eta coordinates as well..
for ip=1:np %loop on panels
    Gamma=Mu(ip); % because mu is constant on the panel
    IPs=Panl(ip,cpanCoord); % list of PanlPoints indexes
    P1=PanlP(IPs(1),cppCoord);
    P2=PanlP(IPs(2),cppCoord);
    P3=PanlP(IPs(3),cppCoord);
    P4=PanlP(IPs(4),cppCoord);
    X=[P1(1) P2(1) P4(1) P3(1)];
    Y=[P1(2) P2(2) P4(2) P3(2)];
    Z=[P1(3) P2(3) P4(3) P3(3)];
    [ui, vi, wi, grad] = fUi_LatticeAllCp( nz,  nx,  X ,  Y,  Z,  ncp,  Xcp,  Ycp,  Zcp,  Gamma,  SmoothModel, [SmoothParam(ip) SmoothParam(ip)],bComputeGrad,bTrailedOnly);
    UI(:,1:3)=UI(:,1:3)+[ui' vi' wi'];
    Grad=Grad+grad;
end %end panel loop
%% One day I'll have to decide which interface to use
grad=Grad;
ui=UI(:,1);
vi=UI(:,2);
wi=UI(:,3);
