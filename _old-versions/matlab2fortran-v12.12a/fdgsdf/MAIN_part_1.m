InitClear

%% part 1
MFloater = 7466330;% kg
zCMFloater = -89.92; %m
IFloaterCM = 4229230000; % kg m2
MTower = 249718; %kg
zCMTower = 43.4;% m
ITowerCM= 1.217*10^8; %kg m2
MNacelle= 240 * 10^3; % kg
MRotor= 110 * 10^3; %kg
zHub= 90; %m
DSpar= 9.4; %m
zBot= -113.4; % m zBottom
Cm= 1.0;
CD= 0.60;
rhow = 1025; % kg/m3 %rho water!!!
g = 9.81; %m/s2
kMoor = 41180; %N/m
zMoor = -70; %m

zBuoy=zBot/2;


CM=Cm+1; %!!!!!!!!!!
%% Part1 Q1
Mtot=(MFloater+MTower+MNacelle+MRotor);
zG=((MFloater*zCMFloater)+(MTower*zCMTower)+(MNacelle*zHub)+(MRotor*zHub))/Mtot; %center of mass

% Huygens  for tower and Floater
IO= (ITowerCM+MTower*zCMTower^2) + (IFloaterCM+MFloater*zCMFloater^2) + (MNacelle*(zHub)^2) +(MRotor*(zHub)^2)    ;

% IO=(MFloater*zCMFloater^2)+(MTower*zCMTower^2)+(MNacelle*zHub^2)+(MRotor*zHub^2);

%% Q4
ASpar=pi*(DSpar/2)^2;
IASpar=pi*(DSpar/2)^4/4;

%% Q7
A=zeros(2,2);
M=zeros(2,2);
K=zeros(2,2);

M(1,1)= Mtot;
M(1,2)= Mtot*zG;
M(2,1)= M(1,2);
M(2,2)= IO;

A(1,1)= -rhow*Cm*ASpar*zBot;
A(1,2)= -rhow*Cm*ASpar*zBot^2/2; 
A(2,1)= A(1,2);
A(2,2)= -rhow*Cm*ASpar*zBot^3/3;

K(1,1)= kMoor;
K(1,2)= kMoor*zMoor;
K(2,1)= K(1,2);
K(2,2)= kMoor*(zMoor)^2 + rhow*g*IASpar+Mtot*g*(zBuoy-zG);

M
A
K

[v,fmat] = eig((M+A)\K) % M\K is the same than M^-1 * K
omega2  = diag(fmat)
% or simply
omega2 = eig((M+A)\K)

eigenfreq = sqrt(omega2)/(2*pi)
eigenT    = 1./eigenfreq





