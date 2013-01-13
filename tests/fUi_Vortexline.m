function Uout=fUi_Vortexline(xa, ya, za, xb, yb, zb, visc_model , t,bComputeGrad) 
% !!!!! No intensity!!!
    norm_a = sqrt(xa*xa + ya*ya + za*za);
    norm_b = sqrt(xb*xb + yb*yb + zb*zb);
    denominator =  norm_a*norm_b*(norm_a*norm_b + xa*xb+ya*yb+za*zb)  ;
    crossprod = [ya*zb-za*yb; za*xb-xa*zb; xa*yb-ya*xb];

    if bComputeGrad
        Uout = [0;0;0;0;0;0;0;0;0;0;0;0];
    else
        Uout = [0;0;0];
    end
    % check for singularity */
    if denominator < 0.000000000001 && (visc_model<4 || visc_model>5)
        return;
    elseif (norm_a < 0.00001 || norm_b < 0.00001 )  && (visc_model< 4)
        return;
    else
    % viscous model */
    Kv=1.0;
    switch(visc_model)
        case 0 % No vortex core model 
            Kv=1.0;
        case 1 %Rankine  - t<=>rc 
            norm_r0 = sqrt((xa-xb)*(xa-xb) + (ya-yb)*(ya-yb) +(za-zb)*(za-zb));
            h = sqrt(crossprod(1)*crossprod(1)+crossprod(2)*crossprod(2)+crossprod(3)*crossprod(3))/ norm_r0; % orthogonal distance r1xr2/r0 */
            if(h<t)
                Kv=h*h/t/t;
            else
                Kv=1.0;
            end
        case 2 %Lamb-Oseen  - t<=>rc */
            norm_r0 = sqrt((xa-xb)*(xa-xb) + (ya-yb)*(ya-yb) +(za-zb)*(za-zb));
            h = sqrt(crossprod(1)*crossprod(1)+crossprod(2)*crossprod(2)+crossprod(3)*crossprod(3))/ norm_r0; % orthogonal distance r1xr2/r0 */
            Kv = 1.0-exp(-1.25643*h*h/t/t);
        case 3 %Vatistas n=2  - t<=>rc */
            norm_r0 = sqrt((xa-xb)*(xa-xb) + (ya-yb)*(ya-yb) +(za-zb)*(za-zb));
            h = sqrt(crossprod(1)*crossprod(1)+crossprod(2)*crossprod(2)+crossprod(3)*crossprod(3))/ norm_r0; % orthogonal distance r1xr2/r0 */
%             h = (norm_a+norm_b)/2;
            Kv = h*h/sqrt(t^4+h^4);
        case 4 %Cut-off radius no dimensions - t<=>delta^2 */
            norm_r0 = sqrt((xa-xb)*(xa-xb) + (ya-yb)*(ya-yb) +(za-zb)*(za-zb));
            Kv=1.0;
            % delta*norm(r0)^2 */
            denominator=denominator+t*norm_r0;
        case 5 %Cut-off radius dimension  - t<=>(delta l)^2 */
            Kv=1.0;
            % (delta l_0)^2 */
            denominator=denominator+t;
        case 33 %Vatistas n=2  - t<=>rc */
%             norm_r0 = sqrt((xa-xb)*(xa-xb) + (ya-yb)*(ya-yb) +(za-zb)*(za-zb));
%             h = sqrt(crossprod(1)*crossprod(1)+crossprod(2)*crossprod(2)+crossprod(3)*crossprod(3))/ norm_r0; % orthogonal distance r1xr2/r0 */
            h = (norm_a+norm_b)/2;
            Kv = h*h/sqrt(t^4+h^4);
    end
    Kv=Kv/4.0/pi*(norm_a+norm_b)/denominator;
    Uout(1:3) = Kv*crossprod;

    if bComputeGrad
        d=(norm_a*norm_b + xa*xb+ya*yb+za*zb);
        if abs(d)>0.000000001 
            ra=[xa;ya;za];
            rb=[xb;yb;zb];
            D=-(ra/norm_a^3+rb/norm_b^3)-(1/norm_a+1/norm_b)*1/d*(ra/norm_a *norm_b+ rb/norm_b*norm_a+ ra+rb);
            
            GradRaRb=zeros(3,3);
            GradRaRb(1,2)=za-zb;
            GradRaRb(2,1)=zb-za;
            GradRaRb(1,3)=yb-ya;
            GradRaRb(3,1)=ya-yb;
            GradRaRb(2,3)=xa-xb;
            GradRaRb(3,2)=xb-xa;


            GradU=1/(4*pi)*1/d*( -(1/norm_a+1/norm_b)*GradRaRb+crossprod*(D'));
            Uout(3+(1:3))=GradU(1,1:3);
            Uout(6+(1:3))=GradU(2,1:3);
            Uout(9+(1:3))=GradU(3,1:3);
        end
%          keyboard

% X1=crossprod(1);
% X2=crossprod(2);
% X3=crossprod(3);
% na=norm_a;
% nb=norm_b;
% t11=(-1).*d.^(-1).*na.^(-3).*nb.^(-1).*(na+nb).*X1.*xa+(-1).*d.^(-1).*na.^(-1).*nb.^(-3).*(na+nb).*X1.*xb+d.^(-1).*na.^(-1).*nb.^(-1).*X1.*(na.^(-1).*xa+nb.^(-1).*xb)+(-1).*d.^(-2).*na.^(-1).*nb.^(-1).*(na+nb).*X1.*(xa+na.^(-1).*nb.*xa+xb+na.*nb.^(-1).*xb);
% t12=(-1).*d.^(-1).*na.^(-3).*nb.^(-1).*(na+nb).*X1.*ya+(-1).*d.^(-1).*na.^(-1).*nb.^(-3).*(na+nb).*X1.*yb+d.^(-1).*na.^(-1).*nb.^(-1).*X1.*(na.^(-1).*ya+nb.^(-1).*yb)+(-1).*d.^(-2).*na.^(-1).*nb.^(-1).*(na+nb).*X1.*(ya+na.^(-1).*nb.*ya+yb+na.*nb.^(-1).*yb)+d.^(-1).*na.^(-1).*nb.^(-1).*(na+nb).*((-1).*za+zb);
% t21=(-1).*d.^(-1).*na.^(-3).*nb.^(-1).*(na+nb).*X2.*xa+(-1).*d.^(-1).*na.^(-1).*nb.^(-3).*(na+nb).*X2.*xb+d.^(-1).*na.^(-1).*nb.^(-1).*X2.*(na.^(-1).*xa+nb.^(-1).*xb)+(-1).*d.^(-2).*na.^(-1).*nb.^(-1).*(na+nb).*X2.*(xa+na.^(-1).*nb.*xa+xb+na.*nb.^(-1).*xb)+d.^(-1).*na.^(-1).*nb.^(-1).*(na+nb).*(za+(-1).*zb);
% t22=(-1).*d.^(-1).*na.^(-3).*nb.^(-1).*(na+nb).*X2.*ya+(-1).*d.^(-1).*na.^(-1).*nb.^(-3).*(na+nb).*X2.*yb+d.^(-1).*na.^(-1).*nb.^(-1).*X2.*(na.^(-1).*ya+nb.^(-1).*yb)+(-1).*d.^(-2).*na.^(-1).*nb.^(-1).*(na+nb).*X2.*(ya+na.^(-1).*nb.*ya+yb+na.*nb.^(-1).*yb);
% t11=t11*1/(4*pi);
% t12=t12*1/(4*pi);
% t21=t21*1/(4*pi);
% t22=t22*1/(4*pi);
% 
    end
        if sum(isnan(Uout))>0
            kbd
        end

end
    %    printf("%4.3f %4.3f %4.3f %4.3f %4.3f\n",Uout(1),Uout(2),Uout(3),Kv,denominator); */

